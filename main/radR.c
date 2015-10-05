/*  svn $Id: radR.c 803 2011-06-19 00:42:21Z john $

    radR : an R-based platform for acquisition and analysis of radar data
    Copyright (C) 2006-2009 John Brzustowski        

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    Utility functions for radR.

*/
#ifdef Win32
#include <windows.h>
#include <winbase.h>
#endif
#include <stdlib.h>
#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <time.h>
#include <errno.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include "radR.h"
#include "radRvars.h"
#include "patchify.h"
#include "random.h"
#include "radRprot.h"
#include "extmatimg_decl.h"
#include "scancvt.h"

#define DEG_TO_RAD(X) ((X) * M_PI / 180.0)
#define RAD_TO_DEG(X) ((X) * 180.0 / M_PI)
#define COMPASS_TO_MATH(X) DEG_TO_RAD(90.0  - (X))
#define MATH_TO_COMPASS(X) (90.0 - RAD_TO_DEG(X))
#define SQUARE(x) ({typeof(x) __x = x; __x*__x;})

// convert (compass) spherical coordinates to cartesian coordinates
#define SPH_TO_RECT(AZI, ELEV, X, Y, Z) ({ double AZI__ = AZI, ELEV__ = ELEV; \
					 X = cos(DEG_TO_RAD(ELEV__)) * cos(COMPASS_TO_MATH(AZI__)); \
					 Y = cos(DEG_TO_RAD(ELEV__)) * sin(COMPASS_TO_MATH(AZI__)); \
					 Z = sin(DEG_TO_RAD(ELEV__));})

void
rpv (SEXP s) {
  // like R_PV but don't test for objecthood
  PrintValue(s);
}

// #ifdef RADR_DEBUG
SEXP
int_wrapped_pointer_to_sexp(SEXP s) {
  // return the object corresponding to the address
  // which is represented as an integer in s

  // DANGEROUS!! and only works on systems where sizeof(int) == sizeof(void *)
  
  return (SEXP) INTEGER(AS_INTEGER(s))[0];
}
// #endif

/* 

An atomic (with respect to tcltk) operation for setting the value of
a global variable and returning its previous value. This is to avoid the
race situation:

  1. radR event loop gets value of variable X
  2. tcltk sets value of variable X
  3. radR event loop resets value of variable X

in which radR loses the value of X set by tcltk in step 2.  Setting
such a global variable X is the method by which the GUI tells the radR
event loop to start/stop play.

Under Windows (and Aqua?): by not calling R's eval(), this function
avoids calling R_ProcessEvents (via R_CheckUserInterrupt) and so no
tcltk events are processed between entering and exiting this function.

Under unix: the way R currently runs tcltk, this function is not
needed.  R evals() are not "interrupted" by the tcltk events, and so
the get and set from R itself is atomic w.r.t. tcltk.  For
consistency, we nonetheless use this function under unix.

*/

SEXP
radR_get_and_set(SEXP symbol, SEXP value, SEXP rho)
{
  // var.name is a character vector giving the name of the symbol
  // val is the value to set it to
  // returns the previous value of the symbol

  SEXP rv, name;

  if (!isString(symbol) || length(symbol) == 0)
	error("first argument is not a symbol name");
  PROTECT(name = install(CHAR(STRING_ELT(symbol, 0))));
  if (TYPEOF(rho) == NILSXP) {
    warning("use of NULL environment is deprecated");
    rho = R_BaseEnv;
  } else if (TYPEOF(rho) != ENVSXP) {
    error("third argument is not an environment");
  }
  PROTECT(rv = findVar(name, rho));
  if (rv == R_UnboundValue)
    error("first argument names an unbound symbol in the environment");
  setVar(name, value, rho);
  UNPROTECT(2);
  return rv;
}


#ifdef WIN32
extern __declspec(dllimport) void (* R_tcldo)();
#endif

SEXP
radR_critical_eval(SEXP expr, SEXP rho)
{
/* 
   radR_critical_eval: evaluate an R expression in an environment
   without interference from other "threads".

   Win32:  just disable the callbacks to 
   Tcl_ServiceAll() that occur at regular intervals in R's eval()

   unix:   just call eval as usual
*/   

  SEXP rv;
#ifdef WIN32
  void (*_save_R_tcldo)();

  _save_R_tcldo = R_tcldo;
#endif

  rv = eval(expr, rho);

#ifdef WIN32
  R_tcldo = _save_R_tcldo;
#endif

  return rv;
}



/*===================================================================

  Hooks: R functions called with data at various stages of processing

=====================================================================*/

#define MAX_R_FUNCTION_ARGS 32

/*
   call an R function passing it a sequence of up to 
   MAX_R_FUNCTION_ARGS protected SEXPs (typically t_extmat wrapped as EXTPTR_SEXPs with class("extmat"))

   name - the name of the R function
   env  - the environment in which to evaluate the function call
   ...  - the list of SEXP arguments, already protected, terminated by a NULL

   This function is used by the macro call_R_hooks.
*/

SEXP
call_R_function(char *name, SEXP env, ...)
{

  va_list ap;

  SEXP args[MAX_R_FUNCTION_ARGS];
  SEXP rv;
  SEXP s, t;
  int i;
  int m;

  /* grab the SEXP parameters */

  va_start(ap, env);

  for (i = 0; i < MAX_R_FUNCTION_ARGS; ++i) {
    if (!(args[i] = va_arg(ap, SEXP)))
      break;
  }

  m = i;

  va_end(ap);

  /* build the call expression */

  PROTECT(t = s = allocList(m + 1));
  SET_TYPEOF(s, LANGSXP);
  SETCAR(t, install(name)); t = CDR(t);
  for (i = 0; i < m; ++i, t=CDR(t))
    SETCAR(t, args[i]);

  /* evaluate the function body in the closure environment */

  rv = eval(s, env);

  /* unprotect the parameter list */

  UNPROTECT(1);
  return(rv);
}

/*
   create a REALSXP, INTSXP, or LOGICALSXP for an arbitrary number of passed arguments.

   name - the name of the R function
   ...  - the list of double or int args (logicals are passed as ints)

*/

SEXP
make_R_vector(int sxp_type, int n, ... )
{

  va_list ap;

  SEXP rv;
  int i;

  /* grab the SEXP parameters */

  PROTECT(rv = allocVector(sxp_type, n));
  va_start(ap, n);

  for (i = 0; i < n; ++i) {
    switch(sxp_type) {
    case INTSXP:
      INTEGER(rv)[i] = va_arg(ap, int);
      break;
    case LGLSXP:
      LOGICAL(rv)[i] = va_arg(ap, int);
      break;
    case REALSXP:
      REAL(rv)[i] = va_arg(ap, int);
      break;
    default:
      break;
    }
  }
    
  UNPROTECT(1);
  return(rv);
}


/*
   determine whether there is at least one defined and enabled function
   for the given hook type.

   hook - the integer corresponding to the hook type; must 
              be one of the RADR_HOOK... constants defined in radRshared.h
*/

int
is_R_hook_active(int hook) 
{

  SEXP rv;
  SEXP s, t;

  /* build the call expression */

  PROTECT(t = s = allocList(2));
  SET_TYPEOF(s, LANGSXP);
  SETCAR(t, install("rss.hook.is.active")); t = CDR(t);
  PROTECT(rv = allocVector(INTSXP, 1));
  INTEGER(rv)[0] = hook;
  SETCAR(t, rv);

  /* evaluate the function body in the closure environment */

  rv = eval(s, R_GlobalEnv);

  /* unprotect the parameter wrappers */

  UNPROTECT(2);
  return LOGICAL(rv)[0];
}


/*================================================================

  functions to process radar scans

  ================================================================*/

// an integer rounding division macro computes floor((x + floor(y/2))/y)

#define RNDIV(X, Y) ({typeof(Y) __y = (Y); ((X) + __y / 2) / __y;})

SEXP
radR_update_stats(SEXP scansxp, SEXP classsxp, SEXP celldims, SEXP meansxp, SEXP devsxp, SEXP ksxp, SEXP modesxp, SEXP numlearningscanssxp) {

  // update moving averages of cell means and deviations
  // called after samples have been classified as hot or cold
  // Parameters;
  //  scanxp is the EXTMAT of scan data
  //  classsxp is the EXTMAT of class data
  //          if not equal to R_NilValue, it is used to ignore samples of class BLIP in updating
  //          cell means and deviances
  //  celldims is num pulses, num samples in a cell
  //  meansxp is the EXTMAT for means
  //  devsxp is the EXTMAT for deviance
  //  ksxp is the decay constant for moving averages
  //  modesxp is an integer:  INIT_ACCUM = first accumulation of totals; mean only
  //                          CONT_ACCUM = continuing accumulation of totals; mean and SD
  //                          LAST_ACCUM = final accumulation of totals; mean and SD are divided by the learning
  //                          UPDATE = use decay constant to blend this scan's mean and SD with existing values
  // numlearningscanssxp: integer scalar: number of scans learned so far, not counting current call;
  //                          only used when mode is LAST_ACCUM
  // this function ensures that mean and dev are large 
  // enough to hold the data corresponding to cells of
  // size celldims and data in scan

  int i;			// scan buffer index
  int j, k;			// general indices
  int cellr, cellc;		// cell row and column indices
  short cols;			// number of cell columns
  short rows;			// number of cell rows
  short cols_1, rows_1;		// cols and rows minus 1
  short cellw;			// cell width
  short cellh;			// cell height
  short cellw_r;		// width of rightmost partial cells
  short cellh_b;		// height of bottom partial cells
  int *cellsum; 		// sum of samples in cells of one row
  int *celldevsum;       	// sum of sample deviances in cells of one row
  int *cellcount = NULL;        // number of samples in each cell of one row (when excluding blip samples)
  static int *cellmeancurr = NULL;      // mean of cell sums in cells of one row 
  static int cellmeancurr_n = 0;        // size of cellmeancurr array
  int samples;                  // number of samples per cell
  unsigned k1, k2, kbits;
  unsigned khalf;
  int num_learning_scans;
  t_stats_mode mode;

  t_sample *scan_buff;
  t_class *class_buff;
  t_frame_cell *cellmean, *celldev;
  t_extmat *scan, *mean, *dev;

  celldims = AS_INTEGER(celldims);
  cellw = INTEGER(celldims)[0];
  cellh = INTEGER(celldims)[1];
  
  mode = INTEGER(AS_INTEGER(modesxp))[0];

  // add 1 to number of scans learned, because this call is included
  num_learning_scans = 1 + INTEGER(AS_INTEGER(numlearningscanssxp))[0];

  // get the passed external matrices 

  scan = SEXP_TO_EXTMAT(scansxp);
  mean = SEXP_TO_EXTMAT(meansxp);
  dev = SEXP_TO_EXTMAT(devsxp);

  scan_buff = (t_sample *) scan->ptr;

  // if a non-null classsxp has been passed,
  // get the buffer pointer
  if (isNull(classsxp)) {
    class_buff = NULL;
  } else {
    class_buff = (t_class *) SEXP_TO_EXTMAT(classsxp)->ptr;
  }

  // cells in the rightmost column might have a different width (fewer samples)
  cellw_r = (scan->cols % cellw) ?: cellw;

  // cells in the bottom row might have a different height (fewer pulses)
  cellh_b = (scan->rows % cellh) ?: cellh;

  rows = ROUND_UP_TO(scan->rows, cellh) / cellh; 
  cols = ROUND_UP_TO(scan->cols, cellw) / cellw; 

  (*pensure_extmat)(mean, rows, cols);
  (*pensure_extmat)(dev, rows, cols);

  cellmean = (t_frame_cell *) mean->ptr;
  celldev = (t_frame_cell *) dev->ptr;

  cols_1 = cols - 1; rows_1 = rows - 1;
  cellsum = (int *) malloc(cols * sizeof(int));
  celldevsum = (int *) malloc(cols * sizeof(int));
  if (class_buff)
    cellcount = (int *) malloc(cols * sizeof(int));

  // keep an array for cell means in current scan.  FIXME: this is never freed
  if (! cellmeancurr || cellmeancurr_n < cols) {
    if (cellmeancurr) {
      free(cellmeancurr);
      cellmeancurr = NULL;
      cellmeancurr_n = 0;
    }
    cellmeancurr = (int *) malloc(cols * sizeof(int));
    cellmeancurr_n = cols;
  }

  samples = cellw * cellh;

  // for speed, set up an integer framework for the moving average decay constant
  kbits = 12;
  khalf = 1 << (kbits - 1);
  k1 = floor(REAL(AS_NUMERIC(ksxp))[0] * (1 << kbits));
  k2 = (1 << kbits) - k1;

  i = 0;

  for (cellr = 0; cellr < rows; ++cellr) {
    // if this is the bottom row, set the cell height and number of
    // samples to the correct values

    if (cellr == rows_1) {
      cellh = cellh_b;
      samples = cellh * cellw;
    }

    // zero sums for this row of cells

    memset(cellsum, 0, sizeof(int) * cols);
    memset(celldevsum, 0, sizeof(int) * cols);

    // process this row of cells
        
    // calculate this scan's mean and dev in each cell, if possible

    if (mode != UPDATE || !class_buff) {

      // compute cellsums for this row of cells using all samples

      for (k = 0; k < cellh; ++k) {			/* each row of samples in this row of cells */
	for (cellc = 0; cellc < cols_1; ++cellc)	/* each cell in this row */
	  for (j = 0; j < cellw; ++j, ++i)		/* each sample in this row of this cell */    
	    cellsum[cellc] += scan_buff[i];
	for (j = 0; j < cellw_r; ++j, ++i)		/* rightmost cell in this row */
	  cellsum[cellc]   += scan_buff[i];
      }
      // compute the mean for each cell in this scan
      // and use it to calculate the 
      // deviances for samples in this row of cells

      for (cellc = 0; cellc < cols_1; ++cellc)   /* each cell in this row */
	cellmeancurr[cellc] = RNDIV(cellsum[cellc], samples);
      cellmeancurr[cellc] = RNDIV(cellsum[cellc], cellw_r * cellh); /* last cell has different size */

      // send i back to the first sample in this row of cells
      i -= cellh * scan->cols;

      // compute the absolute deviation of each sample from the mean
      // for its cell
      for (k = 0; k < cellh; ++k) {                              /* each row of samples in this row of cells */
	for (cellc = 0; cellc < cols_1; ++cellc)	          /* each cell in this row */                   
	  for (j = 0; j < cellw; ++j, ++i) {			  /* each sample in this row of this cell */    
	    celldevsum[cellc] += iabs(scan_buff[i] - cellmeancurr[cellc]);
	  }
	for (j = 0; j < cellw_r; ++j, ++i) {			  /* rightmost cell in this row */
	  celldevsum[cellc] += iabs(scan_buff[i] - cellmeancurr[cellc]);
	}
      }
    } else {
      // compute cellsums for this row of cells using non-blip samples

      memset(cellcount, 0, sizeof(int) * cols);

      for (k = 0; k < cellh; ++k) {                               /* each row of samples in this row of cells */
	for (cellc = 0; cellc < cols_1; ++cellc)	          /* each cell in this row */                   
	  for (j = 0; j < cellw; ++j, ++i) {			  /* each sample in this row of this cell */
	    if (class_buff[i] != CLASS_BLIP) {
	      cellsum[cellc] += scan_buff[i];
	      ++cellcount[cellc];
	    }
	  }
	for (j = 0; j < cellw_r; ++j, ++i) {			  /* rightmost cell in this row */
	  if (class_buff[i] != CLASS_BLIP) {
	    cellsum[cellc] += scan_buff[i];
	    ++cellcount[cellc];
	  }
	}
      }

      // compute the mean for each cell in this scan
      // We skip cells where there are no non-blip samples

      for (cellc = 0; cellc < cols_1; ++cellc)   /* each cell in this row */
	if (cellcount[cellc]) 
	  cellmeancurr[cellc] = RNDIV(cellsum[cellc], cellcount[cellc]);
      if (cellcount[cellc])
	  cellmeancurr[cellc] = RNDIV(cellsum[cellc], cellw_r * cellh); /* last cell has different size */


      // send i back to the first sample in this row of cells
      i -= cellh * scan->cols;

      // Use the means for this scan to calculate the 
      // deviances for samples in this row of cells

      for (k = 0; k < cellh; ++k) {           /* each row of samples in this row of cells */
	for (cellc = 0; cellc < cols_1; ++cellc) {	  /* each cell in this row */                   
	  for (j = 0; j < cellw; ++j, ++i) {			  /* each sample in this row of this cell */
	    if (class_buff[i] != CLASS_BLIP)
	      celldevsum[cellc] += iabs(scan_buff[i] - cellmeancurr[cellc]);
	  }
	}
	for (j = 0; j < cellw_r; ++j, ++i) {			  /* rightmost cell in this row */
	  if (class_buff[i] != CLASS_BLIP)
	    celldevsum[cellc] += iabs(scan_buff[i] - cellmeancurr[cellc]);
	}
      }
    }

    // update running versions of mean and dev, if possible

    switch(mode) {
    case INIT_ACCUM:
      // first scan, we get only the mean, and set the deviance to 0
      for (cellc = 0; cellc < cols; ++cellc, ++cellmean, ++celldev) {
	*cellmean = cellsum[cellc];
	*celldev = celldevsum[cellc];
      }
      break;

    case CONT_ACCUM:
      // on subsequent accumulation scans, we add in the cell means and deviations to running totals
      for (cellc = 0; cellc < cols; ++cellc, ++cellmean, ++celldev) {
	*cellmean += cellsum[cellc];
	*celldev  += celldevsum[cellc];
      }
      break;

    case LAST_ACCUM:
      // on the last accumulation scan, we add in current cell means and deviations, then
      // divide each cell by (samples_in_cell * num_learning_scans)
      // to get the mean sample value and absolute sample deviation in each cell over the learning phase
      for (cellc = 0; cellc < cols_1; ++cellc, ++cellmean, ++celldev) {
	*cellmean = RNDIV(*cellmean + cellsum[cellc],  samples * num_learning_scans);
	*celldev  = RNDIV(*celldev  + celldevsum[cellc], (samples - 1) * num_learning_scans);
      }
      *cellmean = RNDIV(*cellmean + cellsum[cellc],    cellw_r * cellh * num_learning_scans);
      *celldev  = RNDIV(*celldev +  celldevsum[cellc], (cellw_r * cellh - 1) * num_learning_scans);
      ++cellmean;
      ++celldev;
      break;

    case UPDATE:
      // subsequently, we have both new and existing deviations and means, and do updates for both
      // using the decay constant k
      if (class_buff) {
	// use the counts of non-blip samples
	for (cellc = 0; cellc < cols_1; ++cellc, ++cellmean, ++celldev) {
	  if (cellcount[cellc] > 0) {
	    *cellmean = ((unsigned)(k1 * (*cellmean) + k2 * RNDIV(cellsum[cellc],    cellcount[cellc]) + khalf)) >> kbits;
	    if (cellcount[cellc] > 1)
	      *celldev  = ((unsigned)(k1 *  (*celldev) + k2 * RNDIV(celldevsum[cellc], cellcount[cellc] - 1) + khalf)) >> kbits;
	  }
	}
	if (cellcount[cellc] > 0) {
	  *cellmean = ((unsigned)(k1 * (*cellmean) + k2 * RNDIV(cellsum[cellc],     cellcount[cellc]) + khalf)) >> kbits;
	  if (cellcount[cellc] > 1)
	    *celldev  = ((unsigned)(k1 *  (*celldev) + k2 * RNDIV(celldevsum[cellc],  cellcount[cellc] - 1) + khalf)) >> kbits;
	}
      } else {
	// use the counts of all samples
	for (cellc = 0; cellc < cols_1; ++cellc, ++cellmean, ++celldev) {
	  *cellmean = ((unsigned)(k1 * (*cellmean) + k2 * RNDIV(cellsum[cellc],    samples) + khalf)) >> kbits;
	  *celldev  = ((unsigned)(k1 *  (*celldev) + k2 * RNDIV(celldevsum[cellc], samples - 1) + khalf)) >> kbits;
	}
	*cellmean = ((unsigned)(k1 * (*cellmean) + k2 * RNDIV(cellsum[cellc],    cellw_r * cellh) + khalf)) >> kbits;
	*celldev  = ((unsigned)(k1 *  (*celldev) + k2 * RNDIV(celldevsum[cellc], cellw_r * cellh - 1) + khalf)) >> kbits;
      }
      ++cellmean;
      ++celldev;

      break;
    }
  } /* next row of cells */
  free(cellsum);
  free(celldevsum);
    
  call_R_hooks(RADR_HOOK_STATS, meansxp, devsxp, NULL);
  return PASS_SEXP;
}

SEXP
radR_classify_samples(SEXP scoresxp, SEXP classsxp, SEXP prevclasssxp, SEXP threshsxp) {
  // for each score, determine whether it is hot
  // i.e. it was cold on the previous scan and its score is >= the hot threshold
  // or it was in a blip on the previous scan and its score is >= the cold threshold
  // the sample's score is (sample[i] - cellmean[i]) / celldev[i]
  //
  // Parameters:
  // scoresxp - extmat with sample score data
  // classsxp - extmat with sample classifications (or NULL if not to be used)
  // prevclasssxp - extmat with sample classifications (or NULL if not to be used)
  // threshsxp - the hot and cold score thresholds
  // 
  // returns a vector with these elements:
  // - number of cold samples
  // - number of samples deemed hot because their score exceeds hot_score
  // - number of samples deemed hot because they were in a blip and their score exceeds cold_score

  SEXP rv, counts;
  int i, n;			// buffer index
  int hot_thresh, cold_thresh; 

  int num_hot_samples = 0;
  int num_blip_hot_samples = 0;

  t_extmat *score     = SEXP_TO_EXTMAT(scoresxp);
  t_extmat *class     = SEXP_TO_EXTMAT(classsxp);
  t_extmat *prevclass = SEXP_TO_EXTMAT(prevclasssxp);

  t_score *score_buff = (t_score *) score->ptr;

  t_class  *class_buff, *prev_class_buff;

  n = score->rows * score->cols;

  // ensure storage for the new classification; we allocate one extra slop entry
  // needed by the patch-finding algorithm

  (*pensure_extmat_with_slop)(class, score->rows, score->cols, 1);

  class_buff = (t_class *) class->ptr;

  // set the class to COLD by default
  memset(class_buff, CLASS_COLD, (n + 1) * sizeof(t_class));

  if (prevclass->rows != score->rows || prevclass->cols != score->cols) {
    // If the previous classification's dimensions are different
    // than those of the current scan (as might happen if the scan dimensions change during
    // processing), then we don't want to use previous class information in classifying this
    // scan. 
    // In that case, we just make sure the prevclass matrix is sufficiently large, and reset
    // it entirely to CLASS_COLD.
    
    (*pensure_extmat_with_slop)(prevclass, score->rows, score->cols, 1);

    // set the class to COLD by default
    memset(prevclass->ptr, CLASS_COLD, (n + 1) * sizeof(t_class));
  }

  prev_class_buff = (t_class *) prevclass->ptr;

  hot_thresh  = (int) (REAL(threshsxp)[0] * (1 << T_SCORE_FRACTIONAL_BITS));
  cold_thresh = (int) (REAL(threshsxp)[1] * (1 << T_SCORE_FRACTIONAL_BITS));

  for (i = 0; i < n; ++i ) {
    if (score_buff[i] >= hot_thresh || score_buff[i] <= cold_thresh) {
      class_buff[i] = CLASS_HOT;
      ++ num_hot_samples;
    }
  }

  PROTECT(counts = allocVector(INTSXP, 3));
  INTEGER(counts)[0] = n - num_hot_samples - num_blip_hot_samples;
  INTEGER(counts)[1] = num_hot_samples;
  INTEGER(counts)[2] = num_blip_hot_samples;

  // we pass the counts to the classify hook functions; they can choose
  // to return an altered set of counts, or NULL

  rv = call_R_hooks(RADR_HOOK_CLASSIFY, classsxp, counts, NULL);

  UNPROTECT(1);
  return rv != R_NilValue ? rv : counts;
}

SEXP
radR_filter_noise (SEXP mat, SEXP cutoff) 
{
  // set to zero any samples below the noise cutoff in the extmat.
  t_extmat *mat_ = SEXP_TO_EXTMAT(mat);
  t_sample cutoff_ = (t_sample) INTEGER(AS_INTEGER(cutoff))[0];
  t_sample *pstart, *pend;

  if (cutoff_ > 0) {
    pstart = (t_sample *) mat_->ptr;
    for (pend = pstart + mat_->rows * mat_->cols; pstart < pend; ++pstart) {
      if (*pstart < cutoff_)
	*pstart = 0;
      
    }
  }
  return PASS_SEXP;
}

SEXP
radR_calculate_scores (SEXP scansxp, SEXP meansxp, SEXP devsxp, SEXP scoresxp) {
  // calculate the "z-score" for each sample:
  // (sample[i] - mean[cell[i]]) / dev[cell[i]] with saturating divide:
  // i.e. if division overflows the range of t_score, or if dev[cell[i]] == 0,
  // return the minimum or maximum possible z-score, according to the sign
  // of sample[i] - mean[cell[i]]
  //
  // Parameters:
  // scansxp - extmat with scan data
  // meansxp - extmat with cell means
  // devsxp - extmat with cell deviances
  // scoresxp - extmat to hold scores
  // 
  // returns TRUE on success, NULL on failure (if unable to allocate enough
  // memory for the scores)

  int sample_dev;
  t_score_intermediate sample_score; // intermediate variable used to calculate score
  int i;			// scan buffer index
  int j, k;			// general indices
  int cellr, cellc;		// cell row and column indices
  short cols;			// number of cell columns
  short rows;			// number of cell rows
  short cols_1;            	// cols and rows minus 1
  short rows_1;             
  short cellw;			// cell width
  short cellw_save;		// avoid recalculation
  short cellh;			// cell height
  short cellw_r;		// width of rightmost partial cells
  short cellh_b;		// height of bottom partial cells

  t_frame_cell *cellmean, *celldev;	 // pointers to current cells in mean and dev frames
  t_extmat *scan, *mean, *dev, *score;   // pointers to extmats

  t_sample *scan_buff;
  t_score *score_buff;

  scan = SEXP_TO_EXTMAT(scansxp);
  mean = SEXP_TO_EXTMAT(meansxp);
  dev = SEXP_TO_EXTMAT(devsxp);
  score = SEXP_TO_EXTMAT(scoresxp);

  // ensure storage for the new classification
  (*pensure_extmat_with_slop)(score, scan->rows, scan->cols, 1);

  scan_buff = (t_sample *) scan->ptr;
  score_buff = (t_score *) score->ptr;

  rows = mean->rows; rows_1 = rows - 1;
  cols = mean->cols; cols_1 = cols - 1;

  cellh = ROUND_UP_TO(scan->rows, rows) / rows;
  cellw_save = cellw = ROUND_UP_TO(scan->cols, cols) / cols;

  // cells in the rightmost column might have a different width (fewer samples)
  cellw_r = (scan->cols % cellw) ?: cellw;

  // cells in the bottom row might have a different height (fewer pulses)
  cellh_b = (scan->rows % cellh) ?: cellh;

  cellr = cellc = 0;

  cellmean = (t_frame_cell *) mean->ptr;
  celldev = (t_frame_cell *) dev->ptr;

  i = 0;

  // loop over cell rows
  for (cellr = 0; cellr < rows; ++cellr, cellmean += cols, celldev += cols) {
    if (cellr == rows_1)
      cellh = cellh_b;
    // loop over sample rows in this cell row
    for (k = 0; k < cellh; ++k, cellmean -= cols, celldev -= cols) {
      cellw = cellw_save;
      // loop over cells in this cell row
      for (cellc = 0; cellc < cols; ++cellc, ++celldev, ++cellmean) {
	if (cellc == cols_1)
	  cellw = cellw_r;
	// loop over samples in this row and in this cell
	for (j = 0; j < cellw; ++j, ++i) {
	  // compute the deviance for this sample.   Since scan_buff[i]
	  // might be an unsigned type, make sure the other operand to "-" is
	  // signed

	  sample_dev = scan_buff[i] - (int) *cellmean;
	  do {
	    if (*celldev != 0) {
	      // compute the score with as many fractional bits as required
	      sample_score = (sample_dev << T_SCORE_FRACTIONAL_BITS) / *celldev;
	      // if there is no overflow, use the result
	      if (sample_score <= T_SCORE_MAX && sample_score >= T_SCORE_MIN) {
		score_buff[i] = (t_score) sample_score;
		break;
	      }
	    }
	    // celldev is zero or the division overflowed the available precision
	    // so saturate by using the maximum or minimum 
	    // score, based on the sign of the deviation
	    score_buff[i] = sample_dev > 0 ? T_SCORE_MAX : (sample_dev < 0 ? T_SCORE_MIN : 0);
	  } while(FALSE);
	} /* next sample in this row of this cell */
      } /* next cell in this row */
    } /* next sample row in this row of cells */
  } /* next row of cells */

  call_R_hooks(RADR_HOOK_SCORES, scoresxp, NULL);

  return PASS_SEXP;
}

static struct {
  double	area_lo;	/* minimum and maximum area of a blip for it to be recorded (< 0 means no max)*/
  double	area_hi;
  int		ns_lo;          /* minimum number of samples of a blip for it to be recorded (<0 means no max) */
  int		ns_hi;
  int		ang_lo;		/* minimum and maximum angular range (# of pulses), < 0 means no max */
  int		ang_hi;
  int		rad_lo;		/* min and max radial range (# of samples), < 0 means no max */
  int		rad_hi;
} blip_parms;

// blipping variables

t_patch_coords patch_coords = {
  .row = {.size = sizeof(short),    .type = EXTMAT_TYPE_SHORT,  .name="blip sample rows"    },
  .col = {.size = sizeof(short),    .type = EXTMAT_TYPE_SHORT,  .name="blip sample columns" },
  .val = {.size = sizeof(t_sample), .type = EXTMAT_TYPE_SAMPLE, .name="blip sample values"  },
  .z   = {.size = sizeof(double),   .type = EXTMAT_TYPE_DOUBLE, .name="blip sample scores"  }
};

static int max_patch_size;	/* maximum number of samples in an accepted patch - needed for setting up the blip hook call */
static int num_patch_stats;	/* number of patches which have had stats calculated */
static int num_blips;           /* number of patches which have passed the filter so far */
static int patch_index;		/* an index for enumerating patches */
static int num_patches;		/* number of hot-sample patches */
static int scan_rows;		/* a local copy of the image height */
static int scan_cols;		/* a local copy of the image width */
static int blip_hook_active;	/* flag: is there an active BLIP hook */
static int patch_hook_active;	/* flag: is there an active PATCH hook */
static int patch_stats_hook_active; /* flag: is there an active PATCH_STATS hook */
static int do_filtering;	/* flag: do we filter based on these parameters? */
static int do_area_weighting;   /* flag: if TRUE, use area weighted centroids; otherwise, intensity-weighted */
static double area_conversion;	/* factor for converting column sums to area in square metres */
static double rperim_conversion; /* factor for converting radial perimeter count to perimeter in metres */
static double aperim_conversion; /* factor for converting angular perimeter count to perimeter in metres */
static SEXP patch_filter;	/* a logical SEXP for filtering patches, used by the PATCH_STATS hook */
static double scan_timestamp;
static double scan_duration;
static double scan_bearing;     /* bearing of first pulse in scan, in radians */
static double scan_orientation;
static double scan_first_sample_offset;
static int scan_max_sample_value;
static double scan_range_per_sample;
static double scan_antenna_elevation;

// pointers to vectors of pulse metadata
static double *pulse_ts;      // timestamp
static double *pulse_azi;    // azimuth angle (degrees CW from N)
static double *pulse_elev;   // elevation angle (degrees above horizontal)

t_extmat em_row_present =  {.size = sizeof(char),   .type = EXTMAT_TYPE_CHAR,   .name="row present"};
t_extmat em_col_row =      {.size = sizeof(unsigned short),  .type = EXTMAT_TYPE_USHORT,  .name="row of column"};

// pointers to patch stats exmats

t_extmat *ps_x, *ps_y, *ps_z, *ps_t, *ps_ns, *ps_area, *ps_int, *ps_aspan, *ps_rspan, *ps_perim, *ps_max, *ps_range;

// vector of indexes of blips among patches
SEXP blip_index;

t_sample *samples;

/* static pointers needed by some callbacks; not to be treated as
   valid after the call to radR_find_and_filter_blips has returned */

static t_sample *scan_buff;
static t_class *class_buff;
static t_score *score_buff;
     
t_pf_rv
pf_filter_by_stats(t_cell_run *r)
{
  // a patch function to compute patch stats including centroid, and to
  // filter the patch using limits on stats
  // - number of samples
  // - area
  // - angular span
  // - radial span
  // - perimeter

  // if do_filtering is TRUE, we filter patches based on whether their
  // stats fall within the ranges given in global variable blip_parms
  // we save the stats for any non-filtered patch in an array, and 
  // record in the vector patch_filter TRUE for patches to keep, and false
  // for those filtered.

  // regardless of do_filtering, we compute the maximum patch size
  // (in number of samples) which is needed by the patch and blip hooks

  register int i, j;
  register short row;
  int ns = 0;
  double area = 0.0;
  long long aperim = 0; // count angular perimeter; this is the sum of angularly-aligned perimeter edges,
                        // except that each edge is weighted by its range, so that true spatial perimeter
                        // can be calculated
  int rperim = 0; // count radial perimeter; this is the number of radially-aligned perimeter edges
  unsigned short row_lo = ~0; // big unsigned number
  unsigned short row_hi = 0;
  short row_span;
  short col_span;
  unsigned short col_lo = ~0; // big unsigned number
  unsigned short col_hi = 0;
  char *row_present = (char *) (em_row_present.ptr);
  unsigned short *col_row = (unsigned short *) (em_col_row.ptr);
  double xsum=0, ysum=0, zsum=0;
  double dx, dy, dz;
  int weight;
  long long weight_sum = 0;
  int intsum = 0;
  t_sample max_sample = 0;
  double wt_sampno;
  double x, y, z;
  t_sample *scan_ptr;
  t_cell_run *first_run = r;
  t_pf_rv rv = KEEP; // is this patch retained?
  unsigned d_rperim; 
  double planar_factor; // cosine of the elevation angle
  // col_row will hold 1 plus the scan row in which each column (i.e. range cell)
  // was last seen in this patch. -1 means not seen yet.
  memset(col_row, 0xff, scan_cols * sizeof(unsigned short));
  // calculate the stats for this patch
  do {
    row = r->row;
    // unit vector in direction of this pulse
    SPH_TO_RECT(pulse_azi[row], pulse_elev[row], dx, dy, dz);

    planar_factor = cos(DEG_TO_RAD(pulse_elev[row]));

    // add number of samples
    ns += r->length;

    // add radial sides of the run to radial perimeter
    d_rperim = 2 * r->length;

    // process each sample in this run to accumulate centroid sums,
    // and correct radial perimeter by subtracting twice the overlap
    // between this run and any in the previous column

    // for calculating patch intensity, and possibly weighting by sample
    // intensity, we need access to sample data for this run
    scan_ptr = scan_buff + row * scan_cols + r->col;

    for (i = r->col, j = r->col + r->length; i < j; ++i) {
      if (col_row[i] == row)
	d_rperim -= 2;  // subtract two previously-counted radial perimeter edges which
                      // are actually interior 
      col_row[i] = row + 1;

      // in updating centroid sums, weight is column number for area
      // weighting, and sample value for intensity weighting

      weight = do_area_weighting ? (2 * i + 1) : *scan_ptr;
      if (max_sample < *scan_ptr)
	max_sample = *scan_ptr;
      intsum += *scan_ptr++;
      wt_sampno = (i + scan_first_sample_offset + 0.5) * weight;
      xsum += dx * wt_sampno; // in units of weighted scan_range_per_sample
      ysum += dy * wt_sampno; // in units of weighted scan_range_per_sample
      zsum += dz * wt_sampno; // in units of weighted scan_range_per_sample
      weight_sum += weight;
    }

    // we implicitly convert column to origin=1 for this calculation, so that
    // even samples in column zero make a positive contribution to area
    area += r->length * (2 * (r->col + scan_first_sample_offset) + r->length) * SQUARE(planar_factor);

    // because runs are non-adjacent within a pulse, each run makes two 
    // contributions to angular perimeter, one at each end
    aperim += (2 * (r->col + scan_first_sample_offset) + r->length) * planar_factor;
    
    rperim += d_rperim * planar_factor;

    row_hi = max(row_hi, r->row);                 
    row_lo = min(row_lo, r->row);
    col_hi = max(col_hi, r->col + r->length - 1);    
    col_lo = min(col_lo, r->col);
    r += r->next_run_offset;
  } while (r != first_run);

  row_span = row_hi - row_lo + 1;
  col_span = col_hi - col_lo + 1;

  // for patches straddling the zero-degree cut (i.e. which wrap across
  // from the bottom to the top of the image), we need to calculate row span
  // in a different way, since row_hi - row_lo + 1 gives the full image height.
  // We must also remove the internal perimeter edges between range cells in the
  // first and last pulses.

  if (row_span == scan_rows) {
    // this is a patch having at least one run in both the top
    // and bottom rows.  Either it spans the full image, or it
    // wraps around the bottom/top boundary without doing so.
    // The row span must be computed another way.  
    // We use an indicator vector to record presence/absence
    // of rows in the patch, and sum the presences.

    memset(row_present, 0, scan_rows * sizeof(char));
    row_span = 0;
    do {
#ifdef RADR_DEBUG
      if (r->row >= scan_rows)
	printf("Warning: r->row >= scan_rows\n");
#endif
      if (!row_present[r->row]) {
	row_present[r->row] = 1;
	++row_span;
      }
      r += r->next_run_offset;
    } while (r != first_run);

    // the first run must be in row zero
    // process all runs in that row, looking for overlaps with
    // the last pulse
    do {
      for (i = r->col, j = r->col + r->length; i < j; ++i)
	if (col_row[i] == scan_rows)
	  rperim -= 2;  // subtract two previously-counted radial perimeter edges which
      // are actually interior; they straddle the zero cut
      r += r->next_run_offset;
    } while (r->row == 0 && r != first_run);

  }
  // record this patch's stats:
  ((double *) ps_x->ptr)     [num_patch_stats] =  x = xsum / weight_sum * scan_range_per_sample;
  ((double *) ps_y->ptr)     [num_patch_stats] =  y = ysum / weight_sum * scan_range_per_sample;
  ((double *) ps_z->ptr)     [num_patch_stats] = (z = zsum / weight_sum * scan_range_per_sample) + scan_antenna_elevation;
  ((double *) ps_range->ptr) [num_patch_stats] = sqrt(x*x + y*y + z*z);
  // time is the time at the centroid 
  ((double *) ps_t->ptr)     [num_patch_stats] = scan_timestamp + scan_duration * fmod(2 * M_PI + M_PI / 2 - atan2(y, x) - scan_bearing, 2 * M_PI) / (2 * M_PI);
  ((int *)    ps_ns->ptr)    [num_patch_stats] = ns;
  ((double *) ps_area->ptr)  [num_patch_stats] = (area *= area_conversion);
  ((double *) ps_int->ptr)   [num_patch_stats] = intsum / (double)(ns * scan_max_sample_value);
  ((double *) ps_max->ptr)   [num_patch_stats] = max_sample / (double) scan_max_sample_value;
  ((double *) ps_perim->ptr) [num_patch_stats] = rperim * rperim_conversion + aperim * aperim_conversion;
  ((short *)  ps_aspan->ptr) [num_patch_stats] = row_span;
  ((short *)  ps_rspan->ptr) [num_patch_stats] = col_span;

  if (do_filtering
      && (ns < blip_parms.ns_lo 
	 || (blip_parms.ns_hi >= 0 && ns > blip_parms.ns_hi)
	 || area < blip_parms.area_lo
	 || (blip_parms.area_hi >= 0 && area > blip_parms.area_hi)
	 || row_span < blip_parms.ang_lo
	 || (blip_parms.ang_hi >= 0 && row_span > blip_parms.ang_hi)
	 || col_span < blip_parms.rad_lo
	 || (blip_parms.rad_hi >= 0 && col_span > blip_parms.rad_hi)
	 ))
    {
      // disable this patch
      rv = DROP;
      LOGICAL(patch_filter)[num_patch_stats] = 0;
    } else {
      // adjust the maximum patch size, if necessary
      max_patch_size = max(max_patch_size, ns);
      LOGICAL(patch_filter)[num_patch_stats] = 1;
      // record this as a blip
      INTEGER(blip_index)[num_blips++] = num_patch_stats + 1;
    }    
  ++num_patch_stats;
  return rv;
}

t_pf_rv
pf_filter_by_stats_rectangular(t_cell_run *r)
{
  // a patch function to compute patch stats including centroid, and to
  // filter the patch using limits on stats, for data in rectangular coordinates
  // - number of samples
  // - area
  // - angular span (meaning y span)
  // - radial span (meaning x span)
  // - perimeter

  // if do_filtering is TRUE, we filter patches based on whether their
  // stats fall within the ranges given in global variable blip_parms
  // we save the stats for any non-filtered patch in an array, and
  // record in the vector patch_filter TRUE for patches to keep, and false
  // for those filtered.

  // regardless of do_filtering, we compute the maximum patch size
  // (in number of samples) which is needed by the patch and blip hooks

  register int i, j;
  register short row;
  int ns = 0;
  double area = 0.0;
  int perim = 0;
  unsigned short row_lo = ~0; // big unsigned number
  unsigned short row_hi = 0;
  short row_span;
  short col_span;
  unsigned short col_lo = ~0; // big unsigned number
  unsigned short col_hi = 0;
  unsigned short *col_row = (unsigned short *) (em_col_row.ptr);
  double xsum=0, ysum=0;
  int weight;
  long long weight_sum = 0;
  int intsum = 0;
  t_sample max_sample = 0;
  double x, y;
  t_sample *scan_ptr;
  t_cell_run *first_run = r;
  t_pf_rv rv = KEEP; // is this patch retained?
  // col_row will hold 1 plus the scan row in which each column (i.e. range cell)
  // was last seen in this patch. -1 means not seen yet.
  memset(col_row, 0xff, scan_cols * sizeof(unsigned short));
  // calculate the stats for this patch
  do {
    row = r->row;

    // add number of samples
    ns += r->length;

    // add edges from all 4 sides of run to the perimeter (we correct for internal edges below)
    perim += 2 * r->length + 2;

    // process each sample in this run to accumulate centroid sums,
    // and correct radial perimeter by subtracting twice the overlap
    // between this run and any in the previous column

    // for calculating patch intensity, and possibly weighting by sample
    // intensity, we need access to sample data for this run
    scan_ptr = scan_buff + row * scan_cols + r->col;

    for (i = r->col, j = r->col + r->length; i < j; ++i) {
      if (col_row[i] == row)
	perim -= 2;  // subtract two previously-counted "perimeter" edges which
                      // are actually interior
      col_row[i] = row + 1;

      // in updating centroid sums, weight is 1 for area
      // weighting, and sample value for intensity weighting

      weight = do_area_weighting ? 1 : *scan_ptr;
      if (max_sample < *scan_ptr)
	max_sample = *scan_ptr;
      intsum += *scan_ptr++;
      xsum += i * weight;
      ysum += row * weight;
      weight_sum += weight;
    }

    area += r->length;

    row_hi = max(row_hi, r->row);
    row_lo = min(row_lo, r->row);
    col_hi = max(col_hi, r->col + r->length - 1);
    col_lo = min(col_lo, r->col);
    r += r->next_run_offset;
  } while (r != first_run);

  row_span = row_hi - row_lo + 1;
  col_span = col_hi - col_lo + 1;

  // record this patch's stats:
  ((double *) ps_x->ptr)     [num_patch_stats] =  x = xsum / weight_sum;  // Note: these are in matrix coords
  ((double *) ps_y->ptr)     [num_patch_stats] =  y = ysum / weight_sum;
  ((double *) ps_z->ptr)     [num_patch_stats] = 0;
  ((double *) ps_range->ptr) [num_patch_stats] = -1; // will be calculated at R level after x, y are corrected
  // time is the time at the centroid
  ((double *) ps_t->ptr)     [num_patch_stats] = scan_timestamp;
  ((int *)    ps_ns->ptr)    [num_patch_stats] = ns;
  ((double *) ps_area->ptr)  [num_patch_stats] = (area *= scan_range_per_sample * scan_range_per_sample);
  ((double *) ps_int->ptr)   [num_patch_stats] = intsum / (double)(ns * scan_max_sample_value);
  ((double *) ps_max->ptr)   [num_patch_stats] = max_sample / (double) scan_max_sample_value;
  ((double *) ps_perim->ptr) [num_patch_stats] = perim * scan_range_per_sample;
  ((short *)  ps_aspan->ptr) [num_patch_stats] = row_span;
  ((short *)  ps_rspan->ptr) [num_patch_stats] = col_span;

  if (do_filtering
      & (ns < blip_parms.ns_lo
	 || (blip_parms.ns_hi >= 0 && ns > blip_parms.ns_hi)
	 || area < blip_parms.area_lo
	 || (blip_parms.area_hi >= 0 && area > blip_parms.area_hi)
	 || row_span < blip_parms.ang_lo
	 || (blip_parms.ang_hi >= 0 && row_span > blip_parms.ang_hi)
	 || col_span < blip_parms.rad_lo
	 || (blip_parms.rad_hi >= 0 && col_span > blip_parms.rad_hi)
	 ))
    {
      // disable this patch
      rv = DROP;
      LOGICAL(patch_filter)[num_patch_stats] = 0;
    } else {
      // adjust the maximum patch size, if necessary
      max_patch_size = max(max_patch_size, ns);
      LOGICAL(patch_filter)[num_patch_stats] = 1;
      // record this as a blip
      INTEGER(blip_index)[num_blips++] = num_patch_stats + 1;
    }
  ++num_patch_stats;
  return rv;
}


t_pf_rv
pf_filter_by_logical_vector (t_cell_run *run)
{
  // a patch function to activate precisely those patches corresponding to TRUE
  // values in the logical SEXP given by patch_filter

  t_pf_rv rv;

  if (LOGICAL(patch_filter)[patch_index] == 1) {
    // this patch will be active, so link it in and make its id positive
    max_patch_size = max(max_patch_size, ((int *) ps_ns->ptr)[patch_index]);
    INTEGER(blip_index)[num_blips++] = patch_index + 1;
    rv = KEEP;
  } else {
    // this patch will be inactive, so don't link it, and set its id negative
    rv = DROP;
  }
  ++patch_index;
  return rv;
}

t_pf_rv
pf_patch_and_blip_hooks(t_cell_run *r)
{
  // a patch function to :
  // - filter the patch by calling any patch hooks
  // - call any blip hooks on patches remaining (i.e. blips)

  t_cell_run *first_run = r;
  int ns = 0;
  short i;
  int j;
  SEXP patch_hook_rv;
  t_pf_rv rv = KEEP;

  // we need to create vectors of row and column coordinates and sample values

  do {
    short row = r->row;
    short col = r->col;
    short length = r->length;
    j = row * scan_cols + col;
    for (i = 0; i < length; ++i, ++ns, ++j ) {
      ((short *)patch_coords.row.ptr)[ns] = row + 1;
      ((short *)patch_coords.col.ptr)[ns] = col + 1 + i;
      ((t_sample *)patch_coords.val.ptr)[ns] = scan_buff[j];
      if (score_buff) 
	((double *) patch_coords.z.ptr)[ns] = score_buff[j] * T_SCORE_SCALE;
    }
    r += r->next_run_offset;
  } while (r != first_run);

  patch_coords.row.rows = patch_coords.col.rows = patch_coords.val.rows = ns;
  patch_coords.row.cols = patch_coords.col.cols = patch_coords.val.cols = 1;
  if (score_buff) {
    patch_coords.z.rows = ns; patch_coords.z.cols = 1;
  } else {
    patch_coords.z.rows = 0; patch_coords.z.cols = 0;
  }
  
  if (patch_hook_active) {
    patch_hook_rv = call_R_hooks(RADR_HOOK_PATCH, 
				 PROTECT((*pextmat_to_sexp)(&patch_coords.row)),
				 PROTECT((*pextmat_to_sexp)(&patch_coords.col)),
				 PROTECT((*pextmat_to_sexp)(&patch_coords.val)), 
				 PROTECT((*pextmat_to_sexp)(&patch_coords.z)), 
				 NULL);
    UNPROTECT(4);
    if (IS_LOGICAL(patch_hook_rv) && !LOGICAL(patch_hook_rv)[0])
      // FALSE: drop this patch 
      rv = DROP;
  }

  if ((rv == KEEP || rv == KEEP_AND_QUIT) && blip_hook_active) {
    call_R_hooks(RADR_HOOK_BLIP, 
		 PROTECT((*pextmat_to_sexp)(&patch_coords.row)),
		 PROTECT((*pextmat_to_sexp)(&patch_coords.col)),
		 PROTECT((*pextmat_to_sexp)(&patch_coords.val)), 
		 PROTECT((*pextmat_to_sexp)(&patch_coords.z)), 
		 NULL);
    UNPROTECT(4);
  }
  
  return rv;
}

static t_class painting_class;

t_pf_rv
pf_paint_blip (t_cell_run *r)
{
  t_cell_run *first_run = r;
  
  // paint the blip into the class buff
  // FIXME: for speed, we assume sizeof(t_class) = 1 and use memset
  do {
    memset (class_buff + r->row * scan_cols + r->col, painting_class, r->length);
    r += r->next_run_offset;
  } while (r != first_run);
  return KEEP;
}
	

SEXP
radR_find_patches( SEXP classsxp, 
		   SEXP usediagsxp,
		   SEXP patchessxp,
		   SEXP skipsxp,
		   SEXP wrapsxp)
{
  // find patches of contiguous hot samples in the class buffer
  //
  // Parameters:
  // classsxp - extmat with sample classifications
  // usediags - logical: are diagonals adjacencies used in building patches
  // patchessxp - the patch buffer image structure (an EXTPTR)
  // skipsxp - the number of samples to skip at the start of each pulse
  // wrapsxp - logical scalar - if TRUE, wrap the matrix to make the first and last rows
  //           adjacent, so that patches can straddle this cut.

  // returns the number of hot samples and the patches found
  // 
  // side effect: the patches are represented in the image structure
  // passed in patchessxp

  SEXP rv;
  t_image image;
  t_extmat *class;

  usediagsxp = AS_LOGICAL(usediagsxp);

  class = SEXP_TO_EXTMAT(classsxp);
  class_buff = (t_class *) class->ptr;

  // set up parameters for patchifying the class buffer into
  // the image structure

  image = (t_image) EXTPTR_PTR(patchessxp);
  image->run_info.num_rows = class->rows;
  image->run_info.num_cols = class->cols;
  image->use_diags = LOGICAL(usediagsxp)[0];
  image->vertical_wrap = LOGICAL(AS_LOGICAL(wrapsxp))[0];
  image->drop_singletons = TRUE;
  image->fgd = CLASS_HOT;
  if (!patchify_buffer(class_buff, image, (unsigned) max(0, INTEGER(AS_INTEGER(skipsxp))[0])))
    return R_NilValue;

  rv = allocVector(INTSXP, 1);
  INTEGER(rv)[0] = image->run_info.num_active_patches;
  return rv;
}

SEXP
radR_process_patches(SEXP filtersxp,
		     SEXP scansxp, 
		     SEXP scoresxp,
		     SEXP classsxp, 
		     SEXP patchbuff,
		     SEXP statssxp,
		     SEXP sampnumrange, 
		     SEXP arearange, 
		     SEXP angularrange, 
		     SEXP radialrange, 
		     SEXP rps_elev,
		     SEXP areaweighting,
		     SEXP scaninfo,
		     SEXP pulses,
		     SEXP rectangular
		     ) {
  
  // process patches from the image structure
  // passed in patchbuff
  //
  // This does:
  //
  //   - compute patch properties:
  //        centroid x, y, z, t
  //        number of samples, area, perimeter, angular and radial spans
  //        
  //   - optional low-level filtering based on patch statistics;
  //
  //   - R-level PATCH_STATS_HOOK filtering, based on the stats matrices for all patches
  //
  //   - R-level PATCH_HOOK filtering, allowing patch-by-patch filtering
  //     based on sample values for the patch
  //
  //   - R-level BLIP_HOOK calls, allowing plugins to do things with each
  //     "blip" (i.e. accepted patch)  The BLIP_HOOK is called for each
  //     patch immediately after the PATCH_HOOK is called for that patch
  //     (as opposed to calling the PATCH_HOOK for all patches, then the BLIP_HOOK
  //     for all patches)
  //
  //   - R-level RAW_BLIPS_HOOK calls, allowing plugins to do things with
  //     an external pointer to the cell_run structure for a patch
  //
  //   - painting the blips into the class matrix 
  //
  // Parameters:
  // filtersxp - logical: do we filter based on patch statistics
  // scansxp - extmat with scan data
  // scoresxp - extmat with score data
  // classsxp - extmat with sample classifications
  // patchbuff - external pointer to patch structure of type t_image
  // statssxp - data.frame whose columns are extmats holding x, y, z, t, ns, area, int, max, aspan, rspan, perimeter
  // sampnumrange - int: min and max number of samples in a patch (max must be positive)
  // arearange - int: min and max apparent "area" of a patch (in sector units) (max < 0 means no max)
  //           each sample counts as 1 + its column number in sector units
  // angularrange - int: min and max number of pulses in a patch (max < 0 means no max)
  // radialrange - int: min and max sample slots in a patch (max < 0 means no max)
  // rps_elev - double[2]: [0] range per sample (metres)
  //                       [1] elevation of radar antenna (metres)
  // areaweighting - logical: if TRUE, use area weighting (rather than intensity weighting) for centroid calculation
  // scaninfo - double[6]:   [0] timestamp for first pulse in scan
  //                         [1] duration of scan in seconds
  //                         [2] bearing of first pulse (degrees clockwise from North)
  //                         [3] +/- 1: orientation of scan; clockwise = +1; counterclockwise = -1
  //                         [4] skipped samples before first; if min range > 0, this is the min range in multiples of range cell size
  //                         [5] maximum sample value
  // pulses - R list whose first 5 elements are numeric vectors with length = number of pulses in the scan, 
  // with these interpretations:
  //      [[1]] timestamp for each pulse                                               
  //      [[2]] beam axis azimuth for each pulse (degrees CW from N)                   
  //      [[3]] beam axis elevation for each pulse (degrees above horizontal)          
  //      [[4]] azimuth of long waveguide axis for each pulse (degrees CW from N)      
  //      [[5]] elevation of long waveguide axis for each pulse (degrees above horizontal)
  // rectangular - logical scalar: is the data matrix rectangular (e.g. for camera video)?
  
  // returns the integer index vector of blips among patches
  t_extmat *scan;
  t_image image;

  int i;

  // in case the PATCH_STATS hook returns a different vector than we pass to it
  SEXP new_patch_filter = NULL;

  // row names attribute for the RSS$patches dataframe
  SEXP row_names;

  // other scan parameters
  scan_timestamp   	   = REAL(scaninfo)[0];
  scan_duration   	   = REAL(scaninfo)[1];
  scan_bearing     	   = REAL(scaninfo)[2] * M_PI / 180;
  scan_orientation 	   = REAL(scaninfo)[3];
  scan_first_sample_offset = REAL(scaninfo)[4];
  scan_max_sample_value    = (int )REAL(scaninfo)[5];

  pulse_ts = REAL(VECTOR_ELT(pulses, 0));
  pulse_azi = REAL(VECTOR_ELT(pulses, 1));
  pulse_elev = REAL(VECTOR_ELT(pulses, 2));

  do_filtering = LOGICAL(AS_LOGICAL(filtersxp))[0];

  sampnumrange 	   = AS_INTEGER(sampnumrange);
  blip_parms.ns_lo = INTEGER(sampnumrange)[0];
  blip_parms.ns_hi = INTEGER(sampnumrange)[1];

  angularrange 	    = AS_INTEGER(angularrange);
  blip_parms.ang_lo = INTEGER(angularrange)[0];
  blip_parms.ang_hi = INTEGER(angularrange)[1];

  radialrange 	    = AS_INTEGER(radialrange);
  blip_parms.rad_lo = INTEGER(radialrange)[0];
  blip_parms.rad_hi = INTEGER(radialrange)[1];

  scan 	    = SEXP_TO_EXTMAT(scansxp);
  scan_buff = (t_sample *) scan->ptr;
  scan_rows = scan->rows;
  scan_cols = scan->cols;

  score_buff = isNull(scoresxp) ? NULL : (t_score *) SEXP_TO_EXTMAT(scoresxp)->ptr;
  class_buff = (t_class *) SEXP_TO_EXTMAT(classsxp)->ptr;

  image = (t_image) EXTPTR_PTR(patchbuff);

  do_area_weighting = LOGICAL(AS_LOGICAL(areaweighting))[0];

  // get address of extmats for holding stats

  ps_x     = SEXP_TO_EXTMAT(VECTOR_ELT(statssxp, 0));
  ps_y     = SEXP_TO_EXTMAT(VECTOR_ELT(statssxp, 1));
  ps_z     = SEXP_TO_EXTMAT(VECTOR_ELT(statssxp, 2));
  ps_t     = SEXP_TO_EXTMAT(VECTOR_ELT(statssxp, 3));
  ps_ns    = SEXP_TO_EXTMAT(VECTOR_ELT(statssxp, 4));
  ps_area  = SEXP_TO_EXTMAT(VECTOR_ELT(statssxp, 5));
  ps_int   = SEXP_TO_EXTMAT(VECTOR_ELT(statssxp, 6));
  ps_max   = SEXP_TO_EXTMAT(VECTOR_ELT(statssxp, 7));
  ps_aspan = SEXP_TO_EXTMAT(VECTOR_ELT(statssxp, 8));
  ps_rspan = SEXP_TO_EXTMAT(VECTOR_ELT(statssxp, 9));
  ps_perim = SEXP_TO_EXTMAT(VECTOR_ELT(statssxp, 10));
  ps_range = SEXP_TO_EXTMAT(VECTOR_ELT(statssxp, 11));

  // find whether relevant hooks are active

  blip_hook_active = is_R_hook_active(RADR_HOOK_BLIP);
  patch_hook_active = is_R_hook_active(RADR_HOOK_PATCH);
  patch_stats_hook_active = is_R_hook_active(RADR_HOOK_PATCH_STATS) & do_filtering;

  // to convert sums of (2x+1), where x is column value, to area, we need:
  // the polar area element, (dr)^2 dtheta:
  scan_range_per_sample = REAL(rps_elev)[0];
  area_conversion = scan_range_per_sample * scan_range_per_sample * M_PI / scan->rows;
  scan_antenna_elevation = REAL(rps_elev)[1];

  // to convert radial and weighted angular perimeter counts, we need these conversions:
  rperim_conversion = scan_range_per_sample;
  aperim_conversion = scan_range_per_sample * (M_PI * 2 / scan->rows);

  arearange = AS_NUMERIC(arearange);

  // to avoid using floating point in the typical case of no PATCH_STATS hook, we save column sum thresholds too
  blip_parms.area_lo = REAL(arearange)[0];
  blip_parms.area_hi = REAL(arearange)[1];

  num_patches = image->run_info.num_patches;

  num_patch_stats = 0;
  max_patch_size = 0;

  // ensure we have room to store patch stats
  (*pensure_extmat)(ps_x, 1, num_patches);
  (*pensure_extmat)(ps_y, 1, num_patches);
  (*pensure_extmat)(ps_z, 1, num_patches);
  (*pensure_extmat)(ps_t, 1, num_patches);
  (*pensure_extmat)(ps_ns, 1, num_patches);
  (*pensure_extmat)(ps_area, 1, num_patches);
  (*pensure_extmat)(ps_int, 1, num_patches);
  (*pensure_extmat)(ps_max, 1, num_patches);
  (*pensure_extmat)(ps_aspan, 1, num_patches);
  (*pensure_extmat)(ps_rspan, 1, num_patches);
  (*pensure_extmat)(ps_perim, 1, num_patches);
  (*pensure_extmat)(ps_range, 1, num_patches);

  PROTECT(blip_index = allocVector(INTSXP, num_patches));

  /* compute stats for each patch, possibly saving them
     and possibly using them to filter patches.  The temporary
     buffer row_present is for a worst-case row-span algorithm
     occasionally used inside pf_filter_by_stats.
     The temporary buffer col_row is for perimeter calculations.
     This function also computes the maximum patch size (# of samples)
     which is needed in advance for the patch and blip hooks.
  */
  
  (*pensure_extmat)(&em_row_present, 1, scan_rows);
  (*pensure_extmat)(&em_col_row, 1, scan_cols);

  /* regardless of do_filtering, always call pf_filter_by_stats
     since it computes the maximum patch size needed by blip and
       patch hooks */

  num_blips = 0;

  PROTECT(patch_filter = allocVector(LGLSXP, num_patches));

  enumerate_patches(image, LOGICAL(AS_LOGICAL(rectangular))[0] ? & pf_filter_by_stats_rectangular : & pf_filter_by_stats);

  /* set the rownames attribute of statssxp so that the data frame is consistent */
  PROTECT(row_names = allocVector(INTSXP, num_patches));
  for (i = 0; i < num_patches; ++i)
    INTEGER(row_names)[i] = i + 1;

  setAttrib(statssxp, R_RowNamesSymbol, row_names);
  UNPROTECT(1);
  
  if (patch_stats_hook_active) {
    /* call the patch_stats hooks with the current status
       (active/inactive) vector; note that each hook function receives
       the results of the previous one, if any, and that the final
       return value is that of the last hook function
    */
    new_patch_filter = call_R_hooks_accum(RADR_HOOK_PATCH_STATS, patch_filter, NULL);
    if (IS_LOGICAL(new_patch_filter) && LENGTH(new_patch_filter) == num_patch_stats) {
      // a valid filtering vector has been returned.
      // enumerate each patch, using the filter vector to disable it
      // if the corresponding value in the vector is FALSE, and to enable it
      // otherwise
      if (new_patch_filter != patch_filter) {
	PROTECT(new_patch_filter);
	UNPROTECT_PTR(patch_filter);
	patch_filter = new_patch_filter;
      }

      max_patch_size = num_blips = patch_index = 0;
      enumerate_all_patches(image, & pf_filter_by_logical_vector, 0, 0);
    }
  }

  /* resize the blip index vector so it's just big enough for the indexes
     of non-filtered patches */

  PROTECT(SET_LENGTH(blip_index, num_blips));

  if (patch_hook_active || blip_hook_active) {
    // ensure there is enough storage for the patch/blip extmats
    (*pensure_extmat)(&patch_coords.row, max_patch_size, 1);
    (*pensure_extmat)(&patch_coords.col, max_patch_size, 1);
    (*pensure_extmat)(&patch_coords.val, max_patch_size, 1);
    if (score_buff)
      (*pensure_extmat)(&patch_coords.z, max_patch_size, 1);
    else
      (*pensure_extmat)(&patch_coords.z, 0, 0);
  }
  // filter the patches by the patch hook and/or call the blip hook with each
  // patch that remains

  if (patch_hook_active || blip_hook_active)
    enumerate_patches(image, & pf_patch_and_blip_hooks);

  if (is_R_hook_active(RADR_HOOK_RAW_BLIPS))
    call_R_hooks(RADR_HOOK_RAW_BLIPS, patchbuff);

  // paint the blips into the class buffer
  painting_class = CLASS_BLIP;
  enumerate_patches(image, & pf_paint_blip);

  UNPROTECT(3);
  return blip_index;
}

SEXP
radR_unfilter_patches (SEXP classsxp, SEXP patchbuff)
{
  t_extmat *class = SEXP_TO_EXTMAT(classsxp);
  t_image image = (t_image) EXTPTR_PTR(patchbuff);
  int i;

  class_buff = (t_class *) class->ptr;

  // unfilter patches; i.e. restore all patches in the patch_image to active
  // status by relinking them in.  This also repaints all patches to CLASS_HOT
  // in the class buffer, and sets the blips index vector to the entire set of patch
  // indexes.

  // paint the blips back into hot patches in the class buffer
  painting_class = CLASS_HOT;
  enumerate_patches(image, & pf_paint_blip);

  // relink all the patches into the active list
  reactivate_all_patches(image);

  // make blips point to all patches
  blip_index = allocVector(INTSXP, i = image->run_info.num_active_patches);
  for (/**/; i > 0; --i)
    INTEGER(blip_index)[i - 1] = i;

  return blip_index;
}
  
SEXP
radR_alloc_patch_image () 
{
  // return an EXTPTR SEXP pointing to a newly-allocated patch_image structure
  
  t_image_struct *p = Calloc(1, t_image_struct);
  p->runs        = CREATE_USER_EXTMAT(sizeof(t_cell_run), "runs of hot cells");
  p->scratch_row = CREATE_USER_EXTMAT(sizeof(t_cell_run), "temporary runs of hot cells");

  return (PTR_TO_EXTPTR(p));
}

SEXP
radR_free_patch_image (SEXP pi) 
{
  // it is up to the caller to make sure this function is not
  // called more than once for any given patch image

  if (TYPEOF(pi) == EXTPTRSXP)
    shut_down_patchify((t_image_struct *)EXTPTR_PTR(pi));
  return R_NilValue;
}

// needed for the pf_get_blip_index patch filter

static int blip_number;
static int patch_number;
static int patch_id;

static t_pf_rv
pf_get_blip_index (t_cell_run *r)
{
  // increment the blip number until we've found the 
  // patch with abs(id) == patch_id (absolute value
  // since it might be an inactive patch whose ID is negated)
  ++patch_number;
  if (r->patch_id > 0)
    ++blip_number;
  return (iabs(r->patch_id) == patch_id) ? SAME_AND_QUIT : SAME;
}

SEXP
radR_patch_at_sp (SEXP sp, SEXP patchbuff) {

  // return sample/pulse (i.e. row and column coordinates) for all
  // samples in the patch, if any, which includes the sample at column
  // cr[1], row cr[2]
  // Returns an integer matrix with two columns, and one row for
  // each sample in the patch.  The first column gives sample# (i.e. column value)
  // and the second column gives pulse# (i.e. row value).  This 
  // choice is consistent with R's column-major indexing, so that, e.g.
  // the sample values for the patch with a sample in row 100, column 150, are given by:
  // RSS$sample.mat[.Call("radR_patch_at_rc", c(100, 150)]
  //
  // the return value has an integer vector attribute "index", with 2 elements:
  //
  //  - index[1]: the 1-based index of this patch among all patches in this scan 
  //
  //  - index[2]: the 1-based index of this blip among all blips in this scan,
  //              or 0 if this patch is not a blip

  int i, n;
  int active;
  int index;
  int ns;
  int *rp, *cp;
  int row, col;

  t_cell_run *first_run, *r;
  t_image image;
  SEXP rv, rvi;

  image = (t_image) EXTPTR_PTR(patchbuff);

  sp = AS_INTEGER(sp);
  col = INTEGER(sp)[0];
  row = INTEGER(sp)[1];

  r = first_run = get_patch_from_rc (image, row, col);
  if (!r || (r->length == 1 && r->next_run_offset == 0))
    // no patch, or a singleton patch consisting of a single hot sample
    return (R_NilValue);
    
  active = r->patch_id > 0;
  index = iabs(r->patch_id);

  // count the samples

  ns = 0;
  do {
    ns += r->length;
    r += r->next_run_offset;
  } while (r != first_run);
  
  rv = allocMatrix(INTSXP, ns, 2);

  cp = (int *) INTEGER(rv);
  rp = cp + ns;

  // fill the return value with sample coords, converting them to origin 1

  do {
    for (i = 0, n = r->length; i < n; ++i) {
      *rp++ = r->row + 1;
      *cp++ = r->col + i + 1;
    }
    r += r->next_run_offset;
  } while (r != first_run);

  blip_number = patch_number = 0;
  // get the actual blip number for this patch in blip_number
  patch_id = index;
  r = enumerate_all_patches(image, & pf_get_blip_index, 0, 0);

  // if the last active patch found is not the one we sought, set the blip_number to 0
  if (r->patch_id != index)
    blip_number = 0;

  PROTECT(rv);
  PROTECT(rvi = allocVector(INTSXP, 2));
  INTEGER(rvi)[0] = patch_number;
  INTEGER(rvi)[1] = blip_number;
  setAttrib(rv, INDEX_ATTRIBUTE_STRING, rvi);
  UNPROTECT(2);
  return(rv);
}

// static vars for use by radR_get_all_blips, pf_get_blip_coords, pf_count_samples

static SEXP all_blips;
static int sample_number;
static int num_blip_samples;
static int do_linear;
static int do_blipnum;
static int num_cols;

static t_pf_rv
pf_get_blip_coords (t_cell_run *r)
{
  // a patch filter for use by radR_get_all_blips

  t_cell_run *first_run = r;
  int i;
  int sample_loc;

  // append the blip number and sample coordinates to the matrix all_blips
  // these are all for R, and so we use origin=1 (hence the "+1" expressions)
  do {
    if (do_linear) {
      // linear coordinates of the sample in the buffer
      sample_loc = r->col + r->row * num_cols + 1;
      if (do_blipnum) {
	for (i=0; i < r->length; ++i) {
	  INTEGER(all_blips)[sample_number] = blip_number + 1;
	  INTEGER(all_blips)[sample_number + num_blip_samples] = sample_loc++;
	  ++ sample_number;
	}
      } else {
	for (i=0; i < r->length; ++i) {
	  INTEGER(all_blips)[sample_number] = sample_loc++;
	  ++ sample_number;
	}
      }
    } else {
      // rectangular coordinates (blip#, sample#)
      if (do_blipnum) {
	for (i=0; i < r->length; ++i) {
	  INTEGER(all_blips)[sample_number] = blip_number + 1;
	  INTEGER(all_blips)[sample_number + num_blip_samples] = r->col + i + 1;
	  INTEGER(all_blips)[sample_number + 2 * num_blip_samples] = r->row + 1;
	  ++ sample_number;
	}
      } else {
	for (i=0; i < r->length; ++i) {
	  INTEGER(all_blips)[sample_number] = r->col + i + 1;
	  INTEGER(all_blips)[sample_number + num_blip_samples] = r->row + 1;
	  ++ sample_number;
	}
      }
    }
    r += r->next_run_offset;
  } while (r != first_run);
  ++ blip_number;
  return SAME;
}

t_pf_rv
pf_count_samples (t_cell_run *r)
{
  t_cell_run *first_run = r;
  do {
    num_blip_samples += r->length;
    r += r->next_run_offset;
  } while (r != first_run);
  return SAME;
}

SEXP
radR_get_all_blips (SEXP patchbuff, SEXP blipnumsxp, SEXP linearsxp, SEXP whichsxp) {

  // return the coordinates of all samples in all active patches (if
  // whichsxp is R_NilValue) or in the patches specified by the
  // logical vector whichsxp
  //
  // the output format is determined by linearsxp and blipnumsxp
  //
  //   linearsxp == TRUE, blipnumsxp == TRUE
  //      an n x 2 matrix with columns:
  //        1: patch number
  //        2: linear sample coordinate
  // 
  //   linearsxp == FALSE, blipnumsxp == TRUE
  //      an n x 3 matrix with columns:
  //        1: patch number
  //        2: sample number
  //        3: pulse number
  //
  // In both cases, if blipnumsxp == FALSE, the first column is omitted.
  // 
  // Note that the returned patch numbers are in the range 1..num_selected_patches
  // where num_selected_patches is the number of blips, if whichsxp is R_NilValue,
  // otherwise, it is the number of TRUE values in whichsxp.  i.e. "selected"
  // patches are numbered sequentially
  //
  // All coordinates returned are in origin 1

  t_image image;
  image = (t_image) EXTPTR_PTR(patchbuff);
  do_linear = LOGICAL(linearsxp)[0];
  do_blipnum = LOGICAL(blipnumsxp)[0];

  int patches_specified = (whichsxp != R_NilValue);

  // if user specified which patches to return coords for, then we
  // must count how many samples are in those patches
  if (patches_specified) {
    if (TYPEOF(whichsxp) != LGLSXP || LENGTH(whichsxp) != image->run_info.num_patches)
      error ("radR_get_all_blips:  which.patches must be NULL or a logical vector with one item per patch");
    num_blip_samples = 0;
    enumerate_all_patches (image, &pf_count_samples, 0, LOGICAL(whichsxp));
#ifdef RADR_DEBUG
    printf("radR_get_all_blips: enumerated num_blip_samples = %d\n", num_blip_samples);
#endif
  } else {
    // otherwise, the number of samples is already known
    num_blip_samples = image->run_info.num_active_cells;
#ifdef RADR_DEBUG
    printf("radR_get_all_blips: known num_blip_samples = %d\n", num_blip_samples);
#endif
  }
  // set up static vars for the patch filter

  all_blips = allocMatrix(INTSXP, num_blip_samples, 2 + (do_blipnum ? 1 : 0) - (do_linear ? 1 : 0));
#ifdef RADR_DEBUG
  printf("radR_get_all_blips: allocated all_blips to %d, %d\n", num_blip_samples, 2 + (do_blipnum ? 1 : 0) - (do_linear ? 1 : 0));
#endif
  blip_number = 0;
  sample_number = 0;
  num_cols = image->run_info.num_cols;

  // get all blip coordinates
  if (patches_specified) {
    enumerate_all_patches (image, &pf_get_blip_coords, 0, LOGICAL(whichsxp));
  } else {
    enumerate_patches (image, &pf_get_blip_coords);
  }

  return(all_blips);
}

#ifdef RADR_DEBUG
SEXP
gdb(SEXP x) {
  // a function with an easy name for setting a breakpoint on
  // and which can be used to examine internals for an object
  return x;
}
#endif

/* Event loop handling for the unix platform */

#ifndef Win32
#define R_INTERFACE_PTRS 1
#include <R_ext/eventloop.h>
#include <signal.h>

static void (* old_handler)(void);
static int old_timeout;
static int radR_installed = 0;
static int radR_handler_enabled = FALSE;
static int radR_handler_entered = FALSE;
static SEXP radR_handler_expr;

extern void rl_callback_handler_remove(void);

void radR_fix_readline_problem(void) {
  // remove the tcltk-installed callback handler
  // this fixes the problem of readline being in a non-echo
  // state upon GUI-induced exit of radR
  rl_callback_handler_remove();
}


static void radRHandler(void)
{
  if (! radR_handler_entered) {
    radR_handler_entered = TRUE; // don't need a lock since this is all done in a single thread
    if (radR_handler_enabled && radR_handler_expr)
      eval(radR_handler_expr, R_GlobalEnv);
    if (old_handler)
      old_handler();
    radR_handler_entered = FALSE;
  }
}

SEXP radR_install_handler(SEXP handler_function_name)
{
  // install the handler, or replace its R expression

  if (radR_installed) {
    R_ReleaseObject(radR_handler_expr);
  }
  PROTECT(handler_function_name = AS_CHARACTER(handler_function_name));

  /* build the call expression */
  PROTECT(radR_handler_expr = allocList(1));
  SET_TYPEOF(radR_handler_expr, LANGSXP);
  SETCAR(radR_handler_expr, install(CHAR(STRING_ELT(handler_function_name, 0))));
  UNPROTECT(2);
  R_PreserveObject(radR_handler_expr);
  if (radR_installed)
    return FAIL_SEXP;
  radR_installed = 1;
  old_handler = R_PolledEvents;
  old_timeout = R_wait_usec;
  R_PolledEvents = radRHandler;

  if ( R_wait_usec > 10000 || R_wait_usec == 0)
    R_wait_usec = 10000;
  radR_handler_enabled = TRUE;
  return PASS_SEXP;
}

SEXP radR_remove_handler(void)
{
  if (!radR_installed)
    return FAIL_SEXP;  // fail silently
  
  
  if (R_PolledEvents != radRHandler) {
    return FAIL_SEXP; // fail silently: radR is not the last loaded handler
  }
  R_PolledEvents = old_handler;
  R_wait_usec = old_timeout;
  R_ReleaseObject(radR_handler_expr);
  radR_handler_expr = NULL;

  radR_installed = 0;
  return PASS_SEXP;
}

SEXP radR_enable_handler(SEXP enab) {
  radR_handler_enabled = INTEGER(enab)[0];
  return PASS_SEXP;
}

#endif /* not Win32 */

/*
  a function to call during the event loop to make sure the GUI/tcltk
  queue is checked at least once for each scan processed to allow
  precision in pausing */

SEXP
radR_process_UI_events(void) {
#ifdef Win32
  R_ProcessEvents();
#else
  /* call the old event handler, since we are probably already inside the one we installed */
  if (old_handler)
    (*old_handler) ();
#endif
  return R_NilValue;
}

SEXP
radR_sleep(SEXP ms) {
  /* a function that sleeps for ms milliseconds */
  int mtime = INTEGER(AS_INTEGER(ms))[0];
#ifdef Win32
  Sleep((DWORD)mtime);
#else
  struct timespec desired, remaining;
  int rv;
  desired.tv_sec = 0;
  desired.tv_nsec = 1000000 * mtime;
  for(;;) {
    rv = nanosleep(&desired, &remaining);
    if (!rv)
      break;
    if (rv == EINTR) {
#ifdef RADR_DEBUG
      puts("nanosleep was interrupted");
#endif
    }
    desired = remaining;
  }
#endif
  return R_NilValue;
}

SEXP
radR_get_error() {
  /* return the current error code, with zero representing no error */

  return ScalarInteger(error_code);
}

SEXP
radR_get_error_msg() {
  /* return a string representing extra information on the current error, if any 
     resets the internal error text to empty. */
  SEXP s;
  PROTECT(s = allocVector(STRSXP, 1));
  SET_STRING_ELT(s, 0, mkChar(extra_error_info));
  UNPROTECT(1);
  return s;
}

SEXP
first_empty_slot (SEXP list) 
{
  // return the index of the first available (ie. NULL) slot in
  // generic vector list; if none is found, return 1+LENGTH(list),
  // since this is where into_first_empty_slot will put a new item
  // For an R_NULL list, this returns 1 for the same reason.

  int i;
  if (isNull(list)) {
    i = 0;
  } else {
    if (!IS_VECTOR(list))
      error("first_empty_slot: argument must be a list");
    for (i = 0; i < LENGTH(list); ++i)
      if (VECTOR_ELT(list, i) == R_NilValue)
	break;
  }
  return ScalarInteger(i + 1); /* R indices begin at 1 */
}

#define LIST_EXTENSION_INCREMENT 100 // amt by which to extend a list in into_first_empty_slot
SEXP
into_first_empty_slot (SEXP list, SEXP item) 
{
  // install item in the first available (ie. NULL) slot in
  // generic vector list; if all slots are used, append item
  // to list

  int i;
  int n=0; /* -Wall */
  int nn;
  int not_set = TRUE;
  SEXP rv;
  if (isNull(list)) {
    n = 0;
  } else if (!IS_VECTOR(list)) {
    error("into_first_empty_slot: first argument must be a list");
  } else {
    n = LENGTH(list);
  }
  nn = n; /* length of return value */
  for (i = 0; i < n; ++i)
    if (VECTOR_ELT(list, i) == R_NilValue)
      break;
  if (i == n)
    ++nn;
  PROTECT(rv = allocVector(VECSXP, nn));
  for (i = 0; i < n; ++i) {
    if (not_set && VECTOR_ELT(list, i) == R_NilValue) {
      SET_VECTOR_ELT(rv, i, item);
      not_set = FALSE;
    } else {
      SET_VECTOR_ELT(rv, i, VECTOR_ELT(list, i));
    }
  }
  if (nn > n)
    SET_VECTOR_ELT(rv, n, item);
  UNPROTECT(1);
  return rv;
}
  
SEXP
num_full_slots (SEXP list)
{
  // return the number of full slots in a list
  int i, n;
  if (list == R_NilValue)
    return ScalarInteger(0);
  if (!IS_VECTOR(list))
    error("num_full_slots: argument must be a list");
  for (i = 0, n = 0; i < LENGTH(list); ++i)
    if (VECTOR_ELT(list, i) != R_NilValue)
      ++n;
  return ScalarInteger(n);
}  

SEXP
which_slots_full (SEXP list)
{
  // return an integer vector of indexes of the full slots
  // in a list
  int i, n;
  SEXP rv;
  if (list == R_NilValue) {
    n = 0;
  } else {
    if (!IS_VECTOR(list))
      error("which_slots_full: argument must be a list");
    for (i = 0, n = 0; i < LENGTH(list); ++i)
      if (VECTOR_ELT(list, i) != R_NilValue)
	++n;
  }
  rv = allocVector(INTSXP, n);
  if (n > 0) {
    for (i = 0, n = 0; i < LENGTH(list); ++i)
      if (VECTOR_ELT(list, i) != R_NilValue)
	INTEGER(rv)[n++] = i + 1;
  }
  return rv;
}  

#ifdef Win32
SEXP
start_process (SEXP path_and_dir)
{
  // start a process with specified path and working directory,
  // return an EXTPTR-wrapped process handle or NULL on error

  // path_and_dir: a two-element character vector with these elements
  //   [1]: the full path to the executable file
  //   [2]: the directory in which the executable is to be run

  SECURITY_ATTRIBUTES sa;
  PROCESS_INFORMATION pi;
  STARTUPINFO si;

  // inherit current security attributes
  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;
  
  // use default start info

  si.cb = sizeof(si);
  si.lpReserved = NULL;
  si.lpReserved2 = NULL;
  si.cbReserved2 = 0;
  si.lpDesktop = NULL;
  si.lpTitle = NULL;
  
  if (!CreateProcess(0, CHAR(STRING_ELT(path_and_dir, 0)), &sa, &sa, FALSE,
		      0, NULL, CHAR(STRING_ELT(path_and_dir, 1)), &si, &pi))
    return R_NilValue;
  return PTR_TO_EXTPTR((void *) pi.hProcess);
}

SEXP
end_process (SEXP handle)
{
  // end the process with the given handle
  if (handle != R_NilValue)
    return (ScalarLogical(TerminateProcess((HANDLE) EXTPTR_PTR(handle), 0)));
  return ScalarLogical(0);
}

#if 0
#define MAX_INTERFACES 10
unsigned int _saved_Rpc_timeouts[MAX_INTERFACES];

SEXP
zero_network_timeouts() 
{
  int i, n;

  n =  ;//get num bindings

  for (i = 0; i < n; ++i)
    RpcMgmtInqComTimeout(/* get interface #i */, _saved_Rpc_timeouts + i);
  return ScalarLogical(1);
}

SEXP
restore_network_timeouts() 
{
  int i, n;

  n =  ;//get num bindings

  for (i = 0; i < n; ++i)
    RpcMgmtsetComTimeout(/* get interface #i */, _saved_Rpc_timeouts[i]);
  return ScalarLogical(1);
}

#endif 

#endif  /* Win32 */

SEXP
get_addr (SEXP s) {
  // return the address of an SEXP as an integer
  return ScalarInteger((int) s);
}

SEXP
radR_estimate_from_approx (SEXP approx, SEXP span, SEXP time_win) {
  // estimate "state" values across time_win from an approximator
  // 
  // approx: double vector
  // span: integer scalar: number of components in a "state"
  // time_win: double 3-vector: c(start_time, time_step, num_steps)
  //
  // returns a double vector of length num_steps * span
  //
  // if LENGTH(approx) == span, then the approximator is constant
  //
  // otherwise, LENGTH(approx) == n * (1 + span), where n is the number
  // of points in the approximation.  Each point is stored as span + 1 doubles,
  // consisting of the state, followed by a double timestamp.

  unsigned size = LENGTH(approx);

  unsigned num_steps = REAL(time_win)[2];
  
  unsigned span_ = (unsigned) INTEGER(AS_INTEGER(span))[0];

  SEXP rv = allocVector(REALSXP, span_ * num_steps);

  estimate_from_approx(REAL(approx), size, span_, REAL(time_win)[0], REAL(time_win)[1], num_steps, NA_REAL, REAL(rv));

  return rv;
}


/** 
 * get metadata for a sequence of pulses
 * 
 * @param np number of pulses
 * @param ts timestamp of first pulse
 * @param dur duration of pulse sequence, in milliseconds
 * @param orient orientation of scan: +1 = clockwise, -1 = CCW
 * @param ang_offset angle of first pulse in degrees CW from North, if rotation axis were z and rotation from nominal North, if 
 * @param rot_ax REAL vector; approximator for rotation axis; either a pair (azimuth, elevation)
 * or a sequence of (azimuth, elevation, timestamp) values, which are used to interpolate the values of azimuth and elevation.
 * Note: azimuths are degrees CW from N; elevations are above horizontal.
 * @param tilt REAL vector;  approximator for tilt angle; either a single angle (degrees above horizontal) or a sequence of (angle, timestamp)
 * pairs, used for interpolation.
 * @param rv[inout] must be a list whose first 5 elements are REAL vectors of length np; e.g. this can be a dataframe
 * with np rows whose first 5 columns are numeric.  The first 5 elements of \v rv are modified in place.
 * 
 * @return 
 */

SEXP
radR_get_pulse_metadata (SEXP np, SEXP ts, SEXP dur, SEXP orient, SEXP ang_offset, SEXP rot_ax, SEXP tilt, SEXP rv, SEXP naval)
{
  unsigned np_ = INTEGER(AS_INTEGER(np))[0];
  double ts_ = REAL(AS_NUMERIC(ts))[0];
  double dur_ = REAL(AS_NUMERIC(dur))[0] / 1000.0; // convert from milliseconds to seconds
  double orient_ = REAL(AS_NUMERIC(orient))[0];
  double ang_offset_ = REAL(AS_NUMERIC(ang_offset))[0];
  unsigned i;
  int default_rot_ax = 0;

  // sanity check for return storage area

  if (TYPEOF(rv) != VECSXP || LENGTH(rv) != 5)
    return ScalarLogical(0);

  for (i = 0; i < 5; ++i)
    if (TYPEOF(VECTOR_ELT(rv, i)) != REALSXP || LENGTH(VECTOR_ELT(rv, i)) != np_)
      return ScalarLogical(0);

  if (rot_ax == R_NilValue) {
    // no valid rotation axis was provided, so use the z (vertical) axis

    PROTECT(rot_ax = allocVector(REALSXP, 2));

    REAL(rot_ax)[0] =  0.0;  // default rotation axis azimuth 
    REAL(rot_ax)[1] = 90.0;  // default rotation axis elevation
    default_rot_ax = 1;      // flag a temporary SEXP was created
  }

  get_pulse_metadata (np_, ts_, dur_, orient_, ang_offset_, REAL(rot_ax), LENGTH(rot_ax), REAL(tilt), LENGTH(tilt),
		      REAL(VECTOR_ELT(rv, 0)),
		      REAL(VECTOR_ELT(rv, 1)),
		      REAL(VECTOR_ELT(rv, 2)),
		      REAL(VECTOR_ELT(rv, 3)),
		      REAL(VECTOR_ELT(rv, 4)),
		      REAL(naval)[0]);

  if (default_rot_ax)
    UNPROTECT(1);

  return ScalarLogical(1);
}

  
/*================================================================

  radR.dll method registration, initialization, and destruction

  ================================================================*/


R_CallMethodDef radR_call_methods[]  = {
  // R hook functions
#ifdef RADR_DEBUG
  MKREF(gdb				, 1),
  MKREF(int_wrapped_pointer_to_sexp     , 1),
#endif
  MKREF(radR_alloc_patch_image		, 0),
  MKREF(radR_calculate_scores		, 4),
  MKREF(radR_classify_samples		, 4),
  MKREF(radR_convert_scan		, 13),
  MKREF(radR_find_patches		, 5),
  MKREF(radR_filter_noise               , 2),
  MKREF(get_active_runbuf               , 1),
  MKREF(get_runbuf_info                 , 1),
  MKREF(get_indexes_from_runbuf         , 1),
  MKREF(set_runbuf                      , 2),
  MKREF(assign_extmat_by_runbuf         , 3),
  MKREF(index_extmat_by_runbuf          , 2),
  MKREF(radR_estimate_from_approx       , 3),
  MKREF(radR_free_patch_image		, 1),
  MKREF(radR_get_all_blips		, 4),
  MKREF(radR_get_and_set		, 3),
  MKREF(radR_get_error			, 0),
  MKREF(radR_get_error_msg		, 0),
  MKREF(radR_get_pulse_metadata		, 9),
  MKREF(radR_patch_at_sp		, 2),
  MKREF(radR_process_patches		, 15),
  MKREF(radR_process_UI_events		, 0),
  MKREF(radR_sleep			, 1),
  MKREF(radR_unfilter_patches		, 2),
  MKREF(radR_update_stats		, 8),
  MKREF(radR_critical_eval              , 2),
  MKREF(get_addr                        , 1),
#ifndef Win32
  MKREF(radR_install_handler		, 1),
  MKREF(radR_remove_handler		, 0),
  MKREF(radR_enable_handler		, 1),
  MKREF(radR_fix_readline_problem	, 0),
#endif /* Win32 */
#ifdef Win32
  MKREF(start_process                   , 1),
  MKREF(end_process                     , 1),
#endif

  // symbols exported but which are not intended for .Call, but rather direct
  // use by other shared libraries
  MKREF(radR_attach_image_to_extmat     , 3),
  MKREF(copy_image_channel_to_extmat    , 4),
  MKREF(radR_image_extmat_changed       , 1),
  MKREF(radR_image_swap_RB              , 1),
  MKREF(first_empty_slot                , 1),
  MKREF(into_first_empty_slot           , 2),
  MKREF(num_full_slots                  , 1),
  MKREF(which_slots_full                , 1),
  MKREF(random_bytes                    , 1),
  {NULL, NULL, 0}
};

void
init_vars()
{
}

void
R_init_radR(DllInfo *info)
{
  /* Register routines, allocate resources. */

  // set RPC timeout so that loading the seascan library
  // doesn't take forever
  R_registerRoutines(info, NULL, radR_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_PreserveObject(INDEX_ATTRIBUTE_STRING = mkString("index"));
  
  pensure_extmat = (typeof(ensure_extmat)*) R_GetCCallable("extmat", "ensure_extmat");
  pensure_extmat_with_slop = (typeof(ensure_extmat_with_slop)*) R_GetCCallable("extmat", "ensure_extmat_with_slop");
  pensure_extmat_with_slop_trigger = (typeof(ensure_extmat_with_slop_trigger)*) R_GetCCallable("extmat", "ensure_extmat_with_slop_trigger");
  pextmat_to_sexp = (typeof(extmat_to_sexp)*) R_GetCCallable("extmat", "extmat_to_sexp");
  pfree_extmat = (typeof(free_extmat)*) R_GetCCallable("extmat", "free_extmat");
  /* un *nix, set timezone to GMT; in windows, we assume the TZ environment variable is effectively unset, so 
     that tzset() will set time to GMT 
     FIXME: verify/correct this assumption
  */
#ifndef Win32
  setenv("TZ", "GMT", 1);
  signal(SIGSEGV, radR_fix_readline_problem);
#endif
  tzset();  /* set to GMT time, we hope */
}

void
R_unload_radR(DllInfo *info)
{
  /* Release resources. */
  R_ReleaseObject(INDEX_ATTRIBUTE_STRING);
  pfree_extmat(&em_col_row);
  pfree_extmat(&em_row_present);
}
