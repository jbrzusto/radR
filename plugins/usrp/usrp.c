/*  svn $Id$

    radR : an R-based platform for acquisition and analysis of radar data
    Copyright (C) 2006-2011 John Brzustowski        

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


    Wrapper for calling the usrp_radR_plugin C++ code from R.

*/

#include "usrp.h"

static URP my_only_usrp = NULL; // for now, we support only one USRP plugin device, and this is where it lives
static URP_client_data my_client_data;	// client data for the only USRP plugin device

SEXP
create_usrp(SEXP scan_info_names)
{
  // this must be called 
  if (! my_only_usrp) {
    SEXP siv;
    my_only_usrp = URP_make();

    PROTECT(siv = allocVector(VECSXP, LENGTH(scan_info_names))); // NB: LENGTH(scan_info_names) must be >= 11
    SET_NAMES(siv, scan_info_names);
    SET_NAMED(siv, 2);
    R_PreserveObject(siv);
    UNPROTECT(1);


    /* allocate the scan info items which are all protected by the
       preservation of the list itself */

    SET_VECTOR_ELT(siv,  0, ScalarInteger(0));				// pulses               - filled in later
    SET_VECTOR_ELT(siv,  1, ScalarInteger(0));				// samples_per_pulse    - filled in later
    SET_VECTOR_ELT(siv,  2, ScalarInteger(USRP_BITS_PER_SAMPLE));	// bits_per_sample	- constant
    SET_VECTOR_ELT(siv,  3, ScalarReal(-1));				// timestamp		- filled in later
    SET_VECTOR_ELT(siv,  4, ScalarReal(-1));				// duration		- filled in later
    SET_VECTOR_ELT(siv,  5, ScalarReal(-1));				// range_per_sample	- filled in later
    SET_VECTOR_ELT(siv,  6, ScalarReal(0));				// start_range		- constant
    SET_VECTOR_ELT(siv,  7, ScalarReal(0));				// angle_offset		- constant
    SET_VECTOR_ELT(siv,  8, ScalarInteger(+1));				// orientation		- constant (clockwise)
    SET_VECTOR_ELT(siv,  9, ScalarReal(0));				// pulse_length		- filled in later
    SET_VECTOR_ELT(siv, 10, ScalarReal(0));				// PRF			- filled in later

    my_client_data.scan_info_vector = siv;
    URP_set_client_data (my_only_usrp, &my_client_data);
  }
  return ScalarLogical(1);
}

SEXP
get_ports ()
{
  return PTR_TO_EXTPTR(my_only_usrp); // we return only a single EXTPTR
}

URP
ensure_URP(SEXP usxp) 
{
  // if usxp is C:NULL, R:NULL or an extptr with NULL value, return the pointer to our
  // unique USRP object, if it exists; otherwise call R's error function.
  // For future expansion, if usxp already contains a valid URP, return its address.

  if (! usxp || usxp == R_NilValue || ! EXTPTR_PTR(usxp)) {
    if (! my_only_usrp)
      error("usrp.c:  you must call create_usrp() before using other functions");
    usxp = PTR_TO_EXTPTR(my_only_usrp);
  }
  return (URP) EXTPTR_PTR(usxp);
}


SEXP
end_of_data (SEXP usxp)
{
  URP me = ensure_URP(usxp);
  return ScalarLogical(! URP_incoming_data(me));
}

SEXP
get_params (SEXP usxp)
{
  URP me = ensure_URP(usxp);
  SEXP rv = allocVector(REALSXP, N_USRP_RADR_PLUGIN_PARAMS);
  URP_get_params(me, REAL(rv), N_USRP_RADR_PLUGIN_PARAMS);
  return rv;
}

SEXP
set_params (SEXP usxp, SEXP which, SEXP vals)
{
  URP me = ensure_URP(usxp);
  int rv = TRUE;
  int n = LENGTH(which);

  do {
    if (n != LENGTH(vals) || TYPEOF(which) != INTSXP || TYPEOF(vals) != REALSXP) {
      rv = FALSE;
      break;
    } 
    int *ind = INTEGER(which);
    double *v = REAL(vals);
    int i;

    for (i = 0; rv && i < n; ++i)
      if (ind[i] >= 1 && ind[i] <= N_USRP_RADR_PLUGIN_PARAMS)
	rv &= URP_set_param(me, ind[i] - 1, v[i]);
      else
	rv = FALSE;

    if (!rv)
      break;
    
    URP_send_params(me);

  } while (0);

  return ScalarLogical(rv);
}


SEXP
have_trigger (SEXP usxp)
{
  URP me = ensure_URP(usxp);
  return ScalarLogical(URP_digitizing(me, 100)); // wait 100 milliseconds to see if trigger count is increasing
}

SEXP 
connect (SEXP usxp, SEXP filenames, SEXP check_for_daughterboard)
{
  URP me = ensure_URP(usxp);
  return ScalarInteger(URP_connect(me, CHAR(STRING_ELT(filenames, 0)), CHAR(STRING_ELT(filenames, 1)), INTEGER(check_for_daughterboard)[0]));
}

SEXP 
start_up (SEXP usxp)
{
  URP me = ensure_URP(usxp);
  return ScalarInteger(URP_start_up(me));
}

SEXP
shut_down (SEXP usxp)
{
  int rv = 0;
  URP_client_data * cd;
  URP me;

  do {
    if (usxp == R_NilValue || ! EXTPTR_PTR(usxp))
      break;
    me = (URP) EXTPTR_PTR(usxp);
    rv = URP_shut_down(me);
    /* cd = (URP_client_data *) URP_get_client_data (me); */
    /* if (cd && cd->scan_info_vector) { */
    /*   R_ReleaseObject (cd->scan_info_vector); */
    /*   cd->scan_info_vector = NULL; */
    /* } */
  } while (0);
  return ScalarInteger(rv);
}

SEXP
get_scan_data (SEXP usxp, SEXP scanmat)
{
  URP me = ensure_URP(usxp);
  // get the data from the most recently processed scan.  get_scan_info has
  // probably already done the work, so we just check whether data are available.
  //
  // scanmat: the extmat with scan data, wrapped as an EXTPTRSXP
  //
  // Returns:  R_NilValue if the device is invalid or if there are no data available.
  // Otherwise, returns scanmat.

  if (! URP_have_scan_data(me))
    return R_NilValue;

  return scanmat;
}


void
got_scan (int retcode, void *me_void)
{

  // WARNING: NOT MAIN THREAD: don't call R from here
  // Note that because USE_RINTERNALS is defined in radR.h, the SEXP access
  // done by REAL() and VECTOR_ELT() below are okay, because they are macros
  // that don't call any R code.

  URP me = (URP) me_void;
  URP_client_data * cd =  URP_get_client_data (me);
  if (retcode == 0) {
    SEXP siv = cd->scan_info_vector;
#ifdef RADR_DEBUG
    printf("usrp.c:got_scan: cd=%p, siv=%p, ts=%p\n", cd, siv, REAL(VECTOR_ELT(siv, 3)));
#endif
    // scan has been obtained; fill in (possibly) non-constant metadata

    REAL(VECTOR_ELT(siv,  3)) [0] = URP_get_timestamp (me);      
    REAL(VECTOR_ELT(siv,  4)) [0] = URP_get_duration (me) * 1000;
    REAL(VECTOR_ELT(siv,  5)) [0] = URP_get_range_cell_size (me);
    REAL(VECTOR_ELT(siv, 10)) [0] = URP_get_radar_PRF (me);      

    // RISKY:  we set the element of the vector directly (we can't use an R call because we're
    // not in the main thread.  However, we did set this up with the correct aging and protection,
    // so there should be no problem.
    
    *cd->rv = siv;

    // mark that we have scan data
    URP_set_have_scan_data (me, 1);
  } else {
#ifdef RADR_DEBUG
    puts("got_scan got non-zero return code");
#endif
    *cd->rv = R_NilValue;     // tell R polling loop that an error occurred
    URP_set_last_error(me, retcode); // save the error code
  }
}


SEXP
get_scan_info (SEXP usxp, SEXP scanmat, SEXP trv, SEXP trvindex)
{
  // get the header info for the next available scan.  We actually read the whole scan here.

  int trvi = INTEGER(AS_INTEGER(trvindex))[0] - 1;
  SEXP rv;

  URP me = ensure_URP(usxp);
  t_extmat *mat = SEXP_TO_EXTMAT(scanmat);
  URP_client_data *cd;

  int n_pulses, n_samples;

  if (! URP_started(me)) {
#ifdef RADR_DEBUG   
    puts("USRP is not started\n");
#endif
    return R_NilValue;
  }

  // make sure the destination matrix is big enough, and record its dimensions in n_pulses, n_samples,
  // which will be returned as part of scan metadata

  (*pensure_extmat) (mat, n_pulses = URP_get_param(me, URP_PARAM_N_PULSES, NA_REAL), n_samples = URP_get_param(me, URP_PARAM_N_SAMPLES, NA_REAL));

  cd = (URP_client_data *) URP_get_client_data (me);


  cd->rv = & (VECTOR_ELT(trv, trvi));
  cd->have_scan_data = FALSE;

  // copy the matrix dimensions into the scan metadata

  INTEGER(VECTOR_ELT(cd->scan_info_vector, 0))[0] = n_pulses;
  INTEGER(VECTOR_ELT(cd->scan_info_vector, 1))[0] = n_samples;
  
  SET_VECTOR_ELT(trv, trvi, ScalarInteger(NA_INTEGER));

  // allocate our return value now, before we start the other thread

  rv = ScalarInteger(NA_INTEGER);

  if (!URP_get_sweep_nb ( me,
			  (unsigned short *) (mat->ptr), // buffer for data
			  mat->rows, // pulses
			  mat->cols, // samples per pulse
			  TRUE, // want gated data (FIXME: for now)
			  NULL, // FIXME: buffer for pulse angles
			  NULL, // FIXME: buffer for pulse times
			  &got_scan) // callback function
      ) {
#ifdef RADR_DEBUG    
    puts ("URP_get_sweep_nb returned 0\n");
#endif
    return R_NilValue;
  }

  return rv;
}


SEXP
get_raw_data (SEXP usxp, SEXP scanmat)
{
  // fill the given matrix with ungated raw data; can be used to obtain data
  // directly from the 4 radar signal lines

  URP me = ensure_URP(usxp);
  t_extmat *mat = SEXP_TO_EXTMAT(scanmat);

  if (! URP_started(me))
    return R_NilValue;

  if (!URP_get_sweep ( me,
		       (unsigned short *) (mat->ptr), // buffer for data
		       mat->rows, // pulses
		       mat->cols, // samples per pulse
		       FALSE, // ungated data
		       NULL, // FIXME: buffer for pulse angles
		       NULL  // FIXME: buffer for pulse times
		       )
      )
    return R_NilValue;
  return ScalarLogical(1);
}

R_CallMethodDef usrp_call_methods[]  = {
  MKREF(create_usrp     , 1),
  MKREF(get_ports	, 0),
  MKREF(end_of_data	, 1),
  MKREF(get_params	, 1),
  MKREF(get_scan_data	, 2),
  MKREF(get_raw_data    , 2),
  MKREF(get_scan_info	, 4),
  MKREF(have_trigger	, 1),
  MKREF(set_params	, 3),
  MKREF(shut_down	, 1),
  MKREF(connect	        , 3),
  MKREF(start_up	, 1),
  {NULL, NULL, 0}
};

void
R_init_usrp(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines (info, NULL, usrp_call_methods, NULL, NULL);
  my_only_usrp = NULL;
}

void
R_unload_usrp(DllInfo *info)
{
  if (my_only_usrp) {
    URP_client_data * cd = (URP_client_data *) URP_get_client_data (my_only_usrp);
    if (cd && cd->scan_info_vector) {
      R_ReleaseObject (cd->scan_info_vector);
      cd->scan_info_vector = 0;
    }
    URP_set_client_data (my_only_usrp, 0);

    URP_shut_down (my_only_usrp);
    URP_destroy (my_only_usrp);
    my_only_usrp = NULL;
  }
}
