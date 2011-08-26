/* svn $Id: zone.c 231 2009-02-24 20:02:57Z john $

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


     Functions for handling zones

*/

#include "radRmodule.h"

SEXP
sample_pulse_to_zone (SEXP zones, SEXP sampulse, SEXP dest) {
  /* 
     find the first zone containing each point

     zones: the list of compiled zones; i.e. a list of nx4 integer matrices holding
            sample and point index ranges

     sp: the sample, pulse coordinates of the points (origin 1) as an n x 2 matrix
        sample is the first column, pulse is the second

     dest: an extmat or integer vector where the result is to be stored.  If it
           is an extmat, we ensure it has the required size, otherwise
           it must already have the correct size.  The required size is the number
	   of rows in sp (i.e. the number of query points).

     Returns: an integer vector of length equal to LENGTH(zones) giving the number of query
              points found in each zone.

  */

  int i, j, k, np, nz, ns;
  int *sp, *pp, *seg, *rp = NULL, *count = NULL;

  SEXP rv = NULL;
  if (! LENGTH(zones) || ! LENGTH(sampulse))
    return (R_NilValue);

  np = LENGTH(sampulse) / 2;

  if (inherits(dest, "extmat") && SEXP_TO_EXTMAT(dest)->type == EXTMAT_TYPE_INT) {
    (*pensure_extmat)(SEXP_TO_EXTMAT(dest), np, 1);
    rv = dest;
    rp = (int *) SEXP_TO_EXTMAT(dest)->ptr;
  } else {
    if (TYPEOF(dest) != INTSXP || LENGTH(dest) != np)
      error("point_to_zone: dest arg, if supplied, must be an int extmat, or an integer vector of sufficient length");
    rp = INTEGER(dest);
  }
  nz = LENGTH(zones);
  count = INTEGER(PROTECT(rv = allocVector(INTSXP, nz)));
  memset(count, 0, nz * sizeof(int));

  sp = INTEGER(PROTECT(sampulse = AS_INTEGER(sampulse)));
  pp = sp + np;

  // loop over points
  for (i = 0; i < np; ++i) {
    // loop over zones
    for (j = 0; j < nz; ++j) {
      ns = LENGTH(VECTOR_ELT(zones, j)) / 4;
      seg = INTEGER(VECTOR_ELT(zones, j));
      // loop over segments
      for (k = 0; k < ns; ++k) {
	// check whether the point is inside this segment,
	// dealing correctly with negative coordinates (which indicate the complementary range)
	if (sp[i] >= seg[k] && sp[i] <= seg[ns+k]) {
	  if ((seg[2*ns+k] > 0 && (pp[i] >= seg[2*ns+k] && pp[i] <= seg[3*ns+k])) ||
	      (seg[2*ns+k] < 0 && (pp[i] < - seg[2*ns+k] || pp[i] > - seg[3*ns+k]))) {
	    rp[i] = j + 1;
	    ++count[j];
	    goto found_point;
	  }
	}
      }
    }
    rp[i] = 0;
  found_point:
    ; /* loop bottom */
  }
  UNPROTECT(2);
  return(rv);
}

static SEXP r_sym = NULL;
static SEXP a_sym = NULL;

SEXP
x_y_to_zone (SEXP zones, SEXP xy, SEXP dest) {
  /* 
     find the first zone containing each point

     zones: the list of (non-compiled) zones; i.e. a list of environments, each of which
            contains elements r1, r2, a1, a2 which are all numeric vectors of the same length

     xy: an n x 2 matrix of the x,y coordinates of the points relative to the zone origin

     dest: an extmat or integer vector where the result is to be stored.  If it
           is an extmat, we ensure it has the required size, otherwise
           it must already have the correct size, namely the number of rows in sp.

     Returns: an integer vector of length equal to LENGTH(zones) giving the number of query
              points found in each zone.

	      If either zones or xy has length 0, return NULL.  

  */

  int i, j, k, np, nz, ns;
  double *xp, *yp;
  int *rp = NULL, *count = NULL;
  double *r1, *r2, *a1, *a2, r, a;
  SEXP sxp;

  SEXP rv = NULL;
  if (! LENGTH(zones) || ! LENGTH(xy))
    return (R_NilValue);

  if (!r_sym) {
    r_sym = install("r");
    a_sym = install("a");
  }

  np = LENGTH(xy) / 2;

  if (inherits(dest, "extmat") && SEXP_TO_EXTMAT(dest)->type == EXTMAT_TYPE_INT) {
    (*pensure_extmat)(SEXP_TO_EXTMAT(dest), np, 1);
    rv = dest;
    rp = (int *) SEXP_TO_EXTMAT(dest)->ptr;
  } else {
    if (TYPEOF(dest) != INTSXP || LENGTH(dest) != np)
      error("point_to_zone: dest arg, if supplied, must be an int extmat, or an integer vector of sufficient length");
    rp = INTEGER(dest);
  }

  xp = REAL(PROTECT(xy = AS_NUMERIC(xy)));
  yp = xp + np;

  // loop over zones.  Because the environment lookup is relatively slow,
  // we make one pass through zones, checking all "not found" points for membership
  // in each one. Note that we initially mark all points as not found.

  nz = LENGTH(zones);
  count = INTEGER(PROTECT(rv = allocVector(INTSXP, nz)));
  memset(count, 0, nz * sizeof(int));
  memset(rp, 0, sizeof(int) * np);

  for (j = 0; j < nz; ++j) {
    // get coordinate vectors for this zone
    sxp = findVar(r_sym, VECTOR_ELT(zones, j));
    ns = LENGTH(VECTOR_ELT(sxp, 0));
    r1 = REAL(VECTOR_ELT(sxp, 0));
    r2 = REAL(VECTOR_ELT(sxp, 1));
    sxp = findVar(a_sym, VECTOR_ELT(zones, j));
    a1 = REAL(VECTOR_ELT(sxp, 0));
    a2 = REAL(VECTOR_ELT(sxp, 1));
    

    // loop over points that haven't been found
    for (i = 0; i < np; ++i) {
      if (rp[i])
	continue;

      r = sqrt(xp[i]*xp[i] + yp[i]*yp[i]);
      a = 90 - atan2(yp[i], xp[i]) * 180 / M_PI;
      if (a < 0)
	a += 360.0;
      // loop over segments
      for (k = 0; k < ns; ++k) {
	// check whether the point is inside this segment,
	// dealing correctly with angular ranges overlapping the zero line
	if ( ( (r1[k] > r2[k] && r1[k] >= r && r2[k] <= r)
	       || (r1[k] < r2[k] && r1[k] <= r && r2[k] >= r))
	     &&
	     ( (a1[k] + a2[k] <= 360.0 && a1[k] <= a && a1[k] + a2[k] >= a)
	       || (a1[k] + a2[k] > 360.0 && (a1[k] <= a || a <= a1[k] + a2[k] - 360.0)))) {
	  rp[i] = j + 1;
	  ++count[j];
	  goto found_point;
	}
      }
    found_point:
      ; /* loop bottom */
    }
  }
  UNPROTECT(2);
  return(rv);
}

R_CallMethodDef zone_call_methods[] = {
  MKREF(sample_pulse_to_zone, 3),
  MKREF(x_y_to_zone, 3),
  {NULL, NULL, 0}
};

void
R_init_zone(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, zone_call_methods, NULL, NULL);
  //  R_useDynamicSymbols(info, FALSE);
}

void
R_unload_zone(DllInfo *info)
{
  /* Release resources. */
}
