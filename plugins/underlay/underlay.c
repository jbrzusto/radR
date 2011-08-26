/* 
   
underlay.c - speed up for the radR underlay plugin

(C) 2007, John Brzustowski
Licence: GPL

*/
  
#include "radRmodule.h"
  
SEXP
calc_mapper_nice (SEXP src_dim, SEXP src_origin, SEXP src_rotation, SEXP src_size, SEXP dst_dim, SEXP dst_origin, SEXP dst_rotation, SEXP dst_size, SEXP opaque) {

  double scale = REAL(dst_size)[0] / REAL(src_size)[0];
  int i, j, k, m, n, ndx, src_m, src_n, src_i, src_j;
  double theta = (REAL(dst_rotation)[0] + REAL(src_rotation)[0]) / 180 * M_PI;
  double ct = cos(theta);
  double st = sin(theta);
  double dst_x, dst_y;
  double dst_ox = REAL(dst_origin)[0], dst_oy = REAL(dst_origin)[1];
  double src_ox = REAL(src_origin)[0], src_oy = REAL(src_origin)[1];
  int *p_opaque = (int *) LOGICAL(opaque);
  int *p_lhs, *p_rhs;

  SEXP rv;
  SEXP lhs;
  SEXP rhs;

  m = INTEGER(dst_dim)[0];
  n = INTEGER(dst_dim)[1];
  src_m = INTEGER(src_dim)[0];
  src_n = INTEGER(src_dim)[1];
  PROTECT(rv = allocVector(VECSXP, 2));
  PROTECT(lhs = allocVector(INTSXP, m * n));
  PROTECT(rhs = allocVector(INTSXP, m * n));
  p_lhs = INTEGER(lhs);
  p_rhs = INTEGER(rhs);
  
  k = 0;
  // NOTE: for the size of these images, repeated adding should not be too far off from multiplication.
  // Otherwise, calculate dst_x and dst_y from scratch inside their respective loops using multiply.
  dst_x = - dst_ox * scale;
  src_ox += 0.5;
  src_oy += 0.5;
  for (i = 0; i < m; ++i, dst_x += scale) {
    dst_y = - dst_oy * scale;
    for (j = 0; j < n; ++j, dst_y += scale) {
      src_i = (int) floor(ct * dst_x + st * dst_y + src_ox);
      if (src_i >= 0 && src_i < src_m) {
	src_j = (int) floor(ct * dst_y - st * dst_x + src_oy);
	if (src_j >= 0 && src_j < src_n) {
	  ndx = src_i + src_j * src_m;
	  if (p_opaque[ndx]) {
	    p_lhs[k] = 1 + i + j * m;
	    p_rhs[k] = 1 + ndx;
	    ++k;
	  }
	}
      }
    }
  }
  PROTECT(SET_LENGTH(lhs, k));  /* Must re-PROTECT, since SET_LENGTH reallocates. */
  PROTECT(SET_LENGTH(rhs, k));
  SET_VECTOR_ELT(rv, 0, lhs);
  SET_VECTOR_ELT(rv, 1, rhs);
  UNPROTECT(5);
  return (rv);
}

R_CallMethodDef underlay_call_methods[]  = {
  MKREF(calc_mapper_nice, 9),
  {NULL, NULL, 0}
};

void
R_init_underlay(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, underlay_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

void
R_unload_underlay(DllInfo *info)
{
  /* Release resources. */
}
