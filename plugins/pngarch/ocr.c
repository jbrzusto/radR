/*  
    ocr.c: simple R shared library for doing OCR on a nativeRaster

    Uses GNU ocrad which bears the following notice:

    GNU Ocrad - Optical Character Recognition program
    Copyright (C) 2003-2014 Antonio Diaz Diaz.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

typedef unsigned int bool;


#include "R.h"
#define USE_RINTERNALS
#include "Rinternals.h"
#include "Rdefines.h"
#include "R_ext/Rdynload.h"
#include "ocradlib.h"

/* 
   do_ocr: perform OCR on a nativeRaster (32-bit integer pixels in AARRGGBB format)
   raster: integer vector in column-major order
   intArgs:    c(height, width, invert, scale, threshold, bit_mask)

 */

SEXP ocr (SEXP raster, SEXP intArgs) {

  int ec;

  struct OCRAD_Descriptor * ocr = OCRAD_open();

  if (! ocr)
    error("Could not allocate OCRAD structure");

  struct OCRAD_Pixmap pm;
  pm.height = INTEGER(intArgs)[0];
  pm.width = INTEGER(intArgs)[1];
  pm.mode = OCRAD_colormap32;
  pm.data = (unsigned char *) INTEGER(raster);

  int bitmask = INTEGER(intArgs)[5];

  if (bitmask) {
    int i;
    for ( i = 0; i < LENGTH(raster); ++i)
      INTEGER(raster)[i] &= bitmask;
  }

  ec = OCRAD_set_image (ocr, & pm, INTEGER(intArgs)[2]);

  if (ec < 0) 
    error ("Failed calling OCRAD_set_image");

  ec = OCRAD_scale (ocr, INTEGER(intArgs)[3] );

  if (ec < 0) 
    error ("Failed calling OCRAD_scale");

  ec = OCRAD_set_threshold (ocr, INTEGER(intArgs)[4]);

  if (ec < 0) 
    error ("Failed calling OCRAD_set_threshold");

  ec = OCRAD_recognize (ocr, 0);

  if (ec < 0) 
    error ("Failed calling OCRAD_recognize");

  int nc = OCRAD_result_chars_total(ocr);

  if (nc < 0)
    error ("Failed calling OCRAD_result_chars");
    
  if (nc == 0)
    return R_NilValue;

  int nb = OCRAD_result_blocks(ocr);

  if (nb < 1)
    error ("Internal error: positive char count but no blocks");

  // allocate a list of char vectors
  // each vector is for one block, and its elements are the lines

  SEXP rv;

  PROTECT(rv = allocVector(VECSXP, nb));

  int i;
  for (i = 0; i < nb; ++i) {
    SEXP v = allocVector(STRSXP, OCRAD_result_lines(ocr, i));
    SET_VECTOR_ELT(rv, i, v);
    int j;
    for (j = 0; j < nb; ++j) {
      SET_STRING_ELT(v, j, mkChar(OCRAD_result_line(ocr, i, j)));
    }
  }
  UNPROTECT(1);
  return rv; 
}

#define MKREF(FUN, N) {#FUN, (DL_FUNC) &FUN, N}

R_CallMethodDef ocr_call_methods[]  = {
  MKREF(ocr, 2),
  {NULL, NULL, 0}
};

void
R_init_ocr(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, ocr_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

void
R_unload_ocr(DllInfo *info)
{
  /* Release resources. */
}
