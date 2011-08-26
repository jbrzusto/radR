/*  svn $Id$

    radR : an R-based platform for acquisition and analysis of radar data
    Copyright (C) 2006, 2007 John Brzustowski        

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

    Definitions for the scan conversion functions.

*/

/* 
   Structure managing the sparse linear transformations we use for filling a
   cartesian buffer with data from the appropriate part of a polar buffer.  The
   polar buffer has na * nr slots in angle-major order.  The cartesian buffer
   has w * h slots in row-major order.  We maintain sufficient information
   about the transformation to tell whether its coefficients need to be 
   regenerated.
*/

typedef struct scan_converter_struct {
  int nr, nc;  // dimensions of source data buffer in angle count, radius count
               // data must be stored in increasing radius within increasing angle
               // FIXME:  for now we assume the angles are evenly spaced around 
               // a complete circle.

  int w, h;  // dimensions of image sub-buffer in pixels (width, height)

  int x0, y0;  // offset in pixels from left/top of image buffer to left/top of sub-buffer

  int xc, yc;  // offset in pixels from left/top of image buffer to centrepoint of source data

  double scale; // number of output slots per input slot along the cartesian axes

  double first_angle;  // (mathematical) angle of the first row of source data

  double first_range;  // range of the start of the first column of source data

  int first_row_offset;    // index of the first row to be used in the source data
                           // FIXME: we wrap this offset around on the assumption the source data
                           // form a complete circle

  // We don't use floating point coefficients.  Instead, for each output slot,
  // we maintain a list of up to 5 indexes of slots in the input buffer.  The
  // value for the output slot is then obtained as the average of these input
  // slots.

  int inds_alloc; // how big have we allocated the index list
  int num_inds; // how many indices are actually in index list
  int *inds; // pointer to the indexes; if NULL, there are none

} t_scan_converter; 
  
/* 
   To speed up scan conversion and hence blipmovie playback, you can undefine
   these flags:
*/

// Use up to 4 samples per pixel, rather than 1: ~ 7% slowdown:
//#define DO_SCAN_CONVERSION_SMOOTHING

// Do alpha blending (i.e. allow palette transparency) ~ 1% slowdown:
#define DO_ALPHA_BLENDING

// Also, to retain precision for zooming back out close to the plot centre, we leave 6 bits
// of fractional cache index; i.e. the true cache index is ind_cache[y] >> 4
// This means that ceiling(log2(nr * nc)) <= 31 - SCVT_EXTRA_PRECISION_BITS, so the total
// raw data matrix size is limited to 2^25 entries, or ~ 5700 pulses by 5700 samples per pulse.
// If this becomes limiting, reduce SCVT_EXTRA_PRECISION_BITS (4 is probably enough)

// You can speed up scan conversion slightly by setting SCVT_EXTRA_PRECISION_BITS to 0
// ~1% speedup

#define SCVT_EXTRA_PRECISION_BITS 6
#define SCVT_EXTRA_PRECISION_FACTOR (1 << SCVT_EXTRA_PRECISION_BITS)
#define SCVT_EXTRA_PRECISION_DROP (SCVT_EXTRA_PRECISION_FACTOR - 1)

#define SCVT_NODATA_VALUE 0x80000001
#define SCVT_IND(x)      inds[num_inds++] =   (x)
// negative cache entries mark the last index for a given pixel; we use "~" instead of "-" 
// so that index zero can still be marked as last.
#define SCVT_IND_LAST(x) inds[num_inds++] = (~(x))
#define SCVT_NO_IND      inds[num_inds++] = SCVT_NODATA_VALUE

// the number of bits of fractional precision to apply in zooming existing
// indexes; this is the number of fractional bits used in representing old->pps / pps

#define SCVT_ZOOM_FACTOR_PRECISION_BITS 16     

// FORWARD DECLARATIONS

SEXP
radR_convert_scan(SEXP sampsxp, SEXP pixsxp, SEXP classsxp, SEXP palettesxp, SEXP centresxp, SEXP ppssxp, SEXP rotsxp, SEXP bitshiftsxp, SEXP visiblesxp, SEXP cvtsxp, SEXP geomsxp, SEXP firstrangesxp);

#ifdef DO_ALPHA_BLENDING
/*
   an inline assembly macro that alpha blends 0xAARRGGBB onto 0xaarrggbb,
   using the 128-bit SSE2 extensions

   We use AA, the alpha value from the over pixel to blend its colours
   with those of the under pixel, storing the result in the under pixel.
   The alpha component of the under pixel, aa, is preserved.

   Because there's no packed word division instruction in SSE2,
   we cheat a bit by using right shift by 8 bits instead of division by 255.
   This means alpha is used on each channel (R, G, B) as so:

   alpha        result
   ==========================================================
   0x00         under
   other        ((other + 1) * over + (256 - (other + 1)) * under) / 256

   so the effective alpha values are 0, 2/256, 3/256, ..., 256/256
   rather than 0, 1/255, 2/255, ..., 255/255.
   Since this is just for display, who can tell?

*/

#define INLINE_ALPHA_BLEND(over, under) \
  { \
  int __const_0 = 0; \
  static short __attribute__ ((aligned (16))) __const_1[] = {1, 1, 1, 1, 1, 1, 1, 1}; \
  static short __attribute__ ((aligned (16))) __const_256[] = {256, 256, 256, 256, 256, 256, 256, 256}; \
 \
  __asm__ ("xorps	 %%xmm0, %%xmm0        	 \n\t"			\
	   "movd	 %0    , %%xmm1        	 \n\t"			\
	   "punpcklbw	 %%xmm0, %%xmm1	       	 \n\t"			\
	   "movd	 %1    , %%xmm4        	 \n\t"			\
	   "punpcklbw	 %%xmm0, %%xmm4	       	 \n\t"			\
	   "movaps	 %%xmm1, %%xmm3        	 \n\t"			\
	   "pshuflw	 $0xff , %%xmm3, %%xmm3	 \n\t"			\
	   "pinsrw	 $3,     %4,     %%xmm3	 \n\t"			\
	   "movaps	 %2    , %%xmm5	       	 \n\t"			\
	   "pminsw	 %%xmm3, %%xmm5	       	 \n\t"			\
	   "paddw	 %%xmm5, %%xmm3	       	 \n\t"			\
	   "pmullw	 %%xmm3, %%xmm1	       	 \n\t"			\
	   "movaps	 %3    , %%xmm6        	 \n\t"			\
	   "psubw	 %%xmm3, %%xmm6        	 \n\t"			\
	   "pmullw	 %%xmm6, %%xmm4        	 \n\t"			\
	   "paddusw	 %%xmm4, %%xmm1	       	 \n\t"			\
	   "psrlw	 $8    , %%xmm1	       	 \n\t"			\
	   "packuswb	 %%xmm2, %%xmm1        	 \n\t"			\
	   "movd	 %%xmm1, %1            	 \n\t"			\
	   : /* no outputs */						\
	   : "m" (over), "m" (under), "m" (*__const_1), "m" (*__const_256), "m" (__const_0) \
	   : "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6"	\
	   );								\
}

#endif
