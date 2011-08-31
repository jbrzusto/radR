/*  svn $Id: scancvt.c 795 2011-05-31 20:09:01Z john $

    radR : an R-based platform for acquisition and analysis of radar data
    Copyright (C) 2006, 2007, 2008 John Brzustowski        

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

#include "radR.h"
#include "scancvt.h"
#include "radRvars.h"

static t_scan_converter*
make_scan_converter ( t_scan_converter *cvt, 
		      int nr,
		      int nc,
		      int w,
		      int h,
		      int x0,
		      int y0,
		      int xc,
		      int yc,
		      double scale,
		      double first_angle,
		      double first_range,
		      int force_new)
{
  // create a scan converter for mapping polar to cartesian data
  // 
  // cvt:  NULL, or an existing scan converter
  //       Where possible, we modify the existing scan converter
  //       rather than generate a new one from scratch.
  //       
  // nr, nc: dimensions of the polar data:  nr angular rows of nc radial slots each
  // w, h: dimensions of output (sub) block
  // x0, y0: offset of output (sub) block in output buffer
  // xc, yc: offset of polar centre in output buffer (need not 
  //         be within the output sub block)
  // scale:  pixels per sample 
  // first_angle:  angle (in radians clockwise from the output horizontal axis) of the first row of source data
  // first_range:  range of first sample, measured in range-cell size.  This need not be an integer.
  //               Negative means there are bogus (pre-trigger)
  //               samples at the start of each pulse; positive means there are missing samples.
  // force_new: if non-zero, a new scan-converter is always generated
  //
  // Returns NULL if the parameters fail a sanity check

  int regen = TRUE;  // do we need a complete regeneration of the scan_converter?
  int *inds = NULL;  // convenience pointer to the index list
  int num_inds = 0; // number of indices in list
  int inds_alloc = 0; // number of indices allocated
  int inds_needed; // number of indices needed 
  int snc = nc * SCVT_EXTRA_PRECISION_FACTOR; // scaled version of nc with extra precision bits
  int s0 = (int) (first_range * SCVT_EXTRA_PRECISION_FACTOR); // scaled version of index of sample at range zero
  int i;

  // sanity check
  if (scale < 1e-5 || scale > 1e5 || first_angle < -1e5 || first_angle > 1e5)
    return NULL;

  if (!cvt) {
    cvt = Calloc(1, t_scan_converter);
  } else {
    // set up convenience variables

    inds = cvt->inds;
    num_inds = cvt->num_inds;
    inds_alloc = cvt->inds_alloc;

    if (!force_new &&
	nr == cvt->nr && 
	nc == cvt->nc && 
	w  == cvt->w  && 
	h  == cvt->h  &&
	x0 - xc == cvt->x0 - cvt->xc && 
	y0 - yc == cvt->y0 - cvt->yc && 
	first_range == cvt->first_range &&
	scale >= cvt->scale) {

      // we haven't zoomed out (which would require a wider range of input data) and
      // basic parameters have not changed, so we can modify the existing converter

      regen = FALSE;


      // -------------------- SIMPLE ROTATION --------------------
      //
      // If first_angle has changed, we do a relatively quick recomputation of the
      // cached indexes rotating which row each index refers to

      if (cvt->first_angle != first_angle) {
	int first_row_offset = - ((int)floor(first_angle / 2 / M_PI * nr + 0.5) % nr);
	if (first_row_offset != cvt->first_row_offset) {
	  int delta = first_row_offset - cvt->first_row_offset;
	  // we only rotate the cached indexes if the change in angle of rotation is large enough to
	  // yield a different row_delta
	  for (i = 0; i < num_inds; ++i) {
	    if (inds[i] >= 0) {
	      inds[i] = (((inds[i] / snc) + delta) % (unsigned) nr) * snc + (inds[i] % snc);
	    } else if (inds[i] != SCVT_NODATA_VALUE) {
	      inds[i] = ~(((((~inds[i]) / snc) + delta) % (unsigned) nr) * snc + ((~inds[i]) % snc));
	    }
	  }
	  cvt->first_row_offset = first_row_offset;
	  cvt->first_angle = first_angle;
	} else {
	  /* the new angle of rotation is too small to be done by rotating the 
	     index cache, so do nothing.  The user can always drag the
	     plot a tiny amount to force regeneration of a scan converter. */
	
	}
      }

      // -------------------- SIMPLE ZOOM IN --------------------
      //
      // If we've zoomed in, do a relatively quick recomputation of the
      // indexes by scaling down which column each index refers to (we don't do
      // this on zoom out, because that typically requires widening the range of input
      // indices).  If the plot window is not entirely within the data range, then
      // this method is invalid since those pixels outside the data range will not
      // be zoomed.  If we detect, this, we bail on the fast zoom.

      if (scale > cvt->scale) {

	// Note: SCVT_ZOOM_FACTOR_PRECISION_BITS + SCVT_EXTRA_PRECISION_BITS +
	// ceiling(log2(nr * nc)) might exceed 31 bits, so we use a 64-bit int in
	// calculating zoomed indexes

	long long int num = (int) floor(0.5 + cvt->scale / scale * (1 << SCVT_ZOOM_FACTOR_PRECISION_BITS));
	int num2 = num / 2 + (1 << (SCVT_ZOOM_FACTOR_PRECISION_BITS - 1));

	for (i = 0; i < num_inds; ++i) {
	  if (inds[i] >= 0) {
	    inds[i] = (inds[i] - inds[i] % snc) - s0 + (((inds[i] % snc + s0) * num + num2) >> SCVT_ZOOM_FACTOR_PRECISION_BITS);
	  } else if (inds[i] != SCVT_NODATA_VALUE) {
	    inds[i] = ~(((~inds[i]) - (~inds[i]) % snc) - s0 + ((((~inds[i]) % snc + s0) * num + num2) >> SCVT_ZOOM_FACTOR_PRECISION_BITS));
	  } else {
	    /* this is a pixel which has no data associated with it at 
	       the current zoom level, so we can't zoom
	       it; flag that regen is needed and bail out */
	    regen = TRUE;
	    break;
	  }
	}
	if (!regen)
	  cvt->scale = scale;
      }  
    }
  } 
  // FIXME: add optimized case for re-using parts of previous window where it overlaps
  // with new window

  if (regen) {
    char use_radial_neighbours;	/* true if radially neighbouring input slots are used for each output slot */
    int angular_neighbour_thresh; /* the minimum (in pixels) at which angular neighbours are used for each pixel */
    int i, j, l;
    int ihi, jhi;
    int range, theta;
    double x, y;
    double theta0, theta_factor;
    int sample_sum;
    char sample_count;
    // -------------------- INDEX FROM SCRATCH --------------------

    cvt->nr = nr; 
    cvt->nc = nc; 
    cvt->w = w;
    cvt->h = h;
    cvt->xc = xc;
    cvt->yc = yc;
    cvt->x0 = x0;
    cvt->y0 = y0;
    cvt->scale = scale;
    cvt->first_angle = first_angle;
    cvt->first_range = first_range;
    cvt->first_row_offset = - ((int)(first_angle / 2 / M_PI * nr) % nr);

    /* we'll need a list big enough to hold up to 4 input slot indexes per output slot */
      
    inds_needed = w * h * 4;
    if (!inds || num_inds < inds_needed) {
      if (inds)
	Free (inds);
      inds = Calloc(inds_needed, int);
      if (inds) {
	inds_alloc = inds_needed;
      } else {
	error("Out of memory allocating scan converter.  Try using a smaller plot window.");
      }
    }

    num_inds = 0;

    /* if a change of one pixel in the x direction causes a change of
       more than one along the scan row (i.e. samples are represented
       by less than one pixel) then we will average 3
       radially-neighbouring samples */

    use_radial_neighbours = scale < 1.0;  

    first_range *= scale; /* convert first_range into pixel units */

    scale /= SCVT_EXTRA_PRECISION_FACTOR; /* from now on, scale is scaled by extra precision bits */

    /* if a change of one pixel in the y direction causes a change of
       more than one scan row, then we will average 3
       angularly-neighbouring samples; we represent this in terms of
       the minimum sample range at and beyond which no such averaging is
       done */

    angular_neighbour_thresh = (int) (1 + nr / (2 * M_PI * scale));

    l = 0; /* avoid a compiler warning */
    jhi = x0 + h;
    ihi = y0 + w; 
    theta0 = 2 * M_PI - first_angle;
    theta_factor = nr / (2 * M_PI);

    for (j = x0; j < jhi; ++j ) {
      y = - (j - yc + 0.5);
      for (i = y0; i < ihi; ++i) {
	x = i - xc + 0.5;
	theta = ((int) (0.5 + theta_factor * (atan2(x, y) + theta0))) % (unsigned) nr;
	range = (int) (0.5 + (sqrt(x * x + y * y) - first_range) / scale);
	if (range >= 0 && range < snc) {
	  // the pixel has at least one corresponding data sample
	  l = theta * snc + range;
	  sample_sum = sample_count = 0;
	  // use up to three neighbours
#ifdef DO_SCAN_CONVERSION_SMOOTHING
	  if (range < angular_neighbour_thresh) {
	    if (use_radial_neighbours && range <= snc - 2 * SCVT_EXTRA_PRECISION_FACTOR) {
	      // radial, angular, and "diagonal" neighbour
	      SCVT_IND(l + SCVT_EXTRA_PRECISION_FACTOR);
	      if (theta > 0) {
		SCVT_IND(l - snc);
		SCVT_IND(l + SCVT_EXTRA_PRECISION_FACTOR - snc);
	      } else {
		SCVT_IND(l + (nr - 1) * snc);
		SCVT_IND(l + SCVT_EXTRA_PRECISION_FACTOR + (nr - 1) * snc);
	      }
	    } else {
	      // just angular neighbour
	      if (theta > 0) {
		SCVT_IND(l - snc);
	      } else {
		SCVT_IND(l + (nr - 1) * snc);
	      }
	    }
	  } else {
	    if (use_radial_neighbours) {
	      if (range <= snc - 2 * SCVT_EXTRA_PRECISION_FACTOR)
		// just radial neighbour
		SCVT_IND(l + SCVT_EXTRA_PRECISION_FACTOR);
	    }
	  }
	  // use the central sample, and mark it as the last for this pixel
	  SCVT_IND_LAST(l);
#else  // DO_SCAN_CONVERSION_SMOOTHING
	  SCVT_IND(l);
#endif // DO_SCAN_CONVERSION_SMOOTHING

	} else { // no corresponding radar data, so mark it as using no samples (it retains background colour)
	  SCVT_NO_IND;
	}
      }
    }
    // if we're using too small a fraction of allocated memory, give the rest back
    // this fraction is arbitrarily set at 1/2, to avoid too much memory fragmentation
    if (inds_alloc > 2 * num_inds) {
      inds = Realloc(inds, num_inds, int);
      inds_alloc = num_inds;
    }
    // update from convenience variables
    cvt->inds = inds;
    cvt->inds_alloc = inds_alloc;
    cvt->num_inds = num_inds;
  }
  return (cvt);
}
 

static t_scan_converter*
make_bogus_scan_converter ( t_scan_converter *cvt, 
		      int nr,
		      int nc,
		      int w,
		      int h,
		      int x0,
		      int y0,
		      int xc,
		      int yc,
		      double scale,
		      int force_new)
{
  // create a bogus scan converter for data that's already cartesian data (e.g. video)
  // 
  // cvt:  NULL, or an existing scan converter
  //       Where possible, we modify the existing scan converter
  //       rather than generate a new one from scratch.
  //       
  // nr, nc: dimensions of the data:  nr rows of nc columns each
  // w, h: dimensions of output (sub) block
  // x0, y0: offset of output (sub) block in output buffer
  // xc, yc: offset of data centre in output buffer (need not 
  //         be within the output sub block)
  // scale:  pixels per sample 
  // force_new: if non-zero, a new scan-converter is always generated
  //
  // Returns NULL if the parameters fail a sanity check

  int regen = TRUE;  // do we need a complete regeneration of the scan_converter?
  int *inds = NULL;  // convenience pointer to the index list
  int num_inds = 0; // number of indices in list
  int inds_alloc = 0; // number of indices allocated
  int inds_needed; // number of indices needed 

  // sanity check
  if (scale < 1e-5 || scale > 1e5)
    return NULL;

  if (!cvt) {
    cvt = Calloc(1, t_scan_converter);
  } else {
    // set up convenience variables

    inds = cvt->inds;
    num_inds = cvt->num_inds;
    inds_alloc = cvt->inds_alloc;

    if (!force_new &&
	nr == cvt->nr && 
	nc == cvt->nc && 
	w  == cvt->w  && 
	h  == cvt->h  &&
	x0 - xc == cvt->x0 - cvt->xc && 
	y0 - yc == cvt->y0 - cvt->yc &&
	scale == cvt->scale) {

      regen = FALSE; // no need to regenerate
    }
  } 

  if (regen) {
    int i, j;
    int ihi, jhi;
    int x_src, y_src;
    double x, y;
    // -------------------- INDEX FROM SCRATCH --------------------

    /* we'll need a list big enough to hold up to 1 input slot index per output slot */
      
    inds_needed = w * h;
    if (!inds || num_inds < inds_needed) {
      if (inds)
	Free (inds);
      inds = Calloc(inds_needed, int);
      if (inds) {
	inds_alloc = inds_needed;
      } else {
	error("Out of memory allocating scan converter.  Try using a smaller plot window.");
      }
    }

    num_inds = 0;

    jhi = x0 + h;
    ihi = y0 + w; 

    for (j = x0; j < jhi; ++j ) {
      y = j - yc + 0.5;
      y_src = nr / 2 + y / scale;
      for (i = y0; i < ihi; ++i) {
	x = i - xc + 0.5;
	x_src = nc / 2 + x / scale;
	if (x_src >= 0 && x_src < nc && y_src >= 0 && y_src < nr)
#ifdef DO_SCAN_CONVERSION_SMOOTHING
	  SCVT_IND_LAST(SCVT_EXTRA_PRECISION_FACTOR * (x_src + nc * y_src));
#else
	  SCVT_IND(SCVT_EXTRA_PRECISION_FACTOR * (x_src + nc * y_src));
#endif
	else
	  SCVT_NO_IND;
      }
    }

    // if we're using too small a fraction of allocated memory, give the rest back
    // this fraction is arbitrarily set at 1/2, to avoid too much memory fragmentation
    if (inds_alloc > 2 * num_inds) {
      inds = Realloc(inds, num_inds, int);
      inds_alloc = num_inds;
    }
    // update from convenience variables
    cvt->inds = inds;
    cvt->inds_alloc = inds_alloc;
    cvt->num_inds = num_inds;
  }
  return (cvt);
} 
		     
static void
convert_scan(t_sample *samp, 
	     t_class *class,
	     t_pixel *pix,
	     int span,
	     t_palette *pal,
	     int sample_shift,
	     int class_shift,
	     int *showclass, 
	     t_scan_converter *cvt)
{
/*
   fill an image (sub)window from polar data using a scan converter

   samp		: first sample in first row of polar input data
   class	: pointer to class value of first sample in polar input data
   pix	        : pointer to first pixel in the full output image (not the actual subimage being filled)
   span		: total pixels per image buffer row; this is used as the change in address
                  from the start of one sub-buffer line to the next.
   pal	        : pointer to palette array
   sample_shift : number of bits to shift sample value right before looking up in palette
   class_shift  : number of bits to shift class value left before using it as index into palettes
                  The colour for a pixel of class X and mean sample value Y is:
                  pal[(Y >> sample_shift) + (X << class_shift)]
   showclass	: flags for each class; show == non-zero
   cvt		: pointer to a scan converter created by make scan_converter
*/

  int *inds;  // convenience pointer to the index list
  int num_inds; // number of indices in list
  int i, j, k;
  int palind;
  int mask; 
#ifdef DO_SCAN_CONVERSION_SMOOTHING
  int sample_sum;
  char sample_count;
#endif
  unsigned char cls;  /* class of the central sample in a pixel */

  // addjust the pixel buffer pointer to the start of the subimage
  pix += cvt->x0 + cvt->y0 * span;

  // convenience variables
  inds = cvt->inds;
  num_inds = cvt->num_inds;
  k = cvt->w;

  // a mask for the final colour value (in case t_sample is signed and includes negative values) 
  mask = (1 << (8 * sizeof(t_sample) - sample_shift)) - 1; 

  // apply the sparse linear map

#ifdef DO_SCAN_CONVERSION_SMOOTHING
  sample_sum = sample_count = 0;
#endif

  for (i = 0, j = 0; i < num_inds; ++i) {
#ifdef DO_SCAN_CONVERSION_SMOOTHING
    if (inds[i] >= 0) {
      sample_sum += samp[inds[i] >> SCVT_EXTRA_PRECISION_BITS];
      ++sample_count;
    } else { 
#endif
      if (inds[i] != SCVT_NODATA_VALUE) {
	// a negative index represents the last one for
	// its pixel, so compute the mean and lookup the colour from the
	// palette for its class.
#ifdef DO_SCAN_CONVERSION_SMOOTHING
	cls = class[(~ inds[i]) >> SCVT_EXTRA_PRECISION_BITS];
#else
	cls = class[(inds[i]) >> SCVT_EXTRA_PRECISION_BITS];
#endif
	if (showclass[cls]) {
#ifdef DO_SCAN_CONVERSION_SMOOTHING
	  // note: rather than divide by sample_count + 1, we shift right by ((sample_count + 1) / 2)
	  // This works because sample_count is 0, 1, or 3 corresponding to 1, 2, or 4 samples being averaged.
	  palind = ((((sample_sum + samp[ (~ inds[i]) >> SCVT_EXTRA_PRECISION_BITS]) >> ((sample_count + 1) >> 1)) >> sample_shift) & mask) + ( cls << class_shift);
#else
	  palind = ((((samp[inds[i] >> SCVT_EXTRA_PRECISION_BITS])) >> sample_shift) & mask) + ( cls << class_shift);
#endif
#ifdef DO_ALPHA_BLENDING
	  INLINE_ALPHA_BLEND(pal[palind], pix[j]);
#else
	  pix[j] = pal[palind];
#endif
	} else {
	  /* do nothing */
	}
#ifdef DO_SCAN_CONVERSION_SMOOTHING
	sample_sum = sample_count = 0;
#endif
      } else {
	// This is a pixel for which no data value exists;
	// its existing value is preserved.

	/* do nothing */
      }
      // we're finished with the current pixel
      if (++j == k) {
	// we've finished a row, so do alpha blending
	// start the next image row

	j = 0;
	pix += span;
      }
#ifdef DO_SCAN_CONVERSION_SMOOTHING
    }
#endif
  }
}

static void
radR_free_scan_converter(SEXP cvtsxp) {
  t_scan_converter *cvt = (t_scan_converter *) EXTPTR_PTR(cvtsxp);
  if (cvt) {
    if (cvt->inds) 
      Free(cvt->inds);
    Free(cvt);
  }
  R_ClearExternalPtr(cvtsxp);
}

SEXP
radR_convert_scan(SEXP sampsxp, SEXP pixsxp, SEXP classsxp, SEXP palettesxp, SEXP centresxp, SEXP scalesxp, SEXP rotsxp, SEXP bitshiftsxp, SEXP visiblesxp, SEXP cvtsxp, SEXP geomsxp, SEXP firstrangesxp, SEXP isrectangularsxp) 
{
  // convert polar scan data to a cartesian image
  // geomsxp: integer vector: (x0, y0, w, h) giving offset and size in pixels of subimage to be filled within
  //          the pixel buffer
  // 
  // return: the scan converter for the specified geometry as an EXTPTRSXP

  t_extmat *samp, *pix, *class, *palette;
  double scale;
  double rotation;
  double first_range;
  int xc, yc;
  int x0, y0;
  int w, h;
  int sampshift, classshift; 
  int *show_class;
  int i;
  t_scan_converter *cvt;
  int is_rectangular;

  samp = SEXP_TO_EXTMAT(sampsxp);
  pix = SEXP_TO_EXTMAT(pixsxp);
  class = SEXP_TO_EXTMAT(classsxp);
  palette = SEXP_TO_EXTMAT(palettesxp);
  scale = REAL(AS_NUMERIC(scalesxp))[0];
  centresxp = AS_INTEGER(centresxp);
  xc = INTEGER(centresxp)[0];
  yc = INTEGER(centresxp)[1];
  rotation = REAL(AS_NUMERIC(rotsxp))[0] * M_PI / 180.0;
  sampshift = INTEGER(bitshiftsxp)[0];
  classshift = INTEGER(bitshiftsxp)[1];
  show_class = INTEGER(visiblesxp);
  cvt = (cvtsxp == R_NilValue) ? NULL : (t_scan_converter *) EXTPTR_PTR(cvtsxp);
  geomsxp = AS_INTEGER(geomsxp);
  x0 = INTEGER(geomsxp)[0];
  y0 = INTEGER(geomsxp)[1];
  w = INTEGER(geomsxp)[2];
  h = INTEGER(geomsxp)[3];
  first_range = REAL(AS_NUMERIC(firstrangesxp))[0];
  is_rectangular = LOGICAL(AS_LOGICAL(isrectangularsxp))[0];

  // make sure the pixel buffer actually contains the specified subimage, and bail if not
  if (x0 < 0 || y0 < 0 || x0 + w > pix->cols || y0 + h > pix->rows)
    error("radR_convert_scan: specified subimage extends beyond actual image buffer");

  // Even if the class has not been assigned, we create a zero-filled matrix for it,
  // representing cold samples.
  (*pensure_extmat)(class, samp->rows, samp->cols);

  // create a scan_converter

  if (is_rectangular) {
    if (!(cvt = make_bogus_scan_converter (cvt, samp->rows, samp->cols, w, h, x0, y0, xc, yc, scale, FALSE)))
      return R_NilValue;
  } else {
    if (!(cvt = make_scan_converter (cvt, samp->rows, samp->cols, w, h, x0, y0, xc, yc, scale, rotation, first_range, FALSE)))
      return R_NilValue;
  }

  // if at least one class is visible, actually do the scan conversion
  for (i = 0; i < LENGTH(visiblesxp); ++i) {
    if (INTEGER(visiblesxp)[i]) {
      convert_scan((t_sample *) samp->ptr, (t_class *) class->ptr, (t_pixel *) pix->ptr, pix->cols, (t_palette *) palette->ptr, sampshift, classshift, show_class, cvt);
      break;
    }
  }
  // make sure the scan converter is a wrapped in an R EXTPTR object with a finalizer
  if (cvtsxp == R_NilValue) {
    cvtsxp = PTR_TO_EXTPTR(cvt);
    R_RegisterCFinalizer(cvtsxp, radR_free_scan_converter);
  }

  return(cvtsxp);
}
