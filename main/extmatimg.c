//
// extmatimg.c:
//
// Create an extmat with the RGB pixel array for a Tk photo image.
// Causes image update when the extmat is changed, either by indexed
// assignment, redimensioning, or explicit call to radR_image_extmat_changed
//
// (C) 2007 John Brzustowski
//
// Licence: GPL
// 
// This module allows for piggybacking on top of whatever
// image-file-reading code comes with tcltk.  This gives us
// access to the internal data read by a tcltk command like:
//
//   image create photo -file WHATEVER
//
// So we get support for GIF and PPM files, at least.
//
// Usage:
//
//    tcl("toplevel ", ".mywin")
//    tcl.interp <- .Call("get_tcl_interp", as.integer(as.character(tcl("winfo", "id", ".mywin"))))
//    photo.name <- "myphoto"
//    tcl("image", "create", "photo", photo.name, file="whatever.gif")
//    x <- extmat(type="int", dim=c(0,0))
//    .Call("attach_image_to_extmat", photo.name, tcl.interp, x)
//    x[1:50, 1:50]    <- red + green * 256 + blue * 65536 + as.integer(-16777216)
//
// Note: tcltk images use 32 bit pixels with alpha channel: 0xAABBGGRR
//       where AA is the alpha, which should be set to 0xff in order to
//       get an opaque image.  This can be accomplished by adding as.integer(-16777216)
//       to BBGGRR integers.  The order of BBGGRR is opposite to that used
//       internally by radR, but we'll compensate for this by a flag to
//       rss.realize.palette
//
// WARNING: this code depends on the layout of struct PhotoMaster from tkImgPhoto.c.
//          The current code uses the struct as found in tk8.4.13.  If that struct changes
//          in a later version of Tk adopted by R, then this code might also have to change.
//

#include "extmatimg.h"

// forward declaration 
void
do_image_extmat_changed (t_extmat *m, int x, int y, int width, int height, int imgWidth, int imgHeight);

// from tkImgPhoto.c in tk8.4.13:

typedef struct PhotoMaster {
    Tk_ImageMaster tkMaster;	/* Tk's token for image master.  NULL means
				 * the image is being deleted. */
    Tcl_Interp *interp;		/* Interpreter associated with the
				 * application using this image. */
    Tcl_Command imageCmd;	/* Token for image command (used to delete
				 * it when the image goes away).  NULL means
				 * the image command has already been
				 * deleted. */
    int	flags;			/* Sundry flags, defined below. */
    int	width, height;		/* Dimensions of image. */
    int userWidth, userHeight;	/* User-declared image dimensions. */
    Tk_Uid palette;		/* User-specified default palette for
				 * instances of this image. */
    double gamma;		/* Display gamma value to correct for. */
    char *fileString;		/* Name of file to read into image. */
    Tcl_Obj *dataString;	/* Object to use as contents of image. */
    Tcl_Obj *format;		/* User-specified format of data in image
				 * file or string value. */
    unsigned char *pix32;	/* Local storage for 32-bit image. */
    int ditherX, ditherY;	/* Location of first incorrectly
				 * dithered pixel in image. */
    TkRegion validRegion;	/* Tk region indicating which parts of
				 * the image have valid image data. */
    struct PhotoInstance *instancePtr;
				/* First in the list of instances
				 * associated with this master. */
} PhotoMaster;

SEXP
radR_attach_image_to_extmat (SEXP name, SEXP tclinterp, SEXP matsxp) {
  // attach tcl's 32bpp RGB image pixel storage to an existing extmat
  //
  // name:      character vector, tcl name of the image
  // tclinterp: extptr:  pointer to the tcl interpreter
  //            as returned by get_tcl_interp
  // matsxp:    an extmat, e.g. as returned by R function extmat()
  //
  // FIXME: image rows become extmat columns, and vice versa.
  // (because of the extmat row-major order)
  // Frees any existing storage belonging to the extmat.
  
  Tk_PhotoHandle ph;
  Tk_PhotoImageBlock pib;
  Tcl_Interp *my_tcl = (Tcl_Interp*) EXTPTR_PTR(tclinterp);
  t_extmat *m = SEXP_TO_EXTMAT(matsxp);

  ph = Tk_FindPhoto(my_tcl, CHAR(STRING_ELT(name, 0)));
  Tk_PhotoGetImage(ph, &pib);

  if (m->R_owns_storage)
    Free(m->ptr);
  m->ptr = (char *) pib.pixelPtr;
  m->R_owns_storage = 0; // mark that R is not responsible for freeing data storage
  m->changed_fun = m->changed_fun_extra = NULL;
  m->type = EXTMAT_TYPE_INT;
  m->size = EXTMAT_TYPE_INT_SIZE;
  m->cols = pib.width;
  m->rows = pib.height;
  m->slop = 0;
  m->alloc = m->cols * m->rows * m->size;
  m->changed_fun = (t_changed_fun) &do_image_extmat_changed;
  m->changed_fun_extra = (void *) ph;
  return matsxp;
}

SEXP
copy_image_channel_to_extmat (SEXP name, SEXP tclinterp, SEXP matsxp, SEXP chansxp) {
  // copy grayscal values attach tcl's 32bpp RGB image pixel storage to an existing extmat
  // the extmat's type is changed to short, and the channel data will be copied to the
  // low order 8 bits of each word, in row major order
  //
  // name:      character vector, tcl name of the image
  // tclinterp: EXTPTR: pointer to the tcl interpreter
  //            as returned by get_tcl_interp
  // matsxp:    an extmat, e.g. as returned by R function extmat()
  // chansxp:   an integer representing channel: red=0, green=1, blue=2, alpha=3 (on a little-endian machine)
  //
  // Frees any existing storage belonging to the extmat.
  // Does not set a "changed" handler
  
  Tk_PhotoHandle ph;
  Tk_PhotoImageBlock pib;
  Tcl_Interp *my_tcl = (Tcl_Interp*) EXTPTR_PTR(tclinterp);
  t_extmat *m = SEXP_TO_EXTMAT(matsxp);
  unsigned int i;
  unsigned int n;
  unsigned char *src;
  unsigned short *dst;
  int chan_offset = INTEGER(AS_INTEGER(chansxp))[0];
  
  ph = Tk_FindPhoto(my_tcl, CHAR(STRING_ELT(name, 0)));
  Tk_PhotoGetImage(ph, &pib);

  // set up the extmat metadata

  if (! m->R_owns_storage) {
    m->ptr = NULL;
  }
  m->R_owns_storage = 1;
  m->changed_fun = m->changed_fun_extra = NULL;
  m->type = EXTMAT_TYPE_SHORT;
  m->size = EXTMAT_TYPE_SHORT_SIZE;
  (*pensure_extmat) (m, pib.height, pib.width);

  // now copy the desired channel

  dst = (unsigned short *) m->ptr;
  src = ((unsigned char *) pib.pixelPtr) + chan_offset;
  n = pib.height * pib.width;
  for (i = 0; i < n; ++i) {
    *dst++ = (unsigned short) (*src);
    src += 4;
  }

  return matsxp;
}

void
do_image_extmat_changed (t_extmat *m, int x, int y, int width, int height, int imgWidth, int imgHeight) {
  XRectangle rect;
  PhotoMaster *pm = (PhotoMaster *) m->changed_fun_extra;

  pm->flags |= 2;  // mark that image has changed
  pm->ditherX = 0; // mark that dithering is entirely invalid
  pm->ditherY = 0;
  // mark the entire image as valid
  rect.x = x;        
  rect.y = y;
  rect.width = width;
  rect.height = height;
  TkUnionRectWithRegion (&rect, pm->validRegion, pm->validRegion);
  Tk_DitherPhoto ((Tk_PhotoHandle) pm, x, y, width, height);
  Tk_ImageChanged (pm->tkMaster, x, y, width, height, imgWidth, imgHeight);
}
  
SEXP
radR_image_extmat_changed (SEXP matsxp) {
  // record that 
  // FIXME: allow passing a changed subrectangle specificiation

  t_extmat *m = SEXP_TO_EXTMAT(matsxp);
  do_image_extmat_changed (m, 0, 0, m->cols, m->rows, m->cols, m->rows);
  return PASS_SEXP;
}

SEXP 
radR_image_swap_RB (SEXP matsxp) {
  // swap the red and blue bytes of integers representing pixels
  // to convert between Tk's 0xAABBGGRR and radR's 0xAARRGGBB component representations.

  t_extmat *m = SEXP_TO_EXTMAT(matsxp);
  int n = m->cols * m->rows;
  int i;
  typedef struct pix_s {char b[4];} __attribute__((packed)) pix;
  pix *p = (pix *) m->ptr;
  for (i = 0; i < n; ++i) {
    p[i] = (struct pix_s){{p[i].b[2], p[i].b[1], p[i].b[0], p[i].b[3]}};
  }
  return R_NilValue;
}

