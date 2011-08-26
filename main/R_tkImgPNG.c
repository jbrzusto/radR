#include "radRmodule.h"
#include <tcl.h>
#include <tk.h>

extern int Tkpng_Init(Tcl_Interp *interp);

/*
  R wrapper for tkImgPNG: a PNG image file reader for Tk 
*/

SEXP 
init_tk_img_png(SEXP interpsxp)
{
  // call the tkImgPNG init code with 
  // the tcl interpreter

  // interpsxp: EXTPTRSXP wrapping the pointer to the tcl interpreter
  
  // returns NULL

  //  void * p = EXTPTR_PTR(interpsxp);  FIXME: when get_tcl_interp returns an EXTPTR, uncomment this
  void *p = (void *)(INTEGER(interpsxp)[0]);
  if (TCL_ERROR == Tkpng_Init (p)) {
    error("Couldn't load tkImgPNG.\n");
  }
  return R_NilValue;
}

R_CallMethodDef R_tkImgPNG_call_methods[]  = {
  // R hook functions
  MKREF(init_tk_img_png, 1),
  {NULL, NULL, 0}
};

void
R_init_R_tkImgPNG(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, R_tkImgPNG_call_methods, NULL, NULL);
  //  R_useDynamicSymbols(info, FALSE);
}

void
R_unload_R_tkImgPNG(DllInfo *info)
{
}
