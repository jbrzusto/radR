#include <stdlib.h>
#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include "radR.h"
#include "patchify.h"


// extern data structures 

extern t_image_struct patch_image;  /* the patchified image */

extern SEXP INDEX_ATTRIBUTE_STRING; /* a SEXP for holding the word "index" */

// error data structures

#define EXTRA_ERROR_INFO_SIZE 255
extern char extra_error_info[EXTRA_ERROR_INFO_SIZE];
extern int error_code;
#define ERROR_INFO(X) strncpy(extra_error_info, X, EXTRA_ERROR_INFO_SIZE)

/* some static vars for linking to library routines which, even though their declarations
   are available at compile time, are no longer automatically looked up by R, apparently
   (as of R 2.4.0)
   The conditional compilation is to avoid causing cproto to fail, because it doesn't
   understand the typeof construct in this setting.
*/
#ifndef DOING_CPROTO
extern typeof(ensure_extmat) *pensure_extmat;
extern typeof(ensure_extmat_with_slop) *pensure_extmat_with_slop;
extern typeof(ensure_extmat_with_slop_trigger) *pensure_extmat_with_slop_trigger;
extern typeof(extmat_to_sexp) *pextmat_to_sexp;
extern typeof(free_extmat) *pfree_extmat;
#endif
