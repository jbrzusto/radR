/*
  static variables for radR

*/

#include "radRvars.h"

// extern data structures 

t_image_struct patch_image = {.runs = CREATE_USER_EXTMAT(sizeof(t_cell_run), "cell runs"),
			      .scratch_row = CREATE_USER_EXTMAT(sizeof(t_cell_run), "temporary cell runs")}; 	/* the patchified image */

SEXP INDEX_ATTRIBUTE_STRING  = NULL;    /* a SEXP for holding the word "index" */

// error data structures

char extra_error_info[EXTRA_ERROR_INFO_SIZE];
int error_code = RADR_ERROR_NONE;

#ifndef DOING_CPROTO
typeof(ensure_extmat) *pensure_extmat;
typeof(ensure_extmat_with_slop) *pensure_extmat_with_slop;
typeof(ensure_extmat_with_slop_trigger) *pensure_extmat_with_slop_trigger;
typeof(extmat_to_sexp) *pextmat_to_sexp;
typeof(free_extmat) *pfree_extmat;
#endif
