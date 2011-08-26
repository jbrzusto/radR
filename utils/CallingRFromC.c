#include <stdio.h>
#include "R.h"
#include "Rinternals.h"
#include "R_ext/Rdynload.h"

// How to call an R function from C

SEXP x = NULL;
SEXP callback = NULL;

SEXP
set_callback(SEXP f) {
  if (isFunction(f)) {
    callback = duplicate(f);
  } else {
    error_return("set_callback:  Need a function of one numeric variable.");
  }
  return R_NilValue;
}

SEXP
do_callback()
{
  if (x && callback) {
    defineVar(STRING_ELT(FORMALS(callback), 0), x, CLOENV(callback));
    memcpy(INTEGER(x), AS_INTEGER(eval(BODY(callback), CLOENV(callback))), 100 * sizeof(int));
  }
  return R_NilValue;
  
}

SEXP
make_mat() 
{

  int i, j, k;

  PROTECT(x = allocMatrix(INTSXP, 10, 10));

  for (i=1, k = 0; i <= 10; ++i)
    for (j=1; j <= 10; ++j)
      INTEGER(x)[k++] = i * j;

  UNPROTECT(1);
  return x;
}
    
	
SEXP
square_mat(SEXP y) 
{
  int i;
  int n = length(y);
  for (i=0; i < n; ++i)
    INTEGER(y)[i] = INTEGER(y)[i] * INTEGER(y)[i];
  return R_NilValue;
}

SEXP
print_mat() 
{
  int i;
  int n;
  if (!x)
    return R_NilValue;
  n = length(x);
  for (i=0; i < n; ++i)
    Rprintf("%3d: %d\n", i, INTEGER(x)[i]);
  return R_NilValue;
}  
 
void
R_init_radR(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
}

void
R_unload_radR(DllInfo *info)
{
  /* Release resources. */


}
		    
