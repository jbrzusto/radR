#ifndef _EXTMAT_H
#define _EXTMAT_H

// extmat: self-describing matrix structures, which can be passed
// between modules opaquely via R using EXTPTRs to the extmat
// structure.  The number of bytes allocated is saved in alloc.  The
// occupied part is the first sizeof * rows * cols bytes.
// Large arrays, like the scan buffer, are stored as extmats.

#include "R.h"
#define USE_RINTERNALS
#include "Rinternals.h"
#include "Rdefines.h"
#include "R_ext/Rdynload.h"

#define EXTMAT_TYPE_CHAR 1
#define EXTMAT_TYPE_UCHAR 2
#define EXTMAT_TYPE_SHORT 3
#define EXTMAT_TYPE_USHORT 4
#define EXTMAT_TYPE_INT 5
#define EXTMAT_TYPE_UINT 6
#define EXTMAT_TYPE_LONGLONG 7
#define EXTMAT_TYPE_ULONGLONG 8
#define EXTMAT_TYPE_FLOAT 9
#define EXTMAT_TYPE_DOUBLE 10
#define EXTMAT_TYPE_USER 11

#define EXTMAT_TYPE_CHAR_SIZE 1
#define EXTMAT_TYPE_UCHAR_SIZE 1
#define EXTMAT_TYPE_SHORT_SIZE 2
#define EXTMAT_TYPE_USHORT_SIZE 2
#define EXTMAT_TYPE_INT_SIZE 4
#define EXTMAT_TYPE_UINT_SIZE 4
#define EXTMAT_TYPE_LONGLONG_SIZE 8
#define EXTMAT_TYPE_ULONGLONG_SIZE 8
#define EXTMAT_TYPE_FLOAT_SIZE 4
#define EXTMAT_TYPE_DOUBLE_SIZE 8

// the different indexing modes

typedef enum {
  OUTER_PRODUCT = 0,    // every combination of a row index and a column index
  ROW_COL_MATRIX = 1,   // a matrix of (row, column) pairs
  LINEAR_VECTOR = 2     // a vector of linear indexes 
} t_extmat_index_type;

struct t_extmat;

typedef void (*t_changed_fun) (struct t_extmat *mat, int row, int col, int nrow, int ncol, int mat_nrow, int mat_ncol);

typedef struct {
  union {
    int rows;   	// number of rows of elements
    int length; 	// a more logical synonym when it is a vector
  };
  int cols;   		// number of columns of elements
  int slop;		// number of "slop" elements of storage at the end of the matrix,
                        // in addition to row * cols
  int alloc;  		// number of bytes of storage pointed to by ptr; at least (row * cols + slop) * size
  int size; 		// number of bytes per element
  char *name; 		// name of this extmat (for ease of identification inside R)
  char *ptr;  		// pointer to actual storage
  int type;		// type of element
  int R_owns_name;      // does R own the storage for name?  true if created by a function call, false if by a macro
  int R_owns_storage;   // does R own the storage for the extmat?  If FALSE, redimensioning is not allowed and storage
                        // is not freed on extmat destruction
  t_changed_fun changed_fun; // function to call when the dimensions or contents of the extmat change
  void * changed_fun_extra;  // additional value available to the change function
} t_extmat;

#define EXTMAT_ROWS(x) x.rows
#define EXTMAT_LENGTH(x) EXTMAT_ROWS(x)
#define EXTMAT_COLS(x) x.cols
#define EXTMAT_ALLOC(x) x.alloc
#define EXTMAT_TYPE(x) x.type
#define EXTMAT_SIZE(x) x.size
#define EXTMAT_NAME(x) x.name
#define EXTMAT_PTR(x) x.ptr

#define MKREF(FUN, N) {#FUN, (DL_FUNC) &FUN, N}
#define MKREFN(NAME, FUN, N) {NAME, (DL_FUNC) &FUN, N}

// function prototypes:

void ensure_extmat_with_slop_trigger(t_extmat *m, int rows, int cols, int slop, int notrigger);
void ensure_extmat_with_slop(t_extmat *m, int rows, int cols, int slop);
void ensure_extmat(t_extmat *m, int rows, int cols);
void free_extmat (t_extmat *m);
extern SEXP extmat_to_R_numeric(t_extmat *em, SEXP rowinds, SEXP colinds, t_extmat_index_type itype, int raw_output);
extern void R_numeric_to_extmat(t_extmat *em, SEXP rowinds, SEXP colinds, SEXP val, t_extmat_index_type itype);
extern SEXP extmat_to_sexp(t_extmat *m);

// macro for creating a static custom extmat
#define CREATE_USER_EXTMAT(SIZE, NAME) ((t_extmat) {{.rows = 0}, .size=SIZE, .type=EXTMAT_TYPE_USER, .alloc = 0, .ptr = NULL,  .cols = 0, .name = NAME, .R_owns_name = 0, .R_owns_storage = 1, .changed_fun = NULL, .changed_fun_extra = NULL})
// create a normal extmat
#define CREATE_EXTMAT(TYPE, NAME) ((t_extmat) {{.rows = 0}, .size=TYPE ## _SIZE, .type=TYPE, .alloc = 0, .ptr = NULL,  .cols = 0, .name = NAME, .R_owns_name = 0, .R_owns_storage = 1, .changed_fun = NULL, .changed_fun_extra = NULL})
// create a 'foreign' extmat whose data storage is from outside R; the extmat package will not try to realloc or free this memory
#define CREATE_FOREIGN_EXTMAT(TYPE, NAME) ((t_extmat) {{.rows = 0}, .size=TYPE ## _SIZE, .type=TYPE, .alloc = 0, .ptr = NULL,  .cols = 0, .name = NAME, .R_owns_name = 0, .R_owns_storage = 0, .changed_fun = NULL, .changed_fun_extra = NULL})

#define ENSURE_EXTMAT_WITH_SLOP ensure_extmat_with_slop
#define ENSURE_EXTMAT ensure_extmat
#define EXTMAT_TO_SEXP extmat_to_sexp
#define FREE_EXTMAT free_extmat
#define SEXP_TO_EXTMAT(S) ((t_extmat *) EXTPTR_PTR(S))
#endif /* _EXTMAT_H */
