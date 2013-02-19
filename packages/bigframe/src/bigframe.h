/*

  bigframe.h - definitions for the bigframe module

*/

#ifndef _BIGFRAME_H
#define _BIGFRAME_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#ifndef Win32
#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64
#endif
#include <stdlib.h>
#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <sys/types.h>
#include <stdint.h>
#include <unistd.h>
#include "R.h"
#include "Rinternals.h"
#include "Rdefines.h"
#include "R_ext/Rdynload.h"
#include "cache.h"

// functions for opening and seeking in large (> 2 Gig ?) files
#ifdef Win32
#define bigfopen(F, MODE) fopen64(F, MODE)
#define bigfseek(F, OFF, WHENCE) fseeko64(F, OFF, WHENCE)
#define bigftell(F) ftello64(F)
#else /* unix */
// The same functions work on my linux system, but other *nixes
// will presumably vary.  Using (-1 != lseek64(fileno(F), OFF, WHENCE)) does not work on my system!
#define bigfopen(F, MODE) fopen64(F, MODE)
#define bigfseek(F, OFF, WHENCE) fseeko64(F, OFF, WHENCE)
#define bigftell(F) ftello64(F)
#endif

// for building tables of R-accessible functions, this macro is useful:
// X is the name of the function, N is the number of parameters it takes

#define MKREF(FUN, N) {#FUN, (DL_FUNC) &FUN, N}
#define MKREFN(NAME, FUN, N) {NAME, (DL_FUNC) &FUN, N}

// error return codes for pass/fail .Call functions

#define LOGICAL_SEXP(V) ({SEXP __s__ = allocVector(LGLSXP, 1); LOGICAL(__s__)[0] = (V) ? 1 : 0; __s__;})

#define PASS_SEXP LOGICAL_SEXP(1)
#define FAIL_SEXP R_NilValue
#define TRUE_SEXP ScalarLogical(1)
#define FALSE_SEXP ScalarLogical(0)

// create an R external pointer object (EXTPTR SEXP) from a pointer
#define PTR_TO_EXTPTR(_P_) R_MakeExternalPtr((void *)(_P_), R_NilValue, R_NilValue)

// the type for row indexes in the bigframe
typedef int t_bigframe_index;

// forward declaration
struct bigframe_cache_entry;

// a structure for caching bigframe rows

typedef struct bigframe_cache_entry {
  t_bigframe_index key; // index into the file of this cached row (zero-based)
  char *data; // pointer into buffer of cached row
  STRUCT_CACHE_ENTRY_ITEM(bfc, struct bigframe_cache_entry); // cache management fields
} t_bigframe_cache_entry;

typedef
struct {
  FILE *file; // file in which bigframe data is stored
  int num_rows; // current number of rows in bigframe
  int num_cols; // number of columns in bigframe
  int row_bytes; // bytes per row of bigframe
  long long data_offset; // offset to start of first bigframe row
  char *row_buff; // buffer for 2 rows of data plus cache for a user-specified number of rows
                  // the first is for assembling rows for read or write
                  // the second is a row of appropriately-typed NA values
                  // subsequent rows are the cache
  unsigned char *col_types; // vector of column types (???SXP constants from Rinternals.h)
                            // a column type is one of INTSXP, LGLSXP, REALSXP, CPLXSXP
                            // or STRSXP+n-1, where 4*n is the maximum allowed string length
                            // for that column (and STRSXP+n-1 <= 255), INCLUDING the final
                            // zero byte. (e.g. STRSXP+4 means strings of up to length 19)
  int *cum_col_size; // cumulative size (in bytes) of column types
  STRUCT_CACHE_ROOT_ITEM(bfc, struct bigframe_cache_entry);
  t_bigframe_cache_entry *cache_entries;
  int read_only;
} t_bigframe;

#define BIGFRAME_NA_BUFF(bf) ((bf)->row_buff + (bf)->row_bytes)
#define BIGFRAME_CACHE_ROW(bf, x) ((bf)->row_buff + (2+x) * (bf)->row_bytes)
#define SEXP_TO_BIGFRAME(S) ((t_bigframe *) EXTPTR_PTR(S))

void bfc_read_back(t_bigframe *cache, t_bigframe_index key, t_bigframe_cache_entry *entry);
void bfc_write_back(t_bigframe *cache, t_bigframe_cache_entry *entry);
t_bigframe_cache_entry *bfc_get_new_entry(t_bigframe *cache);
void bfc_copy_to_entry(t_bigframe *cache, t_bigframe_cache_entry *entry, void * data);
void bigframe_write_row (t_bigframe *bf, int n, char *data);
void bigframe_read_row (t_bigframe *bf, int n, char *buff);
void do_bigframe_set_num_rows(t_bigframe *bf, int nr);
SEXP bigframe_flush (SEXP bfs) ;


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _BIGFRAME_H */
