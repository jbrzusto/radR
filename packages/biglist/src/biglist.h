/*

  biglist.h - definitions for the biglist module

*/

#ifndef _BIGLIST_H
#define _BIGLIST_H

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
#include <unistd.h>
#define USE_RINTERNALS
#include "R.h"
#include "Rinternals.h"
#include "Rdefines.h"
#include "R_ext/Rdynload.h"
  //#include "Rconnections.h"
#include "cache.h"
#include "bigframe.h"

// the type for item indexes in the biglist
typedef int t_biglist_index;

// forward declaration
struct biglist_cache_entry;

// a structure for caching biglist items

typedef struct biglist_cache_entry {
  t_biglist_index key; // index in the file of this cached item (zero-based)
  STRUCT_CACHE_ENTRY_ITEM(blc, struct biglist_cache_entry); // cache management fields
} t_biglist_cache_entry;

typedef
struct {
  SEXP con_sexp;  // an SEXP referring to the R connection
  t_bigframe *bf;  // bigframe object that holds the index for this biglist
  int max_ind;     // maximum list index to which an object has been assigned
  int max_ind_written; // maximum index for which an object has been written back from the cache
  STRUCT_CACHE_ROOT_ITEM(blc, struct biglist_cache_entry);
  t_biglist_cache_entry *cache_entries;
  SEXP cache; // actual R list of pointers to cached items
  SEXP header_names; // SEXP of names field of header
  int read_only;
} t_biglist;

#define SEXP_TO_BIGLIST(S) ((t_biglist *) EXTPTR_PTR(S))

void blc_read_back(t_biglist *cache, t_biglist_index key, t_biglist_cache_entry *entry);
void blc_write_back(t_biglist *cache, t_biglist_cache_entry *entry);
t_biglist_cache_entry *blc_get_new_entry(t_biglist *cache);
void blc_copy_to_entry(t_biglist *cache, t_biglist_cache_entry *entry, void * data);

extern SEXP BIGLIST_CLASS_STRING;
extern SEXP CACHE_ATTRIBUTE_STRING;
extern SEXP FILE_ATTRIBUTE_STRING;
extern SEXP FILENAME_ATTRIBUTE_STRING;
extern SEXP DEFAULT_NAMES_ATTRIBUTE_STRING;
extern SEXP NDX_ATTRIBUTE_STRING;

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _BIGLIST_H */
