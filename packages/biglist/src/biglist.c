/*
  biglist.c
  
  support routines for the biglist class, which stores large lists on disk

*/




#include "biglist.h"

typeof(bigframe_write_row) *pbigframe_write_row;
typeof(bigframe_read_row) *pbigframe_read_row;
typeof(do_bigframe_set_num_rows) *pdo_bigframe_set_num_rows;

SEXP my_do_seek;
SEXP my_do_flush;
SEXP my_do_truncate;
SEXP my_do_close;
SEXP BIGLIST_CLASS_STRING;
SEXP CACHE_ATTRIBUTE_STRING;
SEXP FILE_ATTRIBUTE_STRING;
SEXP FILENAME_ATTRIBUTE_STRING;
SEXP DEFAULT_NAMES_ATTRIBUTE_STRING;
SEXP NDX_ATTRIBUTE_STRING;

static char *seek_origins[] = { "start", "current", "end" };
static char *seek_rw[] = { "r", "w" };

#define R_SEEK_START 0
#define R_SEEK_CURRENT 1
#define R_SEEK_END 2
#define R_SEEK_READ_POS 0
#define R_SEEK_WRITE_POS 1

static double na_buff[2];

SEXP
seek_connection (SEXP con, double where, int origin, int rw)
{
  /* seek the read or write position of the Rconnection in con to
     offset where from the given origin.  Because we don't have
     access to the non-exported R library routines that do this,
     we build and evaluate an R call to "seek" */
  SEXP tmp;
  SEXP rv;
  SEXP wheresxp, originsxp, rwsxp;
  PROTECT (wheresxp = ScalarReal (where));
  PROTECT (originsxp = mkString (seek_origins[origin]));
  PROTECT (rwsxp = mkString (seek_rw[rw]));
  PROTECT (tmp = CONS (my_do_seek, list4 (con, wheresxp, originsxp, rwsxp)));
  SET_TYPEOF (tmp, LANGSXP);
  rv = eval (tmp, R_GlobalEnv);
  UNPROTECT (4);
  return rv;
}

SEXP inline
flush_connection (SEXP con)
{
  /* flush the Rconnection in con */

  SEXP tmp;
  SEXP rv;
  PROTECT (tmp = list2 (my_do_flush, con));
  SET_TYPEOF (tmp, LANGSXP);
  rv = eval (tmp, R_GlobalEnv);
  UNPROTECT (1);
  return rv;
}

SEXP inline
truncate_connection (SEXP con)
{
  /* truncate the Rconnection in con at the most recently used (for read or write) file offset */
  SEXP tmp;
  SEXP rv;
  PROTECT (tmp = list2 (my_do_truncate, con));
  SET_TYPEOF (tmp, LANGSXP);
  rv = eval (tmp, R_GlobalEnv);
  UNPROTECT (1);
  return rv;
}

SEXP inline
close_connection (SEXP con)
{
  /* close the Rconnection in con */
  SEXP tmp;
  SEXP rv;
  PROTECT (tmp = list2 (my_do_close, con));
  SET_TYPEOF (tmp, LANGSXP);
  rv = eval (tmp, R_GlobalEnv);
  UNPROTECT (1);
  return rv;
}



DECLARE_CACHE_FUNCTIONS (blc, t_biglist, t_biglist_cache_entry,
			 t_biglist_index, key);
DEFINE_CACHE_FUNCTIONS (blc, t_biglist, t_biglist_cache_entry,
			t_biglist_index, key, blc_get_new_entry,
			blc_copy_to_entry, blc_read_back, blc_write_back);

t_biglist * CHECK_VALIDITY(SEXP bls) {
  /* sanity checks on bls as a SEXP representing a biglist:
     - not NULL
     - attr(bls, "file") is not NULL
     - attr(bls, "ndx") is not NULL
     - attr(bls, "ndx") is an SEXP pointing to a valid bigframe whose file is open

     If the biglist object appears correct but its index file is closed, then
     flush and close the biglist object.
  */

    SEXP bfs;
    t_bigframe *bf;
    t_biglist *bl;

    if (bls == R_NilValue) 
      error("biglist object is NULL");
    bl = SEXP_TO_BIGLIST(bls);
    if (R_NilValue == bl->con_sexp)
      error("biglist is closed");
    bfs = getAttrib(bls, NDX_ATTRIBUTE_STRING);
    if (bfs != R_NilValue) {
      if (TYPEOF(bfs) == EXTPTRSXP) {
	bf = SEXP_TO_BIGFRAME(bfs);
	if (bf && bf->file) {
	  return (bl);
	}
      } else {
	error("biglist index is not a bigframe");
      }
    }
    /* flush the cache and close the file connection */
    CACHE_FLUSH (blc, bl);
    close_connection (bl->con_sexp);
    bl->con_sexp = R_NilValue;
    error("index file for the biglist is unknown or closed; the biglist will now be closed");
    
    return NULL; /* never reached */
}


/* declarations for R library functions that are not in any headers */

extern SEXP R_serialize (SEXP object, SEXP icon, SEXP ascii, SEXP fun);
extern SEXP R_unserialize (SEXP icon, SEXP fun);

#define INDEX_IN_CACHE(bl, entry) (entry - bl->cache_entries)

void
blc_read_back (t_biglist * bl, t_biglist_index key,
	       t_biglist_cache_entry * entry)
{
  /* read an item from the file into the given cache entry */
  double buff[2];
  SEXP item;
  (*pbigframe_read_row) (bl->bf, key, (char *) &buff);
  if (isnan (buff[0]))
    {
      /* no item exists for that slot in the list */
      SET_VECTOR_ELT (bl->cache, INDEX_IN_CACHE (bl, entry), R_NilValue);
    }
  else
    {
      /* seek to the start of the item */
      seek_connection (bl->con_sexp, buff[0], R_SEEK_START, R_SEEK_READ_POS);
      /* grab the item */
      SEXP call = lang2(findVar(install("unserialize"), R_GlobalEnv), bl->con_sexp);
      PROTECT(item = eval(call, R_GlobalEnv));
      SET_VECTOR_ELT (bl->cache, INDEX_IN_CACHE (bl, entry), item);
      UNPROTECT(1);
    }
}

void
blc_write_back (t_biglist * bl, t_biglist_cache_entry * entry)
{
  /* write an item from the cache into the file 
     FIXME: items are always written to the end of file,
     regardless of whether their original slot might have contained
     enough room for their new values.  Use a free list or
     something...  (biglists are intended for items whose value is
     more-or-less final when it is written)
   */
  double buff[2];
  int i;
  /* seek to the end of file */
  seek_connection (bl->con_sexp, 0, R_SEEK_END, R_SEEK_WRITE_POS);
  /* get the end of file offset */
  buff[0] =
    REAL (seek_connection
	  (bl->con_sexp, NA_REAL, R_SEEK_START, R_SEEK_WRITE_POS))[0];
  /* output the item */
  SEXP call = lang3(findVar(install("serialize"), R_GlobalEnv), VECTOR_ELT (bl->cache, INDEX_IN_CACHE (bl, entry)), bl->con_sexp);
  eval(call, R_GlobalEnv);
  /* calculate the item's file size by subtracting the new end of file offset from the old */
  buff[1] =
    REAL (seek_connection
	  (bl->con_sexp, NA_REAL, R_SEEK_START,
	   R_SEEK_WRITE_POS))[0] - buff[0];
  /* record the index info for the newly-written item, filling in any new hole with NA rows */
  for (i = bl->max_ind_written + 1; i < entry->key; ++i)
    (*pbigframe_write_row) (bl->bf, i, (char *) &na_buff);
  (*pbigframe_write_row) (bl->bf, entry->key, (char *) &buff);
  if (bl->max_ind_written < entry->key)
    bl->max_ind_written = entry->key;
  if (bl->max_ind < bl->max_ind_written)
    bl->max_ind = bl->max_ind_written;
}

t_biglist_cache_entry *
blc_get_new_entry (t_biglist * bl)
{
  /* return the address of a never-used cache entry */
  return bl->cache_entries + CACHE_SIZE (blc, bl);
}

int 
names_identical (SEXP x, SEXP y)
{
  int i;
  if (x == R_NilValue || y == R_NilValue)
    return (x == y);
  if (!IS_CHARACTER(x) || !IS_CHARACTER(y) || LENGTH(x) != LENGTH(y))
    return FALSE;
  for (i = 0; i < LENGTH(x); ++i)
    if (strcmp(CHAR(STRING_ELT(x, i)), CHAR(STRING_ELT(y, i))))
      return FALSE;
  return TRUE;
}

void
blc_copy_to_entry (t_biglist * bl, t_biglist_cache_entry * entry, void *data)
{
  /* copy an item into the cache using duplicate(); if the names attribute matches the
     names field in the header, names are discarded */
  SEXP dup;
  if ((SEXP) data != R_NilValue)
    {
      dup = (NAMED((SEXP)data) == 2) ? duplicate ((SEXP) data) : (SEXP) data;
      /* avoid writing the names attribute if it matches the names item in the header */
      if (names_identical (GET_NAMES (dup), bl->header_names))
	SET_NAMES (dup, R_NilValue);
      /* to prevent modification of the cached item, set its NAMED field to 2 */
      SET_NAMED (dup, 2);
    }
  else
    dup = R_NilValue;
  SET_VECTOR_ELT (bl->cache, INDEX_IN_CACHE (bl, entry), dup);
}

/* create a biglist object 
 
   filename: a character vector with one element

   filecon: an open read/write binary R connection

   ndx: an EXTPTR for the index bigframe

   cache: a pre-allocated R list list to hold pointers to the cached items
          (e.g.  R:vector("list", 100))

   names: the default names for items in the biglist which have no 
          names attribute and have the same LENGTH() as names

   len:   the length of the list (i.e. maximum R index of an actual
          item in the list; there may be holes)

   Returns an EXTPTR for the new biglist, with these attributes:

     class = "biglist"
     cache = cache
     file = file
     filename = filename
     ndx = ndx
*/


SEXP
biglist_create (SEXP filename, SEXP filecon, SEXP ndx, SEXP cache, SEXP names, SEXP len, SEXP readonly)
{
  t_biglist *bl;
  int csize;
  SEXP rv;

  /* NOTE: we always duplicate filecon, cache, and names to ensure
     that setAttrib will not.  That way, bl->cache, bl->header_names,
     and bl->con_sexp are guaranteed to point to the same SEXPs as the
     biglist R object's attributes (and hence these fields are
     protected from garbage collection).
  */

  bl = Calloc (1, t_biglist);
  PROTECT(bl->con_sexp = duplicate(filecon));
  bl->bf = SEXP_TO_BIGFRAME (ndx);

  csize = LENGTH (cache);
  if (csize < 1)
    error("biglist: cache.size must be at least 1");

  bl->read_only = LOGICAL(AS_LOGICAL(readonly))[0];

  // allocate the cache management entries
  bl->cache_entries = Calloc (csize, t_biglist_cache_entry);

  PROTECT(bl->cache = duplicate(cache));

  // record number of entries
  bl->max_ind = bl->max_ind_written = INTEGER (len)[0] - 1;

  // record the names item in the header, so we can avoid
  // duplicating it when writing biglist items

  PROTECT(bl->header_names = duplicate(names));
  /* initialize the (empty) cache */
  CACHE_INIT (blc, bl, csize);
  rv = PTR_TO_EXTPTR (bl);
  setAttrib(rv, FILENAME_ATTRIBUTE_STRING, filename);
  setAttrib(rv, FILE_ATTRIBUTE_STRING, bl->con_sexp);
  setAttrib(rv, NDX_ATTRIBUTE_STRING, ndx);
  setAttrib(rv, DEFAULT_NAMES_ATTRIBUTE_STRING, bl->header_names);
  setAttrib(rv, CACHE_ATTRIBUTE_STRING, bl->cache);
  SET_CLASS(rv, BIGLIST_CLASS_STRING);
  UNPROTECT(3);
  return rv;
}


SEXP
biglist_get (SEXP bls, SEXP index)
{
  /* return item index from the biglist; if index does not
     correspond to an item in the biglist, return NULL.
     Convert index to origin 0 from origin 1. */

  SEXP item;
  t_biglist *bl;
  int i = INTEGER (AS_INTEGER (index))[0] - 1;

  bl = CHECK_VALIDITY(bls);
  if (i < 0)
    error("biglist: index must be positive");
  /* to avoid filling the cache with NULLs, do a quick test for an
     item we know doesn't exist */
  if (i > bl->max_ind)
    return R_NilValue;

  /* if there is no item at the given index, CACHE_GET will still
     return a valid cache entry, but its data will be R_NilValue */

  item = VECTOR_ELT (bl->cache, INDEX_IN_CACHE (bl, CACHE_GET (blc, bl, i)));
  if (isNull (GET_NAMES (item))
      && LENGTH (item) == LENGTH (bl->header_names))
    SET_NAMES (item, bl->header_names);
  return item;
}


SEXP
biglist_put (SEXP bls, SEXP index, SEXP value)
{
  /* put the given value into slot index of the biglist; return the
     biglist object. Convert index to origin 0 from origin 1. */

  t_biglist *bl;
  int i = INTEGER (AS_INTEGER (index))[0] - 1;

  bl = CHECK_VALIDITY(bls);
  if (i < 0)
    error("biglist: index must be positive");
  if (bl->read_only)
    error("biglist: can't assign items when opened for read only");
  CACHE_PUT (blc, bl, i, value);
  if (i > bl->max_ind)
    bl->max_ind = i;
  return bls;
}

SEXP
biglist_length (SEXP bls)
{
  /* return the length; i.e. the maximum index
     to which an item has been assigned */

  t_biglist *bl;
  bl = CHECK_VALIDITY(bls);
  return ScalarInteger (bl->max_ind + 1);
}

SEXP
biglist_flush (SEXP bls)
{
  /* flush the cache for this biglist, writing all "dirty" entries to
     disk */

  t_biglist *bl;

  bl = CHECK_VALIDITY(bls);
  CACHE_FLUSH (blc, bl);
  flush_connection (bl->con_sexp);
  return PASS_SEXP;
}

SEXP
biglist_close (SEXP bls)
{
  /* close this biglist, writing all "dirty" entries to disk */

  t_biglist *bl;

  bl = CHECK_VALIDITY(bls);
  CACHE_FLUSH (blc, bl);
  close_connection (bl->con_sexp);
  bl->con_sexp = R_NilValue;
  return PASS_SEXP;
}

static int
comp_first_elt (const void *e1, const void *e2)
{
  /* sort in increasing order, but put all nan's first */

  if (isnan (*(double *) e1))
    return -1;
  if (isnan (*(double *) e2))
    return 1;
  return (*(double *) e1 - *(double *) e2);
}

static int
comp_ints (const void *e1, const void *e2)
{
  /* sort integers in increasing order */

  return (*(int *) e1 - *(int *) e2);
}

SEXP
biglist_drop (SEXP bls, SEXP index)
{
  /* put NULL into each slot specified by index;
     then truncate as much of the file as possible
     (i.e. the largest tail consisting only of 
     dropped items)

     return TRUE
   */
  SEXP ndx_buff;
  t_biglist_cache_entry *ce;
  double eof_off;
  int i, n;

  t_biglist *bl;

  bl = CHECK_VALIDITY(bls);
  PROTECT (index = AS_INTEGER (index));
  n = LENGTH (index);
  PROTECT (ndx_buff = allocMatrix (REALSXP, n, 2));

  /* seek to the end of file */
  seek_connection (bl->con_sexp, 0, R_SEEK_END, R_SEEK_WRITE_POS);
  /* get the end of file offset */
  eof_off =
    REAL (seek_connection
	  (bl->con_sexp, NA_REAL, R_SEEK_START, R_SEEK_WRITE_POS))[0];

  /* sort the indexes of items to drop */
  qsort (INTEGER (index), n, sizeof (int), comp_ints);

  /* get the index information for the entries to be dropped */
  for (i = 0; i < n; ++i)
    (*pbigframe_read_row) (bl->bf, INTEGER (index)[i] - 1,
		       (char *) (REAL (ndx_buff) + 2 * i));

  /* sort the index entries in increasing order by file position */

  qsort (REAL (ndx_buff), n, 2 * sizeof (double), comp_first_elt);

  /* adjust eof_off, removing as many contiguous (in the file) items
     as possible from the tail */

  for (i = n - 1; i >= 0; --i)
    {
      if (isnan (REAL (ndx_buff)[2 * i])
	  || eof_off - REAL (ndx_buff)[2 * i + 1] != REAL (ndx_buff)[2 * i])
	break;
      eof_off = REAL (ndx_buff)[2 * i];
    }

  /* if at least one item is at the end of the file, truncate the file */
  if (i < n - 1)
    {
      seek_connection (bl->con_sexp, eof_off, R_SEEK_START, R_SEEK_WRITE_POS);
      flush_connection (bl->con_sexp);	/* R or libc or linux bug:  need to flush after seeking in order for truncation to work */
      truncate_connection (bl->con_sexp);
    }

  /* set the file offsets for the dropped items to NA, and remove them from the cache */
  for (i = 0; i < n; ++i)
    {
      REAL (ndx_buff)[2 * i] = NA_REAL;
      (*pbigframe_write_row) (bl->bf, INTEGER (index)[i] - 1,
			  (char *) (REAL (ndx_buff) + 2 * i));
      ce = CACHE_REMOVE (blc, bl, INTEGER (index)[i] - 1);
      /* if the dropped element was in the cache, remove it from the R cache */
      if (ce)
	SET_VECTOR_ELT (bl->cache, INDEX_IN_CACHE (bl, ce), R_NilValue);
    }

  /* drop the largest tail possible from the biglist size */
  for (i = n - 1; i >= 0; --i)
    {
      if (bl->max_ind == INTEGER (index)[i] - 1)
	--bl->max_ind;
      else
	break;
    }

  /* set the number of rows in the bigframe index to match the
     maximum index of the biglist */
  (*pdo_bigframe_set_num_rows)(bl->bf, bl->max_ind + 1);

  if (bl->max_ind_written > bl->max_ind)
    bl->max_ind_written = bl->max_ind;

  UNPROTECT (2);
  return PASS_SEXP;
}

R_CallMethodDef biglist_call_methods[] = {
  // R hook functions
  MKREF (biglist_create, 7),
  MKREF (biglist_get, 2),
  MKREF (biglist_put, 3),
  MKREF (biglist_drop, 2),
  MKREF (biglist_length, 1),
  MKREF (biglist_flush, 1),
  MKREF (biglist_close, 1),
  {NULL, NULL, 0}
};

void
R_init_biglist (DllInfo * info)
{
  /* Register routines, allocate resources. */

  R_registerRoutines (info, NULL, biglist_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_PreserveObject (my_do_seek = install ("seek"));
  R_PreserveObject (my_do_flush = install ("flush"));
  R_PreserveObject (my_do_truncate = install ("truncate"));
  R_PreserveObject (my_do_close = install ("close"));
  R_PreserveObject (FILENAME_ATTRIBUTE_STRING = mkString("filename"));
  R_PreserveObject (NDX_ATTRIBUTE_STRING = mkString("ndx"));
  R_PreserveObject (CACHE_ATTRIBUTE_STRING = mkString("cache"));
  R_PreserveObject (FILE_ATTRIBUTE_STRING = mkString("file"));
  R_PreserveObject (DEFAULT_NAMES_ATTRIBUTE_STRING = mkString("default.names"));
  R_PreserveObject (BIGLIST_CLASS_STRING = mkString("biglist"));
  na_buff[0] = na_buff[1] = NA_REAL;
  pbigframe_write_row = (typeof(bigframe_write_row)*) R_GetCCallable("bigframe", "bigframe_write_row");
  pbigframe_read_row = (typeof(bigframe_read_row)*) R_GetCCallable("bigframe", "bigframe_read_row");
  pdo_bigframe_set_num_rows = (typeof(do_bigframe_set_num_rows)*) R_GetCCallable("bigframe", "do_bigframe_set_num_rows");
}

void
R_unload_biglist (DllInfo * info)
{
  R_ReleaseObject (BIGLIST_CLASS_STRING);
  R_ReleaseObject (DEFAULT_NAMES_ATTRIBUTE_STRING);
  R_ReleaseObject (FILE_ATTRIBUTE_STRING);
  R_ReleaseObject (CACHE_ATTRIBUTE_STRING);
  R_ReleaseObject (NDX_ATTRIBUTE_STRING);
  R_ReleaseObject (FILENAME_ATTRIBUTE_STRING);
  R_ReleaseObject (my_do_close);
  R_ReleaseObject (my_do_truncate);
  R_ReleaseObject (my_do_flush);
  R_ReleaseObject (my_do_seek);
}
