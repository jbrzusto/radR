/*
  bigframe.c
  
  support routines for the bigframe class, which stores large numerical dataframes on disk

*/

#include "bigframe.h"

#define CHECK_VALIDITY(bfs) ({						\
      t_bigframe *__bf;							\
      if (bfs == R_NilValue)						\
	error("bigframe object is NULL");				\
      __bf = SEXP_TO_BIGFRAME(bfs);					\
      if (!__bf || !__bf->file)						\
	error("bigframe is closed");					\
      __bf;								\
    })


DECLARE_CACHE_FUNCTIONS(bfc, t_bigframe, t_bigframe_cache_entry, t_bigframe_index, key);
DEFINE_CACHE_FUNCTIONS(bfc, t_bigframe, t_bigframe_cache_entry, t_bigframe_index, key, bfc_get_new_entry, bfc_copy_to_entry, bfc_read_back, bfc_write_back);

inline static void
bigframe_seek_row (t_bigframe *bf, int n)
{
  // seek the bigframe file pointer to the nth row
  // bf: pointer to the bigframe object
  // n:  row index (starting at zero)

  if (-1 == bigfseek(bf->file, bf->data_offset + bf->row_bytes * (long long) n, SEEK_SET))
    error("bigframe: unable to seek to row %u of file", n);
}

void 
bfc_read_back(t_bigframe *bf, t_bigframe_index key, t_bigframe_cache_entry *entry) 
{
  /* read a row from the file into the given cache entry */
  bigframe_seek_row(bf, (int) key);
  if (1 != fread(entry->data, bf->row_bytes, 1, bf->file))
    error("bigframe: unable to read row %u from file", (int) (1 + key));
}

void 
bfc_write_back(t_bigframe *bf, t_bigframe_cache_entry *entry) 
{
  bigframe_seek_row(bf, (int) (entry->key));
  if (1 != fwrite(entry->data, bf->row_bytes, 1, bf->file))
    error("bigframe: unable to write row %u to file", 1 + (int) (entry->key));
}

t_bigframe_cache_entry *
bfc_get_new_entry(t_bigframe *bf) 
{
  /* return the address of a never-used cache entry */
  return bf->cache_entries + CACHE_SIZE(bfc, bf);
}

void 
bfc_copy_to_entry(t_bigframe *bf, t_bigframe_cache_entry *entry, void * data) 
{
  /* copy row data from an arbitrary location into the specified cache entry */
  memcpy((char *) entry->data, (char *) data, bf->row_bytes);
}

void
bigframe_write_row (t_bigframe *bf, int n, char *data)
{
  /* write data to the n-th bigframe row
     via the cache */
  CACHE_PUT(bfc, bf, n, data);
  if (n >= bf->num_rows)
    bf->num_rows = n + 1;
}

void
bigframe_read_row (t_bigframe *bf, int n, char *buff)
{
  /* read data from the n-th bigframe row, if it exists, into a buffer
     via the cache.  If the row doesn't exist, fill the buffer with NAs.
  */
  t_bigframe_cache_entry *ce;
  if (n >= 0 && n < bf->num_rows) {
    ce = CACHE_GET(bfc, bf, n);
    if (ce) {
      memcpy(buff, (char *) ce->data, bf->row_bytes);
      return;
    }
    /* CHECKME: under what conditions, if any, could we arrive here? */
  }
  memcpy(buff, BIGFRAME_NA_BUFF(bf), bf->row_bytes);
}

SEXP
bigframe_create (SEXP filename, SEXP data_offset, SEXP row_bytes, SEXP num_rows, SEXP cache_size, SEXP readonly, SEXP frame) 
{
  // create a bigframe object, returning a pointer to it
  //
  // filename		: CHARACTER     ; full filepath for on-disk storage of bigframe
  // data_offset	: DOUBLE	; offset to start of row data (1st byte past header)
  // row_bytes		: INTEGER	; total number of bytes in a row
  // num_rows		: INTEGER	; number of rows in the frame (0 if it is a new frame)
  // cache_size         : INTEGER       ; number of rows to keep in the cache
  // readonly           : LOGICAL       ; if TRUE, open file for read only
  // frame		: DATAFRAME	; (really a list with each element a vector of appropriate type)
  //
  // returns: on success: pointer to a new bigframe object, wrapped as an R EXTPTR
  //          on failure: FAIL_SEXP

  t_bigframe * bf;
  int i, j;
  int size;
  int csize;
  int maxstrlen;
  char *p;
  SEXP e;

  bf = Calloc(1, t_bigframe);
  bf->data_offset = (long long) REAL(data_offset)[0];
  bf->row_bytes = INTEGER(row_bytes)[0];
  bf->num_rows = INTEGER(num_rows)[0];
  bf->read_only = LOGICAL(readonly)[0];
  csize = INTEGER(cache_size)[0];

  bf->num_cols = LENGTH(frame);
  // allocate space for column sizes
  bf->col_types = Calloc(bf->num_cols, unsigned char);
  // allocate space for cumulative column sizes
  bf->cum_col_size = Calloc(bf->num_cols, int);
  // allocate space for buffer and cache rows
  bf->row_buff = Calloc((csize + 2) * bf->row_bytes, char);
  // allocate the cache management entries
  bf->cache_entries = Calloc(csize, t_bigframe_cache_entry);
  bf->file = bigfopen(CHAR(STRING_ELT(filename, 0)), bf->read_only ? "rb" : "rb+");
  if (!bf->file) {
    Free(bf->cache_entries);
    Free(bf->row_buff);
    Free(bf->cum_col_size);
    Free(bf->col_types);
    Free(bf);
    return FAIL_SEXP;
  }
  // record column types and create a row of NA appropriate to the column types
  for (i = 0, size=0, p = BIGFRAME_NA_BUFF(bf); i < bf->num_cols; ++i) {
    bf->cum_col_size[i] = p - BIGFRAME_NA_BUFF(bf);
    switch(bf->col_types[i] = TYPEOF(VECTOR_ELT(frame, i))) {
    case INTSXP:
    case LGLSXP:
      // integer or logical
      *(int *)p = NA_INTEGER;
      p += sizeof(int);
      break;
    case REALSXP:
      // double
      *(double *)p = NA_REAL;
      p += sizeof(double);
      break;
    case CPLXSXP:
      // complex
      *(double *)p = *(1+(double *)p) = NA_REAL;
      p += 2 * sizeof(double);
      break;
    case STRSXP:
      // string (which we will limit to the maximum size of the string passed)
      for (maxstrlen = 0, e = VECTOR_ELT(frame, i), j = 0; j < LENGTH(e); ++j)
	if (maxstrlen < LENGTH(STRING_ELT(e, j)))
	  maxstrlen = LENGTH(STRING_ELT(e, j));
      // add the null byte and round up to the nearest 4 bytes
      maxstrlen = (maxstrlen + 4) / 4 * 4;
      memset(p, 0, maxstrlen);
      p += maxstrlen;
      bf->col_types[i] += (maxstrlen / 4) - 1;
    default:
      /* caller is supposed to make sure this doesn't happen */
      break;
    }
  }
  /* create the cache entries with pointers to appropriate locations in the buffer */
  CACHE_INIT(bfc, bf, csize);
  for (i = 0; i < csize; ++i)
    bf->cache_entries[i].data = BIGFRAME_CACHE_ROW(bf, i);

  return(PTR_TO_EXTPTR(bf));
}

static void
fill_with_NA(t_bigframe *bf, int start_row, int end_row) 
{
  // fill bigframe rows with NA
  // start_row and end_row are zero-based indexes
  int i;
  
  for (i=start_row; i <= end_row; ++i) {
    bigframe_write_row(bf, i, BIGFRAME_NA_BUFF(bf));
  }
}

SEXP
bigframe_get_data (SEXP bfs, SEXP rowind, SEXP colind) 
{
  t_bigframe *bf;
  SEXP rv;
  int i, j;
  char *p, *bp;
  int nr = LENGTH(rowind);
  int nc = LENGTH(colind);
  const int *ri = INTEGER(PROTECT(AS_INTEGER(rowind)));
  const int *ci = INTEGER(PROTECT(AS_INTEGER(colind)));

  bf = CHECK_VALIDITY(bfs);

  // check validity of indexes
  for (j = 0; j < nc; ++j)
    if (ci[j] <= 0 || ci[j] > bf->num_cols)
      error("bigframe: undefined column selected: %d", ci[j]);

  for (i = 0; i < nr; ++i)
    if (ri[i] <= 0)
      error("bigframe: undefined row selected: %d", ri[i]);

  PROTECT(rv = NEW_LIST(nc));
  for (j = 0; j < nc; ++j) 
    PROTECT(SET_ELEMENT(rv, j, allocVector(bf->col_types[ci[j]-1] < STRSXP ? bf->col_types[ci[j]-1] : STRSXP , nr)));
  for (i = 0; i < nr; ++i) {
    if (ri[i] <= bf->num_rows) {
      bigframe_read_row(bf, ri[i]-1, bp = bf->row_buff);
    } else {
      // point to the NA row in the row_buff
      bp = BIGFRAME_NA_BUFF(bf);
    }
    for (j = 0; j < nc; ++j) {
      p = bp + bf->cum_col_size[ci[j] - 1];
      switch(bf->col_types[ci[j] - 1]) {
      case INTSXP:
      case LGLSXP:
	// integer or logical
	INTEGER(VECTOR_ELT(rv, j))[i] = *(int *)p;
	break;
      case REALSXP:
	// double
	REAL(VECTOR_ELT(rv, j))[i] = *(double *)p;
	break;
      case CPLXSXP:
	// complex
	COMPLEX(VECTOR_ELT(rv, j))[i].r = *(double *)p;
	p += sizeof(double);
	COMPLEX(VECTOR_ELT(rv, j))[i].i = *(double *)p;
	break;
      default:
	// string, including max length info
	// two special cases: 00 00 => NA_STRING, 00 XX => R_BlankString
	SET_STRING_ELT(VECTOR_ELT(rv, j), i,  (*p) ? mkChar(p) : (*(p+1) ? R_BlankString : R_NaString));
	break;
      } 
    }
  }
  UNPROTECT(3 + nc);
  return(rv);
}

SEXP
bigframe_put_data (SEXP bfs, SEXP rowind, SEXP colind, SEXP dataframe) 
{
  t_bigframe *bf;
  int i, j, ii;
  char *p;
  int nr = LENGTH(rowind);
  int nc = LENGTH(colind);
  int nc_rhs = LENGTH(dataframe);
  const int *ri = INTEGER(PROTECT(AS_INTEGER(rowind)));
  const int *ci = INTEGER(PROTECT(AS_INTEGER(colind)));
  int get_old = FALSE;
  SEXP coerced_RHS;

  bf = CHECK_VALIDITY(bfs);
  if (bf->read_only)
    error("bigframe: can't assign when created with read.only=TRUE");

  // check indexes
  for (j = 0; j < nc; ++j)
    if (ci[j] <= 0 || ci[j] > bf->num_cols)
      error("bigframe: undefined column index on left side of assignment: %d", ci[j]);
  
  for (i = 0; i < nr; ++i)
    if (ri[i] <= 0)
      error("bigframe: undefined row index on left side of assignment: %d", ri[i]);

  // determine whether all columns are to be written;
  // if not, we need to load existing row data into the buffer
  // before modifying it

  // flag included columns (kludgy, but avoids allocating new storage)
  // ?? WARNING:  assumes the high bit in TYPEOF(...) is clear

  for (j = 0; j < nc; ++j)
    bf->col_types[ci[j] - 1] |= 128;

  for (j = 0, i = 0; j < bf->num_cols; ++j)
    if (bf->col_types[j] & 128) {
      ++i;
      bf->col_types[j] &= 127;
    }

  get_old = i < bf->num_cols;
    
  /* coerce types from RHS */
  PROTECT(coerced_RHS = allocVector(VECSXP, nc));
  for (j = 0; j < nc; ++j) {
    switch(bf->col_types[ci[j] - 1]) {
    case LGLSXP:
      // integer
      SET_ELEMENT(coerced_RHS, j, AS_LOGICAL(VECTOR_ELT(dataframe, j % nc_rhs)));
      break;
    case INTSXP:
      // integer
      SET_ELEMENT(coerced_RHS, j, AS_INTEGER(VECTOR_ELT(dataframe, j % nc_rhs)));
      break;
    case REALSXP:
      // double
      SET_ELEMENT(coerced_RHS, j, AS_NUMERIC(VECTOR_ELT(dataframe, j % nc_rhs)));
      break;
    case CPLXSXP:
      // complex
      SET_ELEMENT(coerced_RHS, j, AS_COMPLEX(VECTOR_ELT(dataframe, j % nc_rhs)));
      break;
    default:
      // string
      SET_ELEMENT(coerced_RHS, j, AS_CHARACTER(VECTOR_ELT(dataframe, j % nc_rhs)));
      break;
    }
  }

  for (i = 0; i < nr; ++i) {
    if (ri[i] > bf->num_rows) {
      // this row is past existing data so
      // fill intermediate rows with NA
      if (ri[i] > bf->num_rows + 1)
	fill_with_NA(bf, bf->num_rows, ri[i] - 2);
      if (get_old)
	// copy the NA row to the buffer, since the current row doesn't exist
	memcpy(bf->row_buff, BIGFRAME_NA_BUFF(bf), bf->row_bytes);
    } else {
      if (get_old) {
	// we need the existing row since only some columns are being written
	bigframe_read_row(bf, ri[i] - 1, bf->row_buff);
      }
    }

    for (j = 0; j < nc; ++j) {
      // wrap index around for permissive element recycling
      ii = i % LENGTH(VECTOR_ELT(coerced_RHS, j));
      p = bf->row_buff + bf->cum_col_size[ci[j] - 1];
      switch(bf->col_types[ci[j] - 1]) {
      case INTSXP:
      case LGLSXP:
	// integer or logical
	*(int *)p = INTEGER(VECTOR_ELT(coerced_RHS, j))[ii];
	break;
      case REALSXP:
	// double
	 *(double *)p = REAL(VECTOR_ELT(coerced_RHS, j))[ii];
	break;
      case CPLXSXP:
	// complex
	*(double *)p = COMPLEX(VECTOR_ELT(coerced_RHS, j))[ii].r;
	 p += sizeof(double);
	*(double *)p = COMPLEX(VECTOR_ELT(coerced_RHS, j))[ii].i;
	break;
      default:
	// string (including max length info)
	strncpy(p, CHAR(STRING_ELT(VECTOR_ELT(coerced_RHS, j), ii)), 4 * (bf->col_types[ci[j] - 1] - STRSXP + 1) - 1);
	break;
      }
    }
    bigframe_write_row(bf, ri[i] - 1, bf->row_buff);
  }
  UNPROTECT(3);
  return bfs;
}

void
do_bigframe_flush(t_bigframe *bf) {
  if (bf && bf->file) {
    CACHE_FLUSH(bfc, bf);
    fflush(bf->file);
  }
}

SEXP
bigframe_flush (SEXP bfs) 
{
  t_bigframe *bf;
  bf = CHECK_VALIDITY(bfs);
  do_bigframe_flush(bf);
  return PASS_SEXP;
}

void
do_bigframe_set_num_rows(t_bigframe *bf, int nr) {
  if (bf) {
    do_bigframe_flush(bf);
    if (nr < bf->num_rows) {
      ftruncate(fileno(bf->file), bf->data_offset + nr * bf->row_bytes);
    } else if (nr > bf->num_rows) {
      fill_with_NA(bf, bf->num_rows, nr - 1);
    }
    bf->num_rows = nr;
  }
}

SEXP
bigframe_set_num_rows (SEXP bfs, SEXP numrows) 
{
  t_bigframe *bf;
  bf = CHECK_VALIDITY(bfs);
  // flush the bigframe first, so that pending writes don't
  // override this length setting
  do_bigframe_set_num_rows(bf, INTEGER(AS_INTEGER(numrows))[0]);
  return bfs;
}

SEXP
bigframe_get_num_rows (SEXP bfs) 
{
  t_bigframe *bf;
  bf = CHECK_VALIDITY(bfs);
  return ScalarInteger(bf->num_rows);
}

SEXP
bigframe_close (SEXP bfs) 
{
  t_bigframe *bf;
  bf = CHECK_VALIDITY(bfs);
  CACHE_FLUSH(bfc, bf);
  fclose(bf->file);
  bf->file = NULL;
  Free(bf->cache_entries);
  Free(bf->row_buff);
  Free(bf->cum_col_size);
  Free(bf->col_types);
  return PASS_SEXP;
}

R_CallMethodDef bigframe_call_methods[]  = {
  // R hook functions
  MKREF(bigframe_create		, 7),
  MKREF(bigframe_get_data	, 3),
  MKREF(bigframe_put_data	, 4),
  MKREF(bigframe_set_num_rows	, 2),
  MKREF(bigframe_get_num_rows	, 1),
  MKREF(bigframe_flush		, 1),
  MKREF(bigframe_close		, 1),
  {NULL, NULL, 0}
};

void
R_init_bigframe(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, bigframe_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_RegisterCCallable("bigframe", "bigframe_write_row", (DL_FUNC) bigframe_write_row);
  R_RegisterCCallable("bigframe", "bigframe_read_row", (DL_FUNC) bigframe_read_row);
  R_RegisterCCallable("bigframe", "do_bigframe_set_num_rows", (DL_FUNC) do_bigframe_set_num_rows);
}

void
R_unload_bigframe(DllInfo *info)
{
  /* nothing to do here; warning: unloading the library when there are
     still unfinalized bigframe objects will cause an error when those
     objects are eventually finalized, since C:bigframe_close is called
     by R:close.bigframe
   */
}
