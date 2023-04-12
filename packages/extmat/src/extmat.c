#include <stdlib.h>
#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include "extmat.h"

SEXP EXTMAT_CLASSNAME_STRING = NULL;

#define SEXP_TO_EXTMAT(S) ((t_extmat *) EXTPTR_PTR(S))

// create an R external pointer object (EXTPTR SEXP) from a pointer
#define PTR_TO_EXTPTR(_P_) R_MakeExternalPtr((void *)(_P_), R_NilValue, R_NilValue)

SEXP
extmat_to_sexp(t_extmat *m)
{
  SEXP sxp;
  sxp = PTR_TO_EXTPTR(m);
  SET_CLASS(sxp, EXTMAT_CLASSNAME_STRING);
  return sxp;
}

void
ensure_extmat_with_slop_trigger(t_extmat *m, int rows, int cols, int slop, int notrigger)
{
  // make sure the extmat *m can hold (rows * cols + slop) items of size m->size
  // and set its dimensions to rows x cols and slop to slop
  // If additional storage is allocated, it is zeroed.

  int need = (rows * cols + slop) * m->size;
  int changed;

  if (m->alloc < need) {
    m->ptr = Realloc(m->ptr, need, char);
    memset(m->ptr + m->alloc, 0, need - m->alloc);
    m->alloc = need;
  }
  changed = rows != m->rows || cols != m->cols;
  m->rows = rows;
  m->cols = cols;
  m->slop = slop;
  if (changed && m->changed_fun && !notrigger) 
    m->changed_fun((struct t_extmat *) m, 0, 0, m->rows, m->cols, m->rows, m->cols);
}

void
ensure_extmat_with_slop(t_extmat *m, int rows, int cols, int slop)
{
  ensure_extmat_with_slop_trigger(m, rows, cols, slop, 0);
}

void
ensure_extmat(t_extmat *m, int rows, int cols)
{
  // ensure the extmat can hold at least rows * cols items of size m->size

  ensure_extmat_with_slop_trigger(m, rows, cols, 0, 0);
}

void
free_extmat (t_extmat *m)
{    
  if (m->R_owns_storage)
    Free(m->ptr);
  if (m->R_owns_name)
    Free(m->name);
  m->alloc = 0;            
  m->alloc = m->rows = m->cols = 0;  
  m->changed_fun =  m->changed_fun_extra = NULL;
  m->ptr = NULL;           
}

SEXP
extmat_create (SEXP name, SEXP type, SEXP size, SEXP dim, SEXP slop) {
  // notice the swap of row and column dimensions, because we avoid transposing data
  SEXP rv;
  t_extmat *m;
  size_t nbytes;

  PROTECT(name=AS_CHARACTER(name));
  PROTECT(type=AS_INTEGER(type));
  PROTECT(size=AS_INTEGER(size));
  PROTECT(dim=AS_INTEGER(dim));
  PROTECT(slop=AS_INTEGER(slop));

  m = Calloc(1, t_extmat);
  nbytes = (INTEGER(dim)[0] * INTEGER(dim)[1] + INTEGER(slop)[0]) * INTEGER(size)[0];
  // 2023-04-05 - perhaps due to a gcc bug, the following cruft added to avoid a SEGV
  // in strlen
  const char *xxx = CHAR(STRING_ELT(name, 0));
  char aaa = xxx[0];
  // /2023-04-05
  m->name = Calloc(1 + strlen(CHAR(STRING_ELT(name, 0))), char);
  strcpy(m->name, CHAR(STRING_ELT(name, 0)));
  m->R_owns_name = 1; // mark that R is responsible for freeing name storage
  m->R_owns_storage = 1; // mark that R is responsible for freeing data storage
  m->ptr = NULL;
  m->changed_fun = m->changed_fun_extra = NULL;
  m->alloc = 0;
  m->size = INTEGER(size)[0];
  m->type = INTEGER(type)[0];
  m->rows = m->cols = m->slop = 0;
  ensure_extmat_with_slop(m, INTEGER(dim)[1], INTEGER(dim)[0], INTEGER(slop)[0]);
  PROTECT(rv = PTR_TO_EXTPTR(m));
  SET_CLASS(rv, EXTMAT_CLASSNAME_STRING);
  UNPROTECT(6);
  return rv;
}
  
SEXP
extmat_get_dim(SEXP s) {
  // return the dimensions of this extmat object
  // notice the swap of 1st and 2nd dims because R storage is column major
  SEXP rv;
  t_extmat *m = SEXP_TO_EXTMAT(s);
  PROTECT(rv = allocVector(INTSXP, 2));
  INTEGER(rv)[1] = m->rows;
  INTEGER(rv)[0] = m->cols;
  UNPROTECT(1);
  return rv;
}

SEXP
extmat_set_dim(SEXP s, SEXP dim, SEXP notrig) {
  // set the dimensions of the extmat to dim, ensuring
  // enough room for at least slop extra elements
  // notice that we swap 1st and 2nd dim because R storage
  // is column major
  // If R doesn't own the storage for the extmat, and the 
  // new size (i.e. product of dimensions) differs from the old
  // size, we do nothing but return NULL.
  
  SEXP rv;
  t_extmat *m = SEXP_TO_EXTMAT(s);
  PROTECT(dim=AS_INTEGER(dim));
  if (m->R_owns_storage || m->rows*m->cols == INTEGER(dim)[0]*INTEGER(dim)[1]) {
    ensure_extmat_with_slop_trigger(m, INTEGER(dim)[1], INTEGER(dim)[0], m->slop, LOGICAL(notrig)[0]);
    rv = s;
  } else {
    rv = R_NilValue;
  }
  UNPROTECT(1);
  return rv;
}
  
SEXP
extmat_zero(SEXP s) {
  // set the contents of this extmat to zero

  t_extmat *m = SEXP_TO_EXTMAT(s);
  memset(m->ptr, 0, (m->rows * m->cols + m->slop) * m->size);
  return R_NilValue;
}

SEXP
extmat_info(SEXP s) {
  // return details of this extmat object
  SEXP rv;
  t_extmat *m = SEXP_TO_EXTMAT(s);
  PROTECT(rv = allocVector(INTSXP, 6));
  INTEGER(rv)[0] = m->type;
  INTEGER(rv)[1] = m->size;
  INTEGER(rv)[2] = m->alloc;
  INTEGER(rv)[3] = m->slop;
  INTEGER(rv)[4] = m->R_owns_storage;
  INTEGER(rv)[5] = (int) m->ptr;
  UNPROTECT(1);
  return rv;
}

SEXP
extmat_pointer(SEXP s) {
  // return details of this extmat object
  t_extmat *m = SEXP_TO_EXTMAT(s);
  return PTR_TO_EXTPTR((void *)(m->ptr));
}

SEXP
extmat_get_name(SEXP s) {
  SEXP rv;
  t_extmat *m = SEXP_TO_EXTMAT(s);
  PROTECT(rv = allocVector(STRSXP, 1));
  SET_STRING_ELT(rv, 0, mkChar(m->name));
  UNPROTECT(1);
  return rv;
}

SEXP
extmat_set_name(SEXP s, SEXP name) {
  t_extmat *m = SEXP_TO_EXTMAT(s);
  Free(m->name);
  m->name = Calloc(1 + strlen(CHAR(STRING_ELT(name, 0))), char);
  strcpy(m->name, CHAR(STRING_ELT(name, 0)));
  return s;
}

SEXP validate_index_vector(SEXP ind, int max, int neg_allowed, int *num_prot) {
  // If an index vector is all positive, check whether its
  // values are all <= max.
  // If it has any zero values, report an error.
  // If it has any negative values, and neg_allowed is FALSE, report an error.
  // Otherwise, verify that all indexes are negative, and
  // return the complement of the absolute value of the set in 1...max
  // (i.e. remove the negated indexes from 1..max)

  int i, s, j, *p, n, nu, bml;
  unsigned int b;
  SEXP rv, bm;
  n = LENGTH(ind);
  if (TYPEOF(ind) != INTSXP && TYPEOF(ind) != LGLSXP) {
    PROTECT(ind = AS_INTEGER(ind));
    ++*num_prot;
  }
  if (TYPEOF(ind) == INTSXP) {
    for (i = 0; i < n; ++i) {
      if (INTEGER(ind)[i] <= 0)
	break;
      if(INTEGER(ind)[i] > max)
	goto out_of_bounds;
    }
    if (i == n) 
      return (ind);
    if (INTEGER(ind)[i] == 0)
      goto out_of_bounds;
    if (!neg_allowed)
      error("a matrix of indexes may not have negative values");
    if (i == 0) {
      for (i = 1; i < n; ++i) {
	if (INTEGER(ind)[i] >= 0)
	  break;
	if (INTEGER(ind)[i] < -max)
	  goto out_of_bounds;
      }
    }
    if (i < n)
      error("if any indexes are negative, all must be negative");

    // Exclude the specified indexes.
    // There are several ways of proceeding.  This method is linear
    // in time and allocates only the required final amount of memory.
    //
    // Allocate a temporary integer vector to hold a bitmap representing
    // which indexes are to be excluded.  i.e. a 1 bit represents
    // an excluded index; a zero bit represents an included index.

    s = 8 * sizeof(int);
    // bit map length (in ints)
    bml = (max + s - 1) / s;
    PROTECT(bm = allocVector(INTSXP, bml));
    p = INTEGER(bm);

    // zero the bitmap
    memset(p, 0, bml * sizeof(int));

    // populate the bitmap with excluded indexes
    // and count the number of unique such indexes
    for (i = 0, nu = 0; i < n; ++i) {
      j = ((-(INTEGER(ind)[i])) - 1) / s;
      b = ((-(INTEGER(ind)[i])) - 1) % s;
      if (! (p[j] & (1 << b))) {
	p[j] |= (1 << b);
	++nu;
      }
    }

    // Now fill the new index array from lowest order to highest order
    // index, skipping those with a one in the bitmap.
    // In this order, the bitmap is only overwritten after it is used.

    // i: index into the new index vector
    // j: current index value
    // b: current bit in the bitmap
    // p: current int in the bitmap

    PROTECT(rv = allocVector(INTSXP, max - nu));
    ++*num_prot;
    for (i = 0, j = 1, b = 1; j <= max; ++j) {
      if (!(*p & b)) {
	// bit is unset in bitmap
	INTEGER(rv)[i++] = j;
      }
      b <<= 1;
      if (!b) {
	++p;
	b = 1;
      }
    }
    UNPROTECT(1);
  } else if (TYPEOF(ind) == LGLSXP) {
    // vector is logical
    // count the true values and return an integer vector
    // corresponding to those indices
    // Note: we recycle elements.
    s = 0;
    if (n > 0)
      for (i = 0, s = 0; i < max; ++i)
	if (LOGICAL(ind)[i % n] != 0)
	  ++s;
    PROTECT(rv = allocVector(INTSXP, s));
    if (n > 0)
      for (i = 0, s = 0; i < max; ++i)
	if (LOGICAL(ind)[i % n] != 0)
	  INTEGER(rv)[s++] = i + 1;
    UNPROTECT(1);
  }
  return rv;
 out_of_bounds:
  error("index %d does not have absolute value in the range 1:%d", INTEGER(ind)[i], max);
  return R_NilValue; /* for -Wall; execution can't reach here */
}

SEXP
extmat_index(SEXP m, SEXP i, SEXP j, SEXP indextype, SEXP drop, SEXP rawsxp)
{
  // return a sub matrix of m, as specified by i and j
  // indextype is an integer value from the t_extmat_index_type enum.
  // drop is a logical: TRUE means a matrix with a dimension of size 1
  // is converted to a vector by dropping the dim attribute
  // In all cases, if the class attribute of m has more than 1 element
  // (i.e. has entries after the "extmat"), then the return value
  // has its class attribute set to the class attribute of m minus
  // the entry "extmat".
  // rawsxp: logical: if TRUE, the returned item is a string scalar
  // containing the indexed items of the extmat at their original size.
  // (i.e. 8 and 16 bit types like char and short will occupy 8 or 16 bits
  // in the returned string)
  
  SEXP rv, class, newclass;
  int ii, jj, n;
  int np = 0;

  t_extmat *me = SEXP_TO_EXTMAT(m);
  PROTECT(rv = extmat_to_R_numeric(me, i, j, INTEGER(indextype)[0], LOGICAL(rawsxp)[0]));
  np = 1;
  if (drop) {
    SEXP dims = GET_DIM(rv);
    if (INTEGER(dims)[0] == 1 || (LENGTH(dims) > 1 && INTEGER(dims)[1] == 1))
      SET_DIM(rv, R_NilValue);
  }
  if ((n = LENGTH(class = GET_CLASS(m))) > 1) {
    PROTECT(newclass = allocVector(STRSXP, n - 1));
    ++np;
    for (ii = 0, jj = 0; ii < n && jj < n - 1; ++ii) {
      if (strcmp(CHAR(STRING_ELT(class, ii)), "extmat")) {
	SET_STRING_ELT(newclass, jj++, PROTECT(duplicate(STRING_ELT(class, ii))));
	++np;
      }
    }
    if (jj != n - 1)
      error("extmat object is missing 'extmat' in its class attribute");
    SET_CLASS(rv, newclass);
  }
  UNPROTECT(np);
  return rv;
}


SEXP
extmat_index_assign(SEXP m, SEXP i, SEXP j, SEXP val, SEXP indextype, SEXP notrigger)
{
  // assign val to the sub matrix of m specified by i and j
  
  t_extmat *me = SEXP_TO_EXTMAT(m);
  R_numeric_to_extmat (me, i, j, val, INTEGER(indextype)[0]);
  // FIXME: only do the subrectangle needed
  if (me->changed_fun && !LOGICAL(notrigger)[0]) 
    me->changed_fun((struct t_extmat *) me, 0, 0, me->cols, me->rows, me->cols, me->rows);
  return m;
}

SEXP
extmat_destroy(SEXP s) {
  // Free the storage used by this extmat.  Although
  // the pointer value is retained in the EXTPTRSXP
  // even after we return, this function is only called
  // by the R finalizer, so the pointer value is never referenced
  // again.

  t_extmat *m = SEXP_TO_EXTMAT(s);
  free_extmat(m);
  Free(m);
  return R_NilValue;
}

// a macro to copy a sub array of one type to an array of another 

#define COPY_SUB_OF_TYPE_TO_TYPE(_const_src_, _src_type_, _const_dst_, _dst_type_, ri, nri, nr, ci, nci, cinc) \
  { \
    _src_type_ * _src_ = (_src_type_ *) _const_src_; \
    _dst_type_ * _dst_ = (_dst_type_ *) _const_dst_; \
    int * _endri_ = ri + nri; \
    int * _endci_ = ci + nci; \
    if ((nri) > 0 && (nci) > 0) { \
      for (;;) { \
        *_dst_++ = _src_[*ri - 1 + nr * (*ci - 1)]; \
        if (++ri < _endri_) { \
          ci += cinc; \
        } else { \
          if (cinc == 0) { \
  	  ri -= nri; \
            if (++ci == _endci_) \
              break; \
          } else { \
            break; \
          } \
        } \
      } \
    } \
  }

// a macro to copy a sub array of user type (with element size _src_size_) to an raw vector of another 

#define COPY_SUB_OF_USER_TO_RAW(_const_src_, _const_dst_, _src_size_, ri, nri, nr, ci, nci, cinc) \
  { \
    char * _src_ = (char *) (_const_src_); \
    char * _dst_ = (char *) (_const_dst_); \
    char * _tmp_src_; \
    int _size_   = (_src_size_); \
    int * _endri_ = ri + nri; \
    int * _endci_ = ci + nci; \
    int  _k_; \
    if ((nri) > 0 && (nci) > 0) { \
      for (;;) { \
        _tmp_src_ = & _src_[(*ri - 1 + nr * (*ci - 1)) * _size_]; \
	for(_k_=0; _k_ < _size_; ++_k_) \
          *_dst_++ = *_tmp_src_++; \
        if (++ri < _endri_) { \
          ci += cinc; \
        } else { \
          if (cinc == 0) { \
  	  ri -= nri; \
            if (++ci == _endci_) \
              break; \
          } else { \
            break; \
          } \
        } \
      } \
    } \
  }


// a macro to copy an array of one type to a subarray of another 
// elements from the source array are recycled as necessary 

#define COPY_TYPE_TO_SUB_OF_TYPE(_const_src_, _src_type_, _const_dst_, _dst_type_, ri, nri, nr, ci, nci, cinc, n) \
  { \
    _src_type_ * _src_ = (_src_type_ *) _const_src_; \
    _src_type_ * _src_end_ = _src_ + n; \
    _dst_type_ * _dst_ = (_dst_type_ *) _const_dst_; \
    int * _endri_ = ri + nri; \
    int * _endci_ = ci + nci; \
    if ((nri) > 0 && (nci) > 0) { \
      for (;;) { \
        _dst_[*ri - 1 + nr * (*ci - 1)] = *_src_++; \
        if (_src_ == _src_end_) \
           _src_ = (_src_type_ *) _const_src_; \
        if (++ri < _endri_) { \
          ci += cinc; \
        } else { \
          if (cinc == 0) { \
  	  ri -= nri; \
            if (++ci == _endci_) \
              break; \
          } else { \
            break; \
          } \
        } \
      } \
    } \
  }
      

// a macro to copy a raw vector to a sub array of user type (with element size _src_size_)

#define COPY_RAW_TO_SUB_OF_USER(_const_src_, _const_dst_, _src_size_, ri, nri, nr, ci, nci, cinc, n) \
  { \
    char * _src_ = (char *) _const_src_; \
    int _size_ = (_src_size_); \
    char * _src_end_ = _src_ + (n) * _size_; \
    char * _dst_ = (char *) _const_dst_; \
    char * _tmp_dst_; \
    int * _endri_ = ri + nri; \
    int * _endci_ = ci + nci; \
    int _k_; \
    if ((nri) > 0 && (nci) > 0) { \
      for (;;) { \
        _tmp_dst_ = & _dst_[(*ri - 1 + nr * (*ci - 1)) * _size_]; \
        for (_k_ = 0; _k_ < _size_; ++_k_) \
           *_tmp_dst_++ = *_src_++; \
        if (_src_ == _src_end_) \
           _src_ = (char *) _const_src_; \
        if (++ri < _endri_) { \
          ci += cinc; \
        } else { \
          if (cinc == 0) { \
  	  ri -= nri; \
            if (++ci == _endci_) \
              break; \
          } else { \
            break; \
          } \
        } \
      } \
    } \
  }
      
SEXP
extmat_to_R_numeric(t_extmat *m, SEXP rowinds, SEXP colinds, t_extmat_index_type itype, int raw_output)
{
  // return an R integer or real matrix or character vector SEXP
  // containing data from a subarray of the extmat *m fails if unable
  // to allocate memory 

  // If the extmat type is USER, or if raw_output is non-zero, we
  // return a character vector with a single element, into which the 
  // source data is copied without widening.

  // rowinds is an INTSXP or LGLSXP of row indices
  // colinds is an INTSXP or LGLSXP of column indices
  // both have origin 1.
  //
  // itype:
  //   OUTER_PRODUCT:   desired elements are every combination of row and column index
  //                    shape is length(rowinds) x length(colinds) with the obvious
  //                    adjustment for a NULL in either spot
  //
  //   ROW_COL_MATRIX:  desired elements are those at [rowind[i],colind[i]]
  //                    shape is length(rowinds) which must equal length(colinds)
  //         
  //   LINEAR_VECTOR:   desired elements are those at [rowind[i]]; i.e. rowinds are 
  //                    absolute indexes into the extmat data treated as a vector
  //

  SEXP rv = NULL;
  SEXPTYPE rv_type = INTSXP;
  void *dst;
  void *src;
  int nr;
  int *ri, *ci;
  int cinc;
  int nri, nci;
  int rv_length;
  int num_prot;

  num_prot = 0;

  switch(itype) {
  case OUTER_PRODUCT:
  case ROW_COL_MATRIX:

    rowinds = validate_index_vector(rowinds, m->cols, itype==OUTER_PRODUCT, &num_prot);
    colinds = validate_index_vector(colinds, m->rows, itype==OUTER_PRODUCT, &num_prot);
    nri = length(rowinds);
    nci = length(colinds);
    ri = INTEGER(rowinds);
    ci = INTEGER(colinds);
    cinc = (itype == OUTER_PRODUCT) ? 0 : 1;
    nr = m->cols;  // note the transposition: extmat cols = R rows
    break;
  case LINEAR_VECTOR:
  default:
    // Pretend the column indexes are the same as
    // the row indexes, but set the number of rows to
    // zero.  This has the effect of treating the
    // row indexes as absolute indexes into the extmat's
    // underlying vector in the COPY_TYPE_TO_SUB_OF_TYPE macro.
    // This is a kludge.

    rowinds = validate_index_vector(rowinds, m->cols * m->rows, TRUE, &num_prot);
    nci = nri = length(rowinds);
    ci = ri = INTEGER(rowinds);
    cinc = 1;
    nr = 0;
    break;
  }

  if (raw_output) {
    PROTECT(rv = allocVector(STRSXP, 1));
    ++num_prot;
    rv_length = (itype == OUTER_PRODUCT) ? nri * nci : nri;
    SET_STRING_ELT(rv, 0, allocVector(CHARSXP, rv_length * m->size));
    dst = CHAR(STRING_ELT(rv, 0));
  } else {
    switch(m->type) {
    case EXTMAT_TYPE_FLOAT:
    case EXTMAT_TYPE_DOUBLE:
      rv_type = REALSXP;
    case EXTMAT_TYPE_CHAR:
    case EXTMAT_TYPE_UCHAR:
    case EXTMAT_TYPE_SHORT:
    case EXTMAT_TYPE_USHORT:
    case EXTMAT_TYPE_INT:
    case EXTMAT_TYPE_UINT:
    case EXTMAT_TYPE_LONGLONG:
    case EXTMAT_TYPE_ULONGLONG:
      if (itype == OUTER_PRODUCT) {
	PROTECT(rv = allocMatrix(rv_type, nri, nci));
	++num_prot;
      } else {
	PROTECT(rv = allocVector(rv_type, nri));
	++num_prot;
      }
      break;
    case EXTMAT_TYPE_USER:
      if (itype == OUTER_PRODUCT) {
	SEXP rv_dim;
	PROTECT(rv_dim = allocVector(INTSXP, 3));
	++num_prot;
	INTEGER(rv_dim)[0] = m->size;
	INTEGER(rv_dim)[1] = nri;
	INTEGER(rv_dim)[2] = nci;
	PROTECT(rv = allocArray(RAWSXP, rv_dim));
	++num_prot;
	rv_length = nri * nci * m->size;
      } else {
	PROTECT(rv = allocMatrix(RAWSXP, m->size, nri));
	++num_prot;
	rv_length = nri * m->size;
      }
      break;
    default:
      error("unknown extmat type: %d", m->type);
      break;
    }
    dst = DATAPTR(rv);
  }

  src = m->ptr;

  switch(m->type) {
  case EXTMAT_TYPE_CHAR:
    if (raw_output)
      COPY_SUB_OF_TYPE_TO_TYPE(src, char, dst, char, ri, nri, nr, ci, nci, cinc)
    else
      COPY_SUB_OF_TYPE_TO_TYPE(src, char, dst, int, ri, nri, nr, ci, nci, cinc);
    break;
  case EXTMAT_TYPE_UCHAR:
    if (raw_output)
      COPY_SUB_OF_TYPE_TO_TYPE(src, unsigned char, dst, unsigned char, ri, nri, nr, ci, nci, cinc)
    else
      COPY_SUB_OF_TYPE_TO_TYPE(src, unsigned char, dst, int, ri, nri, nr, ci, nci, cinc);
    break;
  case EXTMAT_TYPE_SHORT:
    if (raw_output)
      COPY_SUB_OF_TYPE_TO_TYPE(src, short, dst, short, ri, nri, nr, ci, nci, cinc)
    else
      COPY_SUB_OF_TYPE_TO_TYPE(src, short, dst, int, ri, nri, nr, ci, nci, cinc);
    break;
  case EXTMAT_TYPE_USHORT:
    if (raw_output)
      COPY_SUB_OF_TYPE_TO_TYPE(src, unsigned short, dst, unsigned short, ri, nri, nr, ci, nci, cinc)
    else
      COPY_SUB_OF_TYPE_TO_TYPE(src, unsigned short, dst, int, ri, nri, nr, ci, nci, cinc);
    break;
  case EXTMAT_TYPE_INT:
    COPY_SUB_OF_TYPE_TO_TYPE(src, int, dst, int, ri, nri, nr, ci, nci, cinc);
    break;
  case EXTMAT_TYPE_UINT:
    COPY_SUB_OF_TYPE_TO_TYPE(src, unsigned int, dst, int, ri, nri, nr, ci, nci, cinc);
    break;
  case EXTMAT_TYPE_LONGLONG:
    COPY_SUB_OF_TYPE_TO_TYPE(src, long long, dst, int, ri, nri, nr, ci, nci, cinc);
    break;
  case EXTMAT_TYPE_ULONGLONG:
    COPY_SUB_OF_TYPE_TO_TYPE(src, unsigned long long, dst, int, ri, nri, nr, ci, nci, cinc);
    break;
  case EXTMAT_TYPE_FLOAT:
    COPY_SUB_OF_TYPE_TO_TYPE(src, float, dst, double, ri, nri, nr, ci, nci, cinc);
    break;
  case EXTMAT_TYPE_DOUBLE:
    COPY_SUB_OF_TYPE_TO_TYPE(src, double, dst, double, ri, nri, nr, ci, nci, cinc);
    break;
  case EXTMAT_TYPE_USER:
    COPY_SUB_OF_USER_TO_RAW(src, dst, m->size, ri, nri, nr, ci, nci, cinc);
    break;
  default:
    break;
  }

  if (num_prot)
    UNPROTECT(num_prot);
  return (rv);
}  

void
R_numeric_to_extmat(t_extmat *m, SEXP rowinds, SEXP colinds, SEXP val, t_extmat_index_type itype)
{
  // copy data from an R integer or real vector or packed character scalar to a subarray of an extmat

  void *dst;
  void *src;
  int nr;
  int *ri, *ci;
  int cinc;
  int nri, nci;
  int n;
  int num_prot = 0;

  dst = m->ptr;

  // the number of items in the right hand side is either its length (for INTSXP, REALSXP),
  // or the number of bytes in the string scalar, divided by the size of the target
  // elements.

  n = IS_CHARACTER(val) ? strlen(CHAR(STRING_ELT(val, 0))) / m->size : length(val);
  switch(itype) {
  case OUTER_PRODUCT:
  case ROW_COL_MATRIX:
    rowinds = validate_index_vector(rowinds, m->cols, itype==OUTER_PRODUCT, &num_prot);
    colinds = validate_index_vector(colinds, m->rows, itype==OUTER_PRODUCT, &num_prot);
    nri = length(rowinds);
    nci = length(colinds);
    ri = INTEGER(rowinds);
    ci = INTEGER(colinds);
    cinc = (itype == OUTER_PRODUCT) ? 0 : 1;
    nr = m->cols;  // note the transposition: extmat cols = R rows
    break;
  case LINEAR_VECTOR:
  default:
    // Pretend the column indexes are the same as
    // the row indexes, but set the number of rows to
    // zero.  This has the effect of treating the
    // row indexes as absolute indexes into the extmat's
    // underlying vector in the COPY_TYPE_TO_SUB_OF_TYPE macro
    // this is a kludge, but works.

    rowinds = validate_index_vector(rowinds, m->cols * m->rows, TRUE, &num_prot);
    nci = nri = length(rowinds);
    ci = ri = INTEGER(rowinds);
    cinc = 1;
    nr = 0;
    break;
  }

  if (IS_NUMERIC(val)) {
    // copy from an R real (or raw) vector
    src = REAL(val);
    switch(m->type) {
    case EXTMAT_TYPE_CHAR:
      COPY_TYPE_TO_SUB_OF_TYPE(src, double, dst, char, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_UCHAR:
      COPY_TYPE_TO_SUB_OF_TYPE(src, double, dst, unsigned char, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_SHORT:
      COPY_TYPE_TO_SUB_OF_TYPE(src, double, dst, short, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_USHORT:
      COPY_TYPE_TO_SUB_OF_TYPE(src, double, dst, unsigned short, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_INT:
      COPY_TYPE_TO_SUB_OF_TYPE(src, double, dst, int, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_UINT:
      COPY_TYPE_TO_SUB_OF_TYPE(src, double, dst, unsigned int, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_LONGLONG:
      COPY_TYPE_TO_SUB_OF_TYPE(src, double, dst, long long, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_ULONGLONG:
      COPY_TYPE_TO_SUB_OF_TYPE(src, double, dst, unsigned long long, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_FLOAT:
      COPY_TYPE_TO_SUB_OF_TYPE(src, double, dst, float, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_DOUBLE:
      COPY_TYPE_TO_SUB_OF_TYPE(src, double, dst, double, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_USER:
      COPY_RAW_TO_SUB_OF_USER(RAW(val), dst, m->size, ri, nri, nr, ci, nci, cinc, n);
    default:
      error("unknown extmat type: %d", m->type);
      break;
    }
  } else if (IS_INTEGER(val)) {
    // copy from an R integer vector
    src = INTEGER(val);
    switch(m->type) {
    case EXTMAT_TYPE_CHAR:
      COPY_TYPE_TO_SUB_OF_TYPE(src, int, dst, char, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_UCHAR:
      COPY_TYPE_TO_SUB_OF_TYPE(src, int, dst, unsigned char, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_SHORT:
      COPY_TYPE_TO_SUB_OF_TYPE(src, int, dst, short, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_USHORT:
      COPY_TYPE_TO_SUB_OF_TYPE(src, int, dst, unsigned short, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_INT:
      COPY_TYPE_TO_SUB_OF_TYPE(src, int, dst, int, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_UINT:
      COPY_TYPE_TO_SUB_OF_TYPE(src, int, dst, unsigned int, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_LONGLONG:
      COPY_TYPE_TO_SUB_OF_TYPE(src, int, dst, long long, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_ULONGLONG:
      COPY_TYPE_TO_SUB_OF_TYPE(src, int, dst, unsigned long long, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_FLOAT:
      COPY_TYPE_TO_SUB_OF_TYPE(src, int, dst, float, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_DOUBLE:
      COPY_TYPE_TO_SUB_OF_TYPE(src, int, dst, double, ri, nri, nr, ci, nci, cinc, n);
      break;
    default:
      error("unknown extmat type: %d", m->type);
      break;
    }
  } else if (IS_CHARACTER(val)) {
    // copy from a character scalar, which is assumed to hold packed
    // data items of the same size as the destination extmat
    src = CHAR(STRING_ELT(val, 0));
    switch(m->type) {
    case EXTMAT_TYPE_CHAR:
      COPY_TYPE_TO_SUB_OF_TYPE(src, char, dst, char, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_UCHAR:
      COPY_TYPE_TO_SUB_OF_TYPE(src, unsigned char, dst, unsigned char, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_SHORT:
      COPY_TYPE_TO_SUB_OF_TYPE(src, short, dst, short, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_USHORT:
      COPY_TYPE_TO_SUB_OF_TYPE(src, unsigned short, dst, unsigned short, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_INT:
      COPY_TYPE_TO_SUB_OF_TYPE(src, int, dst, int, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_UINT:
      COPY_TYPE_TO_SUB_OF_TYPE(src, int, dst, unsigned int, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_LONGLONG:
      COPY_TYPE_TO_SUB_OF_TYPE(src, long long, dst, long long, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_ULONGLONG:
      COPY_TYPE_TO_SUB_OF_TYPE(src, unsigned long long, dst, unsigned long long, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_FLOAT:
      COPY_TYPE_TO_SUB_OF_TYPE(src, float, dst, float, ri, nri, nr, ci, nci, cinc, n);
      break;
    case EXTMAT_TYPE_DOUBLE:
      COPY_TYPE_TO_SUB_OF_TYPE(src, double, dst, double, ri, nri, nr, ci, nci, cinc, n);
      break;
    default:
      error("unknown extmat type: %d", m->type);
      break;
    }  }
  if (num_prot)
    UNPROTECT(num_prot);
}  


/*================================================================

  extmat.dll method registration, initialization, and destruction

  ================================================================*/

R_CallMethodDef extmat_call_methods[]  = {
  // R hook functions
  MKREF(extmat_create, 5),
  MKREF(extmat_get_dim, 1),
  MKREF(extmat_set_dim, 3),
  MKREF(extmat_zero, 1),
  MKREF(extmat_get_name, 1),
  MKREF(extmat_set_name, 2),
  MKREF(extmat_index, 6),
  MKREF(extmat_index_assign, 6),
  MKREF(extmat_destroy, 1),
  MKREF(extmat_info, 1),
  MKREF(extmat_pointer, 1),
  {NULL, NULL, 0}
};

void
R_init_extmat(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, extmat_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_PreserveObject(EXTMAT_CLASSNAME_STRING = mkString("extmat"));
  R_RegisterCCallable("extmat", "ensure_extmat", (DL_FUNC) ensure_extmat);
  R_RegisterCCallable("extmat", "ensure_extmat_with_slop", (DL_FUNC) ensure_extmat_with_slop);
  R_RegisterCCallable("extmat", "ensure_extmat_with_slop_trigger", (DL_FUNC) ensure_extmat_with_slop_trigger);
  R_RegisterCCallable("extmat", "extmat_to_sexp", (DL_FUNC) extmat_to_sexp);
  R_RegisterCCallable("extmat", "free_extmat", (DL_FUNC) free_extmat);
}

void
R_unload_extmat(DllInfo *info)
{
  /* Release resources. */
  R_ReleaseObject(EXTMAT_CLASSNAME_STRING);
}
