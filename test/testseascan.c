#include "radRmodule.h"
#include "radR.h"

int my_buff_size = 0;
int scan_rows;
int scan_cols;

static t_extmat scan_buff = {.size = sizeof(t_sample), .type=EXTMAT_TYPE_SHORT};

SEXP mkTrue(void)
{
    SEXP s = allocVector(LGLSXP, 1);
    LOGICAL(s)[0] = 1;
    return s;
}

SEXP mkFalse(void)
{
    SEXP s = allocVector(LGLSXP, 1);
    LOGICAL(s)[0] = 0;
    return s;
}

extern SEXP radR_extmat_to_R_integer(t_extmat *m);

#define COPY_TYPE_TO_TYPE(_const_src_, _src_type_, _const_dst_, _dst_type_, _num_items_) \
  { \
    _src_type_ * _src_ = (_src_type_ *) _const_src_; \
    _dst_type_ * _dst_ = (_dst_type_ *) _const_dst_; \
    _src_type_ * _end_ = _src_ + _num_items_; \
    while (_src_ < _end_) *_dst_++ = *_src_++; \
  }

SEXP
radR_extmat_dim(SEXP s) {
  // return the dimensions of this extmat object
  SEXP rv;
  t_extmat *m = SEXP_TO_EXTMAT(s);
  PROTECT(rv = allocVector(INTSXP, 2));
  INTEGER(rv)[0] = m->rows;
  INTEGER(rv)[1] = m->cols;
  UNPROTECT(1);
  return rv;
}

SEXP
radR_extmat_index(SEXP m, SEXP ind)
{
  // return a sub matrix of m, as specified by ind
  
  // FIXME: for now, just return the whole matrix, ignoring ind
  t_extmat *me = SEXP_TO_EXTMAT(m);

  return radR_extmat_to_R_integer(me);
}

SEXP
radR_extmat_to_R_integer(t_extmat *em)
{
  // return an R integer matrix SEXP containing data from the extmat *em
  // fails if unable to allocate memory

  SEXP rv;
  int *dst;
  void *src;
  int n;

  if (!(rv = allocMatrix(INTSXP, em->rows, em->cols)))
    return FAIL_SEXP;

  PROTECT(rv);

  dst = INTEGER(rv);
  src = em->ptr;
  n = em->rows * em->cols;

  switch(em->type) {
  case EXTMAT_TYPE_CHAR:
    COPY_TYPE_TO_TYPE(src, char, dst, int,  n);
    break;
  case EXTMAT_TYPE_UCHAR:
    COPY_TYPE_TO_TYPE(src, unsigned char, dst, int, n);
    break;
  case EXTMAT_TYPE_SHORT:
    COPY_TYPE_TO_TYPE(src, short, dst, int, n);
    break;
  case EXTMAT_TYPE_USHORT:
    COPY_TYPE_TO_TYPE(src, unsigned short, dst, int, n);
    break;
  case EXTMAT_TYPE_INT:
    COPY_TYPE_TO_TYPE(src, int, dst, int, n);
    break;
  case EXTMAT_TYPE_UINT:
    COPY_TYPE_TO_TYPE(src, unsigned int, dst, int, n);
    break;
  case EXTMAT_TYPE_LONGLONG:
    COPY_TYPE_TO_TYPE(src, long long, dst, int, n);
    break;
  case EXTMAT_TYPE_ULONGLONG:
    COPY_TYPE_TO_TYPE(src, unsigned long long, dst, int, n);
    break;
  case EXTMAT_TYPE_FLOAT:
    COPY_TYPE_TO_TYPE(src, float, dst, int, n);
    break;
  case EXTMAT_TYPE_DOUBLE:
    COPY_TYPE_TO_TYPE(src, double, dst, int, n);
    break;
  default:
    // bytewise copying for unknown types
    COPY_TYPE_TO_TYPE(src, char, dst, char, n * em->size);
    break;
  }

  UNPROTECT(1);
  return (rv);
}  


SEXP make_scan_buff() {
  return EXTMAT_TO_SEXP(scan_buff);
}

SEXP get_scan_buff(SEXP sbo, SEXP rect) {
  // return the portion of the scan buffer
  // specified by rect, an integer vector
  // giving row 1, row 2, col1, col2
  SEXP rv;
  t_sample *sb = (t_sample *) SEXP_TO_EXTMAT(sbo)->ptr;
  
  int r1, r2, c1, c2;
  int i, j, k;

  rect = AS_INTEGER(rect);
  r1 = INTEGER(rect)[0];
  r2 = INTEGER(rect)[1];
  c1 = INTEGER(rect)[2];
  c2 = INTEGER(rect)[3];

  PROTECT(rv = allocMatrix(INTSXP, r2 - r1 + 1, c2 - c1 + 1));
  
  for (j = c1, k=0; j <= c2; ++j) {
    for (i = r1; i <= r2; ++i) {
      INTEGER(rv)[k++] = sb[j - 1 + (i - 1) * scan_buff.cols];
    }
  }
  UNPROTECT(1);
  return rv;
}

SEXP gdb () {
  SEXP rv;
  rv = R_NilValue;
  return rv;
}
