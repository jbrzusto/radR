/*
  rawarch.c - support for the radR raw binary read/write plugin

  You would think we'd use a RAWSXP rather than a STRSXP
  to represent arbitrary binary objects.  However, R
  (as of version 2.4.0) has much more efficient serialize/unserialize
  code for a STRSXP than for a RAWSXP (the latter requiring two levels
  of function call for each raw byte!).  Strings in a vector are written
  one at a time, with a single put to the stream.
  We don't use just CHARSXP, because the de/serialize code is not equipped
  to handle these except as elements of a STRSXP (WriteItem for a CHARSXP
  doesn't output the flags integer, on the assumption that ReadItem will
  already know it is reading a sequence of CHARSXP from a STRSXP).

  FIXME: as of R 2.9 or thereabouts, STRSXP are no longer permitted to 
  hold NUL bytes, and so we need to convert to using RAWSXP.

  FIXME: this code assumes a little-endian platform.
*/

#include "radRmodule.h"
#include "intpack.h"
#include "zlib.h"

SEXP
raw_unpack (SEXP src_sexp, SEXP dst_sexp, SEXP parms)
{

/* 
     unpack raw data to a destination 

  src_sexp:   a STRSXP with a single element which is a CHARSXP of raw bytes;
              or a RAWSXP.

  dst_sexp:   a SEXP supplying the pointer to storage, as follows:
                 TYPE     effective pointer
              --------------------------------------------------
                STRSXP      CHAR(STRING_ELT(ptr, 0))
                INTSXP      INTEGER(ptr)
                RAWSXP      RAW(ptr)
                EXTPTRSXP   EXTPTR_PTR(ptr)

  parms:      three integer vector with these elements:

              [1] = size of destination items in bytes
              [2] = total number of items
              [3] = bits per source item
	      [4] = big_endian? non-zero means unpack into bigendian byte order
	            (only relevant for size = 2, 4, 8 bytes)
  returns: dst_sexp

  Currently supports 4, 8, 12, or 16 bits per source item and
  destination items of size at 1, 2, and 4 bytes (which must be at
  least as large as the source item).
  
  */

  int bps, needed, ns, size, big_endian, fail;
  unsigned char *src, *dst;
  
  parms = AS_INTEGER(parms);
  size = INTEGER(parms)[0];
  ns = INTEGER(parms)[1];
  bps = INTEGER(parms)[2];
  big_endian = INTEGER(parms)[3];
  
  fail = FALSE;
  needed = size * ns;
  switch(TYPEOF(dst_sexp)) {
  case STRSXP:
    dst = (unsigned char *) CHAR(STRING_ELT(dst_sexp, 0));
    if (LENGTH(STRING_ELT(dst_sexp, 0)) < needed)
      fail = TRUE;
    break;
  case INTSXP:
    dst = (unsigned char *) INTEGER(dst_sexp);
    if (LENGTH(dst_sexp) * 4 < needed)
      fail = TRUE;
    break;
  case RAWSXP:
    dst = (unsigned char *) RAW(dst_sexp);
    if (LENGTH(dst_sexp) < needed)
      fail = TRUE;
    break;
  case EXTPTRSXP:
    dst = (unsigned char *) EXTPTR_PTR(dst_sexp);
    // not much we can do here.
    break;
  default:
    dst = NULL;
    error("raw_unpack: unknown destination type: %d", TYPEOF(dst_sexp));
  };
  if (fail)
    error("raw_unpack: not enough memory in destination");

  switch(TYPEOF(src_sexp)) {
  case STRSXP:
    src = (unsigned char *) CHAR(STRING_ELT(src_sexp, 0));
    break;
  case RAWSXP:
    src = (unsigned char *) RAW(src_sexp);
    break;
  default:
    src = NULL;
    error("raw_unpack: unknown source type: %d", TYPEOF(dst_sexp));
  };

  if (!intunpack(dst, src, ns, bps, size, big_endian))
    error("raw_unpack: unsupported combination: bsp=%d, size=%d", bps, size);
  return (dst_sexp);
}

SEXP
raw_pack (SEXP src_sexp, SEXP parms)
{

  /* pack data from a pointer into a string
     
  src_sexp:   a SEXP supplying the pointer to storage, as follows:
                 TYPE     effective pointer
              --------------------------------------------------
                STRSXP      CHAR(STRING_ELT(ptr, 0))
                INTSXP      INTEGER(ptr)
                RAWSXP      RAW(ptr)
                EXTPTRSXP   EXTPTR_PTR(ptr)

  parms:      three integer vector with these elements:

              [1] = size of source items in bytes
              [2] = total number of items
              [3] = bits per source item
	      [4] = big_endian? non-zero means pack from bigendian byte order
	            (only relevant for size = 2, 4, 8 bytes)

  returns: a character vector whose single element is a string with packed integers
           from the source.

  Currently supports bits.per.sample = 4, 8, 12, or 16
  */

  SEXP rv;
  int bps, needed, ns, size, big_endian;
  unsigned char *src, *dst;
  
  parms = AS_INTEGER(parms);
  size = INTEGER(parms)[0];
  ns = INTEGER(parms)[1];
  bps = INTEGER(parms)[2];
  big_endian = INTEGER(parms)[3];

  needed = (ns * bps + 7) / 8 ;
  
  PROTECT(rv = allocVector(STRSXP, 1)); 
  SET_STRING_ELT(rv, 0, allocVector(CHARSXP, needed)); // "raw" vector of required size
  dst = (unsigned char *) CHAR(STRING_ELT(rv, 0));

  switch(TYPEOF(src_sexp)) {
  case STRSXP:
    src = (unsigned char *) CHAR(STRING_ELT(src_sexp, 0));
    break;
  case INTSXP:
    src = (unsigned char *) INTEGER(src_sexp);
    break;
  case RAWSXP:
    src = (unsigned char *) RAW(src_sexp);
    break;
  case EXTPTRSXP:
    src = (unsigned char *) EXTPTR_PTR(src_sexp);
    break;
  default:
    error("raw_pack: unknown source type: %d", TYPEOF(src_sexp));
    src = NULL;
  };

  if (!intpack(dst, src, ns, bps, size, big_endian))
    error("raw_pack: unsupported combination: bsp=%d, size=%d", bps, size);
  UNPROTECT(1);
  return (rv);
}


/** 
 * compress a vector using zlib
 * 
 * @param in a CHARACTER or RAW vector or EXTPTR to be compressed
 * @param len integer scalar giving the length of data to compress; must be specified
 * if \p in is an \p EXTPTR, optional otherwise.
 *
 * @return a vector of the same type as \p in which holds the compressed form of \p in,
 * except that a RAW vector is returned when \p in is an EXTPTR.
 * When \p in is uncompressible, the length of the returned vector will be a bit larger
 * than that of \p in.
 */
SEXP
zlib_compress (SEXP in, SEXP len)
{
  SEXP out = R_NilValue;
  SEXP rv = R_NilValue;
  SEXP tmp;
  int r;
  unsigned char *inp = NULL, *outp = NULL;
  unsigned long in_len = 0, out_len;

  if (len != R_NilValue)
    in_len = INTEGER(AS_INTEGER(len))[0];
    
  switch(TYPEOF(in)) {
  case STRSXP:
    inp = (unsigned char *) CHAR(STRING_ELT(in, 0));
    if (! in_len)
      in_len = LENGTH(STRING_ELT(in, 0));
    PROTECT(out = allocVector(STRSXP, 1));
    PROTECT(tmp = allocString(out_len = compressBound(in_len)));
    SET_STRING_ELT(out, 0, tmp);
    UNPROTECT(1);
    outp = (unsigned char *) CHAR(tmp);
    break;
  case RAWSXP:
    inp = RAW(in);
    if (! in_len)
      in_len = LENGTH(in);
    PROTECT(out = allocVector(RAWSXP, out_len = compressBound(in_len)));
    outp = RAW(out);
    break;
    
  case EXTPTRSXP:
    inp = (unsigned char *) EXTPTR_PTR(in);
    PROTECT(out = allocVector(RAWSXP, out_len = compressBound(in_len)));
    outp = RAW(out);
    break;
    
  default:
    error("type of 'in' must be character, raw, or extptr");
  };

  r = compress2(outp, &out_len, inp, in_len, 1);  // use compression level 1 for highest speed
  if (r != Z_OK)
    error("Internal zlib compression error ", r);

  switch(TYPEOF(in)) {
  case STRSXP:
    PROTECT(rv = allocString(out_len));
    memcpy(CHAR(rv), CHAR(STRING_ELT(out, 0)), out_len);
    SET_STRING_ELT(out, 0, rv);
    UNPROTECT(2);
    return out;
    
  case RAWSXP:
  case EXTPTRSXP:
    rv = allocVector(RAWSXP, out_len);
    memcpy(RAW(rv), RAW(out), out_len);
    UNPROTECT(1);
  };
  return rv;
}

/** 
 * decompress a vector using zlib
 * 
 * @param in a CHAR or RAW vector to be uncompressed
 * @param size the known size of the uncompressed result
 * @param dest[out] if not R_NilValue, a vector in which to store the decompressed result;
 * This must already be allocated to the required size.  In this situation, the
 * function returns R_NilValue.
 * 
 * @return a vector of the same type as \p in which holds the uncompressed form of \p
 */

SEXP
zlib_decompress (SEXP in, SEXP size, SEXP dest)
{
  unsigned size_ = (unsigned) INTEGER(AS_INTEGER(size))[0];
  SEXP rv = dest;
  unsigned char *inp, *outp;
  unsigned long in_len, out_len = size_;
  int r;
  int npr = 0;

  outp = NULL;

  if (dest != R_NilValue) {
    switch(TYPEOF(dest)) {
    case EXTPTRSXP:
      outp = (unsigned char *) EXTPTR_PTR(dest);
      break;
    case INTSXP:
    case REALSXP:
    case LGLSXP:
      outp = (unsigned char *) DATAPTR(dest);
      break;
    case STRSXP:
      outp = (unsigned char *) CHAR(STRING_ELT(dest, 0));
      break;
    default:
      break;
    }
  }

  if (TYPEOF(in) != STRSXP && TYPEOF(in) != RAWSXP)
    error("type of 'in' must be char or raw");
  if (TYPEOF(in) == RAWSXP) {
    inp = RAW(in);
    in_len = LENGTH(in);
    if (! outp) {
      PROTECT(rv = allocVector(RAWSXP, size_));
      ++npr;
      outp = RAW(rv);
    }
  } else {
    SEXP tmp;
    inp = (unsigned char *) CHAR(STRING_ELT(in, 0));
    in_len = LENGTH(STRING_ELT(in, 0));
    if (! outp) {
      PROTECT(rv = allocVector(STRSXP, 1));
      ++npr;
      tmp = allocString(size_);
      SET_STRING_ELT(rv, 0, tmp);
      outp = (unsigned char *) CHAR(tmp);
    }
  }
  r = uncompress (outp, &out_len, inp, in_len);
  if (npr)
    UNPROTECT(npr);
  if (r != Z_OK || out_len != size_)
    error("Internal zlib decompression error ", r);
  return rv;
}

R_CallMethodDef rawarch_call_methods[]  = {
  // R hook functions
  MKREF(raw_unpack, 3),
  MKREF(raw_pack, 2),
  MKREF(zlib_compress, 2),
  MKREF(zlib_decompress, 3),
  {NULL, NULL, 0}
};

void
R_init_rawarch(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, rawarch_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

void
R_unload_rawarch(DllInfo *info)
{
  /* Release resources. */
}
