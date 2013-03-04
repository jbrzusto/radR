/*
  rawarch.c - support for the radR raw binary read/write plugin

  We now (version 2.15.2) do everything with RAWSXP, which are read/written
  more efficiently than they used to be (version 2.5.1)/

  (DONT?)FIXME: this code assumes a little-endian platform.
*/

#include "radRmodule.h"
#include "intpack.h"
#include "zlib.h"

SEXP
raw_unpack (SEXP src_sexp, SEXP dst_sexp, SEXP parms)
{

/* 
     unpack raw data to a destination 

  src_sexp:   a RAWSXP.

  dst_sexp:   a SEXP supplying the pointer to storage, as follows:
                 TYPE     effective pointer
              --------------------------------------------------
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

  /* pack data from a pointer into a raw vector
     
  src_sexp:   a SEXP supplying the pointer to storage, as follows:
                 TYPE     effective pointer
              --------------------------------------------------
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
  
  PROTECT(rv = allocVector(RAWSXP, needed)); 
  dst = (unsigned char *) RAW(rv);

  switch(TYPEOF(src_sexp)) {
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
 * @param len integer scalar giving the length of data to compress (in
 * bytes); must be specified if \p in is an \p EXTPTR, optional
 * otherwise.
 *
 * @return an INTEGER vector which holds the compressed form of \p, except that the
 * first element is the true length of the uncompressed object (treated as an unsigned
 * integer).
 *
 * When \p in is uncompressible, the length of the returned vector will be a bit larger
 * than that of \p in.
 */
SEXP
zlib_compress (SEXP in, SEXP len)
{
  SEXP out = R_NilValue;
  SEXP rv = R_NilValue;
  int r;
  unsigned char *inp = NULL, *outp = NULL;
  uint32_t in_len = 0, out_len;

  if (len != R_NilValue)
    in_len = INTEGER(AS_INTEGER(len))[0];
    
  switch(TYPEOF(in)) {
  case RAWSXP:
    inp = RAW(in);
    if (! in_len)
      in_len = LENGTH(in);
    break;
    
  case EXTPTRSXP:
    inp = (unsigned char *) EXTPTR_PTR(in);
    break;
    
  default:
    error("type of 'in' must be character, raw, or extptr");
  };

  // get upper bound for length of output
  out_len = compressBound(in_len);

  PROTECT(out = allocVector(RAWSXP, out_len));
  outp = (unsigned char *) RAW(out);

  r = compress2(outp, (uLongf *) &out_len, inp, in_len, 1);  // use compression level 1 for highest speed
  if (r != Z_OK)
    error("Internal zlib compression error ", r);
          
  // output will be an integer vector, with first element giving actual original size

  rv = allocVector(INTSXP, 1 + (out_len + sizeof(uint32_t) - 1) / sizeof(uint32_t));
  INTEGER(rv)[0] = in_len;

  memcpy((unsigned char *) & INTEGER(rv)[1], RAW(out), out_len);

  UNPROTECT(1);

  return rv;
}

/** 
 * decompress a vector using zlib
 * 
 * @param in an INTEGER vector to be uncompressed
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
    default:
      break;
    }
  }

  if (TYPEOF(in) != RAWSXP)
    error("type of 'in' must be raw");
  inp = RAW(in);
  in_len = LENGTH(in);
  if (! outp) {
    PROTECT(rv = allocVector(RAWSXP, size_));
    ++npr;
    outp = RAW(rv);
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
