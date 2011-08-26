#include "radRmodule.h"

/* simple compression/decompression of Canadian weather radar 478x478 gif images, which use
   14 non-background colour indexes to represent precipitation.
   We compress these as follows:

   16 byte header "ECweatherRad1.0\000"
   short int: width
   short int: height

   The image is encoded one nibble at a time, with byte 0xXY representing nibble
   Y followed by nibble X

    nibble
   0 - 0x0d - represents a pixel at the current coordinate location with given value
 
   0x0e  - indicate the next nibble represents a skip, with 0 = skip 1, 0f = skip 16

   0x0f  - indicate the next byte represents a skip, with 0x00 = 17, 0x0ff = 272

*/
   
int 
encode_len (int *x, int len) {

  /* determine the length of encoding, in nibbles; x is the vector of
     linear coordinates (origin 1) of non-background pixels, and
     len is its length */

  int i, k, px;
  int y = 0; /* counts nibbles */
  px = -1;
  for (i = 0; i < len; ++i) {
    k = x[i] - px;
    px = x[i];
    while(k != 0) {
      if (k == 1) {
        ++y;
        k = 0;
      } else if (k <= 17) {
        y += 2;
        k = 1;
      } else {
        y += 3;
        k = k > 273 ? k - 272 : 1;
      }
    }
  }
  return y;
}

char header_string[] = "ECweatherRad1.0";
typedef struct s_header {
  char string[sizeof(header_string)];
  short w;  /* width of original image */
  short h;  /* height of original image */
  int np;   /* number of encoded pixels */
} t_header;

#define HEADER_SIZE (sizeof(t_header))

SEXP
encode (SEXP dim, SEXP vals, SEXP coords) {
  /* encode the values and locations; 

  dim: dimensions of the original matrix
  coords: integer vector of coordinates of non-background pixels (origin 1)
  vals: integer vector of values of non-background pixels, 1..14

  */

  int size = (1 + encode_len(INTEGER(coords), LENGTH(coords))) / 2 + HEADER_SIZE;
  SEXP rv;
  int i, j, px, k, ii, len;
  unsigned char nibbles[3];
  int nn=0;
  int n = LENGTH(vals);
  PROTECT(rv = allocVector(RAWSXP, size));
  t_header *hdr;
  int *src = INTEGER(vals);
  unsigned char *dst = HEADER_SIZE + (unsigned char *) RAW(rv);
  j = 0;
  px = 0;
  for (i = 0; i < n; ++i) {
    k = INTEGER(coords)[i] - px;
    px = INTEGER(coords)[i];
    while(k > 0) {
      nn = 0;
      if (k == 1) {
        nibbles[nn++] = src[i] - 1;
        k = 0;
      } else if (k <= 17) {
        nibbles[nn++] = 0x0e;
	nibbles[nn++] = k - 2;
	k = 1;
      } else {
        len = ((k >= 273) ? 273 : k) - 18;
        nibbles[nn++] = 0x0f;
	nibbles[nn++] = len % 16;
	nibbles[nn++] = len >> 4;
	k -= len + 17;
      }
      for (ii = 0; ii < nn; ++ii, ++j) {
	if (j % 2) {
	  dst[j / 2] = dst[j / 2] + (nibbles[ii] << 4);
	} else {
	  dst[j / 2] = nibbles[ii];
	}
      }
    }
  }
  hdr = (t_header *) RAW(rv);
  strcpy(hdr->string, header_string);
  hdr->w = INTEGER(dim)[0];
  hdr->h = INTEGER(dim)[1];
  hdr->np = n;
  UNPROTECT(1);
  return rv;
}

#define GET_NIBBLE(x, j) ((j % 2) ? x[j / 2] >> 4 : x[j / 2] & 0x0f )
SEXP
decode (SEXP x) {
  /* decode the raw bytes in x as described above, returning
     an integer matrix of appropriate dimensions with 0 values for
     missing data, and 1-based indexes from 1 to 14 for the rest 
     x is a RAWSXP
  */
  SEXP rv;
  unsigned char * src;
  int *dst;
  int i, j, k, n;
  t_header *hdr = (t_header *) RAW(x);
  if (strcmp(hdr->string, header_string))
    error("invalid compressed weather radar gif string");

  src = (unsigned char *) (RAW(x) + HEADER_SIZE);
  PROTECT(rv = allocMatrix(INTSXP, hdr->w, hdr->h));
  dst = INTEGER(rv);
  n = hdr->np;

  j = 0;
  for (i = 0; i < n; ++i) {
    for (;;) {
      k = GET_NIBBLE(src, j);
      ++j;
      if (k < 0x0e) {
	*dst++ = 1 + k;
	break;
      } else if (k == 0x0e) {
	dst += GET_NIBBLE(src, j) + 1;
	++j;
      } else if (k == 0x0f) {
	dst += GET_NIBBLE(src, j) + 17;
	++j;
	dst += 16 * GET_NIBBLE(src, j);
	++j;
      }
    }
  }
  UNPROTECT(1);
  return (rv);
}
      
  
     
