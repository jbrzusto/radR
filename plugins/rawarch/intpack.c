/*  svn $Id: intpack.c 64 2008-11-12 13:10:41Z john $

    radR : an R-based platform for acquisition and analysis of radar data
    Copyright (C) 2006, 2007, 2008 John Brzustowski        

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

********************************************************************************

  intpack.c - pack/unpack integers of various sizes and endianness
  Packed values are little endian; unpacked can be either endian.

*/

#include <string.h>
#define TRUE 1
#define FALSE 0

int
intpack (void * vdst, void * vsrc, int num, int src_bits, int src_size, int big_endian)
{

  /* 
     vdst: points to the first byte of storage where num items of size
     src_bits bits each will be stored in packed format.

     vsrc: points to the first byte of storage where num items of size
     src_size bytes are stored.  
     
     src_bits: the number of actually used bits per item (which must be
     the lowest order bits)

     src_size: the number of bytes occupied by each item in the source.

     big_endian: if non-zero, the source is bigendian

     We assume dst has been allocated to sufficient size.
     
     Restrictions:

     src_bits <= 8 * src_size

     src_size = 1, 2, 4, or 8.
     src_bits = 4, 8, 12, 16, 32 or 64

     Not all values of src_bits are supported.
  */


  int i, n, delta;

  unsigned char * dst = (unsigned char *) vdst;
  unsigned char * src = (unsigned char *) vsrc;

  if (src_bits == 8 * src_size && (src_size == 1 || !big_endian)) {
    /* dispose of simple cases */
    memmove (dst, src, num * src_size);
    return TRUE;
  }
  if (src_bits > 8 * src_size)
    return FALSE; /* silently fail */

  switch(src_bits) {

  case 4:
    if (big_endian)
      src += src_size - 1;
    n = num & ~1;
    for (i=0; i < n; i += 2, dst += 1, src += src_size * 2)
      dst[0] = src[0] | (src[src_size] << 4);
    if (num & 1)
      dst[0] = src[0];
    break;

  case 8:
    if (big_endian)
      src += src_size - 1;
    for (i=0; i < num; i += 1, dst += 1, src += src_size)
      dst[0] = src[0];
    break;

  case 12:
    /* pack 0xabc, 0xdef -> 0xbc, 0xda, 0xef */
    if (big_endian) {
      src += src_size - 1; /* src_size >= 2 */
      delta = -1;
    } else {
      delta = 1;
    }
    n = num & ~1;
    for (i=0; i < n; i += 2, dst += 3, src += 2 * src_size) {
      dst[0] = src[0]; dst[1] = src[delta] | (src[src_size+delta] << 4); dst[2] = src[src_size];
    }
    if (num & 1) {
      dst[0] = src[0]; dst[1] = src[delta];
    }
    break;

  case 16:
    if (big_endian) {
      src += src_size - 1; /* src_size >= 2 */
      delta = -1;
    } else {
      delta = 1;
    }
    for (i=0; i < num; i += 1, dst += 2, src += src_size) {
      dst[0] = src[0]; dst[1] = src[delta];
    }      
    break;

  case 24:
    if (big_endian) {
      src += src_size - 1; /* src_size >= 3 */
      delta = -1;
    } else {
      delta = 1;
    }
    for (i=0; i < num; i += 1, dst += 3, src += src_size) {
      dst[0] = src[0]; dst[1] = src[delta]; dst[2] = src[2 * delta];
    }      
    break;

  case 32:
    if (big_endian) {
      src += src_size - 1; /* src_size >= 4 */
      delta = -1;
    } else {
      delta = 1;
    }
    for (i=0; i < num; i += 1, dst += 4, src += src_size) {
      dst[0] = src[0]; dst[1] = src[delta]; dst[2] = src[2 * delta]; dst[3] = src[3 * delta];
    }      
    break;

  case 64:
    if (big_endian) {
      src += src_size - 1; /* src_size >= 8 */
      delta = -1;
    } else {
      delta = 1;
    }
    for (i=0; i < num; i += 1, dst += 8, src += src_size) {
      dst[0] = src[0]; dst[1] = src[delta]; dst[2] = src[2 * delta]; dst[3] = src[3 * delta];
      dst[4] = src[4 * delta]; dst[5] = src[5 * delta]; dst[6] = src[6 * delta]; dst[7] = src[7 * delta];
    }      
    break;
  default:
    return FALSE;
  }
  return TRUE;
}  
  
    
  
int
intunpack (void * vdst, void * vsrc, int num, int src_bits, int dst_size, int big_endian)
{

  /* 
     dst: points to the first byte of storage where num items of size
     src_bits bits each will be stored in items of size dst_size bytes.

     src: points to the first byte of storage where num items of size
     src_bits packed by intpack are stored.
     
     src_bits: the number of actually used bits per item (which must be
     the lowest order bits)

     dst_size: the number of bytes occupied by each item in the destination.

     big_endian: if non-zero, the destination bytes are stored in bigendian order

     We assume dst has been allocated to sufficient size.
     
     Restrictions:

     src_bits <= 8 * dst_size

     dst_size = 1, 2, 4, or 8.
     src_bits = 4, 8, 12, 16, 32 or 64

     Not all values of src_bits are supported.  Returns FALSE on failure
     due to unsupported or incompatible parameter combinations,
     TRUE otherwise.

  */


  int i, n, delta;

  unsigned char * dst = (unsigned char *) vdst;
  unsigned char * src = (unsigned char *) vsrc;

  if (src_bits == 8 * dst_size && (dst_size == 1 || !big_endian)) {
    /* dispose of simple cases */
    memmove (dst, src, num * dst_size);
    return TRUE;
  }
  if (src_bits > 8 * dst_size)
    return FALSE; /* fail */

  /* zero all bytes if any are padding bytes */
  if (dst_size > (src_bits + 7) / 8)
    memset(dst, 0, dst_size * num);

  switch(src_bits) {

  case 4:
    if (big_endian) {
      dst += dst_size - 1;
    }
    n = num & ~1;
    for (i=0; i < n; i += 2, dst += dst_size, src += 1) {
      dst[0] = src[0] & 0x0f;  dst[dst_size] = src[0] >> 4;
    }
    if (num & 1)
      dst[0] = src[0];
    break;

  case 8:
    if (big_endian)
      src += dst_size - 1;
    for (i=0; i < num; i += 1, dst += dst_size, src += 1)
      dst[0] = src[0];
    break;

  case 12:
    /* unpack 0xbc, 0xda, 0xef -> 0xabc, 0xdef  */

    if (big_endian) {
      src += dst_size - 1; /* dst_size >= 2 */
      delta = -1;
    } else {
      delta = 1;
    }
    n = num & ~1;
    for (i=0; i < n; i += 2, dst += 2 * dst_size, src += 3) {
      dst[0] = src[0]; dst[delta] = src[1] &0xf; dst[dst_size] = src[2]; dst[dst_size+delta] = src[1] >> 4;
    }
    if (num & 1) {
      dst[0] = src[0]; dst[delta] = src[1];
    }
    break;

  case 16:
    if (big_endian) {
      src += dst_size - 1; /* dst_size >= 2 */
      delta = -1;
    } else {
      delta = 1;
    }
    for (i=0; i < num; i += 1, dst += dst_size, src += 2) {
      dst[0] = src[0]; dst[delta] = src[1];
    }      
    break;

  case 24:
    if (big_endian) {
      src += dst_size - 1; /* dst_size >= 3 */
      delta = -1;
    } else {
      delta = 1;
    }
    for (i=0; i < num; i += 1, dst += dst_size, src += 3) {
      dst[0] = src[0]; dst[delta] = src[1]; dst[2*delta] = src[2];
    }      
    break;

  case 32:
    if (big_endian) {
      src += dst_size - 1; /* dst_size >= 4 */
      delta = -1;
    } else {
      delta = 1;
    }
    for (i=0; i < num; i += 1, dst += dst_size, src += 4) {
      dst[0] = src[0]; dst[delta] = src[1]; dst[2*delta] = src[2]; dst[3*delta] = src[3];
    }      
    break;

  case 64:
    if (big_endian) {
      src += dst_size - 1; /* dst_size >= 8 */
      delta = -1;
    } else {
      delta = 1;
    }
    for (i=0; i < num; i += 1, dst += dst_size, src += 8) {
      dst[0] = src[0]; dst[delta] = src[1]; dst[2*delta] = src[2]; dst[3*delta] = src[3];
      dst[4*delta] = src[4]; dst[5*delta] = src[5]; dst[6*delta] = src[6]; dst[7*delta] = src[7];
    }      
    break;
  default:
    return FALSE;
  }
  return TRUE;
}  
  
    
  
