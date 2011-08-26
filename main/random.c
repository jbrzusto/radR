/* svn $Id: random.c 50 2008-11-07 12:52:13Z john $

   radR : an R-based platform for acquisition and analysis of radar data
   Copyright (C) 2006, 2007 John Brzustowski        

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

*/
 

/* random.c:  true random numbers, used for generating unique layer IDs in radR */

#include "radR.h"

#ifdef Win32
#include <windows.h>
#include <wincrypt.h>
#endif

static char _modname[] = "random_bytes";

SEXP
random_bytes (SEXP num) 
{
  /* generate num random bytes and return them as a RAWSXP

     Call with num = NULL to free resources; this returns NULL */

  int n = INTEGER(AS_INTEGER(num))[0];
  SEXP rv = allocVector (RAWSXP, n);
#ifdef Win32
  static HCRYPTPROV hcp = 0;
#else
  static FILE *f = NULL;
  static char filename[] = "/dev/urandom";
#endif

  if (num == R_NilValue) {
#ifdef Win32
    if (hcp)
      CryptReleaseContext(hcp, 0);
#else
    if (f)
      fclose(f);
#endif
    return (R_NilValue);
  }

#ifdef Win32
  if (!hcp && ! CryptAcquireContext (&hcp, NULL, NULL, PROV_RSA_FULL, 0))
    if (GetLastError() != NTE_BAD_KEYSET || ! CryptAcquireContext (&hcp, NULL, NULL, PROV_RSA_FULL, CRYPT_NEWKEYSET))
      error("%s: unable to acquire cryptographic context", _modname);
  if(! CryptGenRandom (hcp, n, RAW(rv)))
    error("%s: unable to generate %d random bytes", _modname, n);
#else
  if (!f && !(f = fopen(filename, "rb")))
    error("%s: unable to open %s", _modname, filename);
  if (n != fread(RAW(rv), 1, n, f))
    error("%s: unable to read %d bytes from %s", _modname, n, filename);
#endif
  return(rv);
}

