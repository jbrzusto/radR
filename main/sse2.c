/* svn $Id: sse2.c 50 2008-11-07 12:52:13Z john $

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
 
/*
  sse2.c - functions for using i386-family SSE2 instructions, if available
*/

#ifdef Win32
#include <windows.h>
#include <winnt.h>
#include <winbase.h>
#else
#include <stdio.h>
#endif

int
have_sse2 () 
{
  /* 
     Does the processor have SSE2 instructions?  
     Return TRUE if it does, FALSE if it doesn't.
  */

  int rv;

#ifdef Win32
  rv = IsProcessorFeaturePresent(PF_XMMI_INSTRUCTIONS_AVAILABLE);
#else
  char buf[512];
  FILE *f = fopen("/proc/cpuinfo", "rb");
  rv = FALSE;
  if (f) {
    for (;;) {
      if (!fgets(buf, 512, f))
	break;
      if (strstr(" sse2 ", buf)) {
	rv = TRUE;
	break;
      }
    }
    close(f);
  }
#endif
  return rv;
}


