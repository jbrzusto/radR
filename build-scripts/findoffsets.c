#include "wintypes.h"
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include "IDLCONST.H"
#include "IDLTYPES.H"

#define mysizeof(STR, ELT) ({STR __x__; sizeof(__x__.ELT);})
main () {
  int i=0, offset, prev_offset, prev_sizeof;
#include "findoffsets.h"
};
