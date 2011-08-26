#include <stdio.h>

main(int argc, char *argv[]) {
  unsigned short i;
  unsigned int j;
  unsigned long long k;

  i = (~ (unsigned) 0) >> 1;
  j = (~(unsigned) 0) >> 1;
  k = (~(unsigned) 0) >> 1;
  printf("Got i=%u, j=%u k=%llu\n", i, j, k);
}
