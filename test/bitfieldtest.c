#include <stdio.h>
union {
  unsigned long asint;
  struct {
    unsigned long  me:4;
    unsigned long you:16;
    unsigned long them:12;
  };
} h;

main(int argc, char *argv[]) {
  h.me = 3;
  h.you = 32769;
  h.them = 2048;
  printf("%x\n", h.asint);
}
