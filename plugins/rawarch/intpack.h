#include <stdint.h>

extern int intpack (void * vdst, void * vsrc, int num, int src_bits, int src_size, int big_endian);
extern int intunpack (void * vdst, void * vsrc, int num, int src_bits, int dst_size, int big_endian);

