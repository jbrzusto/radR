/*

  sscan.h - defs for sscan.c for reading/writing seascan archives directly

*/

#ifndef Win32
#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64
#endif

#include <stdlib.h>
#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <time.h>
#include <getopt.h>

#include "wintypes.h"
#include "IDLCONST.h"
#include "myIDLTYPES.h"

#define ARCHIVE_LABEL_TEXT "Version 3 - MRI data tape"

#define SEASCAN_DISK_CHUNK_SIZE 1024

#define MAX_SEGMENTS 50   // maximum number of segments in one archive 
    
typedef struct {
  char filename[1024];              	/* full path to archive file */
  FILE *file;		         	/* file handle for archive file */
  int archive_dir_buff_alloc;           /* size of allocated archive dir buffer */
  DISK_DIRECTORY_ENTRY *dir_buff; 	/* buffer for archive directory */
  ARCHIVE_LABEL arlab;			/* buffer for holding the archive label */
  DATA_HEADER dh;                       /* buffer for scan data header */
  BASE_RADAR_HDR brh;		        /* archive data header for each quadrant  */
  TAPE_CONTENTS toc;                    /* table of contents for the archive */
  RSI_LUT *lut_buff;			/* buffer for angle lookup table (when data are not gated) */
  int lut_buff_alloc;		        /* size of allocated lut_buff */
} t_ssa;


/* read data into a structure, then seek over the bytes remaining in the disk chunk 
   allocated to that structure.  Seascan allocates disk storage for structures in 1024
   byte chunks. */

#define TRY_READ_CHUNK(SSA, OBJ) ({\
  t_ssa * __SSA__ = (SSA); \
  typeof(SSA->OBJ) *__PTR__ = & __SSA__ -> OBJ; \
  (1 == fread(__PTR__, sizeof(* __PTR__), 1, __SSA__ -> file))  \
  && ! (bigfseek(__SSA__ -> file, ROUND_UP_TO(sizeof(* __PTR__), SEASCAN_DISK_CHUNK_SIZE) - sizeof(* __PTR__), SEEK_CUR)); \
  })

/* read data into an array of structures.  Note that here obj is the name of the pointer field, not the actual storage. */

#define TRY_READ_CHUNKS(SSA, OBJ, N) ({\
  t_ssa * __SSA__ = (SSA); \
  int __N__ = (N); \
  typeof(SSA->OBJ) __PTR__ =  __SSA__ -> OBJ; \
  (__N__ == fread(__PTR__, sizeof(* __PTR__), __N__, __SSA__ -> file))  \
  && ! (bigfseek(__SSA__ -> file, ROUND_UP_TO(sizeof(* __PTR__) * __N__, SEASCAN_DISK_CHUNK_SIZE) - sizeof(* __PTR__) * __N__, SEEK_CUR)); \
  })

/* write data from a structure, padding it up to the nearest SEASCAN_DISK_CHUNK_SIZE bytes */
#define TRY_WRITE_CHUNK(SSA, OBJ) ({\
  t_ssa * __SSA__ = (SSA); \
  typeof(SSA->OBJ) *__PTR__ = & __SSA__ -> OBJ; \
  int __EXTRA__ = ROUND_UP_TO(sizeof(* __PTR__), SEASCAN_DISK_CHUNK_SIZE) - sizeof(* __PTR__); \
  (1 == fwrite(__PTR__, sizeof(* __PTR__), 1, __SSA__ -> file))  \
  && ( __EXTRA__ == 0 \
       || 1 == fwrite(zero_buffer, __EXTRA__, 1, __SSA__ -> file)); \
  })

/* write data from a buffer, padding it up to the nearest SEASCAN_DISK_CHUNK_SIZE bytes */
#define TRY_WRITE_BUFFER(SSA, PTR, N) ({\
  t_ssa * __SSA__ = (SSA); \
  int _N_ = (N); \
  typeof(PTR) __PTR__ = PTR; \
  int __EXTRA__ = ROUND_UP_TO(_N_ * sizeof(* __PTR__), SEASCAN_DISK_CHUNK_SIZE) - _N_ * sizeof(* __PTR__); \
  (_N_ == fwrite(__PTR__, sizeof(* __PTR__), _N_, __SSA__ -> file))  \
  && ( __EXTRA__ == 0 \
       || 1 == fwrite(zero_buffer, __EXTRA__, 1, __SSA__ -> file)); \
  })


#ifdef Win32
#define bigfopen(F, MODE) fopen64(F, MODE)
#define bigfseek(F, OFF, WHENCE) fseeko64(F, OFF, WHENCE)
#define bigftell(F) ftello64(F)
#else /* unix */
// The same functions work on my linux system, but other *nixes
// will presumably vary.  Using (-1 != lseek64(fileno(F), OFF, WHENCE)) does not work on my system!
#define bigfopen(F, MODE) fopen64(F, MODE)
#define bigfseek(F, OFF, WHENCE) fseeko64(F, OFF, WHENCE)
#define bigftell(F) ftello64(F)
#endif

#define ROUND_UP_TO(X, Y) (((X - 1) / Y + 1) * Y)

#define TRUE 1
#define FALSE 0
