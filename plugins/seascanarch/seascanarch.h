/*

  seascanarch.h - defs for seascanarch.c for reading/writing seascan archives directly

  by John Brzustowski 2006  john AT brz DOT ca

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
#include "radRmodule.h"
#include <sys/types.h>
#include <unistd.h>
#include <time.h>

// SeaScan Client.dll definitions

#include "wintypes.h"
#include "IDLCONST.h"
#include "myIDLTYPES.h"
//#include "../seascan/CLIENT12.h"

#define ARCHIVE_LABEL_TEXT "Version 3 - MRI data tape"

#define SEASCAN_DISK_CHUNK_SIZE 1024
#define ARCHIVE_FILE_BUFF_SIZE (SEASCAN_DISK_CHUNK_SIZE * 1024) /* size (bytes) of the archive file buffer */
#define MAX_BAD_SCAN_HEADERS 40 /* maximum number of bad scan headers allowed before we find a valid one 0-degree one in a segment */
#define BOGUS_ANGLE -1000 /* represents no angle */

typedef enum {
  SSA_NONE = -1,                /* no port */
  SSA_READER = 0,		/* the archive reader */
  SSA_WRITER = 1,		/* the archive writer */
} t_ssa_port;
    
typedef struct {
  char filename[1024];                                   /* full path to archive file */
  FILE *file;                                            /* file handle for archive file */
  char buff[ARCHIVE_FILE_BUFF_SIZE];                     /* buffer for reading/writing archive file data */
  DISK_DIRECTORY_ENTRY *dir_buff;                        /* buffer for archive directory */
  ARCHIVE_LABEL arlab;                                   /* buffer for holding the archive label */
  DATA_HEADER dh;                                        /* buffer for scan data header */
  BASE_RADAR_HDR brh;                                    /* archive data header for each quadrant  */
  TAPE_CONTENTS toc;                                     /* table of contents for the archive */
  int32_t segment_index;                 		 /* index of segment which includes next scan to be read */
  int32_t scan_index;                   		 /* index of next archive scan to be read, among all scans in the archive, not */
                                                         /* just the ones in the current segment zero-based */
  int32_t segment_first_scan_index[MAX_DATA_BLOCKS + 1]; /* extra one at the end to simplify code */
  int32_t segment_num_scans[MAX_DATA_BLOCKS + 1];        /* apparent number of scans in each segment */
  int32_t have_archive_dir;		                 /* has the archive directory been read in? */
  int32_t have_toc;                                      /* is the table of contents in tc valid? */
  int32_t have_scan_data;                                /* is there data available for a call to get_scan_data */
  t_ssa_port port;                                       /* what port am I?  if ssa->port == x, then ports[x] == ssa */
  double time_end_latest_block;                          /* timestamp of end of most recently read data block; estimated if it doesn't exist */
  double time_start_this_block;                          /* timestamp of start of this data block */
  int32_t is_gated;			                 /* is the underlying data archive gated? */
  int32_t use_PCTimestamp;                               /* if non-zero, use the PCTimestamp, rather than GPS timestamp */

  // fields used only for ungated data: ----------------------------------------

  int32_t desired_azimuths;                              /* how many azimuths to be created for the gated data */
  double max_azimuth_err;                                /* how much can an output azimuth differ from the real azimuth used to */
                                                         /* provide its data?  1.0 means a single (output) azimuth and so on */
  BASE_RADAR_HDR brh2;                                   /* a second archive header, for reading ungated data; this holds
                                                           the header for the next (unread) data block */     
  int32_t have_data_block;                               /* has a data block been read in from this archive yet? */
  int32_t input_index;                                   /* input pulse to use for next output pulse */
  double out_dtheta;                                     /* angle change per pulse in desired gated output */
  double prev_pulse_theta;                               /* the angle corresponding to the last pulse of the previous data block */
  t_extmat data_block;                                   /* buffer for current data block */
  t_extmat angle_block;                                  /* buffer for current angle lookup table block */
  int32_t skip_changeover_pulse;                         /* if true, skip the last pulse at a given bearing, which might have bad data */
  int last_seek_scan;                                    /* TERRIBLE KLUDGE: use to skip seeking when moving between consecutive scans */
  double index_angle;                                    /* TERRIBLE KLUDGE: the index angle delimits sweeps; it would be 0, except that 0 is
                                                            not always present (e.g. in selective digitization / sector blanking ) */
  int zero_angle_index;                                  /* TERRIBLE KLUDGE: when a pulse at zero bearing is found, its non-negative angle is stored here */

  // end of fields only for ungated data ----------------------------------------
} t_ssa;


/* attempt to read the next data block, leaving an extra row at the beginning of the buffer */
#define TRY_READ_DATA(SSA) ({ 												  \
  t_ssa * __SSA__ = (SSA); 												  \
  int __NEED__ =  ROUND_UP_TO(__SSA__->brh.yExtent * __SSA__->brh.rAxisBytes,  SEASCAN_DISK_CHUNK_SIZE); 		  \
  (*pensure_extmat)(& __SSA__->data_block, __NEED__+ __SSA__->brh.rAxisBytes, 1); 						  \
  1 == fread(__SSA__->data_block.ptr + __SSA__->brh.rAxisBytes, __NEED__, 1, __SSA__ -> file); 				  \
  })

#define TRY_SKIP_ANGLES(SSA) ({ 											  \
  t_ssa * __SSA__ = (SSA); 												  \
  ! (bigfseek(__SSA__ -> file, ROUND_UP_TO(__SSA__->brh.AssociatedAzimuths *sizeof(RSI_LUT), SEASCAN_DISK_CHUNK_SIZE), 	  \
              SEEK_CUR));												  \
  })

#define TRY_READ_ANGLES(SSA) ({ 											  \
  t_ssa * __SSA__ = (SSA); 												  \
  int __NEED__ =  ROUND_UP_TO(__SSA__->brh.AssociatedAzimuths * sizeof(RSI_LUT),  SEASCAN_DISK_CHUNK_SIZE); 		  \
  (*pensure_extmat)(& __SSA__->angle_block, __NEED__ + sizeof(RSI_LUT), 1); 						  \
  1 == fread(__SSA__->angle_block.ptr + sizeof(RSI_LUT), __NEED__, 1, __SSA__ -> file); 				  \
  })
  
/* attempt to skip the data block and the angle block */

#define TRY_SKIP_DATA_AND_ANGLES(SSA) ({ 										  \
  t_ssa * __SSA__ = (SSA); 												  \
  ! (bigfseek(__SSA__ -> file,  ROUND_UP_TO(__SSA__->brh.yExtent * __SSA__->brh.rAxisBytes,  SEASCAN_DISK_CHUNK_SIZE) 	  \
                               +ROUND_UP_TO(__SSA__->brh.AssociatedAzimuths *sizeof(RSI_LUT), SEASCAN_DISK_CHUNK_SIZE),   \
              SEEK_CUR));												  \
  })
  
// forward declarations
int read_archive_contents (t_ssa *me);
int read_archive_next_scan_hdr_ungated (t_ssa *me);
int read_archive_next_scan_ungated (t_ssa *me, char *buffer);
int try_read_chunk(t_ssa *ssa, char * ptr, int size);

/* read data into a structure, then seek over the bytes remaining in the disk chunk 
   allocated to that structure.  Seascan allocates disk storage for structures in 1024
   byte chunks. */

#define TRY_READ_CHUNK(SSA, OBJ) ({			     \
  t_ssa * __SSA__ = (SSA); 				     \
  char *__PTR__ = (char *) & __SSA__ -> OBJ;                 \
  try_read_chunk(__SSA__, __PTR__, sizeof (__SSA__ -> OBJ)); \
  })
