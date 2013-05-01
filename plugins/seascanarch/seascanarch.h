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
  int32_t first_chunk_index;    /* extra one at the end to simplify code */
  int32_t num_scans;    /* apparent number of scans in each segment */
  int32_t is_gated;             /* are the segment data gated? */
} segment_info_t;

typedef struct {
  char filename[1024];                                   /* full path to archive file */
  FILE *file;                                            /* file handle for archive file */
  char buff[ARCHIVE_FILE_BUFF_SIZE];                     /* buffer for reading/writing archive file data */
  DISK_DIRECTORY_ENTRY *dir_buff;                        /* buffer for archive directory */
  union {
    ARCHIVE_LABEL arlab;                                 /* buffer for holding the archive label */
    char filler1[ROUND_UP_TO(sizeof(ARCHIVE_LABEL), SEASCAN_DISK_CHUNK_SIZE)];                /* filler so we can just read a full chunk */
  };
  DATA_HEADER dh;                                        /* buffer for scan data header */
  union {
    BASE_RADAR_HDR brh;                                    /* archive data header for each quadrant  */
    char filler2[ROUND_UP_TO(sizeof(BASE_RADAR_HDR), SEASCAN_DISK_CHUNK_SIZE)];                 /* filler */
  };
  union {
    TAPE_CONTENTS toc;                               /* table of contents for the archive */
    char filler3[ROUND_UP_TO(sizeof(TAPE_CONTENTS), SEASCAN_DISK_CHUNK_SIZE)];                 /* filler */
  };
  int32_t segment_index;                 		 /* index of segment which includes next scan to be read */
  int32_t chunk_index;                   		 /* index of next archive chunk to be read, among all chunks in the archive, not */
                                                         /* just the ones in the current segment zero-based */
  segment_info_t seg_info[MAX_DATA_BLOCKS + 1];          /* extra info on each segment */

  int32_t have_archive_dir;		                 /* has the archive directory been read in? */
  int32_t have_toc;                                      /* is the table of contents in toc valid? */
  int32_t have_scan_data;                                /* is there data available for a call to get_scan_data */
  t_ssa_port port;                                       /* what port am I?  if ssa->port == x, then ports[x] == ssa */
  double time_end_latest_block;                          /* timestamp of end of most recently read data block; estimated if it doesn't exist */
  double time_start_this_block;                          /* timestamp of start of this data block */

  double time_at_zero_degrees;                           /* timestamp of pulse at zero degrees true heading in most recently read scan */
  int32_t use_PCTimestamp;                               /* if non-zero, use the PCTimestamp, rather than GPS timestamp */

  // fields used only for ungated data: ----------------------------------------

  int32_t desired_azimuths;                              /* how many azimuths to be created for the gated data */
  double max_azimuth_err;                                /* how much can an output azimuth differ from the real azimuth used to */
                                                         /* provide its data?  1.0 means a single (output) azimuth and so on */
  int32_t buffered_chunk_index;                          /* index of ungated chunk (header, data, angles) currently in buffers */
  int32_t input_index;                                   /* input pulse to use for next output pulse */
  double out_dtheta;                                     /* angle change per pulse in desired gated output */
  t_extmat data_block;                                   /* buffer for current data block */
  t_extmat angle_block;                                  /* buffer for current angle lookup table block */
  int zero_angle_index;                                  /* TERRIBLE KLUDGE: when a pulse at zero bearing is found, its non-negative angle is stored here */
  int have_prev_chunk;                                   /* do we have the previous block's last pulse and angle stored ?*/
  double prev_chunk_theta;                               /* the angle corresponding to the last pulse of the previous data block */
  // end of fields only for ungated data ----------------------------------------
} t_ssa;
  
// forward declarations
int gated (t_ssa *me);
int cleanup_toc (t_ssa *me);
int read_archive_contents (t_ssa *me);
int read_archive_next_scan_hdr_ungated (t_ssa *me);
int read_archive_next_scan_ungated (t_ssa *me, char *buffer);
int try_read_chunk(t_ssa *me, void * ptr, int size);
int read_ungated_chunk(t_ssa *me);
