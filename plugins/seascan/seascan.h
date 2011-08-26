/*  svn $Id: seascan.h 574 2010-05-11 02:07:15Z john $

    radR : an R-based platform for acquisition and analysis of radar data
    Copyright (C) 2006-2009 John Brzustowski        

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

  definitions for the seascan server interface

*/

#include <stdlib.h>
#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <time.h>
#include "radRmodule.h"

// SeaScan Client.dll definitions

#include "wintypes.h"
#include "IDLCONST.h"
#include "myIDLTYPES.h"
#include "CLIENT12.h"

/* server status flags */

#define SEASCAN_ATODACTIVE               (1 << 0)
#define SEASCAN_TAPERECORDING            (1 << 1)
#define SEASCAN_TAPEPLAYBACK             (1 << 2)
#define SEASCAN_PLOTEXTRACTIONACTIVE     (1 << 3)
#define SEASCAN_AGCACTIVE                (1 << 4)
#define SEASCAN_RPCENABLED               (1 << 5)
#define SEASCAN_SIGNALPROCESSINGACTIVE   (1 << 6)
#define SEASCAN_CLIENTPACING             (1 << 7)

typedef unsigned short t_seascan_sample; /* type that holds a seascan sample; only lowest 12 bits are valid */

#define ARCHIVE_FILE_BUFF_SIZE (1024 * 1024) /* size (bytes) of the archive file buffer */
#define SEASCAN_MAX_GATED_IMAGE_SIZE (8 * 1024 * 1024) /* max possible size of a scan's data, in bytes; a lower max can be set in SeaScan0.ini */
#define SEASCAN_SAMPLE_SIZE sizeof(t_seascan_sample) 
#define SEASCAN_MAX_SAMPLES (SEASCAN_MAX_GATED_IMAGE_SIZE / SEASCAN_SAMPLE_SIZE) /* max number of samples in a scan */

// structures for SeaScan data

typedef enum {
  SEASCAN_NONE = -1,                 /* no source selected yet */
  SEASCAN_LIVE_0 = 0,		/* the first raw radar channel */
  SEASCAN_LIVE_1 = 1,		/* the second raw radar channel */
  SEASCAN_TAPE_0 = 2,		/* the tape archive, through channel 0 */
  SEASCAN_TAPE_1 = 3,		/* the tape archive, through channel 1 */
} t_seascan_port;

// support for multithreading the reader

#include <pthread.h>
#include <signal.h>
#include "threadutil.h"

typedef struct {
  DATA_HEADER *header;
  t_extmat *mat;
  ULONG flags;
  SHORT server;
  // pointers to preallocated SEXPs where we store the scan metadata
  // These MUST be preallocated because R is not thread safe, so
  // a spawned thread can't call any R library functions.
  int *pulses;
  int *samples_per_pulse;
  int *bits_per_sample;
  int *duration;
  double *timestamp;
  double *range_per_sample;
  double *start_range;
  double *angle_offset;
  int *orientation;
  int *pulse_length;
  int *PRF;

  // location to record whether scan data were successfully obtained
  int *have_scan_data;

  // To return scan info or R_NilValue as an error, we use a pointer to
  // a SEXP.
  // We don't have to worry about ageing the list vector in whose
  // slot we might place R_NilValue, because it has already had that
  // value stored there in the past, and so is appropriately aged.

  SEXP *rv;

  // when returning actual scan info (rather than an error), we re-use
  // the list allocated at R level by the main thread

  SEXP si;
} t_get_scan_parms;
    
typedef struct {
  SHORT svn;                     	/* server number */
  t_seascan_port current_source; 	/* used to keep track of current source on this server */
  int is_running;		        /* has current source been started with start_up (and not shut down with shut_down) */
  time_t start_time;		        /* start time for tape playback */
  int is_pacing;		        /* is ClientPacing enabled on this server? */
  int current_run;               	/* current run: which entry in the TAPE_CONTENTS structure is being played back */
  int have_scan_data;			/* true if there is scan data available to be read by a call to get_scan_data */
  int have_tape_contents;		/* true if we have a valid table of contents for the tape archive */
  pthread_t getter_thread;	        /* holds information about any existing getter thread for this port  */
  t_get_scan_parms getter_parms;       /* holds the parms for any getter thread, which should be freed when the thread is killed */
  SEXP scan_info_vector;  /* a (re-usable) vector for returning scan.info lists */
  
  SERVER_STATUS ss;
  A2D_STATUS ad;
  PROCESSING_PARAMETERS pp;
  TAPE_CONTENTS tc;
  DATA_HEADER dh;
  TAPE_STATUS ts;
} t_seascan;


/* set the error code to X and return failure */

#define ERROR_SEXP(X) {error_code = X; return FAIL_SEXP;}

/* 
   delay values (in milliseconds) apparently needed(?) by seascan
   between starting playback and doing client pacing, and between
   turning off client pacing and stopping playback.  If seeking in an
   archive seems unreliable on your system, increasing these values
   may help, at the cost of longer delays between frames.
*/

#define SEASCAN_LONG_DELAY 1000
#define SEASCAN_SHORT_DELAY 100

/* macros to call functions returning either RADR or SS error codes, and to return
   either a NULL SEXP or the internal error code if the called function itself fails
   Extra error information can be provided if the _INFO form is used. */

#define TRY_INFO(X, Y) \
  if (RADR_ERROR_NONE != (error_code = (X))) { \
    strcpy(extra_error_info, Y); \
    return error_code; \
  }

#define TRY_INFO_SEXP(X, Y) \
  if (RADR_ERROR_NONE != (error_code = (X))) { \
    strcpy(extra_error_info, Y); \
    return FAIL_SEXP; \
  }

#define TRY_INFO_SEASCAN_SEXP(X, Y) \
  if (CLIENT_STATUS_NO_ERROR != (error_code = (X))) { \
    strcpy(extra_error_info, Y); \
    return FAIL_SEXP; \
  }

#define TRY(X) \
  if (RADR_ERROR_NONE != (error_code = X)) \
     return error_code;

#define TRY_SEXP(X) \
  if (RADR_ERROR_NONE != (error_code = X)) \
     return FAIL_SEXP;

#define TRY_SEASCAN_SEXP(X) \
  if (CLIENT_STATUS_NO_ERROR != X) { \
    error_code = RADR_ERROR_UNKNOWN_PORT_ERROR; \
    return FAIL_SEXP; \
  }

#define ASSERT(X) if (!(X)) { \
    return FAIL_SEXP; \
  }
#define ASSERTE(X, ERR) \
  if (!(X)) { \
    error_code = ERR; \
    return FAIL_SEXP; \
  }

#define ASSERTEI(X, ERR, INF) \
  if (!(X)) { \
    error_code = ERR; \
    strcpy(extra_error_info, INF); \
    return FAIL_SEXP; \
  }

/* macro to convert time from SEASCAN_REALTIME format to a double, using a temporary struct tm variable */

#define TIME_FMT_CONVERT(_SRT_, _TM_) ({	\
  (_TM_).tm_sec = (_SRT_)->Second;			\
  (_TM_).tm_min = (_SRT_)->Minute;			\
  (_TM_).tm_hour = (_SRT_)->Hour;			\
  (_TM_).tm_mday = (_SRT_)->Day;			\
  (_TM_).tm_mon = (_SRT_)->Month - 1;		\
  (_TM_).tm_year = (_SRT_)->Year - 1900;		\
  (_TM_).tm_isdst = 0;                                  \
  mktime(&(_TM_)) + (_SRT_)->Milliseconds / 1000.0;	\
})


/* server channels */

#define UNPROCESSED_DATA 0
#define PROCESSED_DATA 2

/* Structures used in SeaScan0.bin binary configuration files;
   determined by comparing with Windows Registry information.  */

typedef struct SSB_radarsite_struct 
{
  int SyncReg;
  int ConfigReg;
  int FTCReg;
  int VideoReg;

  int AcquireModes;
  int PRFs;
  int PulseLengths;
  int ACPs;

  int Gain;
  int Offset;
  int EnableGyro;
  int HeadingAdjust;
  int VideoDelay;

} t_SSB_radarsite;

typedef struct SSB_gps_struct 
{
  int LongDeg;
  int LongMin;
  int LongSec;
  int LongHundredthsSec;
  int LongHemisphere;
  int LatDeg;
  int LatMin;
  int LatSec;
  int LatHundredthsSec;
  int LatHemisphere;
  int GPSBaud;
  int GPSPort;
  int GPSSentence;

} t_SSB_gps; 

typedef struct SSB_plen_struct
{ 
  int nsPulseLength;
  int minPRFIndex; // origin 1
  int maxPRFIndex; // origin 1
  int modeIndex; // origin 1 (not "default" but actual! XSigSetDesiredMode does not appear to do anything)
} t_SSB_plen;


typedef struct SSB_prf_struct
{  // struct PRF

  /* The empirical PRF is represented as the inter-pulse interval,
     measured in microseconds. Seascan finds the first PRF struct for which
     the empirical PRF is between LowerLimit and UpperLimit, and
     then considers that the PRF mode, and uses DefaultPLEN as the assumed
     pulse length.
  */

  int LowerLimit;		/* inter-pulse interval in microseconds */
  int UpperLimit;		/* inter-pulse interval in microseconds */
  int Decimation;
  int MaxPulseBytes;
  int SegmentSize;
  int QuadrantSize;
  int DefaultPLEN;
  int WindowLength;
} t_SSB_prf;

typedef struct SSB_mode_struct
{ 
  int PixelConfig;
  int RangeCollapse; // (*2)
  int SamplesPerPulse;
  int Decimation;  // (*2) might need to be swapped with other (*2)
} t_SSB_mode;

typedef struct SSB_gyro_struct
{
  int GyroBaud;
  int GyroPort;
  int GyroPresent;
} t_SSB_gyro;

#define NUM_SAMPLING_FREQ 8

// PixelConfig constants that provide different sampling rates

double sampling_freq[NUM_SAMPLING_FREQ] = { 2.5,  5.0,  7.5, 10.0, 20.0, 30.0, 40.0, 60.0};
int    pixel_config [NUM_SAMPLING_FREQ] = {0x08, 0x09, 0x07, 0x02, 0x04, 0x0a, 0x01, 0x0b};


