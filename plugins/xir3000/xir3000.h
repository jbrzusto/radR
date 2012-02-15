/*  svn $Id: xir3000.h 669 2010-10-08 18:41:31Z john $

    This file is part of radR.
    Copyright (C) 2006-2010 John Brzustowski

*/

#include <windows.h>

/* millisecond-resolution time structure and function */

#include <sys/timeb.h>

/* to prevent a warning, discard the unused windows definition of ERROR */
#undef ERROR 

#include "radRmodule.h"

/*
  Macros beginning with XIR3_ describe the CANRib.dll interface or XIR3000
  hardware parameters.  Other macros define xir3000 plugin behaviour.

  Notice that despite the documentation, only 506 valid samples are returned per
  pulse, at least in any operating mode I've been able to test.  
*/

#define XIR3_NOMINAL_SAMPLES_PER_PULSE  512
#define XIR3_ACTUAL_SAMPLES_PER_PULSE   506
#define XIR3_STANDARD_PULSES_PER_SWEEP 1024
#define XIR3_EXTENDED_PULSES_PER_SWEEP 4096

// number of bits in a xir3000 sample. This is fixed;
// CSAPI_SetSampleDepth does not affect the DAC or raw scanline values.

#define XIR3_BPS 8        
#define XIR3_MAX_SAMPLE_VALUE ((1 << XIR3_BPS) - 1)

typedef unsigned char t_xir_sample;

#define XIR3_MAX_SAMPLES ((XIR3_ACTUAL_SAMPLES_PER_PULSE) * XIR3_EXTENDED_PULSES_PER_SWEEP * sizeof((XIR3_BPS+7) / 8))

#define XIR3_MAX_ANTNAME_SIZE 128
#define XIR3_ERR_BUFF_SIZE 512

/* How long to sleep (in ms) so that the start time of the current
 time can be estimated by knowing the pulse number at the endpoints of
 the sleep interval: */

#define SLEEP_INTERVAL_FOR_STARTTIME_ESTIMATE 300

/* How many times to try getting a start time estimate */
#define NUM_STARTTIME_ESTIMATE_TRIES 5

/* How long to wait between trying to get start time estimates (in ms) */
#define STARTTIME_ESTIMATE_RETRY_WAIT 200

/* How long to sleep (in ms) when get_scan has caught up with the current pulse, to
   allow further pulses to be acquired by the Virtual radar */

#define SLEEP_INTERVAL_FOR_BUFFER_REFILL 50

/* How many (gated) pulses are a sufficient guarantee that we won't get an 
   inconsistent scan; i.e. one whose pulse in position zero is not the earliest pulse.
   We err on the side of caution here, and use 1% of the sweep total.  When we try to 
   get scan data, we wait until the current scan is not within WRAPAROUND_ZONE_THRESHOLD
   pulses of wrapping around.  That way, we'll be reasonably sure that the XIR3000 doesn't
   catch up with and pass our data copying, causing us to get new pulses where we thought
   we were getting those from the just-completed scan.  The number of sleep intervals for
   which we wait is NUM_WRAPAROUND_SLEEP_ATTEMPTS.  This should be large enough that
   SLEEP_INTERVAL_FOR_BUFFER_REFILL * NUM_WRAPAROUND_SLEEP_ATTEMPTS is considerably larger
   than 60 / antenna_rpm * (WRAPAROUND_ZONE_THRESHOLD / XIR3_EXTENDED_PULSES_PER_SWEEP); i.e. so that
   a full set of sleeps is guaranteed to get us out of the wraparound zone.  Ideally, one
   sleep should do it!

 */

#define WRAPAROUND_ZONE_THRESHOLD (XIR3_EXTENDED_PULSES_PER_SWEEP / 100)
#define NUM_WRAPAROUND_SLEEP_ATTEMPTS 3

#undef PI // -Wall

// CANRib.dll definitions (decorated to function with gcc-mingw)
#include "myCANSTAR.h"

// support for multithreading the reader, as in the seascan plugin
#include <pthread.h>
#include <signal.h>

#ifdef RADR_DEBUG
#define THREADUTIL_DEBUG
#endif

#include "threadutil.h"

// handle an error from calling a CANRib.dll function
extern void xirerror(char *s);

/*
  Table of macros for calling CANRib.dll functions.
  First arg is always bare name of function (i.e. without "CSAPI_")
  2nd arg is an error message for "CSM"; otherwise, it is first arg to CSAPI_ function

    Name      On error do     
  
    CS          <ignore>      
    CSE         xirerror(F)   
    CSM         xirerror(MSG) 
    CSQ         goto quit     

 where
    (F)   = the CSAPI function name (without "CSAPI_")
    (MSG) = the 2nd arg, a quoted string error message
*/

#ifndef RADR_DEBUG
#define CS(f, ...)                    CSAPI_##f(__VA_ARGS__)
#define CSE(f, ...)             if (! CSAPI_##f(__VA_ARGS__)) xirerror(#f)
#define CSM(f, MSG, ...)        if (! CSAPI_##f(__VA_ARGS__)) xirerror(MSG)
#define CSQ(f, ...)             if (! CSAPI_##f(__VA_ARGS__)) goto quit
#else
// debug versions
#define CS(f, ...)                   ({printf("xir3000: calling " #f "\n"); CSAPI_##f(__VA_ARGS__);})
#define CSE(f, ...)             printf("xir3000: calling " #f "\n");if (! CSAPI_##f(__VA_ARGS__)) xirerror(#f)
#define CSM(f, MSG, ...)        printf("xir3000: calling " #f "\n");if (! CSAPI_##f(__VA_ARGS__)) xirerror(MSG)
#define CSQ(f, ...)             printf("xir3000: calling " #f "\n");if (! CSAPI_##f(__VA_ARGS__)) goto quit
#endif
struct xir3000_;  // forward declaration

// structure passed to the getter thread

typedef struct get_scan_params_ {
  t_extmat		*mat;		// pointer to destination buffer (an object from R package extmat)
  int			 running;	// whether getter thread is running (has been started up but not shut down)

  // Pointers to data slots of protected and preallocated R SEXPs where
  // we store the scan metadata.  These MUST be preallocated because R
  // is not thread safe, so a spawned thread can't call any R library
  // functions.

  int		*pulses;
  int		*samples_per_pulse;
  int		*bits_per_sample;
  int		*duration;
  double	*start_range;
  double	*angle_offset;
  int		*orientation;
  double	*timestamp;
  double	*range_per_sample;
  UINT		*pulse_length;
  UINT		*PRF;

  // To return scan info or R_NilValue as an error, we use a pointer
  // to a SEXP.  We don't have to worry about ageing the list vector
  // in whose slot we might place R_NilValue, because it has already
  // had that value stored there in the past, and so is appropriately
  // aged.

  SEXP	*rv;

  // when returning actual scan info (rather than an error), we re-use
  // the list allocated at R level by the main thread

  SEXP	si;
  
} t_get_scan_params;

// structure that keeps track of an instance of an XIR3000 device

typedef struct xir3000_ {
  HDEV			 h;					// handle of the XIR3000 (as a CANStar Virtual Radar Device; NULL means invalid)
  int			 is_running;				// has current source been started with start_up (and not shut down with shut_down)?
  char			 antname[XIR3_MAX_ANTNAME_SIZE + 1];	// name of antenna
  VP_ANTENNA_DEFINITION  ant;					// current antenna params; read from data base by start_up
  unsigned short         max_pulses;				// max possible pulses per sweep
  unsigned short	 pulses;				// desired pulses per sweep (aka pulses per scan)
  unsigned short	 samples_per_pulse;			// samples per pulse
  t_get_scan_params       getter_params;			// holds the params for any getter thread, which should be freed when the thread is killed
  double		 sample_rate;				// video sampling clock rate in MHz
								// optionally re-written at shut.down; any changes to settings are recorded here
  int			 have_scan_data;			// is there scan data available?  set by getter thread
  SEXP			 scan_info_vector;			// a (re-usable) vector for returning scan.info lists
} t_xir3000;


    

#define ENCODE_SAMPLE_RATE(x) ({ typeof(x) _x = x; (_x == 27) ? S_R_27MHZ : (_x == 54) ? S_R_54MHZ : S_R_100MHZ;})
#define DECODE_SAMPLE_RATE(x) ({ typeof(x) _x = x; (_x == S_R_27MHZ) ? 27.0 : (_x == S_R_54MHZ) ? 54.0 : (_x == S_R_100MHZ) ? 100.0 : -1.0;})

// MS FILETIMEs are 64 bit counts of 100 nanosecond intervals since Jan 1, 1601
// How many such intervals separate the UTC time origin from the MS FILETIME origin?
// according to R (version 2.10.1, *not* version 2.5.1): 
//
//  > -as.numeric(ISOdatetime(1601, 1, 1, 0, 0, 0, tz="UTC")) * 1e7
//  [1] 1.16444736e+17
//
// Note: this ignores leap seconds.

#define UTC_FILETIME_ORIGIN_DIFF  1.16444736e+17

// interconvert these times (ignoring leap seconds)

#define FILETIME_TO_UTC(FT)  ((((unsigned)((FT).dwLowDateTime) + (((unsigned long long) (FT).dwHighDateTime) << 32)) - UTC_FILETIME_ORIGIN_DIFF) / 1E7)
#define UTC_TO_FILETIME(X, FT) ({* (unsigned long long *) &FT = (unsigned long long) (((X) * 1e7) + UTC_FILETIME_ORIGIN_DIFF);})

// a sweep is obtained in batches of pulses, and between these batches
// the getter thread sleeps.  The following batch sizes and sleep intervals
// appear to work at desired pulses-per-sweep of 256 up to 4096.

#define PULSES_TO_FETCH_PER_BATCH 64
#define SLEEP_INTERVAL_BETWEEN_PULSE_BATCHES 30

// convert between nominal and extended pulse numbers
// (Nominal pulse numbers range from 0 to n-1, where n is the desired
// number of pulses per sweep.  Extended pulse numbers range from 0
// to m-1, where m is the maximum possible number of pulses per sweep.)

#define NOM_PULSE_TO_EXT_PULSE(X, NP) ((X * me.max_pulses + NP/2) / NP)
#define EXT_PULSE_TO_NOM_PULSE(X, NP) ((X * NP + me.max_pulses / 2) / me.max_pulses)

// range cell size based on sampling rate (in MHz) and video divider (origin 0)
#define RANGE_CELL_SIZE(SAMPLE_RATE, VID_DIV) (VELOCITY_OF_LIGHT / (SAMPLE_RATE * 1.0e6 / (1 + VID_DIV)) / 2)

