/* include file for radR xenex interface

  by John Brzustowski 2008  john AT brz DOT ca

*/

#include <windows.h>

/* millisecond-resolution time structure and function */

#include <sys/timeb.h>

/* to prevent a warning, discard the unused windows definition of ERROR */
#undef ERROR 

#include "radRmodule.h"

// Notice that despite the documentation, only 496 valid samples are returned per pulse,
// at least in any operating mode I've been able to test.

#define NOMINAL_SAMPLES_PER_PULSE 512
#define ACTUAL_SAMPLES_PER_PULSE 506
#define ACTUAL_PULSES_PER_SCAN 1024

// number of bits in a xenex sample
// this is fixed; CSAPI_SetSampleDepth does not affect the DAC or returned scan values, apparently

#define XENEX_BPS 8        

#define XENEX_MAX_SAMPLES ((ACTUAL_SAMPLES_PER_PULSE) * 8192 * sizeof((XENEX_BPS+7) / 8))

typedef unsigned char t_xen_sample;

#define MAX_INTEGRADAR_SERVERS 16

#define MAX_COMPUTER_NAME_LENGTH 32
#define MAX_ANTNAME_SIZE 128
#define ERR_BUFF_SIZE 512

/* How long to sleep (in ms) so that the start time of the current
 time can be estimated by knowing the pulse number at the endpoints of
 the sleep interval:
*/

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
   than 60 / antenna_rpm * (WRAPAROUND_ZONE_THRESHOLD / ACTUAL_PULSES_PER_SCAN); i.e. so that
   a full set of sleeps is guaranteed to get us out of the wraparound zone.  Ideally, one
   sleep should do it!

 */

#define WRAPAROUND_ZONE_THRESHOLD (ACTUAL_PULSES_PER_SCAN / 100)
#define NUM_WRAPAROUND_SLEEP_ATTEMPTS 3

#undef PI // -Wall

// Xenex Client.dll definitions (decorated to function with gcc-mingw)

#include "myCANSTAR.h"

// fast macros for calling CANRib.dll functions

// call a function
#define CSDO(f, ...) CSAPI_##f(__VA_ARGS__)
// call a function with a SEXP-wrapped HDEV
#define CSDOH(f, hdev, ...) CSAPI_##f((HDEV) (EXTPTR_PTR(hdev)), ## __VA_ARGS__)
// call a function and bail with error if unsuccessful
#define CSTRY(f, ...) {if (! CSAPI_##f(__VA_ARGS__)) xenerror(#f);}
// call a function with a SEXP-wrapped HDEV and bail with error if unsuccessful
#define CSTRYH(f, hdev, ...) {if (! CSAPI_##f((HDEV) (EXTPTR_PTR(hdev)), ## __VA_ARGS__)) xenerror(#f);}

// support for multithreading the reader, as in the seascan plugin

#include <pthread.h>
#include <signal.h>
#include "threadutil.h"

struct t_xenex_struct;

typedef struct {
  struct t_xenex_struct *me;  // pointer to t_xenex device
  t_extmat *mat;         // pointer to destination buffer
  int running;         // whether getter thread is running
  int as_raw;          // whether to get unfiltered data
  int negate;	       // if non-zero, subtract data from this value before returning

  // when returning actual scan info (rather than an error), we re-use
  // the list allocated at R level by the main thread
  // pointers to preallocated SEXPs where we store the scan metadata
  // These MUST be preallocated because R is not thread safe, so
  // a spawned thread can't call any R library functions.
  int *pulses;
  int *samples_per_pulse;
  int *bits_per_sample;
  int *duration;
  double *start_range;
  double *angle_offset;
  int *orientation;
  double *timestamp;
  double *range_per_sample;
  UINT *pulse_length;
  UINT *PRF;

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

    
// structure that keeps track of an instance of a CANStar Virtual Radar Device

typedef struct t_xenex_struct {
  HDEV hxen;				/* handle of the CANStar Virtual Radar Device; NULL means invalid / not available */
  int devno;                    	/* device number; i.e. index into the "radars" array  */
                                        /* Note: devno = 0 means a USB Video Processor Board (xir3000) */
					/*       devno = 1 means a PCI(?) Radar Interface Board */
                                        /*       devno = 1 + n means the n'th server detected by CSAPI_FindServers */
  int is_running;		        /* has current source been started with start_up (and not shut down with shut_down)? */
  unsigned short pulses;		/* pulses per sweep */
  unsigned short samples_per_pulse;	/* samples per pulse */
  pthread_t getter_thread;	        /* holds information about any existing getter thread for this port  */
  t_get_scan_parms getter_parms;     /* holds the parms for any getter thread, which should be freed when the thread is killed */
  double sample_rate;			/* video sampling clock rate in MHz */
  int vid_div;		                /* video sampling clock divider; i.e. an integer >= 1 */
  char antname[MAX_ANTNAME_SIZE + 1];	/* name of antenna */
  VP_ANTENNA_DEFINITION ant;		/* current antenna parms; read from data base at start.up; optionally re-written at shut.down;
					   any changes to settings are recorded here */
  int have_scan_data;		        /* there is scan data available */
  SEXP scan_info_vector;                /* a (re-usable) vector for returning scan.info lists */
} t_xenex;

#define XENEX_DEVNO_USBVP 0
#define XENEX_DEVNO_RIB 1
#define XENEX_DEVNO_FIRST_SERVER 2

#define ENCODE_SAMPLE_RATE(x) ((x == 27) ? S_R_27MHZ : (x == 54) ? S_R_54MHZ : S_R_100MHZ)


// MS FILETIMEs are 64 bit counts of 100 nanosecond intervals since Jan 1, 1601
// How many such intervals separate the UTC time origin from the MS FILETIME origin?
// according to R: 
//
//  > -as.numeric(ISOdatetime(1601, 1, 1, 0, 0, "UTC")) * 1e7
//  [1] 1.16444556e+17

#define UTC_FILETIME_ORIGIN_DIFF 1.16444556E17

// macros to interconvert these times (ignoring leap seconds)

#define FILETIME_TO_UTC(FT)  ((((double)(* (long long *) &FT)) - UTC_FILETIME_ORIGIN_DIFF) / 1E7)
#define UTC_TO_FILETIME(X, FT) ({* (long long *) &FT = (long long) (((X) * 1e7) + UTC_FILETIME_ORIGIN_DIFF);})

#define SCANS_TO_FETCH_PER_BATCH 32
#define SLEEP_INTERVAL_BETWEEN_SCAN_BATCHES 30
