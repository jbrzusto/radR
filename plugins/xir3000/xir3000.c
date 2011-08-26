/*  svn $Id: xir3000.c 647 2010-10-05 02:12:32Z john $

    radR : an R-based platform for acquisition and analysis of radar data
    Copyright (C) 2006-2010 John Brzustowski        

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


    Read data from Russell Technologies Inc's XIR3000 USB video processor.
    Unlike the deprecated xenex plugin, this one does not support reading from
    an IntegRadar server, nor from the older PCI(?) Radar Interface Board.
*/

#include "xir3000.h"

// the one and only USB XIR3000 device; this could be made an array of devices
// but I don't think the CANRib.dll library supports multiple USB-attached XIR3000s.

static t_xir3000 me;

// It appears to be necessary to ask for digitized pulses (i.e. scanlines)
// in batches, rather than copying them for the whole sweep at once.
// We use variables for the parameters controlling this approach to let us tune them dynamically.
// The defaults seem to work over a wide range of desired pulses per sweep.

static int	pulses_to_fetch_per_batch	     = PULSES_TO_FETCH_PER_BATCH;
static int	sleep_interval_between_pulse_batches = SLEEP_INTERVAL_BETWEEN_PULSE_BATCHES;

// Declare the thread we'll be using to get scan data.

THREAD_DECLARE(get_scan);
THREAD_DECLARE_DATA(get_scan);

/* methods for the xir3000 R module */

void xirerror(char *s) { 
  char errbuf[XIR3_ERR_BUFF_SIZE + 1], suberrbuf[XIR3_ERR_BUFF_SIZE + 1];

  CS(GetLastError, errbuf, XIR3_ERR_BUFF_SIZE);
  CS(GetLastSubError, suberrbuf, XIR3_ERR_BUFF_SIZE);

  error("xir3000: %s\n%s\n%s\n", s, errbuf, suberrbuf);
}


SEXP
have_xir3000 ()
{
  RETBOOL(CS(IsXIR3000Available));
}

void
init_device_info() {
  // initialize structure fields

  me.h = (HDEV) NULL;
  me.scan_info_vector = NULL;
  me.is_running = FALSE;
  me.pulses = XIR3_STANDARD_PULSES_PER_SWEEP; // will be changed when device is opened
  me.samples_per_pulse = XIR3_ACTUAL_SAMPLES_PER_PULSE;
}
    
SEXP
end_of_data() {
  // try to detect a shutdown of A/D conversion on this port
  // returning TRUE if the appropriate situation is detected,
  // and FALSE otherwise

  STANDBY_MODE standby;

  if (!me.is_running)
    RETBOOL(TRUE);

  CSE(GetStandByMode, me.h, &standby);
  RETBOOL((int) standby == STANDBY_ON);
}

SEXP
get_diag_text ()
{
  // return the diagnostic text
  // BUFFER OVERRUN DANGER - following RTI's RadarSample.exe, 
  // we assume the message always fits in 1024 bytes.

  char msg[1024];

  CSE(INT_VP_GetDiagText, me.h, msg);
  RETCHAR(msg);
}

void
set_null_return_value (void *vp) {
  /* 
     inform the caller of an error (due to shutdown, presumably) via 
     the return code.  Prevents rss.get.scan() from waiting indefinitely
     when this plugin is unloaded asynchronously.
  */
  t_get_scan_params *p = (t_get_scan_params *) vp;
  *p->rv = R_NilValue;
}

// because XIR3_NOMINAL_SAMPLES_PER_PULSE >  XIR3_ACTUAL_SAMPLES_PER_PULSE, and we only allocate an
// extmat large enough to hold  XIR3_ACTUAL_SAMPLES_PER_PULSE * NUM_PULSES, we need a separate
// buffer to serve as destination for CSAPI_GetScanLineRaw()

t_xir_sample full_scan_buff[XIR3_NOMINAL_SAMPLES_PER_PULSE];

void *
get_scan_thread (void *vp) {

  /* sleepable thread function that gets data from the xir3000 device

     DANGER!!! 

     this function MUST NOT CALL ANY R functions, as R is not
     re-entrant.  It is only safe to read/write R variables if pointers
     to protected SEXPs have been passed in vp.

  */

  t_get_scan_params *p = (t_get_scan_params *) vp;
  int i; // nominal pulse index
  int j;
  int pn;  // Extended pulse number
  int ns = *p->samples_per_pulse;
  t_sample *dst;
  HDEV h;
  FILETIME ts;
  DWORD ts_vp;

  // these variables mirror values set by the main thread; they are
  // updated through a semaphore before getting each scan, so that
  // they are constant while getting a single scan.

  int np;                   // desired number of pulses
  int pulses_per_batch;     // pulses to obtain per batch
  int batch_sleep_interval; // how long to sleep between batches

  SEXP ec; /* error code */

  THREAD_ADD_CLEANUP(& set_null_return_value, vp);
  THREAD_STARTING(get_scan);

  /* run forever */
  for (;;) {
    // Get a scan

    // copy digitizing parameters
    
    THREAD_LOCK_DATA(get_scan);
    *p->pulses = np = me.pulses;
    pulses_per_batch = pulses_to_fetch_per_batch;
    batch_sleep_interval = sleep_interval_between_pulse_batches;
    dbgprintf("xir3000: getter thread got np=%d, ppb=%d, sleep=%d\n", np, pulses_per_batch, batch_sleep_interval);
    THREAD_UNLOCK_DATA(get_scan);

    (*pensure_extmat) (p->mat, XIR3_MAX_SAMPLES, 1);

    dst = (t_sample *) (p->mat->ptr);
    ns = *p->samples_per_pulse;
    h = me.h;
    
    /* assume no error; what we return is the pre-allocated list of scan metadata */
    ec = p->si;

    CSQ(GetCurrentScanNumberExt, h, &pn);

    // to make sure we pick up a buffer of consecutive pulses
    // with pulse 0 the earliest, check whether the current
    // "scan number" is very close to wrapping around, and if
    // so, sleep for a while then check again, repeating until
    // the current scan has left the wraparound zone

    i = 0;
    while (pn > me.max_pulses - WRAPAROUND_ZONE_THRESHOLD && i++ < NUM_WRAPAROUND_SLEEP_ATTEMPTS) {
      dbgprintf("xir3000: sleeping in wraparound zone at pn=%d\n", pn);
      Sleep(SLEEP_INTERVAL_FOR_BUFFER_REFILL);
      CSQ(GetCurrentScanNumberExt, h, &pn);
    }

    // we're out of the wraparound zone, so grab the data by chasing the
    // "current scan" around the sweep.

    // get the current time and record the current scan number
    // for later use in scan start time/duration estimation

    CSQ(GetTimeOfSweep, h, &ts, &ts_vp);

    *p->timestamp = FILETIME_TO_UTC(ts);
    dbgprintf("xir3000: ts=%f, ts_vp=%d\n", *p->timestamp, ts_vp);

    for (i = 0; i < np; ++i) {
      if (i == EXT_PULSE_TO_NOM_PULSE(pn, np)) {
	// we've caught up with the current sweep location;
	// wait for a few pulses to come in
	dbgprintf("xir3000: caught up to sweep at pulse %d; np=%d\n", i, np);
	for (j = 0; j < NUM_STARTTIME_ESTIMATE_TRIES && i == EXT_PULSE_TO_NOM_PULSE(pn, np); ++j) {
	  Sleep(SLEEP_INTERVAL_FOR_BUFFER_REFILL);
	  CSQ(GetCurrentScanNumberExt, h, &pn);
	  dbgprintf("xir3000: current scan number is %d\n", pn);
	}
	if (j == NUM_STARTTIME_ESTIMATE_TRIES) {
	  dbgprintf("xir3000: waited for scan to catch up but it didn't\n");
	  goto quit;
	}
	if (i > EXT_PULSE_TO_NOM_PULSE(pn, np)) {
	  // the sweep has wrapped around past the
	  // starting point so we can now obtain the rest of
	  // the scans for this sweep.
	  pn = NOM_PULSE_TO_EXT_PULSE(np, np);
	}
      }

      CSQ(GetScanLineExt, h, NOM_PULSE_TO_EXT_PULSE(i, np), full_scan_buff, GSL_NONE);
      for (j = 0; j < XIR3_ACTUAL_SAMPLES_PER_PULSE; ++j)
	*dst++ = (t_sample) (unsigned short) full_scan_buff[j];  // this copies from 8-bit xir3000 to 16-bit radR samples

      if (i % pulses_per_batch == 0)
	Sleep(batch_sleep_interval);
      
    } // continue getting pulses for this sweep

    // estimate the scan duration by waiting until we have
    // a new sweep time (indicated by a calculated duration > 0

    for (j = 0; j < NUM_STARTTIME_ESTIMATE_TRIES; ++j) {
      CSQ(GetTimeOfSweep, h, & ts, & ts_vp);
      dbgprintf("xir3000: next ts=%f, ts_vp=%d\n", FILETIME_TO_UTC(ts), ts_vp);
      if ((*p->duration = 1000.0 * (FILETIME_TO_UTC(ts) - *p->timestamp)))
	break;
      Sleep(SLEEP_INTERVAL_FOR_BUFFER_REFILL);
    }
    if (j == NUM_STARTTIME_ESTIMATE_TRIES)
      goto quit;

    /* no error; setup up scan info*/
    p->mat->cols = ns;
    p->mat->rows = np;

    *p->range_per_sample = RANGE_CELL_SIZE(me.sample_rate, me.ant.bVidDiv);
    CS(GetPulseRepetitionRate, h, p->PRF);
    CS(GetPulseDuration, h, p->pulse_length);
    if (i < np) {
    quit:
      ec = R_NilValue;
    } else {
      me.have_scan_data = TRUE;
    }
    /* set the return value in the appropriate place 
       This is risky, but we've covered ourselves because the list slot originally had R_NilValue
       in it, so the list vector is appropriately aged.
    */

    *p->rv = ec;

    /* wait until we're awakened */

    dbgprintf("xir3000: about to sleep get_scan thread\n");
    THREAD_SLEEP(get_scan);
    dbgprintf("xir3000: get_scan thread awakened\n");
  }
  THREAD_DROP_CLEANUP(0);
  return NULL; /* never reached */
}

SEXP
get_scan_info(SEXP namesxp, SEXP scanmatsxp, SEXP trvsxp, SEXP trvindexsxp) {

  // get the header info for the next available scan.  We actually read the whole scan here.

  int trvi = INTEGER(AS_INTEGER(trvindexsxp))[0] - 1;
  SEXP rv;
  t_get_scan_params *sgparm;

  if (!me.is_running) 
    return R_NilValue;

  sgparm = & me.getter_params;

  if (!me.scan_info_vector) {
    /* create the scan.info vector which we'll re-use; set its names,
       and mark it as copy-on-change; also, preserve it against
       garbage collection */

    PROTECT(me.scan_info_vector = allocVector(VECSXP, LENGTH(namesxp)));
    SET_NAMES(me.scan_info_vector, namesxp);
    SET_NAMED(me.scan_info_vector, 2);
    R_PreserveObject(me.scan_info_vector);
    UNPROTECT(1);

    /* allocate the scan info items, and set up pointers to them in
       sgparams.  They are all protected by the preservation of the
       list itself */

#define MAKE_SI_ITEM(F, I, X) SET_VECTOR_ELT(me.scan_info_vector, I, X);	\
    sgparm->F = (typeof(sgparm->F)) DATAPTR(VECTOR_ELT(me.scan_info_vector, I))

    MAKE_SI_ITEM(pulses			,  0, ScalarInteger(0));                // filled in later
    MAKE_SI_ITEM(samples_per_pulse	,  1, ScalarInteger(me.samples_per_pulse));
    MAKE_SI_ITEM(bits_per_sample	,  2, ScalarInteger(XIR3_BPS)); 	// bits per sample (constant)
    MAKE_SI_ITEM(timestamp		,  3, ScalarReal(-1));			// filled in later
    MAKE_SI_ITEM(duration		,  4, ScalarInteger(-1));		// filled in later
    MAKE_SI_ITEM(range_per_sample	,  5, ScalarReal(-1));			// filled in later
    MAKE_SI_ITEM(start_range		,  6, ScalarReal(0));			// constant
    MAKE_SI_ITEM(angle_offset		,  7, ScalarReal(0));			// constant
    MAKE_SI_ITEM(orientation		,  8, ScalarInteger(+1));		// orientation of rotation (constant)
    MAKE_SI_ITEM(pulse_length		,  9, ScalarInteger(0));		// filled in later
    MAKE_SI_ITEM(PRF			, 10, ScalarInteger(0));		// filled in later
    
    sgparm->si = me.scan_info_vector;
  }

  /* fill in parameters for the reader thread; some of these may
     change from time to time (e.g. if switching to a new server), so
     we always do this */
    
  sgparm->mat = SEXP_TO_EXTMAT(scanmatsxp);
  sgparm->rv = & (VECTOR_ELT(trvsxp, trvi));
  me.have_scan_data = FALSE;

  /* create the reader thread, if necessary */

  if (! THREAD_IS_CREATED(get_scan))
    THREAD_CREATE(get_scan, get_scan_thread, sgparm);

  /* mark that we're spawning a reader thread */
  PROTECT(rv = ScalarInteger(NA_INTEGER));
  SET_VECTOR_ELT(trvsxp, trvi, rv);

  /* start the data getter */
  dbgprintf("xir3000: waking the reader thread\n");
  THREAD_WAKE(get_scan);

  UNPROTECT(1);
  return rv;
}

SEXP
get_scan_data(SEXP scanmat) {
  // get the data from the most recently processed scan.  get_scan_info has
  // probably already done the work, so we just check whether data are available,
  // and negate them if necessary.
  //
  // scanmat: the extmat with scan data, wrapped as an EXTPTRSXP
  //
  // Returns:  R_NilValue if the device is invalid or if there are no data available.
  // Otherwise, returns scanmat.

  if (! me.have_scan_data)
    return R_NilValue;

  // The following is apparently not needed - calls to CSAPI_GetScanLineExt
  // are already returning negated video when bNegVideo is non-zero

  /* if (me.ant.bNegVideo) { */
  /*   t_extmat *m = SEXP_TO_EXTMAT(scanmat); */
  /*   t_sample *p = (t_sample *) m->ptr; */
  /*   int i, n; */

  /*   n = m->rows * m->cols; */
  /*   for (i = 0; i < n; ++i) */
  /*     p[i] = XIR3_MAX_SAMPLE_VALUE - p[i]; */
  /* } */

  return scanmat;
}

static void
do_shut_down() { 
  if (me.getter_params.running) {
    me.getter_params.running = FALSE;
    THREAD_KILL(get_scan, FALSE);
  }
  if (me.h) {
    CS(SetStandByMode, me.h, STANDBY_ON);
    CS(SetMainMode, me.h, CSAPI_OFF);
    CS(CloseDevice, me.h);
    dbgprintf("do_shut_down: Success!\n");
    me.h = NULL;
    if (me.scan_info_vector) {
      R_ReleaseObject (me.scan_info_vector);
      me.scan_info_vector = NULL;
    }
  }
}

SEXP
  start_up (SEXP antenna, SEXP timeout, SEXP rate) {
  // start up the appropriate radar device
  // antenna: CHARACTER scalar giving antenna name; must match one in database
  // timeout: NUMERIC scalar timeout in milliseconds
  // rate:    NUMERIC scalar digitizing rate, in MHz

  LPCSTR antname = CHAR(STRING_ELT(antenna, 0));
  ULONG timeout_ = INTEGER(AS_INTEGER(timeout))[0];
  double sample_rate = REAL(AS_NUMERIC(rate))[0];

  UINT np, warmup_time;

  // mark that no getter thread is runnning
  me.getter_params.running = FALSE;
  dbgprintf("Trying to open usb with antenna '%s'\n", antname);
  if (sample_rate != 54.0 && sample_rate != 100.0 && sample_rate != 27.0)
    xirerror("Sample rate must be 27, 54 or 100 MHz");
  CS(SetSampleRate, ENCODE_SAMPLE_RATE(sample_rate));
  me.sample_rate = sample_rate;
  me.h = CS(OpenXIR3000, antname, timeout_);
  if (!me.h)
    xirerror("Unable to open USB XIR3000 device");
  CS(GetScanLinesPerSweepExt, me.h, &np);
  me.max_pulses = np;

  // we don't want CANRib.dll to plot
  CS(DisablePixelCallback, me.h);

  // set main mode on
  CS(SetMainMode, me.h, CSAPI_ON);

  // wait for appropriate warmup
  // This can be zero if the XIR3000 does not
  // actually control the radar.

  CS(GetWarmUpTime, me.h, &warmup_time);
  Sleep(1000 * warmup_time);

  // turn off standby mode
  CS(SetStandByMode, me.h, STANDBY_OFF);

  CS(SetSampleDepthEnabled, me.h, TRUE);
  CS(SetSampleDepth, me.h, SAMPLE_DEPTH_8_BITS);
  CS(SetScanLineMode, me.h, EXTENDED_SL);  // is this needed?  Does it simply enable calls to the *Ext functions?

  // We would like to obtain raw data.
  // However, GetScanLineExt, which allows us to access more than 1024 scanlines per sweep, appears to
  // provided cooked data.  So we turn off cooking steps one at a time.

  CS(SetInterferenceRejection, me.h, 0);
  CS(SetEchoExpansion,	       me.h, 0);
  CS(SetControlLevel,	       me.h, RIB_CONTROL_LEVEL_SEA, 0);
  CS(SetControlLevel,	       me.h, RIB_CONTROL_LEVEL_RAIN, 0);
  CS(SetPresetLevel,           me.h, RIB_PRESET_LEVEL_VIDEO_THRES, 0);
  CS(SetPresetLevel,           me.h, RIB_PRESET_LEVEL_VIDEO_REF, 255);

  me.is_running = TRUE;
  RETBOOL(TRUE);
}

SEXP
shut_down() {
  // stop the radar device

  do_shut_down(&me);
  me.is_running = FALSE;
  RETBOOL(TRUE);
}

SEXP
set_plen(SEXP ptsxp) {
  // set the pulse length for a radar device
  // ptsxp: INTEGER from 0 to 3

  int pt = INTEGER(AS_INTEGER(ptsxp))[0];
  CSE(SetPulseType, me.h, (PULSE_LENGTH_TYPE) pt);
  RETBOOL(TRUE);
}

SEXP
get_curr_params ()
{
  // return the contents of the (internal antenna parameters)
  // which are tunable at R level
  // These are:
  //  - trigger offset (16 bits)
  //  - video clock division (8 bits)
  //  - video gain (8 bits)

  SEXP rv;
  int *p;

  rv = allocVector(INTSXP, 3);
  p = INTEGER(rv);
  *p++ = (me.ant.bTrgOffH << 8) | me.ant.bTrgOffL;
  *p++ = me.ant.bVidDiv;
  *p++ = (me.ant.bVidXG << 4)   |  me.ant.bVidG;
  return rv;
}

SEXP
set_curr_params (SEXP vals)
{
  // set our cached copies of the digitizing parameters
  // but don't transmit to the XIR3000 or save to the database
  // These are:
  //  - trigger offset (16 bits)
  //  - video clock division (8 bits)
  //  - video gain (8 bits)

  int *p;

  if (LENGTH(vals) != 3)
    xirerror("set_curr_params: need a vector of length 3");
  p = INTEGER(AS_INTEGER(vals));
  me.ant.bTrgOffH	= *p >> 8;
  me.ant.bTrgOffL	= (*p++) & 0xff;
  me.ant.bVidDiv	= *p++;
  me.ant.bVidXG	        = *p >> 4;
  me.ant.bVidG		= (*p++) & 0x0f;
  RETBOOL(TRUE);
}

SEXP
transmit_curr_params() {
  CSE(INT_VP_UpdateSettings, me.h, &me.ant);
  RETBOOL(TRUE);
}

SEXP
load_db_params ()
{
  // load the digitizing parameters for the current antenna
  // from the CANStar.ant data base into our structure
  // Does not transmit the parameters to the XIR3000

  char buff[XIR3_MAX_ANTNAME_SIZE + 1];

  CSE(GetCurrentAntennaName, me.h, (LPSTR) buff, XIR3_MAX_ANTNAME_SIZE);
  CS(INT_VP_ConfigFileRead, me.h, &me.ant, buff);
  printf("\nVideo ref=%d, thresh=%d\n", me.ant.bVidRef, me.ant.bVidThresh);

  // FIXME: is the following redundant given our use of SetPresetLevel in start_up()?
  me.ant.bVidRef = 255;
  me.ant.bVidThresh=0;

  RETBOOL(TRUE);
}

SEXP
save_db_params ()
{
  // save our cached copy of digitizing parameters 
  // to the CANStar.ant database under the current antenna's name
  // Does not transmit the parameters to the XIR3000
  
  char buff[XIR3_MAX_ANTNAME_SIZE + 1];

  CSE(GetCurrentAntennaName, me.h, (LPSTR) buff, XIR3_MAX_ANTNAME_SIZE);
  CS(INT_VP_ConfigFileWrite, me.h, &me.ant, buff);
  RETBOOL(TRUE);
}

SEXP
usb_send_command (SEXP codeindval)
{
  // send a command to the USBVP
  // DANGER: no error checking is performed here; we assume the caller
  // knows the USB command index and appropriate range of values

  // codeindval is a 3 element integer vector:
  //   [1]: vendor code; presumably 0xD2
  //   [2]: command index; 
  //   [3]: value for output

  int *intparams;

  if (LENGTH(codeindval) != 3)
    RETBOOL(FALSE);
  intparams = INTEGER(AS_INTEGER(codeindval));
  CSE(INT_VP_SendCommand, me.h, (BYTE) intparams[0], (WORD) intparams[1], (BYTE) intparams[2]);
  RETBOOL(TRUE);
}

static int num_ant = 0;

BOOL PASCAL count_antenna_callback(ANTENNA *ant, void *p, int *i) {
  ++num_ant;
  return TRUE;
}

BOOL PASCAL enum_antenna_callback(ANTENNA *ant, void *names, int *i) {
  SET_STRING_ELT((SEXP) names, num_ant++, mkChar(ant->Description.Name));
  return TRUE;
}

SEXP
get_all_antennas ()
{
  // enumerate the antennas available in the database
  // returns a character vector of antenna names; these
  // can be used in the call to CSAPI_OpenXIR3000
  SEXP rv;
  int junk;  // needed for the antenna enumerator

  num_ant = 0;
  CSE(EnumAntennas, &count_antenna_callback, NULL, &junk);

  PROTECT(rv = allocVector(STRSXP, num_ant));

  num_ant = 0;
  CSE(EnumAntennas, &enum_antenna_callback, rv, &junk);
  UNPROTECT(1);
  return rv;
}

SEXP
get_sweep_time ()
{
  // return the timestamp for the start of the current sweep

  FILETIME ft;
  DWORD vpt = 0;

  CSE(GetTimeOfSweep, me.h, &ft, &vpt);
  printf("GetTimeOfSweep returned %ld in vpt.\n", vpt);
  RETDBL(FILETIME_TO_UTC(ft));
}

SEXP
set_batch_params (SEXP bat) 
{
  // set the scans per batch and time (in ms) to sleep between batches

  bat = AS_INTEGER(bat);
  THREAD_LOCK_DATA(get_scan);
  pulses_to_fetch_per_batch = INTEGER(bat)[0];
  sleep_interval_between_pulse_batches = INTEGER(bat)[1];
  THREAD_UNLOCK_DATA(get_scan);
  RETBOOL(TRUE);
}

SEXP
get_sample_rate ()
{
  // return the sampling rate
  // rate: should be 27, 54, or 100 (representing MHz)
  // defaults to 100 if rate is not valid
  SAMPLE_RATE sr;
  CS(GetSampleRate, me.h, &sr);
  RETDBL(DECODE_SAMPLE_RATE(sr));
}

SEXP
set_sample_rate (SEXP rate)
{
  // set the sampling rate
  // rate: should be 27, 54, or 100 (representing MHz)
  // defaults to 100 if rate is not valid

  SAMPLE_RATE sr = ENCODE_SAMPLE_RATE(REAL(AS_NUMERIC(rate))[0]);
  CS(SetSampleRate, sr);
  RETBOOL(TRUE);
}

SEXP
set_pulses (SEXP pulses)
{
  // set the number of pulses to obtain per sweep no error checking
  // since this might be done before we have an open device, which is
  // where we get the upper limit from.

  int p = INTEGER(AS_INTEGER(pulses))[0];

  THREAD_LOCK_DATA(get_scan);
  me.pulses = p;
  THREAD_UNLOCK_DATA(get_scan);
    
  RETBOOL(TRUE);
}

SEXP
get_pulses_per_sweep ()
{
  UINT np;
  CSE(GetScanLinesPerSweepExt, me.h, &np);
  RETINT(np);
}

SEXP
have_trigger()
{
  BOOL trigger_status;
  CS(GetTriggerStatus, me.h, &trigger_status);
  RETBOOL(trigger_status);
}
SEXP
get_all_antenna_parms ()
{
  // return all of the internal antenna parameters
  SEXP rv;
  int *p;

  rv = allocVector(INTSXP, 23);
  p = INTEGER(rv);
  *p++ = me.ant.bTrgH;
  *p++ = me.ant.bTrgL;
  *p++ = me.ant.bTrgG;
  *p++ = me.ant.bTrgE;
  *p++ = me.ant.bBPH;
  *p++ = me.ant.bBPL;
  *p++ = me.ant.bBPG;
  *p++ = me.ant.bBPE;
  *p++ = me.ant.bSHMH;
  *p++ = me.ant.bSHML;
  *p++ = me.ant.bSHMG;
  *p++ = me.ant.bSHME;
  *p++ = me.ant.bTrgOffH;
  *p++ = me.ant.bTrgOffL;
  *p++ = me.ant.bVidDiv;
  *p++ = me.ant.bVidG;
  *p++ = me.ant.bVidXG;
  *p++ = me.ant.bVidThresh;
  *p++ = me.ant.bVidRef;
  *p++ = me.ant.bTuneRange;
  *p++ = me.ant.bNegVideo;
  *p++ = me.ant.uBPPerSweep;
  *p++ = me.ant.bBirdAltitude;
  return rv;
}

SEXP
set_all_antenna_parms (SEXP vals)
{
  // set the contents of all the internal antenna parameters
  // Values are saved in the ant field of the t_xir3000
  // structure because they can't be read from the radar device,
  // and then sent to the device.

  int *p;

  p = INTEGER(AS_INTEGER(vals));
  me.ant.bTrgH		= *p++;
  me.ant.bTrgL		= *p++;
  me.ant.bTrgG		= *p++;
  me.ant.bTrgE		= *p++;
  me.ant.bBPH		= *p++;
  me.ant.bBPL		= *p++;
  me.ant.bBPG		= *p++;
  me.ant.bBPE		= *p++;
  me.ant.bSHMH		= *p++;
  me.ant.bSHML		= *p++;
  me.ant.bSHMG		= *p++;
  me.ant.bSHME		= *p++;
  me.ant.bTrgOffH	= *p++;
  me.ant.bTrgOffL	= *p++;
  me.ant.bVidDiv	= *p++;
  me.ant.bVidG		= *p++;
  me.ant.bVidXG	        = *p++;
  me.ant.bVidThresh	= *p++;
  me.ant.bVidRef	= *p++;
  me.ant.bTuneRange	= *p++;
  me.ant.bNegVideo	= *p++;
  me.ant.uBPPerSweep	= *p++;
  me.ant.bBirdAltitude  = *p++;
  RETBOOL(TRUE);
}

R_CallMethodDef xir3000_call_methods[]  = {
  // R hook functions
  MKREF(end_of_data, 0),
  MKREF(get_all_antennas, 0),
  MKREF(get_all_antenna_parms, 0),
  MKREF(get_curr_params, 0),
  MKREF(get_diag_text, 0),
  MKREF(get_pulses_per_sweep, 0),
  MKREF(get_sample_rate, 0),
  MKREF(get_scan_data, 1),
  MKREF(get_scan_info, 4),
  MKREF(have_trigger, 0),
  MKREF(set_sample_rate, 1),
  MKREF(get_sweep_time, 0),
  MKREF(have_xir3000, 0),
  MKREF(load_db_params, 0),
  MKREF(save_db_params, 0),
  MKREF(set_all_antenna_parms, 1),
  MKREF(set_curr_params, 1),
  MKREF(transmit_curr_params, 0),
  MKREF(set_plen, 1),
  MKREF(set_pulses, 1),
  MKREF(shut_down, 0),
  MKREF(start_up, 3),
  MKREF(usb_send_command, 1),
  MKREF(set_batch_params, 1),
  {NULL, NULL, 0}
};

void
R_init_xir3000(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines (info, NULL, xir3000_call_methods, NULL, NULL);
  R_useDynamicSymbols (info, FALSE);
  init_device_info ();
}

void
R_unload_xir3000(DllInfo *info)
{
  do_shut_down();
}
