/*  svn $Id: xenex.c 624 2010-07-14 02:39:24Z john $

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


    module for interacting with Russel Technologies Inc.'s IntegRadar
    server and USB Video Processer board through their CANRib.dll
    shared library.

*/

#include "xenex.h"

static char server_names[MAX_INTEGRADAR_SERVERS][MAX_COMPUTER_NAME_LENGTH];	/* server names */
static int num_servers = 0;					/* number of servers; set by get_hosts */
static t_xenex *radars[2 + MAX_INTEGRADAR_SERVERS]; 			/* pointers may be NULL */

static int sleep_interval_between_scan_batches = SLEEP_INTERVAL_BETWEEN_SCAN_BATCHES;
static int scans_to_fetch_per_batch = SCANS_TO_FETCH_PER_BATCH;

/* methods for the xenex R module */

void xenerror(char *s) { 
  char errbuf[ERR_BUFF_SIZE + 1], suberrbuf[ERR_BUFF_SIZE + 1];

  CSAPI_GetLastError (errbuf, ERR_BUFF_SIZE);
  CSAPI_GetLastSubError (suberrbuf, ERR_BUFF_SIZE);

  error("xenex: %s\n%s\n%s\n", s, errbuf, suberrbuf);
}

SEXP
get_hosts (SEXP static_hosts, SEXP probe, SEXP timeout, SEXP byname)
{
  // return the list of IntegRadar servers on the network

  // Returns a character vector of IP addresses or server names (if byname is TRUE)
  // and sets num_servers. 

  // probe: logical: should the network be probed for hosts?
  // static_hosts: if not R_NilValue, a character vector of static host names,
  // which are assumed to be present and not checked for; these will appear
  // in the return value before any probed hosts.  A host might appear twice
  // in the list.

  // This function should only be called if have_winsock() has
  // returned TRUE

  ADDR_SERVER_STATE srv_info[MAX_INTEGRADAR_SERVERS];
  int i, j, nsh, nnh;
  int name = LOGICAL(AS_LOGICAL(byname))[0];

  
  SEXP rv;

  nsh = LENGTH(static_hosts); /* number of static hosts */
  nnh = 0;		      /* number of probed network hosts */

  if (LOGICAL(AS_LOGICAL(probe)))
    CSAPI_FindServers (srv_info, MAX_INTEGRADAR_SERVERS, &nnh, INTEGER(AS_INTEGER(timeout))[0], name);

  num_servers = nsh + nnh;

  PROTECT(rv = allocVector(STRSXP, num_servers));

  // FIXME: why does calling FindServers with byname=FALSE hang?
  // we always ask for byname=TRUE, for now

  //  name = TRUE;

  j = 0;
  for (i = 0; i < nsh; ++i, ++j) {
    strcpy(server_names[j], CHAR(STRING_ELT(static_hosts, i)));
    SET_STRING_ELT(rv, j, mkChar (server_names[j]));
  }
  
  for (i = 0; i < nnh; ++i, ++j) {
    if (name)
      strcpy(server_names[j], srv_info[i].HostName);
    else
      sprintf (server_names[j], "%u.%u.%u.%u", 
	       (UINT) ((srv_info[i].ulIPAddr >> 24) & 0xff),
	       (UINT) ((srv_info[i].ulIPAddr >> 16) & 0xff),
	       (UINT) ((srv_info[i].ulIPAddr >>  8) & 0xff),
	       (UINT) ((srv_info[i].ulIPAddr      ) & 0xff));

    SET_STRING_ELT(rv, j, mkChar (server_names[j]));
  }
  UNPROTECT(1);
  return (rv);
}

SEXP
have_radar_board ()
{
  RETBOOL(CSAPI_IsRadarBoardAvailable());
}

SEXP
have_winsock ()
{
  RETBOOL(CSAPI_IsWinSocketAvailable());
}

SEXP
have_xir3000 ()
{
  RETBOOL(CSAPI_IsXIR3000Available());
}

static t_xenex *
ensure_device(SEXP devsxp) {
  // make sure there is a structure allocated for device devno

  t_xenex *me;

  int devno = INTEGER(AS_INTEGER(devsxp))[0];
  if (devno < 0 || devno >= 2 + num_servers)
    xenerror("invalid device number");

  if (radars[devno])
    return radars[devno];
  
  me = Calloc(1, t_xenex);
  me->devno = devno;
  me->hxen = (HDEV) NULL;
  me->is_running = FALSE;
  me->pulses = ACTUAL_PULSES_PER_SCAN;
  me->samples_per_pulse = ACTUAL_SAMPLES_PER_PULSE;

  return (radars[devno] = me);
}
    
SEXP
end_of_data(SEXP devsxp) {
  // try to detect a shutdown of A/D conversion on this port
  // returning TRUE if the appropriate situation is detected,
  // and FALSE otherwise

  t_xenex *me;
  STANDBY_MODE standby;

  me = ensure_device(devsxp);
  if (!me->is_running)
    RETBOOL(TRUE);

  CSTRY(GetStandByMode, me->hxen, &standby);
  RETBOOL((int) standby == STANDBY_ON);
}


SEXP
set_sample_rate (SEXP devsxp, SEXP rate)
{
  // set the sampling rate
  // rate: should be an integer 54, 100, or 27
  t_xenex *me;
  SAMPLE_RATE sr = ENCODE_SAMPLE_RATE(REAL(AS_NUMERIC(rate))[0]);
  me = ensure_device(devsxp);
  CSAPI_SetSampleRate (sr);
  RETBOOL(TRUE);
}


SEXP
get_diag_text (SEXP devsxp)
{
  // return the diagnostic text
  // BUFFER OVERRUN DANGER!!
  t_xenex *me;
  char msg[2048];

  me = ensure_device(devsxp);
  CSTRY(INT_VP_GetDiagText, me->hxen, msg);
  RETCHAR(msg);
}


SEXP
set_range (SEXP devsxp, SEXP rng)
{
  // rng is an integer from 0 to 8
  // representing increasing total ranges; actual range depends on the
  // DSP program currently loaded into the xenex board.
  t_xenex *me;

  me = ensure_device(devsxp);
  CSTRY(SetRange, me->hxen, (UINT) (INTEGER(AS_INTEGER(rng))[0]));
  RETBOOL(TRUE);
}



THREAD_DECLARE(get_scan);

void
set_null_return_value (void *vp) {
  /* 
     inform the caller of an error (due to shutdown, presumably) via 
     the return code.  Prevents rss.get.scan() from waiting indefinitely
     when this plugin is unloaded asynchronously.
  */
  t_get_scan_parms *p = (t_get_scan_parms *) vp;
  *p->rv = R_NilValue;
}

// because NOMINAL_SAMPLES_PER_PULSE > ACTUAL_SAMPLES_PER_PULSE, and we only allocate an
// extmat large enough to hold ACTUAL_SAMPLES_PER_PULSE * NUM_PULSES, we need a separate
// buffer to serve as destination for CSAPI_GetScanLine(Raw)()

t_xen_sample full_scan_buff[NOMINAL_SAMPLES_PER_PULSE];

void *
get_scan_thread (void *vp) {

  /* sleepable thread function that gets data from the xenex device

     DANGER!!! 

     this function MUST NOT CALL ANY R functions, as R is not
     re-entrant It is only safe to read/write R variables if pointers
     to protected SEXPs have been passed in vp.

  */

  t_get_scan_parms *p = (t_get_scan_parms *) vp;
  int i, j, pn;
  int ns = *p->samples_per_pulse;
  int np = *p->pulses;
  t_sample *dst;
  t_xenex *me;
  HDEV hxen;
  FILETIME ts;
  DWORD ts_vp;
  double range_in_nm;

  SEXP ec; /* error code */

  typeof (CSAPI_GetScanLineRaw) *fun; /* pointer to the raw or cooked scan line get function */

  THREAD_ADD_CLEANUP(& set_null_return_value, vp);
  THREAD_STARTING(get_scan);

  /* run forever */
  for (;;) {
    // we're starting a get phase, so set up some pointers
    // set a pointer to the correct scan line getter
    fun = p->as_raw ? & CSAPI_GetScanLineRaw : & CSAPI_GetScanLine;
    dbgprintf("xenex: about to ensure extmat\n");
    (*pensure_extmat) (p->mat, XENEX_MAX_SAMPLES, 1);
    dbgprintf("xenex: get_scan thread ensured extmat\n");


    dst = (t_sample *) (p->mat->ptr);
    ns = *p->samples_per_pulse;
    np = *p->pulses;
    me = p->me;
    hxen = me->hxen;

    /* assume no error; what we return is the pre-allocated list of scan metadata */
    ec = p->si;

    if (!CSAPI_GetCurrentScanNumber (hxen, &pn)) {
      // there is a problem
      goto quit;
    }
    dbgprintf("xenex: pn=%d\n", pn);

    // to make sure we pick up a buffer of consecutive pulses
    // with pulse 0 the earliest, check whether the current
    // "scan number" is very close to wrapping around, and if
    // so, sleep for a while then check again, repeating until
    // the current scan has left the wraparound zone

    i = 0;
    while (pn > np - WRAPAROUND_ZONE_THRESHOLD && i++ < NUM_WRAPAROUND_SLEEP_ATTEMPTS) {
      Sleep(SLEEP_INTERVAL_FOR_BUFFER_REFILL);
      if (!CSAPI_GetCurrentScanNumber (hxen, &pn)) {
	// there is a problem
	goto quit;
      }
      dbgprintf("xenex: to avoid wrap zone, waited until pn=%d\n", pn);
    }

    // we're out of the wraparound zone, so grab the data by chasing the
    // "current scan" around the sweep.

    // get the current time and record the current scan number
    // for later use in scan start time/duration estimation

    if (!CSAPI_GetTimeOfSweep (hxen, &ts, &ts_vp))
      goto quit;

    *p->timestamp = FILETIME_TO_UTC(ts);
    dbgprintf("xenex: ts=%f, ts_vp=%d\n", *p->timestamp, ts_vp);

    for (i = 0; i < np; ++i) {
      if (i == pn) {
	// we've caught up with the current sweep location;
	// wait for a few pulses to come in
	dbgprintf("xenex: caught up to sweep at pulse %d\n", i);
	for (j = 0; j < NUM_STARTTIME_ESTIMATE_TRIES && i == pn; ++j) {

	  Sleep(SLEEP_INTERVAL_FOR_BUFFER_REFILL);
	  if (!CSAPI_GetCurrentScanNumber (hxen, &pn)) {
	    goto quit;
	  }
	  dbgprintf("xenex: waited until pn=%d\n", pn);
	}
	if (j == NUM_STARTTIME_ESTIMATE_TRIES) {
	  dbgprintf("xenex: waited for scan to catch up but it didn't\n");
	  goto quit;
	}
	if (i > pn) {
	  // the sweep has wrapped around past the
	  // starting point so we can now obtain the rest of
	  // the scans for this sweep.
	  pn = np;
	}
      }

      dbgprintf("(%d=%d)", i, CSAPI_GetTimeOfScan(hxen, i));
      if (!(*fun)(hxen, (UINT) i, (t_xen_sample *) full_scan_buff, GSL_NONE)) {
	// we were unable to retrieve data for a pulse that should be in the buffer
	// so die.
	dbgprintf("xenex: no data for pulse %d\n", i);
	goto quit;
      }
      for (j = 0; j < ACTUAL_SAMPLES_PER_PULSE; ++j)
	*dst++ = (t_sample) full_scan_buff[j];

      if (i % scans_to_fetch_per_batch == 0)
	Sleep(sleep_interval_between_scan_batches);
      
    } // continue getting pulses for this sweep

    // estimate the scan duration by waiting until we have
    // a new sweep time (indicated by a calculated duration > 0

    ts_vp = CSAPI_GetTimeOfScan (hxen, np - 1);
    dbgprintf("xenex: time of last scan is %d\n", ts_vp);

    for (j = 0; j < NUM_STARTTIME_ESTIMATE_TRIES; ++j) {
      if (!CSAPI_GetTimeOfSweep (hxen, & ts, & ts_vp))
	goto quit;
      dbgprintf("xenex: next ts=%f, ts_vp=%d\n", FILETIME_TO_UTC(ts), ts_vp);
      if ((*p->duration = 1000.0 * (FILETIME_TO_UTC(ts) - *p->timestamp)))
	break;
      Sleep(SLEEP_INTERVAL_FOR_BUFFER_REFILL);
    }
    if (j == NUM_STARTTIME_ESTIMATE_TRIES)
      goto quit;

    /* no error; setup up scan info*/
    p->mat->cols = ns;
    p->mat->rows = np;

    CSAPI_GetRangeInNM (hxen, &range_in_nm);
    *p->range_per_sample = range_in_nm * METER_PER_NM / ns;
    CSAPI_GetPulseRepetitionRate (hxen, p->PRF);
    CSAPI_GetPulseDuration (hxen, p->pulse_length);
    if (i < np) {
    quit:
      ec = R_NilValue;
    } else {
      *p->have_scan_data = TRUE;
    }
    /* set the return value in the appropriate place 
       This is risky, but we've covered ourselves because the list slot originally had R_NilValue
       in it, so the list vector is appropriately aged.
    */

    *p->rv = ec;

    /* wait until we're awakened */

    dbgprintf("xenex: about to sleep get_scan thread\n");
    THREAD_SLEEP(get_scan);
    dbgprintf("xenex: get_scan thread awakened\n");
  }
  THREAD_DROP_CLEANUP(0);
  return NULL; /* never reached */
}

SEXP
get_scan_info(SEXP dev, SEXP namesxp, SEXP scanmatsxp, SEXP raw, SEXP trvsxp, SEXP trvindexsxp) {

  // get the header info for the next available scan.  We actually read the whole scan here.

  int trvi = INTEGER(AS_INTEGER(trvindexsxp))[0] - 1;
  t_xenex *me;
  SEXP rv;
  t_get_scan_parms *sgparm;

  me = ensure_device(dev);
  if (!me) 
    return R_NilValue;

  sgparm = & me->getter_parms;

  if (!me->scan_info_vector) {
    /* create the scan.info vector which we'll re-use; set its names,
       and mark it as copy-on-change; also, preserve it against
       garbage collection */

    PROTECT(me->scan_info_vector = allocVector(VECSXP, LENGTH(namesxp)));
    SET_NAMES(me->scan_info_vector, namesxp);
    SET_NAMED(me->scan_info_vector, 2);
    R_PreserveObject(me->scan_info_vector);
    UNPROTECT(1);

    sgparm->me = me;
    
    /* allocate the scan info items, and set up pointers to them in
       sgparms.  They are all protected by the preservation of the
       list itself */

#define MAKE_SI_ITEM(F, I, X) SET_VECTOR_ELT(me->scan_info_vector, I, X);	\
    sgparm->F = (typeof(sgparm->F)) DATAPTR(VECTOR_ELT(me->scan_info_vector, I))

    MAKE_SI_ITEM(pulses, 0, ScalarInteger(me->pulses));
    MAKE_SI_ITEM(samples_per_pulse, 1, ScalarInteger(me->samples_per_pulse));
    MAKE_SI_ITEM(bits_per_sample, 2, ScalarInteger(XENEX_BPS)); // bits per sample (constant)
    MAKE_SI_ITEM(timestamp, 3, ScalarReal(-1));  //  filled in later
    MAKE_SI_ITEM(duration, 4, ScalarInteger(-1)); // filled in later
    MAKE_SI_ITEM(range_per_sample, 5, ScalarReal(-1));//  filled in later
    MAKE_SI_ITEM(start_range, 6, ScalarReal(0));//  constant
    MAKE_SI_ITEM(angle_offset, 7, ScalarReal(0));//  constant
    MAKE_SI_ITEM(orientation, 8, ScalarInteger(+1)); // orientation of rotation (constant)
    MAKE_SI_ITEM(pulse_length, 9, ScalarInteger(0));//  filled in later
    MAKE_SI_ITEM(PRF, 10, ScalarInteger(0));//  filled in later
    
    sgparm->si = me->scan_info_vector;

  }

  /* fill in parameters for the reader thread; some of these may
     change from time to time (e.g. if switching to a new server), so
     we always do this */
    
  sgparm->mat = SEXP_TO_EXTMAT(scanmatsxp);
  sgparm->rv = & (VECTOR_ELT(trvsxp, trvi));
  sgparm->have_scan_data = &me->have_scan_data;
  me->have_scan_data = FALSE;
  sgparm->as_raw = LOGICAL(AS_LOGICAL(raw))[0];

  /* create the reader thread, if necessary */

  if (! THREAD_IS_CREATED(get_scan))
    THREAD_CREATE(get_scan, get_scan_thread, sgparm);

  /* mark that we're spawning a reader thread */
  PROTECT(rv = ScalarInteger(NA_INTEGER));
  SET_VECTOR_ELT(trvsxp, trvi, rv);

  /* start the data getter */
  dbgprintf("xenex: waking the reader thread\n");
  THREAD_WAKE(get_scan);

  UNPROTECT(1);
  return rv;
}

SEXP
get_scan_data(SEXP dev, SEXP scanmat, SEXP neg) {
  // get the data from the most recently processed scan.  get_scan_info has
  // probably already done the work, so we just check whether data are available.
  //
  // dev: integer device number
  //
  // scanmat: the extmat with scan data, wrapped as an EXTPTRSXP
  //
  // neg: should the data be negated?  This is an INTSXP scalar,
  // and if non-zero, data are subtracted from it.  The only reason
  // not to do this at the R level is that the way the windows event
  // loop is embedded, the GUI would sometimes see the data before
  // negation, leading to annoying flashing.  e.g. a zoom command in
  // the plot window might trigger a callback between getting the data
  // and negating it, leading to a (pre-) negative image in the plot
  // window.
  //
  // Returns:  R_NilValue if the device is invalid or if there are no data available.
  // Otherwise, returns scanmat.

  t_xenex *me;
  int neg_ = INTEGER(neg)[0];
  t_extmat *m = SEXP_TO_EXTMAT(scanmat);
  t_sample *p = (t_sample *) m->ptr;
  int i, n;
  if (! ((me = ensure_device(dev)) && me->have_scan_data))
    return R_NilValue;

  if (neg_) {
    n = m->rows * m->cols;
    for (i = 0; i < n; ++i)
      p[i] = neg_ - p[i];
  }    
  return scanmat;
}

static void
do_shut_down(t_xenex *me) { 
  if (me->getter_parms.running) {
    me->getter_parms.running = FALSE;
    pthread_kill(me->getter_thread, SIGTERM);
  }
  if (me->hxen) {
    dbgprintf("do_shut_down: Calling SetStandByMode(STANDBY_ON)\n");
    CSAPI_SetStandByMode (me->hxen, STANDBY_ON);
    dbgprintf("do_shut_down: Calling SetMainMode(MAIN_MODE_OFF) with me->hxen=%d\n", (int) me->hxen);
    CSAPI_SetMainMode (me->hxen, MAIN_MODE_OFF);
    dbgprintf("do_shut_down: Calling CloseDevice(%d)\n", (int) me->hxen);
    // FIXME: this crashes for a USB device
    CSAPI_CloseDevice (me->hxen);
    dbgprintf("do_shut_down: Success!\n");
    me->hxen = NULL;
    if (me->scan_info_vector) {
      R_ReleaseObject (me->scan_info_vector);
      me->scan_info_vector = NULL;
    }
  }
}

SEXP
start_up (SEXP devsxp, SEXP parms) {
  // start up the appropriate radar device
  // devsxp: INTEGER; 0 = USB, 1 = RIB, 1 + n = n'th TCPIP server (as returned by get_hosts())
  // startsxp: DOUBLE; start timestamp (including fractional seconds)
  // parms: depends on devsp:
  //        USB => list(CHARACTER radar_antenna_name, INTEGER timeout in millseconds, REAL clock rate in MHz)
  //        RIB => list(CHARACTER radar_antenna_name, LOGICAL as_master, INTEGER timeout in millseconds, INTEGER prg type (0 or 1 for DSP 7 / DSP 9))
  //        TCPIP => list (2 element CHARACTER vector: c(username, password), INTEGER timeout in milliseconds)

  t_xenex *me;
  int warmup;
  HDEV h;

  me = ensure_device(devsxp);

  // mark that no getter thread is runnning
  me->getter_parms.running = FALSE;

  // open / connect the virtual radar device
  switch(me->devno) {
  case 0:       // USB VP
    {
      LPCSTR			antname	   = CHAR(STRING_ELT(VECTOR_ELT(parms, 0), 0));
      ULONG			timeout	   = INTEGER(AS_INTEGER(VECTOR_ELT(parms, 1)))[0];
      double			sample_rate = REAL(AS_NUMERIC(VECTOR_ELT(parms, 2)))[0];
      dbgprintf("Trying to open usb with antenna '%s'\n", antname);
      if (sample_rate != 54.0 && sample_rate != 100.0 && sample_rate != 27.0)
	xenerror("Sample rate must be 27, 54 or 100 MHz");
      if (!(h = CSAPI_OpenXIR3000(antname, timeout)))
	xenerror("Unable to open USB video processor");
      CSAPI_SetSampleRate (ENCODE_SAMPLE_RATE(sample_rate));
      me->sample_rate = sample_rate;
    }
    break;

  case 1:       // RIB
    {
      ANTENNA_RIB_MSCONF	ms	= (LOGICAL(AS_LOGICAL(VECTOR_ELT(parms, 1)))[0]) ? ANTENNA_RIB_MASTER : ANTENNA_RIB_SLAVE;
      ULONG			timeout = INTEGER(AS_INTEGER(VECTOR_ELT(parms, 2)))[0];
      RIB_PRG_TYPE		prgtype = INTEGER(AS_INTEGER(VECTOR_ELT(parms, 3)))[0] ? PRGTYPE_7 : PRGTYPE_9;
      if (!(h = CSAPI_OpenRadarBoard(prgtype, CHAR(STRING_ELT(VECTOR_ELT(parms, 0), 0)), ms, timeout)))
	xenerror("Unable to open Radar Interface Board");

    }
    break;

  default:      // INTEGRADAR SERVER
    {
    // because of ensure_device, we know that 2 <= me->devno <= num_servers + 1
    LOGINPARAM lp;
    lp.uSize = sizeof(struct tagLOGINPARAM);
    strncpy(lp.HostName, server_names[me->devno - 2], MAX_HOSTNAME_LEN);
    if (TYPEOF(VECTOR_ELT(parms, 0)) == STRSXP) {
      strncpy(lp.UserName, CHAR(STRING_ELT(VECTOR_ELT(parms, 0), 0)), MAX_LOGINNAME_LEN);
      strncpy(lp.PassWord, CHAR(STRING_ELT(VECTOR_ELT(parms, 0), 1)), MAX_LOGINNAME_LEN);
    } else {
      strcpy(lp.UserName, "");
      strcpy(lp.PassWord, "");
    }
    UINT timeout = INTEGER(AS_INTEGER(VECTOR_ELT(parms, 1)))[0];
    if (!(h = CSAPI_OpenSocket ((BOOL) 1, &lp, APP_SRC_MODE_DEFAULT, timeout, OSC_AUTO_SERIAL)))
      xenerror("Unable to connect to IntegRadar server");
    }
    break;
  }

  me->hxen = h;

  CSAPI_DisablePixelCallback (me->hxen);

  // just to be safe, try putting into standby mode
  // before and after turning on main mode

  CSAPI_SetMainMode (me->hxen, MAIN_MODE_ON);
  CSAPI_SetStandByMode (me->hxen, STANDBY_ON);

  // wait for a warmup period, if necessary
  if (CSAPI_GetWarmUpTime (me->hxen, &warmup)) {
    Sleep(1000 * warmup); // NOTE: GetWarmUpTime returns a time in seconds
  }
  CSAPI_SetStandByMode (me->hxen, STANDBY_OFF);

  // we want to be able to set sample depth
  CSAPI_SetSampleDepthEnabled (me->hxen, TRUE);

  CSAPI_SetSampleDepth (me->hxen, SAMPLE_DEPTH_8_BITS);
  me->is_running = TRUE;
  RETBOOL(TRUE);
}

SEXP
shut_down(SEXP devsxp) {
  // stop the radar device

  t_xenex *me;

  me = ensure_device(devsxp);
  do_shut_down(me);
  me->is_running = FALSE;
  RETBOOL(TRUE);
}

SEXP
set_plen(SEXP devsxp, SEXP ptsxp) {
  // set the pulse length for a radar device
  // ptsxp: INTEGER from 0 to 3

  int pt = INTEGER(AS_INTEGER(ptsxp))[0];

  t_xenex *me;

  me = ensure_device(devsxp);
  CSTRY(SetPulseType, me->hxen, (PULSE_LENGTH_TYPE) pt);
  RETBOOL(TRUE);
}

SEXP
get_antenna (SEXP devsxp) {
  char name[MAX_ANTNAME_SIZE + 1];
  t_xenex *me;

  me = ensure_device(devsxp);
  CSTRY(GetCurrentAntennaName, me->hxen, name, MAX_ANTNAME_SIZE);
  RETCHAR(name);
}

SEXP
get_prg_type (SEXP devsxp) {
  t_xenex *me;
  RIB_PRG_TYPE prg;

  // return the type of the DSP program on the device
  // returns 7, 9, or 0 (for unknown)

  me = ensure_device(devsxp);
  CSTRY(GetPrgType, me->hxen, &prg);
  RETINT(prg == PRGTYPE_7 ? 7 : prg == PRGTYPE_9 ? 9 : 0);
}

SEXP
get_antenna_parms (SEXP devsxp)
{
  // return the contents of the (internal antenna parameters)
  t_xenex *me;
  SEXP rv;
  int *p;

  me = ensure_device(devsxp);
  rv = allocVector(INTSXP, 23);
  p = INTEGER(rv);
  *p++ = me->ant.bTrgH;
  *p++ = me->ant.bTrgL;
  *p++ = me->ant.bTrgG;
  *p++ = me->ant.bTrgE;
  *p++ = me->ant.bBPH;
  *p++ = me->ant.bBPL;
  *p++ = me->ant.bBPG;
  *p++ = me->ant.bBPE;
  *p++ = me->ant.bSHMH;
  *p++ = me->ant.bSHML;
  *p++ = me->ant.bSHMG;
  *p++ = me->ant.bSHME;
  *p++ = me->ant.bTrgOffH;
  *p++ = me->ant.bTrgOffL;
  *p++ = me->ant.bVidDiv;
  *p++ = me->ant.bVidG;
  *p++ = me->ant.bVidXG;
  *p++ = me->ant.bVidThresh;
  *p++ = me->ant.bVidRef;
  *p++ = me->ant.bTuneRange;
  *p++ = me->ant.bNegVideo;
  *p++ = me->ant.uBPPerSweep;
  *p++ = me->ant.bBirdAltitude;
  return rv;
}

SEXP
set_antenna_parms (SEXP devsxp, SEXP vals)
{
  // set the contents of the (internal antenna parameters)
  // Values are saved in the ant field of the t_xenex
  // structure because they can't be read from the radar device,
  // and then sent to the device.

  t_xenex *me;
  int *p;

  me = ensure_device(devsxp);
  p = INTEGER(AS_INTEGER(vals));
  me->ant.bTrgH		= *p++;
  me->ant.bTrgL		= *p++;
  me->ant.bTrgG		= *p++;
  me->ant.bTrgE		= *p++;
  me->ant.bBPH		= *p++;
  me->ant.bBPL		= *p++;
  me->ant.bBPG		= *p++;
  me->ant.bBPE		= *p++;
  me->ant.bSHMH		= *p++;
  me->ant.bSHML		= *p++;
  me->ant.bSHMG		= *p++;
  me->ant.bSHME		= *p++;
  me->ant.bTrgOffH	= *p++;
  me->ant.bTrgOffL	= *p++;
  me->ant.bVidDiv	= *p++;
  me->ant.bVidG		= *p++;
  me->ant.bVidXG	= *p++;
  me->ant.bVidThresh	= *p++;
  me->ant.bVidRef	= *p++;
  me->ant.bTuneRange	= *p++;
  me->ant.bNegVideo	= *p++;
  me->ant.uBPPerSweep	= *p++;
  me->ant.bBirdAltitude = *p++;
  CSTRY(INT_VP_UpdateSettings, me->hxen, &me->ant);
  RETBOOL(TRUE);
}

SEXP
load_antenna_parms (SEXP devsxp)
{
  // load the current internal antenna parameters from
  // their xenex antenna configuration file
  
  t_xenex *me;
  char buff[MAX_ANTNAME_SIZE + 1];

  me = ensure_device(devsxp);
  CSTRY(GetCurrentAntennaName, me->hxen, (LPSTR) buff, MAX_ANTNAME_SIZE);
  CSAPI_INT_VP_ConfigFileRead(me->hxen, &me->ant, buff);
  RETBOOL(TRUE);
}

SEXP
save_antenna_parms (SEXP devsxp)
{
  // save the current internal antenna parameters back to
  // their xenex antenna configuration file
  
  t_xenex *me;
  char buff[MAX_ANTNAME_SIZE + 1];

  me = ensure_device(devsxp);
  CSTRY(GetCurrentAntennaName, me->hxen, (LPSTR) buff, MAX_ANTNAME_SIZE);
  CSAPI_INT_VP_ConfigFileWrite(me->hxen, &me->ant, buff);
  RETBOOL(TRUE);
}

SEXP
get_control_level (SEXP devsxp, SEXP which)
{
  UINT val;
  // return level of a control
  // which: 0 = GAIN, 1 = TUNE, 2 = SEA, 3 = RAIN
  t_xenex *me;

  me = ensure_device(devsxp);
  CSTRY(GetControlLevel, me->hxen, INTEGER(AS_INTEGER(which))[0], &val);
  RETINT(val);
}

SEXP
set_control_level (SEXP devsxp, SEXP which, SEXP val)
{
  // set the level of a control
  t_xenex *me;

  me = ensure_device(devsxp);
  CSTRY(SetControlLevel, me->hxen, INTEGER(AS_INTEGER(which))[0], INTEGER(AS_INTEGER(val))[0]);
  RETBOOL(TRUE);
}


SEXP
get_preset_level (SEXP devsxp, SEXP which)
{
  // return level of a preset
  // which: 4 = TUNE, 5 = VIDEO_THRESH, 6 = VIDEO_REF
  UINT val;
  t_xenex *me;

  me = ensure_device(devsxp);
  CSTRY(GetPresetLevel, me->hxen, INTEGER(AS_INTEGER(which))[0], &val);
  RETINT(val);
}

SEXP
set_preset_level (SEXP devsxp, SEXP which, SEXP val)
{
  // set the level of a preset
  t_xenex *me;

  me = ensure_device(devsxp);
  CSTRY(SetPresetLevel, me->hxen, INTEGER(AS_INTEGER(which))[0], INTEGER(AS_INTEGER(val))[0]);
  RETBOOL(TRUE);
}

SEXP
get_cable_length (SEXP devsxp)
{
  // return the cable length setting
  t_xenex *me;

  me = ensure_device(devsxp);
  UINT lng;

  CSTRY(GetCableLength, me->hxen, &lng);
  RETINT(lng);
}

SEXP
set_cable_length (SEXP devsxp, SEXP lng)
{
  // set the cable length
  t_xenex *me;

  me = ensure_device(devsxp);
  CSTRY(SetCableLength, me->hxen, INTEGER(AS_INTEGER(lng))[0]);
  RETBOOL(TRUE);
}

SEXP
usb_send_command (SEXP devsxp, SEXP codeindval)
{
  // send a command to the USBVP
  // DANGER: no error checking is performed here; we assume the caller
  // knows the USB command index and appropriate range of values

  // codeindval is a 3 element integer vector:
  //   [1]: vendor code; presumably 0xD2
  //   [2]: command index; 
  //   [3]: value for output

  t_xenex *me;
  int *intparms;

  me = ensure_device(devsxp);
  if (LENGTH(codeindval) != 3)
    RETBOOL(FALSE);
  intparms = INTEGER(AS_INTEGER(codeindval));
  CSTRY(INT_VP_SendCommand, me->hxen, (BYTE) intparms[0], (WORD) intparms[1], (BYTE) intparms[2]);
  RETBOOL(TRUE);
}

SEXP
get_scanline_address (SEXP dev, SEXP line, SEXP raw)
{
  t_xenex *me;
  BYTE *addr;
  me = ensure_device(dev);
  if (LOGICAL(AS_LOGICAL(raw))[0]) {
    CSTRY(GetScanLineRawAddress, me->hxen, (unsigned) INTEGER(AS_INTEGER(line))[0], &addr);
  } else {
    CSTRY(GetScanLineStdAddress, me->hxen, (unsigned) INTEGER(AS_INTEGER(line))[0], &addr);
  }
  return ScalarInteger((int) addr);
}

  
SEXP 
my_open_socket (SEXP loginparms)
{
  LOGINPARAM lp;
  lp.uSize = sizeof(struct tagLOGINPARAM);
  strncpy(lp.HostName, CHAR(STRING_ELT(loginparms, 0)), MAX_HOSTNAME_LEN);
  strncpy(lp.UserName, CHAR(STRING_ELT(loginparms, 1)), MAX_LOGINNAME_LEN);
  strncpy(lp.PassWord, CHAR(STRING_ELT(loginparms, 2)), MAX_LOGINNAME_LEN);

  HDEV h = CSAPI_OpenSocket ((BOOL) 1, 
			     &lp,
			     APP_SRC_MODE_USB,
			     (UINT) 5,
			     OSC_AUTO_SERIAL 
			    );
  if (!h)
    xenerror("OpenSocket");

  // if case the server is running a board with main mode enabled, turn
  // main mode on and wait for the required warmup time.

  if (CSAPI_SetMainMode (h, MAIN_MODE_ON)) {
    int warmup;
    if (!CSAPI_GetWarmUpTime (h, &warmup)) {
      CSAPI_SetMainMode (h, MAIN_MODE_OFF);
      xenerror("GetWarmUpTime");
    }
    Sleep(1000 * warmup); // NOTE: GetWarmUpTime returns a time in seconds
  }
  return PTR_TO_EXTPTR(h);
}
  
SEXP
my_close_device (SEXP hdev)
{
  RETBOOL(CSDOH(CloseDevice, hdev));
}
  
SEXP
my_get_server_count (SEXP hdev)
{
  UINT num;
  CSTRYH(GetServerCount, hdev, &num);
  RETBOOL(num);
}

SEXP
my_is_radar_board_available ()
{
  RETBOOL(CSAPI_IsRadarBoardAvailable());
}

SEXP
my_is_winsock_available ()
{
  RETBOOL(CSAPI_IsWinSocketAvailable());
}

SEXP
my_is_xir3000_available ()
{
  RETBOOL(CSAPI_IsXIR3000Available());
}

SEXP
my_open_xir3000 (SEXP namesxp, SEXP timeoutsxp)
{
  char *antname = CHAR(STRING_ELT(namesxp, 0));
  ULONG timeout = INTEGER(AS_INTEGER(timeoutsxp))[0];
  HDEV h;
  h = CSAPI_OpenXIR3000(antname, timeout);
  if (!h) {
    xenerror("OpenXir3000");
  }
  return PTR_TO_EXTPTR(h);
}

SEXP
my_get_antenna (SEXP hdev) {
  char name[MAX_ANTNAME_SIZE + 1];
  CSTRYH(GetCurrentAntennaName, hdev, name, MAX_ANTNAME_SIZE);
  RETCHAR(name);
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
  // can be used in the call to open_usbvp
  SEXP rv;
  int junk;  // needed for the antenna enumerator.  Weird.
  // count antennas
  num_ant = 0;
  CSTRY(EnumAntennas, &count_antenna_callback, NULL, &junk);
  // allocate storage
  PROTECT(rv = allocVector(STRSXP, num_ant));
  num_ant = 0;
  CSTRY(EnumAntennas, &enum_antenna_callback, rv, &junk);
  UNPROTECT(1);
  return rv;
}

SEXP
my_get_range (SEXP hdev)
{
  double range;
  // return the current range in m
  CSTRYH(GetRangeInNM, hdev, &range);
  RETDBL(range * METER_PER_NM);
}

SEXP
my_get_sample_depth (SEXP hdev)
{
  SAMPLE_DEPTH dep;
  // return the sample depth in bpp
  CSTRYH(GetSampleDepth, hdev, &dep);
  RETINT(dep);
}

SEXP
my_get_sample_depth_enabled (SEXP hdev)
{
  // return whether sample depth is enabled
  BOOL enab;
  CSTRYH(GetSampleDepthEnabled, hdev, &enab);
  RETBOOL(enab);
}

SEXP
my_get_samples_per_pulse (SEXP hdev)
{
  UINT ns;
  // return the # of samples per pulse
  CSTRYH(GetSamplesPerScanLine, hdev, &ns);

  ns = 496; // This is the true number of samples available, regardless of what the docs say.
  RETINT(ns);
}

SEXP
my_get_pulses_per_scan (SEXP hdev)
{
  // return the # of pulses per scan
  UINT np;
  CSTRYH(GetScanLinesPerSweep, hdev, &np);
  RETINT(np);
}

SEXP
my_set_range (SEXP hdev, SEXP rng)
{
  CSTRYH(SetRange, hdev, INTEGER(AS_INTEGER(rng))[0]);
  RETBOOL(TRUE);
}

SEXP
my_set_sample_depth (SEXP hdev, SEXP dep)
{
  if (!CSAPI_SetSampleDepth((HDEV) EXTPTR_PTR(hdev), (SAMPLE_DEPTH) (INTEGER(AS_INTEGER(dep))[0])))
    error("CSAPI_SetSampleDepth failed");
  RETBOOL(TRUE);
}

SEXP
my_set_sample_depth_enabled (SEXP hdev, SEXP enab)
{
  if (!CSAPI_SetSampleDepthEnabled((HDEV) EXTPTR_PTR(hdev), (BOOL) (LOGICAL(AS_LOGICAL(enab))[0])))
    error("CSAPI_SetSampleDepthEnabled failed");
  RETBOOL(TRUE);
}

SEXP
my_get_scanline (SEXP hdev, SEXP lno)
{
  SEXP rv = allocVector(INTSXP, ACTUAL_SAMPLES_PER_PULSE);
  unsigned char *src;
  int *dst;
  int i;
  PROTECT(rv);
  if (!CSAPI_GetScanLine((HDEV) EXTPTR_PTR(hdev), (UINT) INTEGER(AS_INTEGER(lno))[0], (BYTE *)INTEGER(rv), GSL_NONE))
    error("CSAPI_GetScanLine failed");
  // convert bytes to integers in place
  for (i = (ACTUAL_SAMPLES_PER_PULSE - 1), src = i + (unsigned char *) INTEGER(rv), dst = i + INTEGER(rv); i >= 0; --i, --dst, --src)
    *dst = *src;
  UNPROTECT(1);
  return(rv);
}

SEXP
my_get_scanline_address (SEXP hdev, SEXP lno)
{
  BYTE *p;
  if (!CSAPI_GetScanLineAddress((HDEV) EXTPTR_PTR(hdev), (UINT) INTEGER(AS_INTEGER(lno))[0], &p))
    error("CSAPI_GetScanLineAddress failed");
  return PTR_TO_EXTPTR(p);
}

SEXP
my_get_scanline_raw (SEXP hdev, SEXP lno)
{
  SEXP rv;
  unsigned char *src;
  int *dst;
  int i;
  PROTECT(rv = allocVector(INTSXP, ACTUAL_SAMPLES_PER_PULSE));
  if (!CSAPI_GetScanLineRaw((HDEV) EXTPTR_PTR(hdev), (UINT) INTEGER(AS_INTEGER(lno))[0], (BYTE *)INTEGER(rv), GSL_NONE))
    error("CSAPI_GetScanLineRaw failed");
  // convert bytes to integers in place
  for (i = (ACTUAL_SAMPLES_PER_PULSE - 1), src = i + (unsigned char *) INTEGER(rv), dst = i + INTEGER(rv); i >= 0; --i, --dst, --src)
    *dst = *src;
  UNPROTECT(1);
  return(rv);
}

SEXP
my_get_current_scan_number (SEXP hdev)
{
  // return the index of the most recently updated scan line
  SEXP rv = allocVector(INTSXP, 1);
  unsigned int scanno;
  if (!CSAPI_GetCurrentScanNumber((HDEV) EXTPTR_PTR(hdev), &scanno))
    error("CSAPI_GetCurrentScanNumber failed");
  INTEGER(rv)[0] = (int) scanno;
  return rv;
}

SEXP
my_get_current_antenna (SEXP hdev)
{
  // return the name of the current antenna
  char buff[MAX_ANTNAME_SIZE + 1];
  if (!CSAPI_GetCurrentAntennaName((HDEV) EXTPTR_PTR(hdev), (LPSTR) buff, MAX_ANTNAME_SIZE))
    error("CSAPI_GetCurrentAntennaName failed");
  RETCHAR(buff);
}

SEXP
my_get_scan (SEXP hdev, SEXP as_raw)
{
  // return a full scan of data
  SEXP rv;
  int np, ns;
  unsigned char *src;
  int *dst;
  int i, j;
  typeof (CSAPI_GetScanLineRaw) *fun;
  if (!CSAPI_GetScanLinesPerSweep((HDEV) EXTPTR_PTR(hdev), &np))
    error("CSAPI_GetScanLinesPerSweep failed");
  if (!CSAPI_GetSamplesPerScanLine((HDEV) EXTPTR_PTR(hdev), &ns))
    error("CSAPI_SamplesPerScanLine failed");
  ns = ACTUAL_SAMPLES_PER_PULSE;
  PROTECT(rv = allocMatrix(INTSXP, ns, np));

  fun = LOGICAL(AS_LOGICAL(as_raw))[0] ? & CSAPI_GetScanLineRaw : & CSAPI_GetScanLine;
  for (i = 0; i < 1023; ++i) {
    if (!(*fun)((HDEV) EXTPTR_PTR(hdev), (UINT) i, (BYTE *)(INTEGER(rv) + ACTUAL_SAMPLES_PER_PULSE * i), GSL_NONE))
      xenerror("GetScanLine(Raw)");
    // convert bytes to integers in place
    for (j = (ACTUAL_SAMPLES_PER_PULSE - 1), src = j + (char *) (INTEGER(rv) + ACTUAL_SAMPLES_PER_PULSE * i), dst = (j + ACTUAL_SAMPLES_PER_PULSE * i) + INTEGER(rv); j >= 0; --j, --dst, --src)
      *dst = *src;
  }
  UNPROTECT(1);
  return(rv);
}

SEXP
my_set_main_mode (SEXP hdev, SEXP enab)
{
  CSTRYH(SetMainMode, hdev, ((LOGICAL(AS_LOGICAL(enab))[0]) ? MAIN_MODE_ON : MAIN_MODE_OFF));
  RETBOOL(TRUE);
}

SEXP
my_get_main_mode (SEXP hdev)
{
  unsigned int mode;
  CSTRYH(GetMainMode, hdev, &mode);
  RETBOOL(mode == MAIN_MODE_ON);
}

SEXP
my_set_standby_mode (SEXP hdev, SEXP enab)
{
  CSTRYH(SetStandByMode, hdev, ((LOGICAL(AS_LOGICAL(enab))[0]) ? STANDBY_ON : STANDBY_OFF));
  RETBOOL(TRUE);
}

SEXP
my_get_standby_mode (SEXP hdev)
{
  unsigned int mode;
  CSTRYH(GetStandByMode, hdev, &mode);
  RETBOOL(mode == STANDBY_ON);
}

SEXP
my_set_master_mode (SEXP hdev, SEXP enab)
{
  CSTRYH(SetMasterMode, hdev, ((LOGICAL(AS_LOGICAL(enab))[0]) ? ANTENNA_RIB_MASTER : ANTENNA_RIB_SLAVE));
  RETBOOL(TRUE);
}

SEXP
my_get_master_mode (SEXP hdev)
{
  unsigned int mode;
  CSTRYH(GetMasterMode, hdev, &mode);
  RETBOOL(mode == ANTENNA_RIB_MASTER);
}

SEXP
my_get_prg_type (SEXP hdev)
{
  RIB_PRG_TYPE prg_type;
  CSTRYH(GetPrgType, hdev, &prg_type);
  RETINT(prg_type);
}

SEXP
my_set_video_neg (SEXP hdev, SEXP enab)
{
  CSTRYH(SetVideoNegative, hdev, LOGICAL(AS_LOGICAL(enab))[0]);
  RETBOOL(TRUE);
}

SEXP
my_get_video_neg (SEXP hdev)
{
  BOOL enab;
  CSTRYH(GetVideoNegative, hdev, &enab);
  RETBOOL(enab);
}


SEXP
my_get_cable_length (SEXP hdev)
{
  UINT lng;
  // return the cable length setting
  CSTRYH(GetCableLength, hdev, &lng);
  RETINT(lng);
}

SEXP
my_set_cable_length (SEXP hdev, SEXP lng)
{
  // set the cable length
  CSTRYH(SetCableLength, hdev, INTEGER(AS_INTEGER(lng))[0]);
  RETBOOL(TRUE);
}

SEXP
my_get_control_level (SEXP hdev, SEXP which)
{
  UINT val;
  // return level of a control
  // which: 0 = GAIN, 1 = TUNE, 2 = SEA, 3 = RAIN
  CSTRYH(GetControlLevel, hdev, INTEGER(AS_INTEGER(which))[0], &val);
  RETINT(val);
}

SEXP
my_set_control_level (SEXP hdev, SEXP which, SEXP val)
{
  // set the level of a control
  CSTRYH(SetControlLevel, hdev, INTEGER(AS_INTEGER(which))[0], INTEGER(AS_INTEGER(val))[0]);
  RETBOOL(TRUE);
}

SEXP
my_save_antenna_settings (SEXP hdev)
{
  // save the antenna's administrative settings back to the .CFG file
  CSTRYH(SaveSettings, hdev, SAVE_ANTENNA_ADMINISTRATIONSET);
  RETBOOL(TRUE);
}

SEXP
my_revert_antenna_settings (SEXP hdev)
{
  // save the antenna's administrative settings back to the .CFG file
  CSTRYH(ResetAntennaAdminSet, hdev);
  RETBOOL(TRUE);
}

SEXP
my_reset_server (SEXP hdev)
{
  // reset the IntegRadar Server (assuming hdev is a virtual TCPIP radar)
  // and we make sure to put the server in standby mode (and possibly turn
  // it off)
  CSTRYH(SetStandByMode, hdev, TRUE);
  CSDOH(SetMainMode, hdev, CSAPI_OFF);
  CSTRYH(ResetServer, hdev);
  RETBOOL(TRUE);
}

SEXP
my_is_connected (SEXP hdev)
{
  // check whether a handle is still connected to its TCPIP server
  BOOL iscon;
  CSTRYH(IsConnected, hdev, &iscon);
  RETBOOL(iscon);
}

SEXP
my_have_TRIG (SEXP hdev)
{
  // check whether we have a valid trigger signal
  BOOL isact;
  CSTRYH(GetTriggerStatus, hdev, &isact);
  RETBOOL(isact);
}
  
SEXP
my_have_SHM (SEXP hdev)
{
  // check whether we have a valid Ship Heading Marker signal
  BOOL isact;
  CSTRYH(GetShipHeadingMarkerStatus, hdev, &isact);
  RETBOOL(isact);
}

SEXP
my_get_SHM_offset (SEXP hdev)
{
  UINT shmo;
  // return the offset of the ship's heading marker signal and the true ship's heading
  // units are 0..1023 around the circle
  CSTRYH(GetShipHeadingMarkerOffset, hdev, &shmo);
  RETINT(shmo);
}

SEXP
my_set_SHM_offset (SEXP hdev, SEXP shmo)
{
  // set the offset of the ship's heading marker signal and the true ship's heading
  // units are 0..1023 around the circle
  CSTRYH(SetShipHeadingMarkerOffset, hdev, INTEGER(AS_INTEGER(shmo))[0]);
  RETBOOL(TRUE);
}


SEXP
my_get_PLEN (SEXP hdev)
{
  // return the current pulse length in nanoseconds
  UINT plen;
  CSTRYH(GetPulseDuration, hdev, &plen);
  RETINT(plen);
}

SEXP
my_get_PRF (SEXP hdev)
{
  // return the current pulse repetition frequency in Hz
  UINT prf;
  CSTRYH(GetPulseRepetitionRate, hdev, &prf);
  RETINT(prf);
}

SEXP
my_have_INTS (SEXP hdev)
{
  // check whether we have valid interrupt signals from the USB board
  BOOL isact;
  CSTRYH(GetInterruptStatus, hdev, &isact);
  RETBOOL(isact);
}

SEXP
my_get_INTS (SEXP hdev)
{
  // return the number of interrupts processed since 
  // leaving standby mode.
  UINT ints;
  CSTRYH(GetInterruptCount, hdev, &ints);
  RETINT(ints);
}

SEXP
my_have_BP (SEXP hdev)
{
  // check whether we have valid bearing pulse signals
  BOOL isact;
  CSTRYH(GetBearingPulseStatus, hdev, &isact);
  RETBOOL(isact);
}

SEXP
my_get_BPR (SEXP hdev)
{
  // get the number of bearing pulses per rotation
  UINT bpr;
  CSTRYH(GetBearingPulseStatus, hdev, &bpr);
  RETINT(bpr);
}

SEXP
my_clear_scan (SEXP hdev)
{
  // clear the scan buffer
  CSTRYH(ClearScanBuffer, hdev);
  RETBOOL(TRUE);
}

SEXP
get_sweep_time (SEXP devsxp)
{
  // return the timestamp for the start of the current sweep

  FILETIME ft;
  DWORD vpt = 0;

  t_xenex *me;

  me = ensure_device(devsxp);

  CSTRY(GetTimeOfSweep, me->hxen, &ft, &vpt);
  printf("GetTimeOfSweep returned %ld in vpt.\n", vpt);
  RETDBL(FILETIME_TO_UTC(ft));
}

SEXP
set_batch_parms (SEXP bat) 
{
  // set the scans per batch and time (in ms) to sleep between batches

  bat = AS_INTEGER(bat);
  scans_to_fetch_per_batch = INTEGER(bat)[0];
  sleep_interval_between_scan_batches = INTEGER(bat)[1];
  RETBOOL(TRUE);
}

R_CallMethodDef xenex_call_methods[]  = {
  // R hook functions
  MKREF(end_of_data, 1),
  MKREF(get_all_antennas, 0),
  MKREF(get_antenna, 1),
  MKREF(get_antenna_parms, 1),
  MKREF(get_cable_length, 1),
  MKREF(get_control_level, 2),
  MKREF(get_diag_text, 1),
  MKREF(get_hosts, 4),
  MKREF(get_preset_level, 2),
  MKREF(get_prg_type, 1),
  MKREF(get_scan_data, 3),
  MKREF(get_scan_info, 6),
  MKREF(get_scanline_address, 3),
  MKREF(set_sample_rate, 2),
  MKREF(get_sweep_time, 1),
  MKREF(have_radar_board, 0),
  MKREF(have_winsock, 0),
  MKREF(have_xir3000, 0),
  MKREF(load_antenna_parms, 1),
  MKREF(save_antenna_parms, 1),
  MKREF(set_antenna_parms, 2),
  MKREF(set_cable_length, 2),
  MKREF(set_control_level, 3),
  MKREF(set_plen, 2),
  MKREF(set_preset_level, 3),
  MKREF(set_range, 2),
  MKREF(shut_down, 1),
  MKREF(start_up, 2),
  MKREF(usb_send_command, 2),
  MKREF(my_clear_scan, 1),
  MKREF(my_close_device, 1),
  MKREF(my_get_antenna, 1),
  MKREF(my_get_BPR, 1),
  MKREF(my_get_cable_length, 1),
  MKREF(my_get_control_level, 2),
  MKREF(my_get_current_antenna, 1),
  MKREF(my_get_current_scan_number, 1),
  MKREF(my_get_INTS, 1),
  MKREF(my_get_main_mode, 1),
  MKREF(my_get_master_mode, 1),
  MKREF(my_get_PLEN, 1),
  MKREF(my_get_PRF, 1),
  MKREF(my_get_PRF, 1),
  MKREF(my_get_prg_type, 1),
  MKREF(my_get_pulses_per_scan, 1),
  MKREF(my_get_range, 1),
  MKREF(my_get_sample_depth, 1),
  MKREF(my_get_sample_depth_enabled, 1),
  MKREF(my_get_samples_per_pulse, 1),
  MKREF(my_get_scan, 2),
  MKREF(my_get_scanline, 2),
  MKREF(my_get_scanline_address, 2),
  MKREF(my_get_scanline_raw, 2),
  MKREF(my_get_server_count, 1),
  MKREF(my_get_SHM_offset, 1),
  MKREF(my_get_standby_mode, 1),
  MKREF(my_get_video_neg, 1),
  MKREF(my_have_BP, 1),
  MKREF(my_have_INTS, 1),
  MKREF(my_have_SHM, 1),
  MKREF(my_have_TRIG, 1),
  MKREF(my_is_connected, 1),
  MKREF(my_is_radar_board_available, 0),
  MKREF(my_is_winsock_available, 0),
  MKREF(my_is_xir3000_available, 0),
  MKREF(my_open_socket, 1),
  MKREF(my_open_xir3000, 2),
  MKREF(my_reset_server, 1),
  MKREF(my_revert_antenna_settings, 1),
  MKREF(my_save_antenna_settings, 1),
  MKREF(my_set_cable_length, 2),
  MKREF(my_set_control_level, 3),
  MKREF(my_set_main_mode, 2),
  MKREF(my_set_master_mode, 2),
  MKREF(my_set_range, 2),
  MKREF(my_set_sample_depth, 2),
  MKREF(my_set_sample_depth_enabled, 2),
  MKREF(my_set_SHM_offset, 2),
  MKREF(my_set_standby_mode, 2),
  MKREF(my_set_video_neg, 2),
  MKREF(set_batch_parms, 1),
  {NULL, NULL, 0}
};

void
R_init_xenex(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, xenex_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

void
R_unload_xenex(DllInfo *info)
{
  /* Release resources by calling shutdown for each server */
  int i;
  for (i=0; i < MAX_INTEGRADAR_SERVERS + 2; ++i) {
    if (radars[i]) {
      do_shut_down(radars[i]);
      Free(radars[i]);
      radars[i] = NULL;
    }
  }
}
