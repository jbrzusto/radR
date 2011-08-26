/*  svn $Id: seascan.c 625 2010-07-15 11:47:12Z john $

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

  interface for the Rutter Seascan server via its Client.dll

*/

#ifdef RADR_DEBUG
#define PF(...) printf(__VA_ARGS__); fflush(stdout)
#define THREADUTIL_DEBUG
#else
#define PF(...)
#endif

#include "seascan.h"

static char server_names[MAX_SERVERS_EX][MAX_COMPUTER_NAME_LENGTH];	/* server names */
static unsigned short num_servers = 0;					/* number of servers; set by get_hosts */
static t_seascan *radars[MAX_SERVERS_EX]; 				/* only num_servers pointers are non-null */

SEXP
get_hosts(SEXP ignore_unknown_sxp, SEXP known_hosts_sxp) {

  // return the character vector hostnames
  // which are potential seascan servers
  // ignore_unknown is a flag indicating that we are to
  // ignore all but the hosts listed in the character vector known_hosts; this prevents
  // having to wait through many long timeouts on 
  // networks having machines which seascan's
  // client.dll incorrectly identifies as seascan
  // servers. (Or I just don't understand the docs...)

  SEXP rv;
  __p_sig_fn_t save_segv_handler;
  int ignore_unknown = INTEGER(ignore_unknown_sxp)[0];
  int num_known_servers = LENGTH(known_hosts_sxp);
  int i, j=0, n=1;

  // save the segfault handler and begin ignoring SIGSEGV because RPC 
  // (called by XSigGetServerStatus often does SIGSEGV for no apparent reason)

  save_segv_handler = signal(SIGSEGV, SIG_IGN);

  // shortcut: if we're only interested in the localhost, don't call
  // XSigQueryNumOfServersEx because it takes forever on a machine
  // with multiple network connections

  if (ignore_unknown_sxp && num_known_servers == 1 && 0 == strcasecmp("local", CHAR(STRING_ELT(known_hosts_sxp, 0)))) {
    num_servers = 1;
    strcpy(server_names[0], "local");
    error_code = RADR_ERROR_NONE;
  } else {
    error_code = XSigQueryNumOfServersEx(&num_servers, server_names, 8);
    if (!error_code) {
      // count and flag servers for which we are able to get the status
      for (i = 0, n = 0; i < num_servers; ++i) {
	if (ignore_unknown) {
	  for (j = 0; j < num_known_servers; ++j)
	    if (0 == strcasecmp(server_names[i], CHAR(STRING_ELT(known_hosts_sxp, j))))
	      break;
	}
	if ((! ignore_unknown || j != num_known_servers)){ 
	  // keep this server
	  ++n;
	} else {
	  // drop this unknown or unresponsive server
	  server_names[i][0] = '\0';
	} 
      }
    }
  }
  // common to both "local only" and general cases

  if (!error_code) {
    // return the list of actually good servers
    PROTECT(rv = allocVector(STRSXP, n));
    for (i = 0, j = 0; i < num_servers && j < n; ++i) {
      if (server_names[i][0]) {
	SET_STRING_ELT(rv, j, mkChar(server_names[i]));
	++j;
      }
    }
    UNPROTECT(1);
  } else {
    rv = FAIL_SEXP;
  }

  // restore signal handler
  signal(SIGSEGV, save_segv_handler);
  return rv;
}

static t_seascan *
ensure_server(int svn) {
  // make sure there is a structure allocated for server svn 

  t_seascan *me;

  if (radars[svn])
    return radars[svn];
  
  // if there are no servers, fail
  if (num_servers == 0)
    return NULL;

  me = Calloc(1, t_seascan);
  me->svn = svn;
  me->have_tape_contents = me->have_scan_data = FALSE;
  me->current_source = -1;
  me->current_run = -1;
  me->start_time = 0;

  error_code = XSigGetServerStatus(&me->ss, me->svn);
  if (error_code) {
    Free(me);
    return NULL;
  }
  // disable pulse filtering - we want the same data as we get by reading an
  // archive directly

  XSigGetProcessing(&me->dh, me->svn);
  XSigSetPF((short) -1, me->svn);
  radars[svn] = me;
  return me;
}
  

#define MAX_TAPE_START_STOP_TRIES (3000 / SEASCAN_SHORT_DELAY)

static int
stop_tape(short server) {
  
  // Get the specified seascan server to stop its tape playback.
  // Returns TRUE if successful,
  // FALSE otherwise.

  SERVER_STATUS ss;
  int i;

  // disable pacing otherwise some function might hang

  if (radars[server]->is_pacing) {
    XSigDisableClientPacing (server);
    radars[server]->is_pacing = FALSE;
  }

  if (CLIENT_STATUS_NO_ERROR == XSigGetServerStatus(&ss, server) && ! ss.TapePlayback)
    return TRUE;

  // wait for the tape to stop
  
  for (i = 0; i < MAX_TAPE_START_STOP_TRIES; ++i) {
    XSigStopPlayback (server);
    Sleep(SEASCAN_SHORT_DELAY);
    if (CLIENT_STATUS_NO_ERROR == XSigGetServerStatus(&ss, server) && ! ss.TapePlayback)
      break;
  }
  return i < MAX_TAPE_START_STOP_TRIES;
}

static int
start_tape(short server, int channel, time_t t) {

  // Get the specified seascan server to start its tape playback,
  // and enable client pacing.
  // Returns TRUE if successful,
  // FALSE otherwise.
  // The various sleeps were arrived at through trial and error.

  SERVER_STATUS ss;
  int i;

  if (CLIENT_STATUS_NO_ERROR == XSigGetServerStatus(&ss, server) && ss.TapePlayback)
    return TRUE;

  XSigDisableClientPacing (server);      // ignore errors from this; failure implies pacing is off
  radars[server]->is_pacing = FALSE;        
  XSigSetRadarChannel (channel, server); // FIXME: ignore errors from this
  
  // wait for the tape to start, up to 3 seconds
  
  for (i=0; i < MAX_TAPE_START_STOP_TRIES; ++i) {
    XSigStartPlayback (t, server);

    Sleep(SEASCAN_SHORT_DELAY);
    if (CLIENT_STATUS_NO_ERROR == XSigGetServerStatus(&ss, server) && ss.TapePlayback)
      break;
  }
  if (i == MAX_TAPE_START_STOP_TRIES)
    return FALSE;

  // give the seascan tape system a chance to warm up (is it using vacuum tubes?)
  Sleep(SEASCAN_LONG_DELAY);

  // wait up to 3 seconds for a call to EnableClientPacing to succeed
  for (i=0; i < MAX_TAPE_START_STOP_TRIES; ++i) {
    if (CLIENT_STATUS_NO_ERROR == XSigEnableClientPacing (server))
      break;

    Sleep(SEASCAN_SHORT_DELAY);
  }
  if (i == MAX_TAPE_START_STOP_TRIES)
    return FALSE;
  radars[server]->is_pacing = TRUE;
  return TRUE;
}

static int
start_radar (short server, int channel) {
  // WE IGNORE ERROR CODES HERE.
  XSigSetRadarOnline (server);
  XSigSetRadarChannel (channel, server);
  return TRUE;
}

static int
stop_radar (short server) {
  // WE IGNORE ERROR CODES HERE.
  XSigSetRadarOffline (server);
  return TRUE;
}
  
static int
stop_source(t_seascan *me, t_seascan_port i) {
  int rv = FALSE;
  if (! me->is_running)
    return RADR_ERROR_NONE;
  switch(i) {
  case SEASCAN_NONE:
    rv = TRUE;
    break;
  case SEASCAN_LIVE_0:
  case SEASCAN_LIVE_1:
    rv = stop_radar(me->svn);
    break;
  case SEASCAN_TAPE_0:
  case SEASCAN_TAPE_1:
    rv = stop_tape(me->svn);
    break;
  default:
    break;
  }
  if (rv) {
    me->is_running = FALSE;
    me->current_source = SEASCAN_NONE;
    return RADR_ERROR_NONE;
  }
  return RADR_ERROR_UNKNOWN_PORT_ERROR;
}

static int
start_source(t_seascan *me, t_seascan_port p) {
  // start a source; if there is already a source running on this host, shut it down
  int rv = FALSE;

  if (me->is_running)
    stop_source(me, me->current_source);
  switch(p) {
  case SEASCAN_NONE:
    rv = TRUE;
    break;
  case SEASCAN_LIVE_0:
    rv = start_radar(me->svn, 0);
    break;
  case SEASCAN_LIVE_1:
    rv = start_radar(me->svn, 1);
    break;
  case SEASCAN_TAPE_0:
    rv = start_tape(me->svn, 0, me->start_time);
    break;
  case SEASCAN_TAPE_1:
    rv = start_tape(me->svn, 1, me->start_time);
    break;
  default:
    break;
  }
  if (rv) {
    me->is_running = TRUE;
    me->current_source = p;
    return RADR_ERROR_NONE;
  }
  me->current_source = SEASCAN_NONE;
  me->is_running = FALSE;
  return RADR_ERROR_UNKNOWN_PORT_ERROR;
}
  
SEXP
get_contents(SEXP svnsxp, SEXP portsxp) {
  SEXP rv;
  int i;

  int svn = INTEGER(svnsxp)[0];
  int port = INTEGER(portsxp)[0];
  t_seascan *me;
  TAPE_CONTENTS *tc;

  error_code = RADR_ERROR_NONE;
  ASSERT(me = ensure_server(svn));
  ASSERTEI(port >= 2, RADR_ERROR_FEATURE_NOT_AVAILABLE, "No table of contents on live radar.");
  tc = & me->tc;

  TRY_SEASCAN_SEXP(XSigGetTapeContents(tc, me->svn));
  PROTECT(rv = allocVector(INTSXP, me->tc.NumSegments * 3)); // WARNING:  Y2037K time_t != int issue?
  for (i = 0; i < tc->NumSegments; ++i) {
    INTEGER(rv)[3*i] = tc->NumImages[i] / 4; /* seascan returns number of quadrant images, not full scans */
    INTEGER(rv)[3*i+1] = tc->StartTime[i];
    INTEGER(rv)[3*i+2] = tc->StopTime[i];
  }
  UNPROTECT(1);
  me->have_tape_contents = TRUE;
  return rv;
}

SEXP
end_of_data(SEXP svnsxp, SEXP portsxp) {
  // try to detect the end of tape
  // or a shutdown of A/D conversion on this port
  // returning TRUE if the appropriate situation is detected,
  // and FALSE otherwise

  int svn = INTEGER(svnsxp)[0];
  int port = INTEGER(portsxp)[0];
  t_seascan *me;
  SEXP rv;

  error_code = RADR_ERROR_NONE;
  me = ensure_server(svn);
  if (!me) 
    return FAIL_SEXP;

  if (CLIENT_STATUS_NO_ERROR != (error_code = XSigGetServerStatus(&me->ss, me->svn)))
    return FAIL_SEXP;
  
  rv = allocVector(LGLSXP, 1);

  if (port < 2) {
    // for live channels, just check whether A/D conversion is
    // still going on
    LOGICAL(rv)[0] = me->ss.AtoDActive ? 0 : 1;

  } else {
    // if the seascan tape module has less than 4 quadrants of data left,
    // the effective end-of-data condition will not be
    // recognized here, but rather in get_scan_data
    // or get_next_scan
    LOGICAL(rv)[0] = me->ss.TapePlayback ? 0 : 1;
  }
  return rv;
}


THREAD_DECLARE(get_scan[MAX_SERVERS_EX]);

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

/* vp is a pointer to the t_get_scan_parms. */

void *
get_scan_thread (void *vp) {

  /* sleepable thread function that gets data from one seascan server 

     DANGER!!! 

     this function MUST NOT CALL ANY R functions, as R is not
     re-entrant It is only safe to read/write R variables if pointers
     to protected SEXPs have been passed in vp.

  */

  t_get_scan_parms *p = (t_get_scan_parms *) vp;
  int i, j;
  BYTE *dat;
  TIME_POS_INFO tpi;
  DATA_HEADER *dh, tmpdh;
  unsigned ns; /* total number of samples in the scan */
  struct tm tm;
  SEXP ec; /* error code */

  THREAD_ADD_CLEANUP(& set_null_return_value, vp);
  THREAD_STARTING(get_scan[p->server]);

  /* run forever */
  for (;;) {
    // if we're doing tape playback ask seascan to process the next image
    //    if (p->server >= 2)
      XSigProcessNextImage(p->server);

    /* 
       make sure the extmat is large enough to hold the max possible
       size of data, because we don't know until calling
       XSigGetQuadrant what the current data size is (the user might have
       switched the digitizing mode, and seascan doesn't seem to coordinate
       that) .  We'll set the actual size after we get the whole scan.
    */

    PF("seascan: about to ensure extmat\n");
    (*pensure_extmat) (p->mat, SEASCAN_MAX_SAMPLES, 1);
    PF("seascan: get_scan thread ensured extmat\n");

    /* assume no error; what we return is the pre-allocated list of scan metadata */
    ec = p->si;

    dat = (BYTE *) p->mat->ptr;
    ns = 0;

    dh = p->header;

    /* get each quadrant of data */
    for (i = 0; i < 4; ++i) {
      PF("seascan: get_scan thread getting quadrant %d\n", i);
      error_code = XSigGetQuadrant(dh, dat, p->flags, i, p->server);
      dat += dh->SamplesPerLine * dh->Lines * SEASCAN_SAMPLE_SIZE;
      if (error_code != CLIENT_STATUS_NO_ERROR && error_code != CLIENT_STATUS_QUAD_ERROR) {
	PF("seascan: get_scan XSigGetQuadrant returned %d\n", error_code);
	break;
      }
      PF("seascan: get_scan thread got quadrant %d\n", i);
      ns += dh->Lines * dh->SamplesPerLine;
      if (i == 0) {
	/* keep a copy of the first quadrant's timestamp, to be used
	   as that of the entire scan */
	tpi = dh->TimePosStamp;
      }
    }
    if (i == 4) {
      /* no error; setup up scan info*/
      p->mat->cols = *p->samples_per_pulse = dh->SamplesPerLine;
      p->mat->rows = *p->pulses            = ns / dh->SamplesPerLine;
      /* if the timestamp isn't valid, we report an error */
      if (!finite(*p->timestamp = TIME_FMT_CONVERT(&tpi.Time, tm)))
	ec = R_NilValue;
      else
	*p->have_scan_data = TRUE;

      /* Uggh: determine scan duration by waiting until we see the
	 timestamp at the start of the next processed quadrant.  We
	 allow up to 1 second of wait (10 x 0.1 s), and then fall back
	 to an estimate given by 4 / 3 * (time elapsed for 3
	 quadrants) */
      
      for (j = 0; j < 10; ++j) {
	XSigGetProcessing(&tmpdh, p->server);
	*p->duration = (int) (1000 * (TIME_FMT_CONVERT(&tmpdh.TimePosStamp.Time, tm) - *p->timestamp) + 0.5);
	if (*p->duration != 0)
	  // duration is not zero, so it is the difference between timestamps for consecutive quadrant 0s,
	  // which is what we need
	  break;
	// wait for seascan processing 
	Sleep (100); 
      }
      if (*p->duration == 0) {
	// Seascan didn't process another quadrant within timeout, so we fall back to an estimate of scan
	// duration based on the time elapsed for 3 quadrants
	*p->duration = (int) (4 / 3.0 * 1000 * (TIME_FMT_CONVERT(&dh->TimePosStamp.Time, tm) - *p->timestamp) + 0.5);
      }
      *p->range_per_sample = dh->RangePerSample;
      *p->start_range = dh->StartRange;
      *p->angle_offset = dh->NorthAligned ? 0 : dh->Heading;
      *p->pulse_length = dh->PulseLength;
      *p->PRF = dh->PRF;

    } else {
      ec = R_NilValue;
    }
    /* set the return value in the appropriate place 
       This is risky, but we've covered ourselves because the list slot originally had R_NilValue
       in it, so the list vector is appropriately aged.
    */

    *p->rv = ec;

    /* wait until we're awakened */

    PF("about to sleep get_scan thread %d\n", p->server);
    THREAD_SLEEP(get_scan[p->server]);
    PF("seascan: get_scan thread %d awakened\n", p->server);
  }
  THREAD_DROP_CLEANUP(0);
  return NULL; /* never reached */
}

SEXP
get_scan_info(SEXP svnsxp, SEXP portsxp, SEXP namesxp, SEXP scanmatsxp, SEXP trvsxp, SEXP trvindexsxp) {

  // get the header info for the next available scan.  We actually read the whole scan here
  // because of timestamp wonkiness that is easily avoided this way.  This is done with a
  // separate thread.

  int svn = INTEGER(svnsxp)[0];
  int trvi = INTEGER(AS_INTEGER(trvindexsxp))[0] - 1;
  t_seascan *me;
  SEXP rv;
  DATA_HEADER *dh;
  t_get_scan_parms *sgparm;

  me = ensure_server(svn);
  if (!me) 
    return R_NilValue;

  sgparm = & me->getter_parms;

  dh = &me->dh;

  if (!me->scan_info_vector) {
    /* create the scan.info vector which we'll re-use; set its names,
       and mark it as copy-on-change; also, preserve it against
       garbage collection */

    PROTECT(me->scan_info_vector = allocVector(VECSXP, 11));
    SET_NAMES(me->scan_info_vector, namesxp);
    SET_NAMED(me->scan_info_vector, 2);
    R_PreserveObject(me->scan_info_vector);
    UNPROTECT(1);

    sgparm->header = dh;
    sgparm->mat = SEXP_TO_EXTMAT(scanmatsxp);
    sgparm->flags = UNPROCESSED_DATA;
    sgparm->server = me->svn;
    sgparm->rv = & (VECTOR_ELT(trvsxp, trvi));
    sgparm->have_scan_data = &me->have_scan_data;
    
    /* allocate the scan info items, and set up pointers to them in
       sgparms.  They are all protected by the preservation of the
       list itself */

#define MAKE_SI_ITEM(F, I, X) SET_VECTOR_ELT(me->scan_info_vector, I, X);	\
    sgparm->F = (typeof(sgparm->F)) DATAPTR(VECTOR_ELT(me->scan_info_vector, I))

    MAKE_SI_ITEM(pulses, 0, ScalarInteger(0));
    MAKE_SI_ITEM(samples_per_pulse, 1, ScalarInteger(0));
    MAKE_SI_ITEM(bits_per_sample, 2, ScalarInteger(T_SAMPLE_BITS_USED)); // bits per sample (constant)
    MAKE_SI_ITEM(timestamp, 3, ScalarReal(0));
    MAKE_SI_ITEM(duration, 4, ScalarInteger(0));
    MAKE_SI_ITEM(range_per_sample, 5, ScalarReal(0));
    MAKE_SI_ITEM(start_range, 6, ScalarReal(0));
    MAKE_SI_ITEM(angle_offset, 7, ScalarReal(0));
    MAKE_SI_ITEM(orientation, 8, ScalarInteger(+1)); // orientation of rotation (constant)
    MAKE_SI_ITEM(pulse_length, 9, ScalarInteger(0));
    MAKE_SI_ITEM(PRF, 10, ScalarInteger(0));
    
    me->have_scan_data = FALSE;
    sgparm->si = me->scan_info_vector;

  } else {

    /* fill in parameters for the reader thread; some of these may
       change from time to time (e.g. if switching to a new server), so
       we always do this */
    
    sgparm->header = dh;
    sgparm->mat = SEXP_TO_EXTMAT(scanmatsxp);
    sgparm->flags = UNPROCESSED_DATA;
    sgparm->server = me->svn;
    sgparm->rv = & (VECTOR_ELT(trvsxp, trvi));
    sgparm->have_scan_data = &me->have_scan_data;
    me->have_scan_data = FALSE;
  }

  /* create the reader thread, if necessary */

  if (! THREAD_IS_CREATED(get_scan[me->svn]))
    THREAD_CREATE_HP(get_scan[me->svn], get_scan_thread, sgparm);

  /* mark that we're spawning a reader thread */
  PROTECT(rv = ScalarInteger(NA_INTEGER));
  SET_VECTOR_ELT(trvsxp, trvi, rv);

  /* start the data getter */
  PF("seascan: waking the reader thread\n");
  THREAD_WAKE(get_scan[me->svn]);

  UNPROTECT(1);
  return rv;
}

SEXP
get_scan_data(SEXP svnsxp) {
  // get the data from the most recently processed scan.  get_scan_info has
  // probably already done the work, so we just check whether data are available.
  //
  // svnsxp: integer server number
  //
  // portsxp: integer port number (0..3)
  //
  // buffsxp: extmat wrapped in an SEXP
  //
  // thrstatsxp: integer vector where the exit status will be stored
  // after the getter thread completes.  If R_NilValue, the getter is
  // run in the caller's thread, and get_scan_data only returns when
  // the data have been obtained.
  //
  // indsxp: which item in the vector thrstatsxp will be used for the
  // getter thread's exit status This is an integer scalar with value
  // 1, 2, ...
  //
  // Returns:  R_NilValue if there was an error in creating the thread, or
  // if rvsxp was R_NilValue and there was an error from the getter.

  int svn = INTEGER(svnsxp)[0];
  t_seascan *me;

  me = ensure_server(svn);
  if (!me) 
    return R_NilValue;
  return me->have_scan_data ? PASS_SEXP : R_NilValue;
}

static SEXP
do_shut_down(t_seascan *me) {
  error_code = RADR_ERROR_NONE;
  
  if (me->current_source >= SEASCAN_TAPE_0)
    XSigDisableClientPacing (me->svn);
  PF("seascan: about to kill get_scan thread %d\n", me->svn);
  THREAD_KILL(get_scan[me->svn], TRUE);
  PF("seascan: killed get_scan thread %d\n", me->svn);
  stop_source(me, me->current_source);
  if (me->scan_info_vector) {
    R_ReleaseObject (me->scan_info_vector);
    me->scan_info_vector = NULL;
  }
  return PASS_SEXP;
}

SEXP
start_up (SEXP svnsxp, SEXP portsxp, SEXP startsxp) {
  int svn = INTEGER(svnsxp)[0];
  int port = INTEGER(portsxp)[0];
  time_t start = INTEGER(startsxp)[0];
  t_seascan *me;

  error_code = RADR_ERROR_NONE;
  me = ensure_server(svn);
  if (!me) 
    return FAIL_SEXP;
  me->start_time = start;
  if (port != me->current_source) {
    TRY_INFO_SEXP(start_source (me, port), "Unable to open or restart port");
  }
  return PASS_SEXP;
}

SEXP
shut_down(SEXP svnsxp) {
  // stop the tape playback and/or A/D conversion
  // for this host

  int svn = INTEGER(svnsxp)[0];
  t_seascan *me;

  // No need to call ensure_server, since if we haven't
  // connected yet to the server, nothing need be
  // done to shut it down.  We're not taking responsibility
  // for shutting down operations started by other clients.

  if (! (me = radars[svn])) 
    return FAIL_SEXP;

  error_code = RADR_ERROR_NONE;

  TRY_INFO_SEXP(stop_source (me, me->current_source), "Unable to shut down port");
  return do_shut_down(radars[svn]);
}

SEXP
flush_seascan_buffers(SEXP svnsxp) {
  // turn radar digitizing off and on, in an attempt to flush
  // seascan's scan buffer.

  int rv = CLIENT_STATUS_NO_ERROR;
  int svn = INTEGER(AS_INTEGER(svnsxp))[0];
  t_seascan *me;

  me = ensure_server(svn);
  if (!me) {
    rv = CLIENT_STATUS_INVALID_ID;
  } else {
    if (me->current_source == SEASCAN_LIVE_0 || me->current_source == SEASCAN_LIVE_1) {
      XSigSetRadarOffline (me->svn);
      XSigSetRadarOnline (me->svn);
    }
  }
  return ScalarInteger(rv);
}

SEXP
set_mode(SEXP svnsxp, SEXP modesxp) {
  // set the mode for a server
  // this determines samples per pulse, based
  // on a preset list of ranges
  //
  // NOTE: THIS FUNCITON APPEARS NOT TO DO ANYTHING, 
  // since XSigSetDesiredMode is effectively a NOOP

  int svn = INTEGER(AS_INTEGER(svnsxp))[0];
  int mode = INTEGER(AS_INTEGER(modesxp))[0];
  int rv = CLIENT_STATUS_NO_ERROR;

  t_seascan *me;

  me = ensure_server(svn);
  if (!me) {
    rv = CLIENT_STATUS_INVALID_ID;
  } else {
    rv = XSigSetDesiredMode((USHORT) mode, (SHORT) svn);
  }
  return ScalarInteger(rv);
}


SEXP
set_plen(SEXP svnsxp, SEXP plsxp) {
  // set the pulse length for a server
  // and wait until it is seen to be correctly set,
  // or fail 

  int svn = INTEGER(AS_INTEGER(svnsxp))[0];
  int pl = INTEGER(AS_INTEGER(plsxp))[0];
  int rv = CLIENT_STATUS_NO_ERROR;

  t_seascan *me;

  me = ensure_server(svn);
  if (!me) {
    rv = CLIENT_STATUS_INVALID_ID;
  } else {

    rv = XSigSetDesiredPlen((USHORT) pl, (SHORT) svn);
    /*
      we don't care about this anymore; it is in fact necessary to
      allocate a maximum buffer size for samples because of the possibility
      of an asynchronous change in mode (due to the user changing the radar's
      pulse length), so there's no reason to wait here. 

    if (rv == CLIENT_STATUS_NO_ERROR) {
      int i;

      // now wait until we're actually using the new processing mode
      // since otherwise it might happen between calls to XSigGetProcessing
      // and XSigGetData, which means the call to the latter will be with
      // a possibly too small buffer.
      // We wait up to 40*250 ms == 10 seconds for a successful switchover
      for(i = 0; i < 40; ++i )  {
	XSigGetProcessing(&me->dh, me->svn);
	if (me->dh.ModeNdx == pl)
	  break;
	Sleep(250);
      }
      if (me->dh.ModeNdx != pl)
	rv = CLIENT_STATUS_FUNCTION_FAILED;
    }
    */
  }
  return ScalarInteger(rv);
}

SEXP
get_proc(SEXP svnsxp) {
  // return the full processing info
  // or R_NilValue on error

  int svn = INTEGER(AS_INTEGER(svnsxp))[0];
  DATA_HEADER *dh;
  AGC_PARAMETERS agc;
  SEXP rv;
    
  t_seascan *me;
  double *d;

  me = ensure_server(svn);
  if (!me)
    return FAIL_SEXP;
  dh = &me->dh;
  TRY_SEXP(XSigGetProcessing(dh, me->svn));
  TRY_SEXP(XSigGetAGCParameters(&agc, me->svn));
  rv = allocVector(REALSXP, 26);

  d = REAL(rv);
  *d++ = dh->Lines * 360.0 / dh->AngleCoverage;
  *d++ = dh->SamplesPerLine;
  *d++ = dh->ScanConverted;
  *d++ = dh->StartBearing;
  *d++ = dh->AngleCoverage;
  *d++ = dh->StartRange;
  *d++ = dh->RangeCoverage;
  *d++ = dh->PRF;
  *d++ = dh->AntennaSpeed;
  *d++ = dh->NorthAligned;
  *d++ = dh->RangePerSample;
  *d++ = dh->SelectedModeNdx;
  *d++ = dh->ModeNdx;
  *d++ = dh->SelectedPlenNdx;
  *d++ = dh->PlenNdx;
  *d++ = dh->PulseLength;
  *d++ = dh->CFARed;
  *d++ = dh->PulseFiltered; 
  *d++ = dh->MotionCompensated;
  *d++ = dh->ScansAveraged;
  *d++ = dh->FTCNdx;
  *d++ = dh->State;
  *d++ = dh->XpolImage;
  *d++ = agc.AutomaticMode;
  *d++ = agc.NewGain;
  *d   = agc.NewOffset;

  return rv;
}

SEXP
set_agc_params (SEXP svnsxp, SEXP modesxp) {
  // set the gain control for a server
  // modesxp is an INTSXP with 3 elements:
  //   - automaticmode: use automatic gain control if non-zero
  //   - newgain: value for gain
  //   - newoffset: value for offset

  int svn = INTEGER(AS_INTEGER(svnsxp))[0];
  int rv = CLIENT_STATUS_NO_ERROR;
  AGC_PARAMETERS agc;

  t_seascan *me;

  me = ensure_server(svn);
  if (!me) {
    rv = CLIENT_STATUS_INVALID_ID;
  } else {
    agc.AutomaticMode = INTEGER(modesxp)[0];
    agc.NewGain = INTEGER(modesxp)[1];
    agc.NewOffset = INTEGER(modesxp)[2];
    rv = XSigSetAGCParameters(&agc, (SHORT) svn);
    PF("seascan: set agc params to %d, %d, %d; got %d back\n", agc.AutomaticMode, agc.NewGain, agc.NewOffset, rv);
  }
  return ScalarInteger(rv);
}

SEXP
get_sampling_freqs () {
  SEXP rv;
  int i;

  rv = allocVector(REALSXP, NUM_SAMPLING_FREQ);
  for (i=0; i < NUM_SAMPLING_FREQ; ++i)
    REAL(rv)[i] = sampling_freq[i];

  return rv;
}

SEXP
create_BIN_bytes (SEXP srcbinsxp, SEXP prfsxp, SEXP parmdfsxp, SEXP aziressxp) {
  // return a raw vector for the contents of a SeaScan0.bin
  // file, based on the supplied pulse-repetition frequencies,
  // pulse-lengths, and desired mode combinations.
  //
  // srcbinsxp: RAWSXP contents of an existing SeaScan0.bin file, used for filling
  //         in portions of the new structure.
  //
  // prfsxp: REALSXP vector of possible nominal pulse repetition frequencies, in Hz
  //         These are determined by the radar-driving hardware.  Must be in DECREASING order.
  // 
  // parmdfsxp: DATAFRAME with these columns (or list with these entries):
  //         prf.Hz:  pulse repetition frequency in Hz
  //         plen.ns: pulse length in nanoseconds
  //         npulse:  number of pulses to grab per scan
  //         sample.rate.MHz: sampling rate in MHz
  //         sample.count: samples per pulse
  //
  //         NOTE: The rows must be in DECREASING order by prf.Hz
  //
  //         This data frame holds all desired combinations of 
  //         pulse repetition frequency, pulse length, sampling rate, and samples per pulse
  //         Radar hardware will limit the combinations of pulse length and prf allowed here:
  //            - there are typically only a few preset pulselengths
  //            - the longer the pulse, the lower the maximum allowed prf, 
  //              to prevent the magnetron duty cycle from exceeding safety limits
  //            - only a small number of nominal prfs are typically available
  //         There can be multiple rows in the table with the same values of pulse length
  //         and prf.  Seascan works by empirically estimating the prf.  The user can
  //         then select among all rows having that prf.  The user should select a row
  //         that has the current pulse length (in those cases where more than one pulse length
  //         is available for a given prf) and the user's choice of sampling rate and sample count.
  //
  // aziressxp: INTEGER number of pulses per scan for which SeaScan should return data.
  //            rounded to nearest 256.
  
  int nmode, nplen, nprf;
  int onmode, onplen, onprf;
  int i, j;
  int size;
  char *old = (char *) RAW(srcbinsxp);
  char *new;
  int azires;
  SEXP df_prf, df_plen, df_np, df_sr, df_sc;
  t_SSB_mode smode;
  t_SSB_plen splen;
  t_SSB_prf  sprf;
  t_SSB_radarsite *srad;

  SEXP rv;

  azires = INTEGER(AS_INTEGER(aziressxp))[0];

  PROTECT(prfsxp = AS_NUMERIC(prfsxp));

  nprf = LENGTH(prfsxp);
  
  PROTECT(df_prf =  AS_NUMERIC(VECTOR_ELT(parmdfsxp, 0)));
  PROTECT(df_plen = AS_INTEGER(VECTOR_ELT(parmdfsxp, 1)));
  PROTECT(df_np   = AS_INTEGER(VECTOR_ELT(parmdfsxp, 2)));
  PROTECT(df_sr =   AS_NUMERIC(VECTOR_ELT(parmdfsxp, 3)));
  PROTECT(df_sc =   AS_INTEGER(VECTOR_ELT(parmdfsxp, 4)));

  nplen = nmode = LENGTH(df_prf);
  if (nplen > 31)
    error("seascan:create_BIN_bytes: too many mode rows in table; max allowed is 31", nplen);

  // remember how many elements are in the old .BIN file

  onprf = ((t_SSB_radarsite *) old)->PRFs;
  onplen = ((t_SSB_radarsite *) old)->PulseLengths;
  onmode = ((t_SSB_radarsite *) old)->AcquireModes;
  
  // get size of raw vector needed:

  size = 2*(sizeof(t_SSB_radarsite) + sizeof(t_SSB_gps)) // this pair is repeated identically, apparently
    + (1 + nplen) * sizeof(t_SSB_plen)
    + (1 + nprf)  * sizeof(t_SSB_prf)
    + (1 + nmode) * sizeof(t_SSB_mode)
    + sizeof(t_SSB_gyro);

  PROTECT(rv = allocVector(RAWSXP, size));
  new = (char *) RAW(rv);
    
  // fill in what's needed:

#define DO_COPY(size) {memcpy(new, old, size); new += size; old += size;}
#define DO_ADD(x) {memcpy(new, &x, sizeof(x)); new += sizeof(x);}
#define PRF_TO_LIMIT(x) (int)(1.0e6 / x)

  // change the old mode, prf, plen counts, and azimuth resolution to the new values
  // (We change the values in the old data before they are copied to the new data).

  srad = (t_SSB_radarsite *)old;
  srad->AcquireModes = nmode;
  srad->PRFs = nprf;
  srad->PulseLengths = nplen;
  srad->ACPs = azires;

  srad = ((t_SSB_radarsite *)(old + sizeof(t_SSB_radarsite) + sizeof(t_SSB_gps)));
  srad->AcquireModes = nmode;
  srad->PRFs = nprf;
  srad->PulseLengths = nplen;
  srad->ACPs = azires;
  
  // copy over two copies of the RADARSITE and two copies of the GPS structures
  DO_COPY(2 * (sizeof(t_SSB_radarsite) +sizeof(t_SSB_gps)));

  // copy over an empty plen record
  DO_COPY(sizeof(t_SSB_plen));

  // skip remaining plen records in old file
  old += onplen * sizeof(t_SSB_plen);

  // create pulse length entries, one per mode

  for (i = 0; i < nplen; ++i) {
    // look up the prf for this row
    for (j = 0; j < nprf; ++j)
      if (REAL(prfsxp)[j] == REAL(df_prf)[i])
	break;
    if (j == nprf)
      error("seascan:create_BIN_bytes: value of PRF at row %d in table is %f Hz, which is not valid", i + 1, REAL(df_prf)[i]);
    splen.nsPulseLength = (int) INTEGER(df_plen)[i];
    splen.minPRFIndex = splen.maxPRFIndex = j + 1;  // for prf yet to be created
    splen.modeIndex = i + 1;  // for mode yet to be created
    DO_ADD(splen);
  }

  // copy over an empty prf record
  DO_COPY(sizeof(t_SSB_prf));

  // create pulse repetition frequency entries

  for (i = 0; i < nprf; ++i) {
    // copy over values from 1st non-empty old prf record
    sprf = * ((t_SSB_prf *) old);

    // make midpoints between adjacent PRFs the range endpoints,
    // adding 1 at the bottom and 16384 at the top.  The range limits
    // are the mean of the adjacent inter-pulse intervals, in microseconds

    sprf.LowerLimit = (i > 0)        ? (PRF_TO_LIMIT(REAL(prfsxp)[i]) + PRF_TO_LIMIT(REAL(prfsxp)[i - 1])) / 2 : 1;
    sprf.UpperLimit = (i < nprf - 1) ? (PRF_TO_LIMIT(REAL(prfsxp)[i]) + PRF_TO_LIMIT(REAL(prfsxp)[i + 1])) / 2 : 16384;

    // The PRF window length.  Although we don't use it, it should be set
    // to 3 (the minimum) to keep SeaScanRCU happy.
    sprf.WindowLength = 3;

    // find the first modeline for which this is the prf, and use its plen
    // as the default for this prf, and its number of pulses per scan (divided
    // by 4) as the quadrant size.

    for (j = 0; j < nmode; ++j)
      if (REAL(prfsxp)[i] == REAL(df_prf)[j])
	break;
    if (j == nmode)
      error("seascan:create_BIN_bytes: no modeline has a PRF of %f Hz", REAL(prfsxp)[i]);
    sprf.QuadrantSize = INTEGER(df_np)[j] / 4;
    sprf.DefaultPLEN = 1 + j;

    DO_ADD(sprf);
  }

  // skip remaining prf records in old file
  old += onprf * sizeof(t_SSB_prf);

  // copy over an empty mode record
  DO_COPY(sizeof(t_SSB_mode));

  // create mode entries

  for (i = 0; i < nmode; ++i) {
    // copy over values from 1st non-empty old mode record
    smode = * ((t_SSB_mode *) old);
    smode.SamplesPerPulse = (int) INTEGER(df_sc)[i];
    for (j = 0; j < NUM_SAMPLING_FREQ; ++j)
      if (sampling_freq[j] == REAL(df_sr)[i])
	break;
    if (j == NUM_SAMPLING_FREQ)
      error ("seascan:create_BIN_bytes: value of Sampling Freq at row %d in table is %f MHz, which is not valid.  Use seascan.get.freqs() to get allowed values.", i + 1, REAL(df_sr)[i]);
    smode.PixelConfig = pixel_config[j];
    DO_ADD(smode);
  }
  // skip remaining mode records in old file
  old += onmode * sizeof(t_SSB_mode);

  // copy over old gyro record
  DO_COPY(sizeof(t_SSB_gyro));

  UNPROTECT(7);
  return rv;
}
  

SEXP
get_a2d_status(SEXP svnsxp) {
  // return the hardware status info from the given server
  // including the timestamp from the current processed header
  // or R_NilValue on error

  int svn = INTEGER(AS_INTEGER(svnsxp))[0];
  SEXP rv;
   
  t_seascan *me;
  DATA_HEADER *dh;
  struct tm tm;
  double *p;

  me = ensure_server(svn);
  if (!me)
    return FAIL_SEXP;

  dh = &me->dh;

  TRY_SEXP(XSigGetADStatus(&me->ad, me->svn));
  p = REAL(rv = allocVector(REALSXP, 22));
  *p++ = me->ad.Dbg;
  *p++ = me->ad.Sequence;
  *p++ = me->ad.Rotation;
  *p++ = me->ad.Pulse;
  *p++ = me->ad.Hdg;
  *p++ = me->ad.Quadrant;
  *p++ = me->ad.Hardware;
  *p++ = me->ad.Month;
  *p++ = me->ad.Day;
  *p++ = me->ad.Year;
  *p++ = me->ad.Fatal;  
  *p++ = me->ad.Valid; 
  *p++ = me->ad.Configured;
  *p++ = me->ad.Overrun; 
  *p++ = me->ad.Fault;
  *p++ = me->ad.Synchro; 
  *p++ = me->ad.Online;
  *p++ = me->ad.Running; 
  *p++ = me->ad.Started;
  *p++ = me->ad.ErrorCode; 
  *p++ = me->ad.ErrorParm;

  TRY_SEXP(XSigGetProcessing(dh, me->svn));

  // encode the broken-apart date/time in TimePosStamp into a time_t

  tm.tm_sec = dh->TimePosStamp.Time.Second;
  tm.tm_min = dh->TimePosStamp.Time.Minute;
  tm.tm_hour = dh->TimePosStamp.Time.Hour;
  tm.tm_mday = dh->TimePosStamp.Time.Day;
  tm.tm_mon = dh->TimePosStamp.Time.Month - 1;
  tm.tm_year = dh->TimePosStamp.Time.Year - 1900;

  // compute the real start-of-scan timestamp by subtracting the duration of a quarter scan,
  // and adding the milliseconds time offset.  FIXME:  The timestamps from seascan apparently
  // correspond to the last pulse in a quadrant; is this correct?

  *p++ = mktime(&tm) + dh->TimePosStamp.Time.Milliseconds / 1000.0;

  return rv;
}

SEXP
get_last_scan_info(SEXP svnsxp) {
  t_seascan *me;
  int svn = INTEGER(AS_INTEGER(svnsxp))[0];

  me = ensure_server(svn);
  if (!me)
    return FAIL_SEXP;
  return me->scan_info_vector;
}

#define ASCOPE_NUM_SAMPLES 256
static char ascope_data[ASCOPE_NUM_SAMPLES * SEASCAN_SAMPLE_SIZE];

SEXP
get_ascope_data(SEXP svnsxp, SEXP anglesxp) {
  t_seascan *me;
  int svn = INTEGER(AS_INTEGER(svnsxp))[0];
  SEXP rv;
  int i;
  int *rvp;

  me = ensure_server(svn);
  if (!me)
    return FAIL_SEXP;
  TRY_SEXP(XSigGetAscopeData(&me->dh, ascope_data, (float) REAL(AS_NUMERIC(anglesxp))[0], me->svn));
  rvp = INTEGER(rv = allocVector(INTSXP, ASCOPE_NUM_SAMPLES));
  for (i = 0; i < ASCOPE_NUM_SAMPLES; ++i)
    rvp[i] = ((t_seascan_sample *)ascope_data) [i];
  return (rv);
}

	   

R_CallMethodDef seascan_call_methods[]  = {
  // R hook functions
  MKREF(get_hosts, 2),
  MKREF(get_contents, 2),
  MKREF(end_of_data, 2),
  MKREF(get_scan_info, 6),
  MKREF(get_scan_data, 1),
  MKREF(start_up, 3),
  MKREF(shut_down, 1),
  MKREF(set_agc_params, 2),
  MKREF(set_mode, 2),
  MKREF(set_plen, 2),
  MKREF(get_proc, 1),
  MKREF(get_sampling_freqs, 0),
  MKREF(create_BIN_bytes, 4),
  MKREF(get_a2d_status, 1),
  MKREF(get_last_scan_info, 1),
  MKREF(get_ascope_data, 2),
  MKREF(flush_seascan_buffers, 1),
  {NULL, NULL, 0}
};

void
R_init_seascan(DllInfo *info)
{
  /* Register routines, allocate resources. */

  int i;

  R_registerRoutines(info, NULL, seascan_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);

  for (i=0; i < MAX_SERVERS_EX; ++i)
    THREAD_created_get_scan[i] = 0;
}

void
R_unload_seascan(DllInfo *info)
{
  /* Release resources by calling shutdown for each server */
  int i;
  for (i=0; i < MAX_SERVERS_EX; ++i) {
    if (radars[i]) {
      do_shut_down(radars[i]);
      Free(radars[i]);
      radars[i] = NULL;
    }
  }
}

