/*================================================================

  THIS FILE IS NOT USED 

  functions to interface directly from R to the SeaScan Client dll

  ================================================================*/


void
RSigQueryNumOfServers(int *ns, char **sn, int *err) {
  // ns: needs room for one integer
  // sn: needs room for 8 strings
  char ssn[8][16];
  unsigned short nns;

  *err = XSigQueryNumOfServersEx(&nns, ssn, 8);
  if (!*err) {
    int i;
    *ns = nns;
    for (i=0; i < nns; ++i) 
      strncpy(sn[i], ssn[i], 16);
  }
}

void
RSigGetServerStatus(int *svn, char **site, int *flags, int *err) {
  // site: needs room for 16 chars (including \0)
  // flags: needs room for 7 integers
  // we maintain internal copies of the status information
  
  __p_sig_fn_t save_segv_handler;

  save_segv_handler = signal(SIGSEGV, SIG_IGN);

  *err = XSigGetServerStatus(& ss, (SHORT) *svn);

  if (!*err) {
    
    strncpy(site[0], ss.SiteName, MAX_COMPUTER_NAME_LENGTH);
    strncpy(server_name[*svn], ss.SiteName, MAX_COMPUTER_NAME_LENGTH);
    SS_SET(*svn, ATODACTIVE,             flags[0] = ss.AtoDActive);
    SS_SET(*svn, TAPERECORDING,          flags[1] = ss.TapeRecording);
    SS_SET(*svn, TAPEPLAYBACK,           flags[2] = ss.TapePlayback);
    SS_SET(*svn, PLOTEXTRACTIONACTIVE,   flags[3] = ss.PlotExtractionActive);
    SS_SET(*svn, AGCACTIVE,              flags[4] = ss.AGCActive);
    SS_SET(*svn, RPCENABLED,             flags[5] = ss.RPCEnabled);
    SS_SET(*svn, SIGNALPROCESSINGACTIVE, flags[6] = ss.SignalProcessingActive);
  }
  signal(SIGSEGV, save_segv_handler);

}


void
RSigGetRPCStatus(int *cmd, int *srv, int *succ, int *err) {
  // cmd, srv, succ: need room for 10 integers each
  int i;
    
  *err = XSigGetRPCStatus(& rs);
  if (!*err) {
    for (i=0; i < 10; ++i) {
      cmd[i] = rs.commands[i].Command;
      srv[i] = rs.commands[i].DestServer;
      succ[i] = rs.commands[i].Success;
    }
  }
}

void
RSigGetADStatus(int *svn, int *parm, int *err) {
  // parm: requires room for 21 ints
  int i = 0;
  *err = XSigGetADStatus(& ad, (SHORT) *svn);
  if (!*err) {
    parm[i++] = ad.Dbg;
    parm[i++] = ad.Sequence;
    parm[i++] = ad.Rotation;
    parm[i++] = ad.Pulse;
    parm[i++] = ad.Hdg;
    parm[i++] = ad.Quadrant;
    parm[i++] = ad.Hardware;
    parm[i++] = ad.Month;
    parm[i++] = ad.Day;
    parm[i++] = ad.Year;
    parm[i++] = ad.Fatal;
    parm[i++] = ad.Valid;
    parm[i++] = ad.Configured;
    parm[i++] = ad.Overrun;
    parm[i++] = ad.Fault;
    parm[i++] = ad.Synchro;
    parm[i++] = ad.Online;
    parm[i++] = ad.Running;
    parm[i++] = ad.Started;
    parm[i++] = ad.ErrorCode;
    parm[i++] = ad.ErrorParm;
  }
}


void
RSigGetSignalProcessingParameters(int *svn, int *parms, int *err) {
  // parms: needs room for 12 integers

  *err = XSigGetSignalProcessingParameters(&pp, (SHORT) *svn);
  if (!*err) {
    parms[0] = pp.ScanAveragingEnabled;
    parms[1] = pp.ScansToAverage;		
    parms[2] = pp.Reserved1;
    parms[3] = pp.MotionCompEnabled;
    parms[4] = pp.ScanConversionEnabled;
    parms[5] = pp.CFAREnabled;
    parms[6] = pp.CFARWindowLength;	
    parms[7] = pp.CFAROffset;		
    parms[8] = pp.CFARRank;		
    parms[9] = pp.Reserved2;
    parms[10] = pp.PulseFilterEnabled;
    parms[11] = pp.FTCIndex;		
  }
}

void
RSigSetSignalProcessingParameters(int *svn, int *parms, int *err) {
  // parms: needs room for 12 integers
  pp.ScanAveragingEnabled = (BOOL) parms[0];
  pp.ScansToAverage = (USHORT) parms[1];
  pp.Reserved1 = (USHORT) parms[2];
  pp.MotionCompEnabled = (BOOL) parms[3];
  pp.ScanConversionEnabled = (BOOL) parms[4];
  pp.CFAREnabled = (BOOL) parms[5];
  pp.CFARWindowLength = (USHORT) parms[6];
  pp.CFAROffset = (SHORT) parms[7];
  pp.CFARRank = (USHORT) parms[8];
  pp.Reserved2 = (USHORT) parms[9];
  pp.PulseFilterEnabled = (BOOL) parms[10];
  pp.FTCIndex = (USHORT) parms[11];
  *err = XSigSetSignalProcessingParameters(&pp, (SHORT) *svn);
}


void
RSigGetProcessing(int *svn, double *dpar, int *err) {
  // dpar: needs room for 46 doubles
  int i=0;
  *err = XSigGetProcessing(&dh, (SHORT) *svn);
  if (!*err) {
    dpar[i++] = dh.TimePosStamp.Time.Year;
    dpar[i++] = dh.TimePosStamp.Time.Month;
    dpar[i++] = dh.TimePosStamp.Time.DayOfWeek;
    dpar[i++] = dh.TimePosStamp.Time.Day;
    dpar[i++] = dh.TimePosStamp.Time.Hour;
    dpar[i++] = dh.TimePosStamp.Time.Minute;
    dpar[i++] = dh.TimePosStamp.Time.Second;
    dpar[i++] = dh.TimePosStamp.Time.Milliseconds;
    dpar[i++] = dh.TimePosStamp.Latitude;	
    dpar[i++] = dh.TimePosStamp.Longitude;	
    dpar[i++] = dh.GPSTimeStamp;	
    dpar[i++] = dh.Heading;			
    dpar[i++] = dh.SpeedFromGPS;			
    dpar[i++] = dh.CourseFromGPS;			
    dpar[i++] = dh.SpeedFromLOG;			
    dpar[i++] = dh.CourseFromLOG;			
    dpar[i++] = dh.SpeedFromMAN;			
    dpar[i++] = dh.CourseFromMAN;			
    dpar[i++] = dh.Lines;		
    dpar[i++] = dh.SamplesPerLine;	
    dpar[i++] = dh.SingleQuadrant;	
    dpar[i++] = dh.QuadrantNumber;	
    dpar[i++] = dh.ScanConverted;	
    dpar[i++] = dh.StartBearing;	
    dpar[i++] = dh.AngleCoverage; 	
    dpar[i++] = dh.StartRange;	
    dpar[i++] = dh.RangeCoverage; 	
    dpar[i++] = dh.originX;	
    dpar[i++] = dh.originY;	
    dpar[i++] = dh.PRF;		
    dpar[i++] = dh.AntennaSpeed;	
    dpar[i++] = dh.NorthAligned;	
    dpar[i++] = dh.RangePerSample;	
    dpar[i++] = dh.SelectedModeNdx; 
    dpar[i++] = dh.ModeNdx;	        
    dpar[i++] = dh.SelectedPlenNdx; 
    dpar[i++] = dh.PlenNdx;	        
    dpar[i++] = dh.PulseLength;	
    dpar[i++] = dh.CFARed;
    dpar[i++] = dh.PulseFiltered;
    dpar[i++] = dh.MotionCompensated;
    dpar[i++] = dh.ScansAveraged;
    dpar[i++] = dh.FTCNdx;		
    dpar[i++] = dh.State;		
    dpar[i++] = dh.XpolImage;	
    dpar[i++] = dh.reserved;
  }
}

void RSigSetProcessing(int *svn, int *parms, int *err) {
  // parms needs room for 5 integers
  *err = XSigSetProcessing((USHORT) parms[0],
			   (USHORT) parms[1],
			   (USHORT) parms[2],
			   (SHORT) parms[3],
			   (SHORT) parms[4],
			   (SHORT) *svn);
}

void RSigGetData(int *svn, int *chan, int *n, int *quad, int *raw, int *data, int *err) {
  // data needs room for n ints, or half
  // that many if (*raw) is TRUE
  // if quad == -2, then get a whole frame (four quads)
  // otherwise, quad is in [-1, 3] and we get a quad
  int i;
  short *sp;
  int *ip;
  short getquad = -2 != *quad;
  if (getquad) {
    *err = XSigGetQuadrant(&dh, (BYTE *) data, (ULONG) *chan, (LONG) *quad, (SHORT) *svn);
    if (*err && *err != CLIENT_STATUS_QUAD_ERROR)
      return;
  } else {
    if((*err = XSigGetData(& dh, (BYTE *) data, (ULONG) *chan, (SHORT) *svn)))
      return;
  }
  // currently stored as two shorts per int
  // if requested, convert to one short per int
  // this must be done from back to front
  // (we're implicitly assuming n mod 2 == 0)
  if (!*raw) {
    ip = & data[(*n)-1];
    sp = ((short *) data) + (*n)-1;
    for (i = (*n); i > 0; --i)
      *ip-- = *sp--;
  }
}  

void RSigPlayback(int *svn, int *time, int *start, int *err) {
  *err = *start ? XSigStartPlayback((time_t) *time, (SHORT) *svn) : XSigStopPlayback((SHORT) *svn);
  if (!*err)
    SS_SET(*svn, TAPEPLAYBACK, *start);
}

void RSigClientPacing(int *svn, int *enable, int *err) {
  *err = *enable ? XSigEnableClientPacing((SHORT) *svn) : XSigDisableClientPacing((SHORT) *svn);
  if (!*err) 
    SS_SET(*svn, CLIENTPACING, *enable);
}

void
RSigNextImage(int *svn, int *err) {
  *err = XSigProcessNextImage((SHORT) *svn);
  return;
}

void
RSigGetSTCParameters(int *svn, int *enable, double *parms, int *err) {
  if((*err = XSigGetSTCParameters(&stc, (SHORT) *svn)))
    return;
  *enable = stc.Enabled;
  parms[0] = stc.MaximumAppliedRange;
  parms[1] = stc.MaximumAttenuation;
}

void
RSigSetSTCParameters (int *svn, int *enable, double *parms, int *err) {
  stc.Enabled = (BOOL) *enable;
  stc.MaximumAppliedRange = (float) parms[0];
  stc.MaximumAttenuation = (float) parms[1];
  *err = XSigGetSTCParameters(&stc, (SHORT) *svn);
}

void 
RSigGetAscopeData(int *svn, double *radial, int *data, int *err) {
  // data should store 256 ints
  int i;
  int *ip;
  short *sp;
  float rad = (float) *radial;
  if ((*err = XSigGetAscopeData(&dh, (BYTE *) data, rad, *svn))) 
    return;
  ip = & data[255];
  sp = ((short*) data) + 255;
  for (i=255; i >= 0; --i) 
    *ip-- = *sp--;
}

void
RSigScanConversion(int *svn, int *enable, int *err) {
  if (*enable)
    *err = XSigEnableScanConversion((SHORT) *svn);
  else
    *err = XSigDisableScanConversion((SHORT) *svn);
}

void
RSigMotionCompensation(int *svn, int *enable, int *err) {
  if (*enable)
    *err = XSigEnableMotionCompensation((SHORT) *svn);
  else
    *err = XSigDisableMotionCompensation((SHORT) *svn);
}

void RSigSetScans(int *svn, int *val, int *err) {
  *err = XSigSetScans((USHORT) *val, (SHORT) *svn);
}

void RSigSetPF(int *svn, int *val, int *err) {
  *err = XSigSetPF((SHORT) *val, (SHORT) *svn);
}

void RSigSetCFAR(int *svn, int *val, int *err) {
  *err = XSigSetCFAR((SHORT) *val, (SHORT) *svn);
}

void RSigSetDesiredMode(int *svn, int *val, int *err) {
  *err = XSigSetDesiredMode((USHORT) *val, (SHORT) *svn);
}

void RSigSetDesiredPlen(int *svn, int *val, int *err) {
  *err = XSigSetDesiredPlen((USHORT) *val, (SHORT) *svn);
}

void RSigGetTapeStatus(int *svn, int *val, char *label, int *err) {
  // val must have room for 10 ints
  int i=0;
  if((*err = XSigGetTapeStatus(& ts, (SHORT) *svn)))
    return;
  val[i++] = ts.device_present;
  val[i++] = ts.media_present;
  val[i++] = ts.media_WP;
  val[i++] = ts.media_initialized;
  val[i++] = ts.device_locked;
  val[i++] = ts.access_mode;
  val[i++] = ts.startup_mode;
  val[i++] = ts.data_mode;
  val[i++] = ts.overall_status;
  val[i++] = ts.tape_remaining;
  sprintf(label, "%.80s", ts.TapeLabel);
}


void RSigGetTapeContents(int *svn, int *val, int *err) {
  // val must have room for 2 + 3 * MAX_DATA_BLOCKS = 152 integers
  if((*err = XSigGetTapeContents(&tc, (SHORT) *svn)))
    return;
  copy_tape_contents(val);
}

void RSigRadarChannel(int *svn, int *set, int *chan, int *err) {
  if (*set) 
    *err = XSigSetRadarChannel(*chan, (SHORT) *svn);
  else
    *err = XSigGetRadarChannel(chan, (SHORT) *svn);
}

void RSigRadarOnline(int *svn, int *online, int *err) {
  if (*online) 
    *err = XSigSetRadarOnline( (SHORT) *svn);
  else
    *err = XSigSetRadarOffline( (SHORT) *svn);
}

void RSigDisplayOrigin(int *svn, int *set, int *coords, int *err) {
  if (*set)
    *err = XSigSetDisplayOrigin(coords[0], coords[1], (SHORT) *svn);
  else
    *err = XSigGetDisplayOrigin(coords, coords+1, (SHORT) *svn);
}
    
void RSigMaskPoints(int *svn, int *set, int *which, int *enable, double *coords, int *numseg, int *numpts, int *err) {
  // coords must have room for up to 2 * MAX_NUM_MASK_POINTS doubles
  // numpts must have room for up to MAX_NUM_MASK_SEGMENTS integers
  
  // *set is true if values are sent to server, false if they are obtained from the server
  // *which is an integer selecting the source

  int i, j, k;
  BOOL en;

  if (*set) {
    for (i=0, k=0; i < *numseg; ++i)
      for (j=0; j < numpts[i]; ++j, ++k) {
	mc[k].Range = (float) coords[2*k];
	mc[k].Bearing = (float) coords[2*k+1];
      }
    en = (BOOL) (*enable) ? TRUE : FALSE;
    switch (*which) {
    case 0:
      *err = XSigSetDisplayClutterMapMaskPoints( (MASK_COORD *) &mc, *numseg, numpts, en, (SHORT) *svn);
      break;
    case 1:
      *err = XSigSetPlotExtractionMaskPoints( (MASK_COORD *) &mc, *numseg, numpts, en, (SHORT) *svn);
      break;
    case 2:
      *err = XSigSetPlotExtractionClutterMapMaskPoints( (MASK_COORD *) &mc, *numseg, numpts, en, (SHORT) *svn);
      break;
    default:
      *err = CLIENT_STATUS_INVALID_ARGUMENT;
    }
  } else {
    switch (*which) {
    case 0:
      *err = XSigGetDisplayClutterMapMaskPoints( (MASK_COORD *) &mc, numseg, numpts, &en, (SHORT) *svn);
      break;
    case 1:
      *err = XSigGetPlotExtractionMaskPoints( (MASK_COORD *) &mc, numseg, numpts, &en, (SHORT) *svn);
      break;
    case 2:
      *err = XSigGetPlotExtractionClutterMapMaskPoints( (MASK_COORD *) &mc, numseg, numpts, &en, (SHORT) *svn);
      break;
    default:
      *err = CLIENT_STATUS_INVALID_ARGUMENT;
    }
    if (!*err) {
      for (i=0, k=0; i < *numseg; ++i)
	for (j=0; j < numpts[i]; ++j, ++k) {
	  coords[2*k] = mc[k].Range;
	  coords[2*k+1] = mc[k].Bearing;
	}
      *enable = en;
    }
  } 
}      

#define MAXCLASS 4095
#define NUM_BUCKETS 1 + 2 * MAXCLASS
void entropy (int *x, int *n, double *rv ) {
  int p[NUM_BUCKETS];
  double e = 0.0;
  double s = 0.0;
  int i;
  for(i = 0; i < NUM_BUCKETS; ++i)
    p[i] = 0;
  for (i=0; i < *n ; ++i)
    ++p[x[i]+MAXCLASS];
  e = 0;
  s = 0;
  for (i = 0; i < NUM_BUCKETS; ++i) {
    s += p[i];
    if (p[i] > 0)
      e += p[i] * log(p[i]);
  }
  rv[0] = (log(s) - e / s) / log(2);
}


/*
 * format_win_error
 */
void
format_win_err(char *buff)
{
  LPVOID lpMsgBuf;

  FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER 
		| FORMAT_MESSAGE_FROM_SYSTEM 
		| FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		GetLastError(),
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPTSTR) &lpMsgBuf,
		0,
		NULL );

  strcpy(buff, lpMsgBuf);
  LocalFree(lpMsgBuf);
}

void print_win_err() {
  format_win_err(win_error_text);
  Rprintf(win_error_text);
}

void RSigLastError(char **err, int *len) {
  strcpy(win_error_text, win_error_function_name);
  strcat(win_error_text, ": ");
  format_win_err(win_error_text + strlen(win_error_text));
  strncpy(err[0], win_error_text, *len);
}
  
#define TRYX(x, f, y...) printf("Calling %s\n", #f); if(!(x = f(y))){*err = -2; strcpy(win_error_function_name, #f); return;}
#define TRY(f, y...) printf("Calling %s\n", #f); if(!f(y)){*err = -2; strcpy(win_error_function_name, #f); return;}
     
#define MKREF(X, N) {#X, (DL_FUNC) &X, N}



/*================================================================

  seascan.dll method registration, initialization, and destruction

  ================================================================*/

R_CMethodDef cMethods[]  = {

  // wrappers for SeaScan Client DLL

  MKREF(RSigQueryNumOfServers, 3),
  MKREF(RSigGetServerStatus, 4),
  MKREF(RSigGetRPCStatus, 4),
  MKREF(RSigGetADStatus, 3),
  MKREF(RSigGetSignalProcessingParameters, 3),
  MKREF(RSigSetSignalProcessingParameters, 3),
  MKREF(RSigGetProcessing, 3),
  MKREF(RSigSetProcessing, 3),
  MKREF(RSigGetData, 7),
  MKREF(RSigClientPacing, 3),
  MKREF(RSigPlayback, 4),
  MKREF(RSigGetAscopeData, 4),
  MKREF(RSigNextImage, 2),
  MKREF(RSigGetSTCParameters, 4),
  MKREF(RSigSetSTCParameters, 4),
  MKREF(RSigScanConversion, 3),
  MKREF(RSigMotionCompensation, 3),
  MKREF(RSigSetScans, 3),
  MKREF(RSigSetPF, 3),
  MKREF(RSigSetCFAR, 3),
  MKREF(RSigSetDesiredMode, 3),
  MKREF(RSigSetDesiredPlen, 3),
  MKREF(RSigGetTapeStatus, 4),
  MKREF(RSigGetTapeContents, 3),
  MKREF(RSigRadarChannel, 4),
  MKREF(RSigRadarOnline, 3),
  MKREF(RSigDisplayOrigin, 4),
  MKREF(RSigMaskPoints, 8),
  MKREF(RSigLastError, 2),
  {NULL, NULL, 0}
};                                                             
 
void
R_init_seascan(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, cMethods, NULL, NULL, NULL);
}

void
R_unload_seascan(DllInfo *info)
{
  /* Release resources. */

}
