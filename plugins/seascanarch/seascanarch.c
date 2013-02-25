/*================================================================

  seascanarch.c

  - functions to read and write SeaScan archived files directly
  for the radR seascanarch interface plugin

  by John Brzustowski 2006  john AT brz DOT ca  

==================================================================*/

#include "seascanarch.h"

// pointers for two ports, one a reader, the other a writer

t_ssa *ports[2];

static t_ssa *
ensure_port(t_ssa_port port) {
  // make sure the structure for this port has been allocated
  t_ssa *me;

  if (ports[port])
    return ports[port];
  
  me = Calloc(1, t_ssa);
  me->port = port;
  ports[port] = me;
  return me;
}

static SEXP
do_shut_down(t_ssa_port port, int final) {
  t_ssa * me = ports[port];
  if (!me)
    return PASS_SEXP;

  if (me->port == SSA_READER) {
    if (me->file) {
      fclose(me->file);
      me->file = NULL;
    }
  } else {
    // TODO: handle closing of the SSA_WRITER port
  }
  if (me->dir_buff)
    Free(me->dir_buff);
  me->scan_index = me->segment_index = 0;
  me->have_toc = me->have_scan_data = me->have_archive_dir = FALSE;
  if (final) {
    Free(me);
    ports[port] = NULL;
  }
  return PASS_SEXP;
}

SEXP
shut_down(SEXP portsxp) {
  t_ssa_port port = INTEGER(AS_INTEGER(portsxp))[0];
  return do_shut_down(port, FALSE);
}

SEXP
do_start_up(int port)
{
  t_ssa *me;

  if (!(me = ensure_port(port))) {
    error_code = RADR_ERROR_NOMEM;
    return FAIL_SEXP;
  }

  if (me->file)
    return PASS_SEXP;

  me->have_archive_dir = FALSE;
  me->use_PCTimestamp = 0;
  if (! (me->file = bigfopen(me->filename, port == SSA_READER ? "rb": "wb"))) {
    strncpy(extra_error_info, me->filename, EXTRA_ERROR_INFO_SIZE);
    *me->filename = '\0';
    error_code = RADR_ERROR_CANT_OPEN_ARCHIVE;
    return FAIL_SEXP;
  }
  me->segment_index = me->scan_index = 0;

  if (RADR_ERROR_NONE != read_archive_contents (me))  // sets an error code
    return FAIL_SEXP;

  if (!me->is_gated) {
    // a char extmat to hold temporary raw data
    me->data_block  = CREATE_EXTMAT(EXTMAT_TYPE_CHAR, "ungated data block");
    // a char extmat to hold temporary angle data
    me->angle_block = CREATE_EXTMAT(EXTMAT_TYPE_CHAR, "angle lookup table");
    // ungated data headers are read into me->brh2, so make it look like we did
    me->brh2 = me->brh;
    me->have_data_block = FALSE;
  }

  return me->is_gated ? PASS_SEXP : ScalarLogical(FALSE);
}

SEXP
start_up(SEXP portsxp)
{
  t_ssa_port port = INTEGER(AS_INTEGER(portsxp))[0];

  return do_start_up(port);
}


SEXP
set_filename (SEXP portsxp, SEXP fnsxp) {
  t_ssa_port port = INTEGER(AS_INTEGER(portsxp))[0];
  t_ssa *me;

  if (!(me = ensure_port(port))) {
    error_code = RADR_ERROR_NOMEM;
    return FAIL_SEXP;
  }

  strncpy(me->filename, CHAR(STRING_ELT(fnsxp, 0)), sizeof(me->filename));
  return PASS_SEXP;
}

SEXP
set_desired_azimuths (SEXP portsxp, SEXP azisxp) {
  t_ssa_port port = INTEGER(AS_INTEGER(portsxp))[0];
  t_ssa *me;

  if (!(me = ensure_port(port))) {
    error_code = RADR_ERROR_NOMEM;
    return FAIL_SEXP;
  }

  me->desired_azimuths = INTEGER(azisxp)[0];
  me->out_dtheta = 360.0 / me->desired_azimuths;
  return PASS_SEXP;
}

SEXP
set_skip_changeover_pulse (SEXP portsxp, SEXP skipsxp) {
  t_ssa_port port = INTEGER(AS_INTEGER(portsxp))[0];
  t_ssa *me;

  if (!(me = ensure_port(port))) {
    error_code = RADR_ERROR_NOMEM;
    return FAIL_SEXP;
  }

  me->skip_changeover_pulse = INTEGER(skipsxp)[0];
  return PASS_SEXP;
}

SEXP
set_max_azimuth_err (SEXP portsxp, SEXP errsxp) 
{
  t_ssa_port port = INTEGER(AS_INTEGER(portsxp))[0];
  t_ssa *me;

  if (!(me = ensure_port(port))) {
    error_code = RADR_ERROR_NOMEM;
    return FAIL_SEXP;
  }

  me->max_azimuth_err = REAL(errsxp)[0];
  return PASS_SEXP;
}

SEXP
set_use_pc_timestamp (SEXP portsxp, SEXP usepcsxp) 
{
  t_ssa_port port = INTEGER(AS_INTEGER(portsxp))[0];
  t_ssa *me;

  if (!(me = ensure_port(port))) {
    error_code = RADR_ERROR_NOMEM;
    return FAIL_SEXP;
  }

  me->use_PCTimestamp = INTEGER(usepcsxp)[0];
  return PASS_SEXP;
}


int
seek_archive_scan (t_ssa *me, int i, int intraseg) 
{
  // seek to the start of scan i in the archive,
  // return TRUE (and set me->scan_index) if successful, FALSE otherwise
  // if intraseg is TRUE, the index must be of an scan in the current segment
  // otherwise, the new segment index is computed
  

  int j;

  if ( ! me->file )
    return FALSE;

  if (i < 0  || i >= me->toc.TotalImages) {
    error_code = RADR_ERROR_SEEK_BEYOND_ARCHIVE;
    return FALSE;
  }
  if (intraseg 
	   && (i < me->segment_first_scan_index[me->segment_index]
	       || i >= me->segment_first_scan_index[me->segment_index + 1])) {
    error_code = RADR_ERROR_INVALID_RUN;
    return FALSE;
  }
  if (bigfseek(me->file, me->dir_buff[i].Position.QuadPart, SEEK_SET)) {
    error_code = RADR_ERROR_INVALID_ARCHIVE;
    return FALSE;
  }

  me->scan_index = i;

  if (!intraseg) {
    /* determine which segment this scan is from */
    /* NumSegments is limited to a small number, so efficiency here is not important */
    
    for (j = 0; j < me->toc.NumSegments && me->segment_first_scan_index[j] <= i; ++j)
      /* empty body */;
    
    me->segment_index = j - 1;
  } else {
    j = me->segment_index + 1;
  }

  return TRUE;
}

void
ensure_archive_dir_buff(t_ssa *me, int i)
{
  me->dir_buff = Realloc (me->dir_buff, i, DISK_DIRECTORY_ENTRY);
}

int
read_archive_contents (t_ssa *me) 
{
  // read the tape contents and scan directories of
  // the currently open archive

  int i, tot;
  TAPE_CONTENTS *toc;

  toc = &me->toc;

  if (bigfseek(me->file, 0, SEEK_SET))
    return RADR_ERROR_INVALID_ARCHIVE;

  if (! TRY_READ_CHUNK(me, arlab)
      || strcmp(me->arlab.SystemName, ARCHIVE_LABEL_TEXT))
    return RADR_ERROR_INVALID_ARCHIVE;

  // check for whether data is gated
  // by reading until a valid header is found
  // and then checking its Gated flag

  do {
    if (! TRY_READ_CHUNK(me, brh))
      return FALSE;
  } while (me->brh.TimeStamp == 0);
  
  me->is_gated = me->brh.Gated;

  if (me->is_gated) {
    if (bigfseek(me->file, SEASCAN_DISK_CHUNK_SIZE + me->arlab.directoryPosition.QuadPart, SEEK_SET))
      return RADR_ERROR_INVALID_ARCHIVE;

    if (! TRY_READ_CHUNK(me, toc))
      return RADR_ERROR_INVALID_ARCHIVE;

    ensure_archive_dir_buff(me, toc->TotalImages);

    if (1 != fread(me->dir_buff, sizeof(DISK_DIRECTORY_ENTRY) * toc->TotalImages, 1, me->file))
      return RADR_ERROR_INVALID_ARCHIVE;

    // we add an extra bogus segment to simplify the seek code
    for (i=0, tot=0; i < toc->NumSegments + 1; tot += toc->NumImages[i++] )
      me->segment_first_scan_index[i] = tot;
    me->segment_index = 0;
    me->scan_index = 0;
    me->have_archive_dir = TRUE;
  }
  return RADR_ERROR_NONE;
}

int
compare_directory_timestamps (const void *dde1, const void *dde2) 
{
  return ((DISK_DIRECTORY_ENTRY *)dde1)->TimeStamp - ((DISK_DIRECTORY_ENTRY *)dde2)->TimeStamp;
}

int
seek_archive_time (t_ssa *me, time_t t) 
{
  // assuming the archive file is open
  // and that static var tc holds the correct
  // archive contents, advance the archive file pointer
  // to the data object whose timestamp
  // (i.e. value of DISK_DIRECTORY_ENTRY.TimeStamp)
  // is as large as possible but not greater than t
  // return TRUE if successful, false otherwise
  // i.e. the scan which "contains" the time t
  // 
  // See Note under seek_archive_scan.

  int i, j;

  // check for sanity and bounds

  if ( !  me->file
       || t < me->toc.StartTime[0] 
       || t >= me->toc.StopTime[me->toc.NumSegments - 1] ) {
    error_code = RADR_ERROR_SEEK_BEYOND_ARCHIVE;
    return FALSE;
  }

  // see whether the about-to-be-read scan or 
  // any of its close neighbours (within +/- 2) 
  // encloses the requested timestamp

  for (i=max(0, (me->scan_index - 2)); i < (min(me->toc.TotalImages, me->scan_index + 2)); ++i)
    if (t >= me->dir_buff[i].TimeStamp
	&& (i == me->toc.TotalImages - 1 || t < me->dir_buff[i+1].TimeStamp))
      return seek_archive_scan (me, i, FALSE);
  
  // see whether the timestamp is outside of any segments

  {
    for (i=0; i < me->toc.NumSegments; ++i)
      if (t >= me->toc.StartTime[i] && t <= me->toc.StopTime[i])
	goto FOUND_RUN;
    error_code = RADR_ERROR_SEEK_BEYOND_ARCHIVE;
    return FALSE;
  }
 FOUND_RUN:

  // estimate where the scan corresponding to t should be, assuming a linear, contiguous segment

  j = (t - me->toc.StartTime[i]) * (me->toc.NumImages[i] - 1) / (me->toc.StopTime[i] - me->toc.StartTime[i]);

  // back up until we are earlier than t

  while (me->dir_buff[j].TimeStamp > t && j > me->segment_first_scan_index[i])
    --j;
  
  // go forward until we find the first block after t

  while (j < me->toc.TotalImages && me->dir_buff[j].TimeStamp <= t)
    ++j;

  /* j is the index of a scan with a timestamp later than that sought.
     So the 0-degree quadrant of the scan containing the timestamp sought
     will be among the previous 4 quadrants.  We subtract 4 from j
     to guarantee that the next call to read_archive_next_scan_hdr_full() will pick
     up this target quadrant.

     FALSE specifies that this is not necessarily in the current segment */

  return seek_archive_scan (me, max(0, j-4), FALSE);
}

double make_timestamp (SEASCAN_REALTIME *t, int milliseconds) {
  // create a double GMT timestamp from a broken-down Seascan-style timestamp
  // adding in a possible offset in milliseconds
  struct tm tm;
  // encode the broken-apart date/time in TimePosStamp into a time_t

  tm.tm_sec = t->Second;
  tm.tm_min = t->Minute;
  tm.tm_hour = t->Hour;
  tm.tm_mday = t->Day;
  tm.tm_mon = t->Month - 1;
  tm.tm_year = t->Year - 1900;
  tm.tm_isdst = 0;

  return mktime(&tm) + (t->Milliseconds + milliseconds) / 1000.0;
}

int
read_archive_scan_hdr (t_ssa *me, int filldh) 
{
  // read the scan header from the archive at the current
  // archive scan index.  Return true if we succeed, 0 otherwise.  Also, if
  // filldh is non-zero, fill in the static DATA_HEADER structure dh
  // with information from this header.  the file pointer is left at
  // the start of the data for this data object, if successful.

  DATA_HEADER *dh = &me->dh;

  if (me->scan_index >= me->segment_first_scan_index[me->segment_index + 1])
    return FALSE;

  dbgprintf("Seeking to seascan archive scan_index = %d\n", me->scan_index);

  if (! seek_archive_scan (me, me->scan_index, TRUE))
    return FALSE;

  ++ me->scan_index;

  if (! TRY_READ_CHUNK(me, brh))
    return FALSE;

  if (!filldh)
    return TRUE;

  // FIXME?:  this apparently marks a recorded but invalid data block
  if (!me->brh.TimeStamp)
    return FALSE;

  dh->TimePosStamp.Time.Year = me->brh.PCTimeStamp.wYear;
  dh->TimePosStamp.Time.Month = me->brh.PCTimeStamp.wMonth;
  dh->TimePosStamp.Time.DayOfWeek = me->brh.PCTimeStamp.wDayOfWeek;
  dh->TimePosStamp.Time.Day = me->brh.PCTimeStamp.wDay;
  dh->TimePosStamp.Time.Hour = me->brh.PCTimeStamp.wHour;
  dh->TimePosStamp.Time.Minute = me->brh.PCTimeStamp.wMinute;
  dh->TimePosStamp.Time.Second = me->brh.PCTimeStamp.wSecond;
  dh->TimePosStamp.Time.Milliseconds = me->brh.PCTimeStamp.wMilliseconds;

  dh->TimePosStamp.Latitude = me->brh.Latitude;
  dh->TimePosStamp.Longitude = me->brh.Longitude;
  dh->GPSTimeStamp = me->brh.TimeStamp;
  dh->Heading = me->brh.Heading;
  dh->Lines = me->brh.AssociatedAzimuths;
  dh->SamplesPerLine = me->brh.Samples;
  dh->SingleQuadrant = FALSE;
  // FIXME: we probably don't deal correctly with scan-converted data
  dh->ScanConverted = me->brh.current.ScanConversion;
  dh->StartBearing = me->brh.StartAngle;
  dh->AngleCoverage = me->brh.Degrees;
  dh->StartRange = me->brh.StartRange * METERS_PER_NMI;
  // the rangecell value reflects round-trip travel time,
  // so halve it before computing range
  dh->RangeCoverage = me->brh.usecRangeCell * 1.0E-6 / 2 * VELOCITY_OF_LIGHT * me->brh.Samples;
  // we want Hz, PulsePeriod is in usec
  dh->PRF = floor (0.5 + 1.0E6 / me->brh.BandwidthStatus.PulsePeriod);  
  // we want RPM; rotation time is in milliseconds
  dh->AntennaSpeed = floor (0.5 + 60000 / me->brh.BandwidthStatus.RotationTime);
  // FIXME?: is this correct?
  dh->NorthAligned = me->brh.NorthStabilized;  
  dh->RangePerSample = me->brh.usecRangeCell * 1.0E-6 / 2 * VELOCITY_OF_LIGHT;
  dh->SelectedModeNdx = me->brh.BandwidthStatus.SelectedModeNdx;
  dh->ModeNdx = me->brh.BandwidthStatus.ModeNdx;
  dh->SelectedPlenNdx = me->brh.BandwidthStatus.SelectedPlenNdx;
  dh->PlenNdx = me->brh.BandwidthStatus.PlenNdx;
  dh->PulseLength = me->brh.BandwidthStatus.PulseLength;
  dh->CFARed = me->brh.current.CFAR;
  dh->PulseFiltered = me->brh.current.PulseFiltering;
  dh->MotionCompensated = me->brh.current.MotionCompensation;
  dh->ScansAveraged = me->brh.current.ScanAveraging;
  dh->FTCNdx = me->brh.current.FTC;
  dh->State = 0;

  dbgprintf ("%d,%d,%d,%d,%d,%d,%d,%ld,%.3f\n", me->brh.PCTimeStamp.wYear, me->brh.PCTimeStamp.wMonth, me->brh.PCTimeStamp.wDay, me->brh.PCTimeStamp.wHour, me->brh.PCTimeStamp.wMinute, me->brh.PCTimeStamp.wSecond, me->brh.PCTimeStamp.wMilliseconds, me->brh.TimeStamp, me->brh.StartAngle);

  /* if this is the last header block for this scan, then record its timestamp */
  if (dh->StartBearing + dh->AngleCoverage == 360.0) {
    if (me->use_PCTimestamp) {
      me->time_end_latest_block = make_timestamp (&dh->TimePosStamp.Time, dh->TimePosStamp.Time.Milliseconds);
    } else {
      me->time_end_latest_block = make_timestamp (&dh->TimePosStamp.Time, 0);
    }
  }
  return TRUE;
}

int
read_archive_next_scan_hdr_full (t_ssa *me)
{
  // read scan headers until we get one whose angle coverage
  // starts at zero
  // return TRUE if sucessful (leaving the file pointer at
  // the start of the Data portion of the corresponding Data Object)
  // return FALSE otherwise (leaving the file pointer in an undefined location) 

  for (;;) {
    if (! read_archive_scan_hdr(me, TRUE)) {
      return FALSE;
    }
    if (me->dh.StartBearing == 0.0) {
      return TRUE;
    }
  }
}

void
expand_12_bit_packed (char *buf, int n)
{
  // expand the n packed 12-bit values pointed to by buf
  // into 16 bit shorts at the same location
  // do this from right to left to avoid clobbering the
  // input; we assume n is even and positive
  //
  // the nibble-packing order is as follows:
  //
  // input:     byte0    byte1    byte2
  // nibble:    A   B    C   D    E   F
  //            lo hi    lo hi    lo hi     
  //
  // output:    short0           short1
  //            A   B   C   0    D   E   F   0
  //            lo         hi    lo         hi
      
  unsigned short *out = (unsigned short *) buf + (n - 2);
  unsigned char *in = (unsigned char *) buf + (3 * n / 2 - 3);
  unsigned char *inlo = (unsigned char *) buf + 9;  // the threshold at which we need to be more careful
  register unsigned short out0, out1;
  // loop over 3 input bytes, 2 output shorts 
  // at a time

  // FIXME:  deal gracefully with odd n instead of doing nothing
  if (n & 1 || n < 1) return;

  while(in >= inlo) {
    out[0] = in[0] + (((unsigned short) in[1] & 0x0f) << 8);
    out[1] = ((unsigned short) in[2] << 4) + (in[1] >> 4);
    out -= 2;
    in -= 3;
  }
  // special slower case for the last few iterations where input and output
  // overlap; calculate the first output in a register so as not to clobber
  // input for the second output.

  inlo = (unsigned char *)buf;
  while(in >= inlo) {
    out0 = in[0] + (((unsigned short) in[1] & 0x0f) << 8);
    out1 = ((unsigned short) in[2] << 4) + (in[1] >> 4);
    out[0] = out0;
    out[1] = out1;
    // the bounds-checking-gcc-compiled version of radR
    // generates
    //   Bounds warning: created an ILLEGAL pointer in pointer arithmetic.
    // for the next two lines on the last iteration of this loop.
    // This can be safely ignored.
    out -= 2;
    in -= 3;
  }
}

int
read_archive_next_scan_full (t_ssa *me, char *buffer) 
{
  // read and convert a full scan of gated data and store 
  // it in buffer.
  //
  // Assumptions
  // 
  // - a call has been made to read_archive_next_scan_hdr_full() to
  // determine the size of the scan data
  //
  // - the me->file file pointer is at the start of the Data part of
  // the Data Object for the first scan (i.e. quadrant) in the scan
  //
  // - the static structures dh and brh hold the headers for this Data Object
  //
  // - buffer has space to hold dh.Lines * dh.LinesPerSample samples
  // of size 16 bits, rounded up to the nearest SEASCAN_DISK_CHUNK_SIZE bytes
  //
  
  // This function returns TRUE if it is succesful in reading
  // consecutive quadrants (or whatever fractions of the circle are
  // used) adding up to a full frame of angular coverage. Otherwise,
  // it returns FALSE.  This can happen if the storage mode changes
  // during the scan.
  
  DATA_HEADER *dh 	= &me->dh;
  BASE_RADAR_HDR *brh 	= &me->brh;
  int ang 		= 0;						/* angle of start of data */
  int rows 		= dh->Lines * 360 / dh->AngleCoverage;	/* number of lines of data left to read */
  int cols 		= dh->SamplesPerLine; 			/* number of samples per line to read */


  for(;;) {
    // check that the angle, coverage, and resolution are consistent
    if (ang != brh->StartAngle 
	|| ang + brh->Degrees > 360.0
	|| brh->yExtent > rows
	|| brh->Samples != cols ) 
      return FALSE;

    // read the current segment of data
    // Don't round up to 1k, since that may exceed pre-allocated storage. The caller
    // should not need to know about archive file chunkiness.

    if (1 != fread (buffer, brh->yExtent * brh->rAxisBytes, 1, me->file))
      return FALSE;
  
    // if we're reading packed 12-bit samples, unpack them into 16 bits
    // this test works but does not seem robust
    // FIXME:  this won't work unless the number of padding bytes at
    // the end of each line is equal to zero (mod 3);
    // FIXME: This works for 16 bit samples and packed or unpacked
    // 12 bit samples, but not for 8 bit samples.

    if (brh->BitsPerSample == 16 && brh->rAxisBytes < 2 * brh->Samples)
      expand_12_bit_packed (buffer, brh->rAxisBytes * 2 * brh->yExtent / 3);

    // update the buffer pointer
    buffer += brh->Samples * brh->yExtent * sizeof (t_sample);

    // record the amount and coverage of data read
    ang += brh->Degrees;
    rows -= brh->yExtent;
    
    if (rows == 0)
      break;

    // read the next scan header; serves to position the file pointer
    // to the next data block and to record the time for the end of
    // the last data block in this scan

    read_archive_scan_hdr (me, TRUE);
  }
  return TRUE;
}


void
cleanup_toc (t_ssa *me)
{
  // for gated data, remove any partial scans from the start
  // and end of each run in the table of contents, and adjust
  // segment start times back 1/4 scan, based on antenna RPM

  TAPE_CONTENTS *toc = &me->toc;
  DATA_HEADER save_dh;
  char is_valid[MAX_DATA_BLOCKS];
  int i, j, have_first;
  
  memset(is_valid, 0, MAX_DATA_BLOCKS);
  have_first = FALSE; // have we read the header from the first valid run?

  for (i = 0; i < toc->NumSegments; ++i) {
    seek_archive_scan(me, me->segment_first_scan_index[i], FALSE);
    // Find the first scan header that starts at 0 degrees; this is
    // the first "full" scan.
    // We allow for some bad scan headers.
    
    for (j = 0; j < MAX_BAD_SCAN_HEADERS && j < toc->NumImages[i]; ++j) {
      if (read_archive_scan_hdr(me, TRUE) && me->dh.StartBearing == 0.0)
	break;
    }
    if (!me->brh.TimeStamp || me->dh.StartBearing != 0.0) {
      /* there do not seem to be any full scans in this segment */
      is_valid[i] = FALSE;
      continue;
    }
    if (!have_first) {
      have_first = TRUE;
      save_dh = me->dh;
    }

    // Adjust the first scan index upward to drop the incomplete scan
    me->segment_first_scan_index[i] += j;
    if (me->use_PCTimestamp) {
      toc->StartTime[i] = make_timestamp(&me->dh.TimePosStamp.Time, me->dh.TimePosStamp.Time.Milliseconds);
    } else {
      toc->StartTime[i] = me->brh.TimeStamp - 15.0 / me->dh.AntennaSpeed;
    }

    // Starting at the end of the segment, look for a 4th scan quadrant
    j = me->segment_first_scan_index[i + 1] - 1;

    while (j > me->segment_first_scan_index[i]) {
      seek_archive_scan(me, j, FALSE);
      if (read_archive_scan_hdr(me, TRUE) && me->dh.StartBearing == 270.0)
	break;
      --j;
    }

    if (j > me->segment_first_scan_index[i]) {
    // Continue backing up until the 1st scan quadrant
      while (j > me->segment_first_scan_index[i]) {
	seek_archive_scan(me, j, FALSE);
	if (read_archive_scan_hdr(me, TRUE) && me->dh.StartBearing == 0.0)
	  break;
	--j;
      }

      if (j > me->segment_first_scan_index[i]) {
	is_valid[i] = TRUE;
        if (me->use_PCTimestamp) {
          toc->StopTime[i] = make_timestamp(&me->dh.TimePosStamp.Time, me->dh.TimePosStamp.Time.Milliseconds);
        } else {
          toc->StopTime[i] = me->brh.TimeStamp - 15.0 / me->dh.AntennaSpeed;
        }
	toc->NumImages[i] = j - me->segment_first_scan_index[i] + 4;
      }
    }
  }

  // compress out the invalid segment entries
  // copy valid ones from slot j to slot i

  for (i = 0, j = 0; j < toc->NumSegments; ++j) {
    if (is_valid[j]) {
      toc->NumImages[i] = toc->NumImages[j];
      toc->StartTime[i] = toc->StartTime[j];
      toc->StopTime[i] = toc->StopTime[j];
      ++i;
    }
  }

  // leave the file pointer at the first valid scan
  if ((toc->NumSegments = i) > 0) 
    seek_archive_scan(me, me->segment_first_scan_index[0], FALSE);

  // restore the data header for that scan
  me->dh = save_dh;
}


SEXP
get_contents (SEXP portsxp) {
  t_ssa_port port = INTEGER(AS_INTEGER(portsxp))[0];
  t_ssa *me = ensure_port(port);
  TAPE_CONTENTS *toc = &me->toc;
  SEXP rv;
  int i;

  switch (port) {
  case SSA_WRITER:
    // FIXME:  provide the TOC even if this is the writer.
    // it should be a TOC of what has been written so far.
    break;

  case SSA_READER:
    if (!me->have_archive_dir)
      if(do_start_up (port) == FAIL_SEXP)
	return FAIL_SEXP;
      
    if (me->is_gated)
      cleanup_toc(me);

    PROTECT(rv = allocVector(INTSXP, toc->NumSegments * 3)); // WARNING:  Y2037K time_t != int issue?
    for (i = 0; i < toc->NumSegments; ++i) {
      INTEGER(rv)[3*i] = toc->NumImages[i] / 4; /* convert number of quadrant scans to number of full scans */
      INTEGER(rv)[3*i+1] = toc->StartTime[i];
      INTEGER(rv)[3*i+2] = toc->StopTime[i];
    }
    me->have_toc = TRUE;
    UNPROTECT(1);
    return rv;
    break;
  default:
    break;
  };
  return FAIL_SEXP;
}
  
SEXP
get_scan_info(SEXP portsxp) {
  // get the header info for the next available scan
  // fail if no header is available
  // repeated calls to get_scan_info will return headers
  // for different scans
  // We return the estimated duration of the scan, based
  // on the angle coverage and duration of the first chunk.

  int port = INTEGER(AS_INTEGER(portsxp))[0];
  t_ssa *me = ports[port];
  double time_end_this_block;
  DATA_HEADER *dh;
  SEXP rv;

  if (!me)
    return FAIL_SEXP;

  me->have_scan_data = FALSE;

  if (!(me->is_gated ? read_archive_next_scan_hdr_full : read_archive_next_scan_hdr_ungated) (me)) {
    error_code = RADR_ERROR_EOF_ON_PORT;
    return FAIL_SEXP;
  }

  dh = &me->dh;

  PROTECT(rv = allocVector(VECSXP, 11));
  SET_VECTOR_ELT(rv, 0, ScalarInteger(floor(0.5 + dh->Lines * 360.0 / dh->AngleCoverage)));  // scale first quadrant pulse count to entire scan
  SET_VECTOR_ELT(rv, 1, ScalarInteger(dh->SamplesPerLine));
  SET_VECTOR_ELT(rv, 2, ScalarInteger(12)); // bits per sample

  // the timestamp in this header corresponds to the time immediately after the last pulse in this
  // block (quadrant, typically)

    if (me->use_PCTimestamp) {
     time_end_this_block = make_timestamp (&dh->TimePosStamp.Time, dh->TimePosStamp.Time.Milliseconds);
    } else {
      time_end_this_block = make_timestamp (&dh->TimePosStamp.Time, 0);
    }

  if (me->time_end_latest_block >= 0) {
    // If we have the timestamp for the end of the previous data block, then we use it as the start
    // time for this block, and estimate a duration.
    dbgprintf("Estimating start time and duration from time_end_latest_block=%.3f\n", me->time_end_latest_block);
    SET_VECTOR_ELT(rv, 3, ScalarReal(me->time_start_this_block = me->time_end_latest_block));
    SET_VECTOR_ELT(rv, 4, ScalarInteger((time_end_this_block - me->time_end_latest_block) * 1000 * (360 / dh->AngleCoverage)));
  } else {
    // we don't have the time for the end of the previous block, so we approximate it using
    // the antenna rotation speed and fraction of the scan covered by this block
    dbgprintf("Estimating start time and duration from rpm:%f", me->time_end_latest_block);
    SET_VECTOR_ELT(rv, 4, ScalarInteger(floor(0.5 + 60000.0 / dh->AntennaSpeed))); // duration in ms; antenna speed is in rpm 
    if (me->use_PCTimestamp) {
      me->time_start_this_block = make_timestamp(&dh->TimePosStamp.Time, dh->TimePosStamp.Time.Milliseconds);
    } else {
      me->time_start_this_block = make_timestamp(&dh->TimePosStamp.Time, - REAL(VECTOR_ELT(rv, 4))[0] * dh->AngleCoverage / 360.0);
    }
    SET_VECTOR_ELT(rv, 3, ScalarReal(me->time_start_this_block));
  }
  SET_VECTOR_ELT(rv, 5, ScalarReal(dh->RangePerSample));  
  SET_VECTOR_ELT(rv, 6, ScalarReal(dh->StartRange));	// distance to first sample
  SET_VECTOR_ELT(rv, 7, ScalarReal(dh->NorthAligned ? 0 : dh->Heading)); // FIXME: how should this work?
  SET_VECTOR_ELT(rv, 8, ScalarInteger(+1)); // orientation.  FIXME: how would we tell whether clockwise or counterclockwise?
  SET_VECTOR_ELT(rv, 9, ScalarReal(dh->TimePosStamp.Latitude));
  SET_VECTOR_ELT(rv, 10, ScalarReal(dh->TimePosStamp.Longitude));
  UNPROTECT(1);

  me->have_scan_data = TRUE;
  return(rv);
}

SEXP
get_scan_data(SEXP portsxp, SEXP buffsxp) {
  // get the data from the current scan
  // returning an INTSXP representing the scan duration in milliseconds,
  // or FAIL_SEXP on error

  int port = INTEGER(AS_INTEGER(portsxp))[0];
  t_ssa *me = ports[port];
  DATA_HEADER *dh;
  t_extmat *scan_buff = SEXP_TO_EXTMAT(buffsxp);

  if (!me || !me->have_scan_data) {
    error_code = RADR_ERROR_EOF_ON_PORT;
    return FAIL_SEXP;
  }

  dh = &me->dh;

  (*pensure_extmat)(scan_buff, dh->Lines * 360 / dh->AngleCoverage, dh->SamplesPerLine);

  if (!(me->is_gated ? read_archive_next_scan_full : read_archive_next_scan_ungated) (me, (char *) scan_buff->ptr)) {
    error_code = RADR_ERROR_EOF_ON_PORT;
    return FAIL_SEXP;
  }
    
  me->have_scan_data = FALSE;
    
  // return measured scan duration (in milliseconds), to replace
  // estimate, unless we don't have the end of the previous block, in
  // which case we estimate.

  return ScalarInteger(floor(me->time_end_latest_block ? 1000 * (me->time_end_latest_block - me->time_start_this_block) :
			     0.5 + 60000.0 / dh->AntennaSpeed)); 
}

SEXP
end_of_data(SEXP portsxp) {
  // have we reached the end of data for this segment in the archive?
  int port = INTEGER(AS_INTEGER(portsxp))[0];
  t_ssa *me = ports[port];
  TAPE_CONTENTS *toc;

  if (!me)
    return FAIL_SEXP;

  toc = &me->toc;

  if (me->is_gated)
  // if the last timestamp 
    //    return LOGICAL_SEXP(me->scan_index - me->segment_first_scan_index[me->segment_index] > toc->NumImages[me->segment_index] - 4 || dh->GPSTimeStamp > toc->StopTime[me->segment_index]);
    return LOGICAL_SEXP(me->scan_index - me->segment_first_scan_index[me->segment_index] > toc->NumImages[me->segment_index] - 4);
  // for ungated data, the error will be generated when it is encountered - we don't
  // really have a way of telling other than just reaching the end of file
  return ScalarLogical(FALSE);
}

SEXP
seek_time(SEXP portsxp, SEXP time) {
  int port = INTEGER(AS_INTEGER(portsxp))[0];
  t_ssa *me = ports[port];

  time_t t = (time_t) INTEGER(AS_INTEGER(time))[0];

  if (!seek_archive_time(me, t)) 
    return FAIL_SEXP;

  return LOGICAL_SEXP(1);
}

SEXP
seek_scan(SEXP portsxp, SEXP runsxp, SEXP scansxp) {
  // seek to the specified scan of the specified segment
  // if runsxp is -1, use the current segment

  // Note: if possible, we seek to the header for the datablock
  // immediately preceding the desired data block in the same segment.
  // That way, read_archive_scan_hdr() will read the header for the
  // last portion of the preceding scan, which will give us the
  // timestamp for the end of the preceding scan, and hence the start
  // of the desired scan.  (Seascan archives record the time immediately after
  // the last pulse in each data block; we want the time of the first pulse.)
  // We always mark me->time_end_latest_block as invalid, but it will be 
  // given the correct value when read_archive_scan_hdr() reads the header
  // for the last data block in the previous scan.

  int port = INTEGER(AS_INTEGER(portsxp))[0];
  t_ssa *me = ports[port];
  int run = INTEGER(AS_INTEGER(runsxp))[0];
  // notice the "4 *" to convert a radR 360-degree scan number to an internal scan (i.e. quadrant) number
  int scan = 4 * INTEGER(AS_INTEGER(scansxp))[0];

  if (run < 0)
    run = me->segment_index;
  else
    me->segment_index = run;

  if (scan > 0)
    --scan;
  else
    me->time_end_latest_block = -1;

  if (!me || !seek_archive_scan(me, me->segment_first_scan_index[run] + scan, FALSE))
    return FAIL_SEXP;
  
  return LOGICAL_SEXP(1);
}

static SEXP
do_no_writing() {
  error_code = RADR_ERROR_NOT_IMPLEMENTED;
  strcpy (extra_error_info, 
	  "Seascanarch does not support writing seascan archive files.");
  return FAIL_SEXP;
}

SEXP
start_run(SEXP portsxp, SEXP start_t) {
  return do_no_writing();
}

SEXP
put_scan(SEXP portsxp, SEXP header, SEXP extmat) {
  return do_no_writing();
}

SEXP
end_run(SEXP portsxp, SEXP end_t) {
  return do_no_writing();
}

SEXP
is_started(SEXP portsxp) {
  int p = INTEGER(portsxp)[0];
  return ScalarLogical(ports[p] && (ports[p]->file != NULL));
}

static inline double
ang_diff (double theta1, double theta2) {
  // return the (smaller) absolute angle difference between
  // theta1 degrees and theta2 degrees
  double tmp;

  if (theta2 > theta1) {
    tmp = theta2; 
    theta2 = theta1;
    theta1 = tmp;
  }
  return (theta1 - theta2 < 180.0) ? theta1 - theta2 : 360 - (theta1 - theta2);
}

int
advance_to_next_ungated_block (t_ssa *me) {
  // on entry: the header block corresponding to 
  // me->prev_brh has been read, but its data has not
  // on exit: the data block has been read, the angles
  // have been skipped, the current brh2 has been copied
  // to brh; brh2 has the next header; me->in_dtheta and me->angle_range have been calculated

  if (me->have_data_block) {
    // save the last pulse from the current block into the zeroth slot
    memcpy(me->data_block.ptr, me->data_block.ptr + me->brh.AssociatedAzimuths * me->brh.rAxisBytes, me->brh.rAxisBytes);
    // save the last RSI_LUT from the current angle block into the zeroth slot
    memcpy(me->angle_block.ptr, me->angle_block.ptr + me->brh.AssociatedAzimuths * sizeof(RSI_LUT), sizeof(RSI_LUT));
    // remember what angle this pulse corresponds to
    me->prev_pulse_theta = me->brh.StartAngle +  (me->brh.AssociatedAzimuths - 1) * me->in_dtheta;
    // mark that the current input pulse is in slot 0
    me->input_index = 0;
  } else {
    // there is no data; we have just read in one header so far
    me->input_index = 1;
  }
  // copy next header to current header slot
  me->brh = me->brh2;
  // read next data block and skip its angles
  if (! TRY_READ_DATA(me))
    return FALSE;

  if (! TRY_READ_ANGLES(me))
    return FALSE;

  do {
    if ( !TRY_READ_CHUNK(me, brh2))
      return FALSE;
  } while (me->brh2.TimeStamp == 0);
  me->have_data_block = TRUE;
  // calculate new angle range and dtheta
  me->angle_range = ang_diff(me->brh2.StartAngle, me->brh.StartAngle);
  me->in_dtheta = me->angle_range / me->brh.AssociatedAzimuths;
  return TRUE;
}

int
read_archive_next_scan_hdr_ungated (t_ssa *me) {

  DATA_HEADER *dh = &me->dh;
  double block_time;
  double next_block_time;
  double pulse_time;

  // ensure we have a current data block
  if (! me->have_data_block) {
    if (! advance_to_next_ungated_block(me))
      return FALSE;
  }

  // read until we get a header containing the start angle of zero
  for(;;) {
    if (me->brh.StartAngle <= me->out_dtheta / 2) {
      // the first pulse of the current data block is close enough to angle zero
      me->input_index = 1; // data will be read in after the first row
      me->prev_pulse_theta = me->brh.StartAngle;
      break;
    } else if (me->brh2.StartAngle <= me->out_dtheta / 2) {
      // the first pulse of the next data block is close enough to angle zero
      // do nothing, since the loop will handle this
      ; /* do nothing */
    } else if (me->brh2.StartAngle < me->brh.StartAngle) {
      // the pulse corresponding to angle zero is contained in the current data block
      me->input_index = 1 + (int) ((360.0 - me->brh.StartAngle) / me->angle_range * me->brh.AssociatedAzimuths + 0.5);
      // record its actual theta
      me->prev_pulse_theta = fmod(me->brh.StartAngle + (me->input_index - 1) * me->angle_range / me->brh.AssociatedAzimuths, 360.0);
      break;
    }
    // move to the next header
    if (! advance_to_next_ungated_block(me))
      return FALSE;
  }

  // loop post condition: me->brh is the header for data contained in me->data_block
  // me->brh2 is the header for the next data block
  // the file position is just before the next data block

  // fill in the scan_info header on the assumption we will get an entire scan
  dh->TimePosStamp.Time.Year = me->brh.PCTimeStamp.wYear;
  dh->TimePosStamp.Time.Month = me->brh.PCTimeStamp.wMonth;
  dh->TimePosStamp.Time.DayOfWeek = me->brh.PCTimeStamp.wDayOfWeek;
  dh->TimePosStamp.Time.Day = me->brh.PCTimeStamp.wDay;
  dh->TimePosStamp.Time.Hour = me->brh.PCTimeStamp.wHour;
  dh->TimePosStamp.Time.Minute = me->brh.PCTimeStamp.wMinute;
  dh->TimePosStamp.Time.Second = me->brh.PCTimeStamp.wSecond;

  // The zero pulse is part way through the data block.  Estimate a
  // start time for it.

  block_time = me->brh.TimeStamp + me->brh.PCTimeStamp.wMilliseconds / 1000.0;
  next_block_time = me->brh2.TimeStamp + me->brh2.PCTimeStamp.wMilliseconds / 1000.0;
  pulse_time = me->brh.TimeStamp + (me->input_index - 1) * (next_block_time - block_time) / me->brh.AssociatedAzimuths;

  dh->TimePosStamp.Latitude = me->brh.Latitude;
  dh->TimePosStamp.Longitude = me->brh.Longitude;

  dh->GPSTimeStamp = (int) pulse_time;

  // FIXME: this currently only calculates the milliseconds part of the pulse time in dh->TimePosStamp.Time
  // but this is the only part radR uses

  dh->TimePosStamp.Time.Milliseconds = (int) ((pulse_time - dh->GPSTimeStamp) * 1000 + 0.5);

  dh->Heading = me->brh.Heading;
  dh->SamplesPerLine = me->brh.Samples;

  // the following are set by fiat: we will accumulate one full scan worth of data
  // with desired_azimuths pulses
  dh->Lines = me->desired_azimuths;
  dh->SingleQuadrant = FALSE;
  dh->ScanConverted = FALSE;
  dh->StartBearing = 0.0;
  dh->AngleCoverage = 360.0;

  dh->StartRange = me->brh.StartRange * METERS_PER_NMI;
  // the rangecell value reflects round-trip travel time,
  // so halve it before computing range
  dh->RangeCoverage = me->brh.usecRangeCell * 1.0E-6 / 2 * VELOCITY_OF_LIGHT * me->brh.Samples;
  // we want Hz, PulsePeriod is in usec
  dh->PRF = floor (0.5 + 1.0E6 / me->brh.BandwidthStatus.PulsePeriod);  
  // we want RPM; rotation time is in milliseconds
  dh->AntennaSpeed = floor (0.5 + 60000 / me->brh.BandwidthStatus.RotationTime);
  // FIXME?: is this correct?
  dh->NorthAligned = me->brh.NorthStabilized;  
  dh->RangePerSample = me->brh.usecRangeCell * 1.0E-6 / 2 * VELOCITY_OF_LIGHT;
  dh->SelectedModeNdx = me->brh.BandwidthStatus.SelectedModeNdx;
  dh->ModeNdx = me->brh.BandwidthStatus.ModeNdx;
  dh->SelectedPlenNdx = me->brh.BandwidthStatus.SelectedPlenNdx;
  dh->PlenNdx = me->brh.BandwidthStatus.PlenNdx;
  dh->PulseLength = me->brh.BandwidthStatus.PulseLength;
  dh->CFARed = me->brh.current.CFAR;
  dh->PulseFiltered = me->brh.current.PulseFiltering;
  dh->MotionCompensated = me->brh.current.MotionCompensation;
  dh->ScansAveraged = me->brh.current.ScanAveraging;
  dh->FTCNdx = me->brh.current.FTC;
  dh->State = 0;
  
  return TRUE;
}

// FIXME: reading ungated data vs. reading gated data mismatches the pulses by
// approximatley 230 (when doing 4096 pulses per scan); ie. for gated pulse i,
// we find the matching ungated pulse at index i+230. (when reading ungated data
// into radR)
  
int
read_archive_next_scan_ungated (t_ssa *me, char *buffer) 
{

  // read and convert a full scan of ungated data and store 
  // it in buffer.
  // buffer has been ensured to be the correct size
  // FIXME: doesn't deal correctly with an incomplete scan
  // surrounded by complete scans (overlaps complete, incomplete scans)

  int j;
  double output_theta;
  double pulse_theta;
  char *src = me->data_block.ptr + me->input_index * me->brh.rAxisBytes;
  RSI_LUT *anglep = ((RSI_LUT *)me->angle_block.ptr) + me->input_index;
  char *dst = buffer;

  j = 0;
  output_theta = 0;
  while (j < me->desired_azimuths) {
    // the desired output angle
    if (me->input_index == me->brh.AssociatedAzimuths) {
      if(!advance_to_next_ungated_block(me))
	return FALSE;
      // reset source pointer to zeroth pulse (i.e. the one from the previous scan)
      // NOTE: just in case the extmat me->data_block got reallocated to enlarge it,
      // we must update our copy of the me->data_block.ptr anyway.
      // same holds for me->angle_block.ptr
      src = me->data_block.ptr + me->input_index * me->brh.rAxisBytes;
      anglep = ((RSI_LUT *)me->angle_block.ptr) + me->input_index;
    }
    if (me->skip_changeover_pulse && anglep->Bearing != (anglep + 1)->Bearing) {
      // this is a changeover pulse and we are supposed to skip it
      ++ me->input_index;
      ++ anglep;
      src += me->brh.rAxisBytes;
      // <FIXME>: with the following code included, we get more correct pulses
      // so is the whole changeover pulse issue bogus???
      memcpy(dst, src, me->brh.rAxisBytes); 
      dst += me->brh.rAxisBytes;
      ++j;
      output_theta = j * 360.0 / me->desired_azimuths;
      // </FIXME>
    } else {
      pulse_theta = fmod(me->brh.StartAngle + (me->input_index + 1) * me->in_dtheta, 360.0);
      if (ang_diff (pulse_theta, output_theta)
	  >= ang_diff(me->prev_pulse_theta, output_theta)) {
	// the angle of the current input pulse is closer to the desired angle than is that
	// of the next input pulse
	// copy the current source pulse to the output
	memcpy(dst, src, me->brh.rAxisBytes); 
	dst += me->brh.rAxisBytes;
	++j;
	output_theta = j * 360.0 / me->desired_azimuths;
      } else {
	// the next pulse's angle is closer to what we need than
	// this pulse's angle, so advance to the next pulse
	++me->input_index;
	++anglep;
	src += me->brh.rAxisBytes;
	me->prev_pulse_theta = pulse_theta;
	// <FIXME>: with the following code included, we get more correct pulses
	// so just what is seascan's algorithm for decimating pulses from (e.g. 2.4 * 2100 to 4096)
	memcpy(dst, src, me->brh.rAxisBytes);
	dst += me->brh.rAxisBytes;
	++j;
	output_theta = j * 360.0 / me->desired_azimuths;
	// </FIXME>
      }
    }
  }
  // if we're reading packed 12-bit samples, unpack them into 16 bits
  // this test works but does not seem robust
  // FIXME:  this assumes that for packed 12-bit data, the number
  // of bytes per line is 0 (mod 6)
  
  if (me->brh.BitsPerSample == 16 && me->brh.rAxisBytes < 2 * me->brh.Samples)
    expand_12_bit_packed(buffer, me->brh.Samples * me->desired_azimuths);
  
  return TRUE;
}

R_CallMethodDef seascanarch_call_methods[]  = {
  // R hook functions
  MKREF(shut_down, 1),
  MKREF(set_filename, 2),
  MKREF(get_contents, 1),
  MKREF(end_of_data, 1),
  MKREF(get_scan_info, 1),
  MKREF(get_scan_data, 2),
  MKREF(set_desired_azimuths, 2),
  MKREF(set_max_azimuth_err, 2),
  MKREF(set_skip_changeover_pulse, 2),
  MKREF(set_use_pc_timestamp, 2),
  MKREF(seek_time, 2),
  MKREF(seek_scan, 3), 
  MKREF(start_up, 1),
  MKREF(shut_down, 1),
  MKREF(start_run, 2), 
  MKREF(put_scan, 3),
  MKREF(end_run, 2),
  MKREF(is_started, 1),
  {NULL, NULL, 0}
};

void
R_init_seascanarch(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, seascanarch_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

void
R_unload_seascanarch(DllInfo *info)
{
  /* Release resources by calling shutdown for both ports */

  do_shut_down(SSA_READER, TRUE);
  do_shut_down(SSA_WRITER, TRUE);
}
