//
// blipmoviearch.c - R module for playing back movies saved by the blipmovie plugin
//
// by John Brzustowski, 2006 (jbrzusto AT fastmail DOT fm)
//
// indexes have origin 1 in R, origin 0 in C

#include "blipmoviearch.h"

// pointers for two ports, one a reader, the other a writer

t_bma *bma_ports[2];

#ifdef RADR_DEBUG
// force gcc/gdb to know about the types t_scan_header and t_scan_header_mem
t_scan_header bogus1;
t_scan_header_mem bogus2;
#endif

// variable sized-storage for reading in blip sample and score data

static t_extmat sample_mat = CREATE_USER_EXTMAT(BYTES_PER_SAMPLE_RECORD, "");
static t_extmat score_mat = CREATE_USER_EXTMAT(BYTES_PER_SCORE_RECORD, "");

static t_bma *
ensure_port(t_bma_port port) {
  // make sure the structure for this port has been allocated
  t_bma *me;

  if (bma_ports[port])
    return bma_ports[port];
  
  me = (t_bma *) calloc(sizeof(t_bma), 1);
  if (!me)
    return NULL;
  me->port = port;
  me->is_started = FALSE;
  bma_ports[port] = me;
  me->scan_headers = CREATE_USER_EXTMAT(sizeof(t_scan_header_mem), "");
    
  return me;
}

static t_bma*
ensure_port_started(t_bma_port port) {
  t_bma *me;
  if (!(me = ensure_port(port)))
    return NULL;
  if (me->is_started)
    return me;
  start_up(ScalarInteger(port));
  if (!me->files[0])
    return NULL;
  me->is_started = TRUE;
  return me;
}

static SEXP
do_shut_down(t_bma_port port) {
  int i;
  t_bma * me = bma_ports[port];
  if (!me || !me->is_started)
    return PASS_SEXP;

  switch(me->port) {
  case BMA_WRITER:
    // record file headers , ignoring errors

    PUT_SCAN_HEADER(me, me->run_index);
    PUT_BLIP_HEADER(me);
    PUT_SAMP_HEADER(me);
    if (me->have_scores)
      PUT_SCOR_HEADER(me);
    
    // passthrough to case BMA_READER

  case BMA_READER:

    // close files

    for (i=0; i < 4; ++i) {
      if (me->files[i]) {
	fclose(me->files[i]);
	me->files[i] = NULL;
      }
    }
    break;
  default:
    break;
  }
  me->is_started = FALSE;
  return PASS_SEXP;
}

SEXP
shut_down(SEXP portsxp)
{
  t_bma_port port = INTEGER(portsxp)[0];

  return do_shut_down(port);
}

SEXP
start_up(SEXP portsxp)
{
  t_bma_port port = INTEGER(portsxp)[0];
  t_bma *me;
  int i;

  char tmp_filename[1024+4];
  // given the filename, open the files
  
  if (!(me = bma_ports[port])) {
    error_code = RADR_ERROR_BAD_PORT;
    return FAIL_SEXP;
  }

  for (i=0; i < 4; ++i) {
    strcpy(tmp_filename, me->basename);
    strcat(tmp_filename, bma_file_suffixes[i]);
    switch(port) {
    case BMA_READER:
      me->files[i] = bigfopen(tmp_filename, "rb");
      break;
    case BMA_WRITER:
      // open a new file for (over)writing
      me->files[i] = bigfopen(tmp_filename, "rb+") ?: bigfopen(tmp_filename, "wb+");
      break;
    default:
      break;
    }  
    me->have_scores = TRUE;
    if (! me->files[i]) {
      // the only file open error we allow is to not have a score file for the reader
      // to support legacy blipmovies without scores
      if (i == 3 && port == BMA_READER) {
	me->have_scores = FALSE;
      } else {
	strncpy(extra_error_info, tmp_filename, EXTRA_ERROR_INFO_SIZE);
	*me->basename = '\0';
	error_code = RADR_ERROR_CANT_OPEN_ARCHIVE;
	while (--i >= 0) {
	  fclose(me->files[i]);
	  me->files[i] = NULL;
	}
	return FAIL_SEXP;
      }
    }
  }

  // get the scan file preamble

  if (READ_SCAN_PREAMBLE(me)) {
    // unable to read scan preamble
    switch(port) {
    case BMA_WRITER:
      // create and initialize the preamble
      strcpy(me->scan_preamble.text, SCANFILE_PREAMBLE_TEXT);
      (void) (WRITE_SCAN_PREAMBLE(me));
      break;
    case BMA_READER:
      // the file must be corrupt
      error_code = RADR_ERROR_INVALID_ARCHIVE;
      return FAIL_SEXP;
    default:
      break;
    }
  }

  // read any existing scan headers
  // this determines how many scan runs are (already) in the file
  me->run_index = me->scan_index = me->num_runs = 0;

  SEEK_SCAN_HEADER(me, 0);
  for ( ; ; ) {
    // ensure and initialize a new slot for a header
    (*pensure_extmat)(&me->scan_headers, me->run_index + 1, 1);
    SCAN_HEADER_OFFSET(me, me->run_index) = bigftell(me->files[BMA_SCAN_FILE]);
    SCAN_HEADER(me, me->run_index) = (t_scan_header){.num_scans = 0,.first_scan_timestamp = 0, .last_scan_timestamp = 0};
    
    if (READ_SCAN_HEADER(me, me->run_index)) 
      // break on error
      break;

    if (CURRENT_NUM_SCANS(me) > 0) {
      // fix the first/last timestamps in the scan header by  reading
      // 

      // read the first scan record and get the timestamp from it
      if (READ_SCAN(me)) {
	// assume there is cruft at end of file, and just ignore it
	CURRENT_NUM_SCANS(me) = 0;
	break;
      }
      SCAN_HEADER(me, me->run_index).first_scan_timestamp = (time_t) (me->scan_record.timestamp);

      // seek to the last scan record, read it, and use its timestamp 
      SEEK_SCAN_CURRENT_RUN(me, CURRENT_NUM_SCANS(me) - 1);
      if (READ_SCAN(me)) {
	error_code = RADR_ERROR_INVALID_ARCHIVE;
	return FAIL_SEXP;
      } 
      SCAN_HEADER(me, me->run_index).last_scan_timestamp = (time_t) (me->scan_record.timestamp);
    } else if (CURRENT_NUM_SCANS(me) < 0) {
      // header is bogus; assume there is cruft at end of file
      break;
    }
    ++me->run_index;
  }

  me->num_runs = me->run_index;

  // if the last run is empty or bogus, pretend it's not there
  if (me->num_runs >= 1 && SCAN_HEADER(me, me->num_runs - 1).num_scans <= 0)
    --me->num_runs;

  switch(port) {
  case BMA_WRITER:
    me->run_index = me->num_runs; // index of the next run to be begun
    // if there were scan file headers,
    // then read the blip and sample file headers
    if (me->num_runs > 0) {
      (void) (GET_BLIP_HEADER(me));
      (void) (GET_SAMP_HEADER(me));
    } else {
      // otherwise, set the blip and sample counters to zero
      me->blip_header.num_blips = 0.0;
      me->samp_header.num_samples = 0.0;
      me->scor_header.num_samples = 0.0;
    }
    break;
  case BMA_READER:
    me->run_index = me->scan_index = 0;
    SEEK_SCAN_HEADER(me, 0);
    break;
  default:
    break;
  }
  me->is_started = TRUE;
  return PASS_SEXP;
}

SEXP
set_filename (SEXP portsxp, SEXP fnsxp)
{
  t_bma_port port = INTEGER(portsxp)[0];
  t_bma *me;

  if (!(me = ensure_port(port))) {
    error_code = RADR_ERROR_NOMEM;
    return FAIL_SEXP;
  }

  if (me->is_started)
    do_shut_down(port);

  strncpy(me->basename, CHAR(STRING_ELT(fnsxp, 0)), sizeof(me->basename));
  me->is_started = FALSE;
  return PASS_SEXP;
}


SEXP
get_contents (SEXP portsxp)
{
  int i, j;
  t_bma_port port = INTEGER(portsxp)[0];
  t_bma *me; 
  SEXP rv = R_NilValue;
  
  if (!(me = ensure_port_started(port)))
    return FAIL_SEXP;

  // FIXME?: we are passing time_t as a double; will this work even with the
  // 64 bit version of time_t?
  
  PROTECT(rv = allocVector(REALSXP, 3 * me->num_runs));
  for (i=0, j=0; i < me->num_runs; ++i) {
    REAL(rv)[j++] = (double) SCAN_HEADER(me, i).num_scans;
    REAL(rv)[j++] = (double) SCAN_HEADER(me, i).first_scan_timestamp;
    REAL(rv)[j++] = (double) SCAN_HEADER(me, i).last_scan_timestamp;
  }
  UNPROTECT(1);
  me->have_toc = TRUE;

  return rv;
}

SEXP
get_scan_info(SEXP portsxp)
{
  int port = INTEGER(portsxp)[0];
  t_bma *me;
  t_scan_record *sr;
  SEXP rv;
  
  if (!(me = ensure_port_started(port)))
    return FAIL_SEXP;

  if (me->scan_index >= CURRENT_NUM_SCANS(me)) {
    error_code = RADR_ERROR_EOF_ON_PORT;
    return FAIL_SEXP;
  }
  if (GET_SCAN_CURRENT_RUN(me, me->scan_index)) {
    error_code = RADR_ERROR_INVALID_ARCHIVE;
    strncpy(extra_error_info, "unexpected error reading .BMA file", EXTRA_ERROR_INFO_SIZE);
    return FAIL_SEXP;
  }
  ++me->scan_index;
  
  sr = &me->scan_record;

  PROTECT(rv = allocVector(VECSXP, 9));
  SET_VECTOR_ELT(rv, 0, ScalarInteger(sr->pulses));
  SET_VECTOR_ELT(rv, 1, ScalarInteger(sr->samples_per_pulse));
  SET_VECTOR_ELT(rv, 2, ScalarInteger(sr->bits_per_sample));
  SET_VECTOR_ELT(rv, 3, ScalarReal(sr->timestamp));
  SET_VECTOR_ELT(rv, 4, ScalarInteger(sr->duration));
  SET_VECTOR_ELT(rv, 5, ScalarReal(sr->sample_dist));
  SET_VECTOR_ELT(rv, 6, ScalarReal(sr->first_sample_dist));
  SET_VECTOR_ELT(rv, 7, ScalarReal(sr->bearing));
  SET_VECTOR_ELT(rv, 8, ScalarInteger(sr->orientation));
  UNPROTECT(1);
  me->have_scan_data = TRUE;
  return(rv);
}


SEXP
get_scan_data(SEXP portsxp, SEXP scansxp, SEXP classsxp, SEXP patchessxp, SEXP scoresxp)
{
  SEXP rv = FAIL_SEXP;
  
  int port = INTEGER(portsxp)[0];
  int i, j, k, n, m, lastj;
  t_scan_dim *row, *col;
  t_sample *val;
  t_score *scr;
  int nc, nr;
  int numruns;
  int num_hot_samples = 0;

  short *count_buff = NULL; // for counting sort of runs
  short min_row, max_row; // for counting sort

  t_bma *me;
  t_extmat *scan_buff = SEXP_TO_EXTMAT(scansxp);
  t_extmat *class_buff = SEXP_TO_EXTMAT(classsxp);
  t_extmat *score_buff = SEXP_TO_EXTMAT(scoresxp);
  t_image image = (t_image) EXTPTR_PTR(patchessxp);

  int total_num_runs = 0;
  t_blip_record *br;
  t_cell_run *run, *first_run;
  t_cell_run tmp;
  
  if (!(me = ensure_port_started(port)))
    goto DONE;
  if (!me->have_scan_data) 
    goto DONE;

  br = &me->blip_record;

  // ensure the scan and class buffers have the same dimensions as this scan
    (*pensure_extmat)(scan_buff, nr=me->scan_record.pulses, nc = me->scan_record.samples_per_pulse);
  (*pensure_extmat)(class_buff, nr, nc);
  (*pensure_extmat)(score_buff, nr, nc);

  memset(scan_buff->ptr, 0, sizeof(t_sample) * nr * nc);
  memset(class_buff->ptr, 0, sizeof(t_class) * nr * nc);
  if (me->have_scores)
    memset(score_buff->ptr, 0, sizeof(t_score) * nr * nc);

  SEEK_BLIP(me, (long long) me->scan_record.first_blip);
  m = me->scan_record.num_blips;
  image->run_info.num_cells = 0;
  image->run_info.num_runs = 0;
  image->run_info.num_patches = m;
  image->run_info.num_active_cells = 0;
  image->run_info.num_active_runs = 0;
  image->run_info.num_active_patches = m;
  image->run_info.num_singletons = 0;
  image->run_info.num_rows = nr;
  image->run_info.num_cols = nc;
  image->run_info.runs_are_sorted = FALSE; /* we won't have runs sorted by increasing column within increasing row, since we
				   create them patch-by-patch */
  image->drop_singletons = TRUE;
  // make sure there's room for at least two runs, even if this scan uses none
  // This is the bogus run which is used to point to the first run in an active
  // patch.

  (*pensure_extmat)(&image->runs, 2, 1);

  // allocate buffer for counting sort, used for all blips in this scan
  count_buff = Calloc(nr, short);

  for (i = 0; i < m; ++i) {
    // read the i'th blip
    if (READ_BLIP(me)) {
      error_code = RADR_ERROR_INVALID_ARCHIVE;
      strncpy(extra_error_info, "reading blip file", EXTRA_ERROR_INFO_SIZE);
      goto DONE;
    }
    if (i == 0) {
      if (SEEK_SAMP(me, (long long) br->first_sample)) {
	strncpy(extra_error_info, "seeking in sample file", EXTRA_ERROR_INFO_SIZE);
	goto DONE;
      }
      if (me->have_scores) {
	if (SEEK_SCOR(me, (long long) br->first_sample)) {
	  strncpy(extra_error_info, "seeking in score file", EXTRA_ERROR_INFO_SIZE);
	  goto DONE;
	}
      }
    }
    
    n = br->num_samples;
    num_hot_samples += n;
    (*pensure_extmat)(&sample_mat, n, 1);
    if (READ_SAMPLES(me, sample_mat.ptr, n)) {
      error_code = RADR_ERROR_INVALID_ARCHIVE;
      goto DONE;
    }
    if (me->have_scores) {
      (*pensure_extmat)(&score_mat, n, 1);
      if (READ_SCORES(me, score_mat.ptr, n)) {
	error_code = RADR_ERROR_INVALID_ARCHIVE;
	goto DONE;
      }
    }
    row = (t_scan_dim *) sample_mat.ptr;
    col = row + n;
    val = (t_sample *) (col + n);
    
    // keep track of how many runs are represented by this patch
    // Notice that values are stored in the file sorted by column within runs.
    numruns = 1;
    for (j = 0; j < n; ++j) {
      // paint the values into the scan and class buffers
      ((t_sample *)(scan_buff->ptr))[(row[j]-1) * nc + (col[j]-1)] = (t_sample) val[j];
      ((t_class *)(class_buff->ptr))[(row[j]-1) * nc + (col[j]-1)] = (t_class) CLASS_HOT;
      // check whether we are beginning a new run
      if (j > 0 && (row[j] != row[j-1] || col[j] != col[j-1] + 1))
	++numruns;
    }

    image->run_info.num_active_runs += numruns;
    image->run_info.num_active_cells += n;

    if (me->have_scores) {
      scr = (t_score *) score_mat.ptr;
      // paint the scores into the score matrix
      for (j = 0; j < n; ++j)
	((t_score *)(score_buff->ptr))[(row[j]-1) * nc + (col[j]-1)] = (t_score) scr[j];
    }
    
    // ensure storage for the runs in the patch_image buffer, plus two extra
    (*pensure_extmat)(&image->runs, total_num_runs + numruns + 2, 1);

    // point to the first new run
    run = first_run = & ((t_cell_run *)image->runs.ptr)[total_num_runs + 1];

    // add the sample runs to the patch_image buffer, with an extra 
    // iteration for closing the final run.  Notice that we don't
    // actually use j=n as a subscript.

    min_row = max_row = row[0];

    for (j = 0, lastj = 0; j <= n; ++j) {
      if (j > 0 && (j == n || row[j] != row[j-1] || col[j] != col[j-1] + 1)) {
	// we're at the first cell of a new run, so record the old one
	
	run->row = row[j-1] - 1;
	run->length = j - lastj;
	run->col = col[lastj] - 1;
	run->patch_id = total_num_runs + 1; // this is the number of runs for all previous blips plus 1
	                                    // and so is the index of the first run in this patch, as required.
	lastj = j;
	run->next_run_offset = 0; // flag to be used later
	if (run->row < min_row) min_row = run->row;
	if (run->row > max_row) max_row = run->row;
	++run;
      }
    }

    // use a counting sort to make sure the runs within this patch are in order by row (pulse)

    for (j = min_row; j <= max_row; ++j) 
      count_buff[j] = 0;

    // count number of runs in each row
    for (j = 0; j < numruns; ++j)
      ++count_buff[first_run[j].row];

    // accumulate counts of runs per row
    for (j = min_row + 1; j <= max_row; ++j)
      count_buff[j] += count_buff[j - 1];
 
    // position runs, starting from the last
    for (j = numruns - 1; j >= 0; --j) {
      // skip, if this run was already correctly placed
      if (first_run[j].next_run_offset == 1)
	continue;
      for(;;) {
	// mark this run as having been correctly placed,
	// since we're about to do that
	first_run[j].next_run_offset = 1;
	// get target location for run j
	k = --count_buff[first_run[j].row];
	if (k == j) 
	  // run is already in correct position
	  break;
	// swap runs at j and k
	tmp = first_run[k];
	first_run[k] = first_run[j];
	first_run[j] = tmp;
	// continue processing with run j
      }
    }
    
    // circularize the list of runs in this patch
    first_run[numruns - 1].next_run_offset = -(numruns - 1);

    // set the offset to the first run of the next patch,
    first_run -> next_patch_offset = (i < m-1) ? numruns : 0;

    total_num_runs += numruns;
  }
  image->run_info.num_runs = total_num_runs;
  image->run_info.num_cells = image->run_info.num_active_cells;
  ((t_cell_run *)image->runs.ptr)[0].next_patch_offset = 1; // the first (fake) run always points to the first real run

  PROTECT(rv = allocVector(INTSXP, 2));
  INTEGER(rv)[0] = m;
  INTEGER(rv)[1] = num_hot_samples;
  UNPROTECT(1);
  me->have_scan_data = FALSE;
 DONE:
  if (count_buff)
     Free(count_buff);
  return rv;
}


static int port;
static int num_blips;
static t_image image;
static t_bma *me;
static t_scan_dim *row, *col;
static t_scan_dim nc;
static t_sample *val;
static t_score *scr;
static t_sample *scan_buff;
static t_score *score_buff;

static t_pf_rv
pf_write_blips(t_cell_run *r)
{
  // a patch function to write blips to the blips and samples files
  // of a blipmovie archive

  int ns = 0;
  int i;
  t_sample *sptr;
  t_score *scptr;
  t_cell_run *first_run = r;

  // calculate the size of this blip
  do {
    ns += r->length;
    r += r->next_run_offset;
  } while (r != first_run);

  // fill and write the blip record

  me->blip_record.first_sample = me->samp_header.num_samples;
  me->blip_record.num_samples = ns;

  if (WRITE_BLIP(me)) {
    error_code = RADR_ERROR_UNABLE_TO_WRITE_TO_FILE;
    strncpy(extra_error_info, "writing blip file", EXTRA_ERROR_INFO_SIZE);
    return KEEP_AND_QUIT;
  }
  ++ me->blip_header.num_blips;

  // write the samples; we save the row, col and value data
  // for this blip in an extmat, then write it all at once.

  (*pensure_extmat)(&sample_mat, 3*ns, 1);
  if (me->have_scores)
    (*pensure_extmat)(&score_mat, ns, 1);

  row = (t_scan_dim *) sample_mat.ptr;
  col = row + ns;
  val = (t_sample *) (row + 2 * ns);
  scr = (t_score *) score_mat.ptr;
  do {
    sptr = &scan_buff[r->row * nc + r->col];
    if (me->have_scores) {
      scptr = &score_buff[r->row * nc + r->col];
      for (i=0; i < r->length; ++i) {
	*row++ = r->row+1;   // save row as origin 1
	*col++ = r->col+i+1; // save col as origin 1
	*val++ = *sptr++;
	*scr++ = *scptr++;
      }
    } else {
      for (i=0; i < r->length; ++i) {
	*row++ = r->row+1;   // save row as origin 1
	*col++ = r->col+i+1; // save col as origin 1
	*val++ = *sptr++;
      }
    }
    r += r->next_run_offset;
  } while (r != first_run);

  if (WRITE_SAMPLES(me, sample_mat.ptr, ns)) {
    error_code = RADR_ERROR_UNABLE_TO_WRITE_TO_FILE;
    strncpy(extra_error_info, "writing sample file", EXTRA_ERROR_INFO_SIZE);
    return KEEP_AND_QUIT;
  }
  if (me->have_scores) {
    if (WRITE_SCORES(me, score_mat.ptr, ns)) {
      error_code = RADR_ERROR_UNABLE_TO_WRITE_TO_FILE;
      strncpy(extra_error_info, "writing score file", EXTRA_ERROR_INFO_SIZE);
      return KEEP_AND_QUIT;
    }
    me->scor_header.num_samples += ns;
  }
  me->samp_header.num_samples += ns;
  return KEEP;
}

SEXP
put_scan(SEXP portsxp, SEXP scansxp, SEXP blipsxp, SEXP scaninfosxp, SEXP numblipssxp, SEXP scoresxp)
{

  port = INTEGER(portsxp)[0];
  num_blips = INTEGER(numblipssxp)[0];
  image = (t_image) EXTPTR_PTR(blipsxp);
  if (!(me = ensure_port_started(port)))
    return FAIL_SEXP;

  scan_buff = (t_sample *) (SEXP_TO_EXTMAT(scansxp)->ptr);
  if (me->have_scores)
    score_buff = (t_score *) (SEXP_TO_EXTMAT(scoresxp)->ptr);

  me->scan_record.timestamp          = (double) REAL(scaninfosxp)[0];
  me->scan_record.duration           = (short)  REAL(scaninfosxp)[1];
  me->scan_record.pulses             = (short)  REAL(scaninfosxp)[2];
  me->scan_record.samples_per_pulse  = nc = (short)  REAL(scaninfosxp)[3];
  me->scan_record.orientation        = (short)  REAL(scaninfosxp)[4];
  me->scan_record.bits_per_sample    = (short)  REAL(scaninfosxp)[5];
  me->scan_record.sample_dist        = (double) REAL(scaninfosxp)[6];
  me->scan_record.first_sample_dist  = (double) REAL(scaninfosxp)[7];
  me->scan_record.bearing            = (double) REAL(scaninfosxp)[8];

  // set the first and last timestamps for this run,
  // on the assumption scans within a run are written in increasing temporal order

  if (!CURRENT_FIRST_SCAN_TIMESTAMP(me))
    CURRENT_FIRST_SCAN_TIMESTAMP(me) = (time_t) (me->scan_record.timestamp);
  CURRENT_LAST_SCAN_TIMESTAMP(me) = (time_t) (me->scan_record.timestamp);

  me->scan_record.first_blip = me->blip_header.num_blips;
  me->scan_record.num_blips = num_blips;

  // seek and save the scan record
  if (PUT_SCAN_CURRENT_RUN(me, CURRENT_NUM_SCANS(me))) {
    error_code = RADR_ERROR_UNABLE_TO_WRITE_TO_FILE;
    strncpy(extra_error_info, "writing scan file", EXTRA_ERROR_INFO_SIZE);
    goto FAIL;
  }
  ++CURRENT_NUM_SCANS(me);

  // ensure the blip and sample file pointers
  // are in the correct locations

  SEEK_BLIP(me, me->blip_header.num_blips);
  SEEK_SAMP(me, me->samp_header.num_samples);
  if(me->have_scores)
    SEEK_SCOR(me, me->scor_header.num_samples);

  // record blips and files

  enumerate_patches (image, pf_write_blips);
  if (error_code) 
    goto FAIL;

  return PASS_SEXP;

  // there was an error
 FAIL:
  return FAIL_SEXP;
}  
  
SEXP
end_of_data(SEXP portsxp)
{
  int port = INTEGER(portsxp)[0];
  t_bma *me;

  if (!(me = ensure_port_started(port))) {
    error_code = RADR_ERROR_BAD_PORT;
    return FAIL_SEXP;
  }
  return LOGICAL_SEXP(me->scan_index >= CURRENT_NUM_SCANS(me));
}


SEXP
seek_time(SEXP portsxp, SEXP timesxp)
{
  // seek to the last scan with a timestamp less than or equal to time
  // unless time is at least 10 seconds outside of the range 
  // of times in this archive, in which case fail

  int port = INTEGER(portsxp)[0];
  time_t t = (time_t) INTEGER(timesxp)[0];
  t_bma *me;
  int scan;

  if (!(me = ensure_port_started(port))) {
    error_code = RADR_ERROR_BAD_PORT;
    return FAIL_SEXP;
  }
  if (t < CURRENT_FIRST_SCAN_TIMESTAMP(me) - 10
      || t >  CURRENT_LAST_SCAN_TIMESTAMP(me) + 10) {
    error_code = RADR_ERROR_SEEK_BEYOND_ARCHIVE;
    return FAIL_SEXP;
  }
  // guess an initial position for which scan;
  // on the assumption the scans are consecutive and linearly spaced in time

  scan = (t - CURRENT_FIRST_SCAN_TIMESTAMP(me)) * CURRENT_NUM_SCANS(me) / (CURRENT_LAST_SCAN_TIMESTAMP(me) + 1 - CURRENT_FIRST_SCAN_TIMESTAMP(me)); 
  while (scan >= 0 && scan < CURRENT_NUM_SCANS(me)) {
    if (GET_SCAN_CURRENT_RUN(me, scan)) {
      error_code = RADR_ERROR_INVALID_ARCHIVE;
      return FAIL_SEXP;
    }
    if (t >= me->scan_record.timestamp) {
      if (t < me->scan_record.timestamp + me->scan_record.duration / 1000.0) {
	me->scan_index = scan;
	SEEK_SCAN_CURRENT_RUN(me, scan);
	return PASS_SEXP;
      } else {
	++scan;
      }
    } else {
      --scan;
    }
  }
  return FAIL_SEXP;
}


SEXP
seek_scan(SEXP portsxp, SEXP runsxp, SEXP scansxp)
{
  // seek to the specified scan of the specified run
  // if both are -1, we just want the next scan,
  // so we do nothing
  int port = INTEGER(portsxp)[0];
  t_bma *me;
  int run = INTEGER(runsxp)[0];
  int scan = INTEGER(scansxp)[0];

  if (!(me = ensure_port_started(port))) {
    error_code = RADR_ERROR_BAD_PORT;
    return FAIL_SEXP;
  }

  if (scan < 0 && run < 0)
    return PASS_SEXP;

  if (run >= me->num_runs || scan < 0) {
    error_code = RADR_ERROR_SEEK_BEYOND_ARCHIVE;
    return FAIL_SEXP;
  }

  me->run_index = run;

  if (scan > CURRENT_NUM_SCANS(me)) {
    error_code = RADR_ERROR_SEEK_BEYOND_ARCHIVE;
    return FAIL_SEXP;
  }

  if (!GET_SCAN_CURRENT_RUN(me, scan)) {
    me->scan_index = scan;
    return PASS_SEXP;
  } else {
    error_code = RADR_ERROR_INVALID_ARCHIVE;
  }
  return FAIL_SEXP;
}


SEXP
start_run(SEXP portsxp)
{
  // create a new header for this run of scans
  int port = INTEGER(portsxp)[0];
  t_bma *me;

  if (!(me = ensure_port_started(port))) {
    error_code = RADR_ERROR_BAD_PORT;
    return FAIL_SEXP;
  }

  me->run_index = me->num_runs++;
  (*pensure_extmat)(&me->scan_headers, me->num_runs, 1);
  CURRENT_NUM_SCANS(me) = 0;
  CURRENT_FIRST_SCAN_TIMESTAMP(me) = 0;
  CURRENT_LAST_SCAN_TIMESTAMP(me) = 0;
  SCAN_HEADER_OFFSET(me, me->run_index) = (me->run_index > 0) ?
    (SCAN_HEADER_OFFSET(me, me->run_index - 1) + SCAN_HEADER(me, me->run_index - 1).num_scans * sizeof(t_scan_record) + sizeof(t_scan_header))
    : sizeof(t_scan_preamble);

  return(R_NilValue);
}


SEXP
end_run(SEXP portsxp)
{
  // end the current run on the blipmoviearchive writer

  int port = INTEGER(portsxp)[0];
  t_bma *me;
  int i;


  if (!(me = ensure_port_started(port))) {
    error_code = RADR_ERROR_BAD_PORT;
    return FAIL_SEXP;
  }

  switch(me->port) {
  case BMA_READER:
    // nothing to do here
    break;

  case BMA_WRITER:
    // flush buffers and write headers
    PUT_SCAN_HEADER(me, me->run_index);
    PUT_BLIP_HEADER(me);
    PUT_SAMP_HEADER(me);
    if (me->have_scores)
      PUT_SCOR_HEADER(me);
      
    for (i=0; i < 4; ++i)
      if (me->files[i])
	fflush(me->files[i]);

    break;
  default:
    break;
  }
  return PASS_SEXP;
}

SEXP
is_started(SEXP portsxp) {
  int p = INTEGER(portsxp)[0];
  return ScalarLogical(bma_ports[p] && (bma_ports[p]->is_started));
}

SEXP
have_scores(SEXP portsxp) {
  int p = INTEGER(portsxp)[0];
  return ScalarLogical(bma_ports[p] && (bma_ports[p]->have_scores));
}
  
  
// return the class object for this module


R_CallMethodDef blipmoviearch_call_methods[]  = {
  // R hook functions
  MKREF(end_of_data	, 1),
  MKREF(end_run		, 1),
  MKREF(get_contents	, 1),
  MKREF(get_scan_data	, 5),
  MKREF(get_scan_info	, 1),
  MKREF(is_started      , 1),
  MKREF(put_scan	, 6),
  MKREF(seek_scan	, 3), 
  MKREF(seek_time	, 2),
  MKREF(set_filename	, 2),
  MKREF(shut_down	, 1),
  MKREF(start_run	, 1), 
  MKREF(start_up	, 1),
  MKREF(have_scores     , 1),
  {NULL, NULL, 0}
};

void
R_init_blipmoviearch(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, blipmoviearch_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

void
R_unload_blipmoviearch(DllInfo *info)
{
  /* Release resources by calling shutdown for both ports */

  do_shut_down(BMA_READER);
  do_shut_down(BMA_WRITER);
  (*pfree_extmat)(&sample_mat);
  (*pfree_extmat)(&score_mat);
}
