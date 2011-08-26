#include "bma2.h"

/*

  Support functions for the new blipmovie archive module.
  
  These functions just convert between R raw objects and internal radR data structures.

*/

#define abs(x) ({typeof(x) _x_ = x; (_x_ >= 0) ? _x_ : - _x_;})
#define COPYTOBUFF(p, x) { *((typeof(x)*)p) = x; p += sizeof(x);}
#define COPYFROMBUFF(p, x) {x = *(typeof(x)*)p; p += sizeof(x);}

static int num_samples;
static int have_num_samples = FALSE;
static int num_runs;
static t_scan_dim run_row;
static t_scan_dim run_col;
static int buff_len;
static signed char *buff_p;
static int num_cols;
static t_sample *scan_buff;
static t_score *score_buff;
static t_sample prev_sample;

t_pf_rv
pf_calc_compressed_geometry_length (t_cell_run *r)
{
  // patch filter:
  // count the samples and runs in all blips
  // and determine the length of the compressed
  // blip geometry representation

  t_cell_run *first_run = r;
  
  do {
    ++num_runs;
    num_samples += r->length;
    if (r->row <= 1 + run_row && r->row >= run_row - 2 && r->col <= run_col + 31 && r->col >= run_col - 32) {
      // delta
      ++buff_len;
    } else {
      // absolute flag byte followed by absolute row, col
      buff_len += 1 + sizeof(run_row) + sizeof(run_col); 
    }
    if (r->length < 128) {
      // single-byte
      buff_len += 1;
    } else {
      buff_len += 1 + sizeof(r->length);
    }
    run_row = r->row;
    run_col = r->col;
    r += r->next_run_offset;
  } while (r != first_run);
  return KEEP;
}

t_pf_rv
pf_compress_to_buffer (t_cell_run *r)
{
  // patch filter:
  // append the compressed geometry representation
  // of all blips to the buffer buff_p

  t_cell_run *first_run = r;

  do {
    if (r->row <= run_row + 1 && r->row >= run_row - 2 && r->col <= run_col + 31 && r->col >= run_col - 32) {
      // delta: two high order bits for row delta, lower 6 bits for column delta, both signed
      COPYTOBUFF(buff_p, (signed char)(0xff & (((r->row - run_row) << 6) | (0x3f & (r->col - run_col)))));
    } else {
      // absolute flag byte followed by absolute row, col
      COPYTOBUFF(buff_p, (signed char) 0);
      COPYTOBUFF(buff_p, r->row);
      COPYTOBUFF(buff_p, r->col);
    }
    if (r->length < 128) {
      // single-byte
      COPYTOBUFF(buff_p, (signed char) (r->length | (r == first_run ? -128 : 0)));
    } else {
      COPYTOBUFF(buff_p, (signed char) (r == first_run ? -128 : 0));
      COPYTOBUFF(buff_p, r->length);
    }
    run_row = r->row;
    run_col = r->col;
    r += r->next_run_offset;
  } while (r != first_run);
  return KEEP;
}

SEXP patch_buff_to_raw (SEXP patchsxp) 
{
  /*
    input: extptr to the t_image struct
    output: a raw vector with the following structure:
            int num patches;
            int num runs;
            int num samples;
	    deltas[1 ... num_runs];
	  where deltas are the runs encoded as follows:
	  start: either zero, indicating that the next 4 bytes give the absolute
	         row and column of the start of the run, or byte interpreted as follows:
		   high order 2 bits: signed value giving the change in row of the next run:
                                          00 = same row
                                          01 = next row
                                          10 = back two rows
					  11 = back one row
                   low order 6 bits: signed value giving the change in column of the next run:
		                          -32..31
	  length: the high-order bit flags whether this run begins a new blip
                  the rest of the byte is either from 1..127 giving the length of
                  this run, or 0, indicating that a two-byte short length follows

	  Note that for the first run, the implicit previous row and col are 0 and 0.
  */

  int num_patches;
  SEXP rv;

  t_image img = (t_image) EXTPTR_PTR(patchsxp);

  num_patches = img->num_active_patches;
  num_runs = 0;
  num_samples = 0;
  buff_len = 0;
  run_row = run_col = 0;

  // make a pass through the patch buffer to count runs
  // and samples, and calculate the length of the compressed buffer

  enumerate_patches(img, & pf_calc_compressed_geometry_length);
  have_num_samples = TRUE;

  buff_len += sizeof(num_patches) + sizeof(num_runs) + sizeof(num_samples);

  // allocate a sufficiently large raw vector return value

  rv = allocVector(RAWSXP, buff_len);
  buff_p = (signed char *) RAW(rv);

  // record basic stats
  COPYTOBUFF(buff_p, num_patches);
  COPYTOBUFF(buff_p, num_runs);
  COPYTOBUFF(buff_p, num_samples);

  run_row = run_col = 0;

  // compress the patches into the buffer
  enumerate_patches(img, & pf_compress_to_buffer);

  return (rv);
}


SEXP raw_to_patch_buff (SEXP rawsxp, SEXP patchsxp) 
{
  // the converse of patch_buff_to_raw:
  // recreate the t_image structure encoded in the raw vector rawsxp
  // also, paint the patches into the class matrix
  // return NULL is there is a problem, TRUE otherwise

  t_image img = (t_image) EXTPTR_PTR(patchsxp);
  int num_patches;
  int num_runs;
  int num_samples;
  signed char *p = (signed char *) RAW(rawsxp);
  signed char *pmax = p + (LENGTH(rawsxp) - 1);
  t_scan_dim row, col;
  t_cell_run *runs, *run, *patch_first_run;
  SEXP rv;

  COPYFROMBUFF(p, num_patches);
  img->num_patches = img->num_active_patches = num_patches;
  img->num_singletons = 0;
  img->runs_are_sorted = FALSE;
  COPYFROMBUFF(p, num_runs);
  COPYFROMBUFF(p, num_samples);

  ENSURE_EXTMAT(&img->runs, num_runs + 1, 1);

  runs = (t_cell_run *) img->runs.ptr;
  patch_first_run = runs;
  run = runs + 1;

  //FIXME: the reader assumes the current sizeof(row), sizeof(col), etc.
  // are the same as those in the file

  row = col = 0;

  while (p <= pmax) {
    // process the offset to start of next run
    if (*p) {
      // two signed deltas:  upper two bits for row, lower 6 bits for column
      // warning: *p must be signed for this to work
      row += (*p) >> 6;
      col += ((signed char) (0xff & (*p++ << 2))) >> 2; // to extend the sign of the lower 6 bits
    } else {
      ++p; // skip flag
      COPYFROMBUFF(p, row);
      COPYFROMBUFF(p, col);
    }
    // process the run length
    if (*p & -128) {
      // link this newly beginning patch to the one just ended
      patch_first_run->next_patch_offset = run - patch_first_run;
      // close the run loop for the patch just ended
      (run - 1)->next_run_offset = patch_first_run - (run - 1);
      patch_first_run = run;
    }
    run->next_run_offset = 1;  // will get corrected if this is the last run in the patch
    run->row = row;
    run->col = col;
    if (*p & 127) {
      run->length = 127 & *p++;
    } else {
      ++p; // skip flag
      COPYFROMBUFF(p, run->length);
    }
    ++run;
  }
  // close the last patch's run loop
  (run - 1)->next_run_offset = patch_first_run - (run - 1);

  // mark the last patch as being the last
  patch_first_run->next_patch_offset = 0;

  // paint the blips into the class buff
  enumerate_patches (img, &pf_paint_blip);

  rv = allocVector(INTSXP, 2);
  INTEGER(rv)[0] = num_patches;
  INTEGER(rv)[1] = num_samples;
  return (rv);
}

t_pf_rv
pf_calc_compressed_scan_length (t_cell_run *r)
{
  // patch filter:
  // determine the length of buffer needed for
  // simple delta compression on the scan data for the blips

  t_cell_run *first_run = r;
  t_sample *p, *pmax;

  do {
    num_samples += r->length;
    p = scan_buff + r->row * num_cols + r->col;
    for(pmax = p + (r->length - 1); p <= pmax; ++p) {
      if (abs(*p - prev_sample) <= 127) {
	// delta
	++buff_len;
      } else {
	// absolute flag byte followed by full sample size
	buff_len += 1 + sizeof(t_sample);
      }
      prev_sample = *p;
    }
    r += r->next_run_offset;
  } while (r != first_run);
  return KEEP;
}

t_pf_rv
pf_compress_scan (t_cell_run *r)
{
  // patch filter:
  // compress the scan data for the blips using 
  // simple delta compression

  t_cell_run *first_run = r;
  t_sample *p, *pmax;

  do {
    p = scan_buff + r->row * num_cols + r->col;
    for(pmax = p + (r->length - 1); p <= pmax; ++p) {
      if (abs(*p - prev_sample) <= 127) {
	// delta
	COPYTOBUFF(buff_p, (signed char) (*p - prev_sample));
      } else {
	// absolute flag byte followed by full sample size
	COPYTOBUFF(buff_p, (signed char) -128);
	COPYTOBUFF(buff_p, *p);
      }
      prev_sample = *p;
    }
    r += r->next_run_offset;
  } while (r != first_run);
  return KEEP;
}

SEXP scan_mat_to_raw (SEXP patchsxp, SEXP scansxp) 
{
  /*
    create a raw object with somewhat compressed raw
    data corresponding to the blips
    the raw object will look like this:

    deltas[1.. num_samples]
    where delta[i] is the difference between sample i+1 and sample i, encoded as
    either a byte from -127 to 127 or as -128 followed by a short
    The implicit zeroth sample value is zero.

  */

  t_image img = (t_image) EXTPTR_PTR(patchsxp);
  t_extmat *scan_mat = SEXP_TO_EXTMAT(scansxp);
  scan_buff = (t_sample *) scan_mat->ptr;
  SEXP rv;

  buff_len = 0;
  num_cols = scan_mat->cols;
  num_samples = 0;

  prev_sample = 0;
  enumerate_patches (img, & pf_calc_compressed_scan_length);
  
  rv = allocVector(RAWSXP, buff_len);
  buff_p = (signed char *) RAW(rv);
  prev_sample = 0;
  enumerate_patches (img, & pf_compress_scan);

  return (rv);
}

t_pf_rv
pf_decompress_scan (t_cell_run *r)
{
  // patch filter:
  // decompress the scan data for the blips using 
  // simple delta compression

  t_cell_run *first_run = r;
  t_sample *p, *pmax;

  do {
    p = scan_buff + r->row * num_cols + r->col;
    for(pmax = p + (r->length - 1); p <= pmax; ++p) {
      if (*buff_p != -128) {
	*p = prev_sample + *buff_p++;
      } else {
	++buff_p;
	COPYFROMBUFF(buff_p, *p);
      }
      prev_sample = *p;
    }
    r += r->next_run_offset;
  } while (r != first_run);
  return KEEP;
}


SEXP raw_to_scan_mat (SEXP rawsxp, SEXP dimsxp, SEXP scansxp, SEXP patchsxp)
{
  t_image img = (t_image) EXTPTR_PTR(patchsxp);
  t_extmat *scan_mat = SEXP_TO_EXTMAT(scansxp);
  t_scan_dim rows, cols;
  t_sample *scan_buff;
  
  dimsxp = AS_INTEGER(dimsxp);
  rows = INTEGER(dimsxp)[0];
  cols = INTEGER(dimsxp)[1];

  ENSURE_EXTMAT(scan_mat, rows, cols);
  scan_buff = (t_sample *) scan_mat->ptr;

  prev_sample = 0;
  buff_p = (signed char *) RAW(rawsxp);

  enumerate_patches (img, & pf_decompress_scan);
  
  return (PASS_SEXP);
}


t_pf_rv
pf_copy_score_mat_to_buff (t_cell_run *r)
{
  // patch filter:
  // copy values from the score matrix to the buffer

  t_cell_run *first_run = r;
  t_sample *p, *pmax;

  do {
    p = score_buff + r->row * num_cols + r->col;
    for(pmax = p + (r->length - 1); p <= pmax; ++p) {
      COPYTOBUFF(buff_p, *p);
    }
    r += r->next_run_offset;
  } while (r != first_run);
  return KEEP;
}

t_pf_rv
pf_copy_buff_to_score_mat (t_cell_run *r)
{
  // patch filter:
  // copy values from the buffer to the score matrix
  // simple delta compression

  t_cell_run *first_run = r;
  t_sample *p, *pmax;

  do {
    p = score_buff + r->row * num_cols + r->col;
    for(pmax = p + (r->length - 1); p <= pmax; ++p) {
      COPYFROMBUFF(buff_p, *p);
    }
    r += r->next_run_offset;
  } while (r != first_run);
  return KEEP;
}


SEXP score_mat_to_raw (SEXP patchsxp, SEXP scoresxp) 
{
  /*
    create a raw object with uncompressed score data
    data corresponding to the blips
    the raw object will look like this:

    score[1..num_samples]

  */

  t_image img = (t_image) EXTPTR_PTR(patchsxp);
  t_extmat *score_mat = SEXP_TO_EXTMAT(scoresxp);
  score_buff = (t_score *) score_mat->ptr;
  SEXP rv;

  num_cols = score_mat->cols;
  
  if (!have_num_samples) {
    enumerate_patches(img, & pf_calc_compressed_geometry_length);
    have_num_samples = TRUE;
  }

  rv = allocVector(RAWSXP, num_samples * sizeof(t_score));
  buff_p = (signed char *) RAW(rv);
  enumerate_patches (img, & pf_copy_score_mat_to_buff);

  return (rv);
}

SEXP raw_to_score_mat (SEXP rawsxp, SEXP dimsxp, SEXP scoresxp, SEXP patchsxp) 
{
  t_image img = (t_image) EXTPTR_PTR(patchsxp);
  t_extmat *score_mat = SEXP_TO_EXTMAT(scoresxp);
  t_scan_dim rows, cols;
  t_sample *score_buff;
  
  dimsxp = AS_INTEGER(dimsxp);
  rows = INTEGER(dimsxp)[0];
  cols = INTEGER(dimsxp)[1];

  ENSURE_EXTMAT(score_mat, rows, cols);
  score_buff = (t_sample *) score_mat->ptr;

  prev_sample = 0;
  buff_p = (signed char *) RAW(rawsxp);

  enumerate_patches (img, & pf_copy_buff_to_score_mat);
  
  return (PASS_SEXP);
}

R_CallMethodDef bma2_call_methods[]  = {
  // R hook functions
  MKREF(patch_buff_to_raw, 1),
  MKREF(scan_mat_to_raw, 2),
  MKREF(score_mat_to_raw, 2),
  MKREF(raw_to_patch_buff, 2),
  MKREF(raw_to_scan_mat, 4),
  MKREF(raw_to_score_mat, 4),
  {NULL, NULL, 0}
};

void
R_init_bma2(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_useDynamicSymbols(info, FALSE);
  R_registerRoutines(info, NULL, bma2_call_methods, NULL, NULL);
}

void
R_unload_bma2(DllInfo *info)
{
  // nothing to do here?
}
