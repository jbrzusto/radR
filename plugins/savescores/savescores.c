#include "radRmodule.h"
#include "patchify.h"

#define COPYTOBUFF(p, x) { *((typeof(x)*)p) = x; p += sizeof(x);}

static signed char *buff_p;
static int num_samples;
static t_score *score_buff;
static short num_cols;

static t_pf_rv
pf_calc_num_samples (t_cell_run *r)
{
  // patch filter:
  // count the samples in all blips

  t_cell_run *first_run = r;
  
  do {
    num_samples += r->length;
    r += r->next_run_offset;
  } while (r != first_run);
  return KEEP;
}

static t_pf_rv
pf_copy_score_mat_to_buff (t_cell_run *r)
{
  // patch filter:
  // copy values from the score matrix to the buffer

  t_cell_run *first_run = r;
  t_score *p, *pmax;

  do {
    p = score_buff + r->row * num_cols + r->col;
    for(pmax = p + (r->length - 1); p <= pmax; ++p) {
      COPYTOBUFF(buff_p, *p);
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

  num_samples = 0;
  enumerate_patches(img, & pf_calc_num_samples);

  rv = allocVector(RAWSXP, num_samples * sizeof(t_score));
  buff_p = (signed char *) RAW(rv);
  enumerate_patches (img, & pf_copy_score_mat_to_buff);

  return (rv);
}

R_CallMethodDef savescores_call_methods[]  = {
  // R hook functions
  MKREF(score_mat_to_raw, 2),
  {NULL, NULL, 0}
};

void
R_init_savescores(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, savescores_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

void
R_unload_savescores(DllInfo *info)
{
  /* FIXME: flush files and close ports? */
}
