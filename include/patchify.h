#ifndef _PATCHIFY_H
#define _PATCHIFY_H

#include "radR.h"

/* definition of the patch_cell type; here, it is the same as the classification type
   used by radR, since patches are built from the sample class matrix */

#define t_patch_cell t_class

/* definitions for patchify.c and modules wishing to call it */

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif
#ifndef min
#define min(a,b) (a) <= (b) ? (a) : (b)
#endif
#ifndef max
#define max(a,b) (a) >= (b) ? (a) : (b)
#endif

#define PATCH_FOREGROUND_MODE	 /* flag: cells are treated as having only two
				    possible colours, namely foreground and everything else.
				    radR uses PATCH_FOREGROUND_MODE because patches are being
				    built from only those samples classified as hot (ie; not
				    cold or excluded)
				 */

#define PATCH_INVALID_CELL (-1)	 /* a value for a cell that does not occur in real data */
#define OUT_OF_MEMORY -1

typedef int16_t t_dim;		/* the type used for linear dimensions (e.g. image width, height) */
typedef int32_t t_dim_2;		/* the type used for squared dimensions (e.g. number of patches)
				   this type should generally be twice as large as t_dim */ 
typedef struct cell_run
  {
    int32_t next_run_offset;	 /* offset from this run to the next run in this patch */
    int32_t next_patch_offset;	 /* offset from this run to the first run in the next patch  */
    t_dim_2 patch_id;		 /* runs in the same patch share this value; initially positive for all patches,
				    negated in a patch's first run when the patch is deactivated; its absolute value is 
				    the index of the first run in this patch (numbered starting at 1) */
    t_dim length;		 /* length of this run */
    t_dim row;			 /* physical row of first cell in this run */
    t_dim col;			 /* physical column of first cell in this run */
#ifndef PATCH_FOREGROUND_MODE
    t_patch_cell value;		 /* value of cells in this run */
#endif /* PATCH_FOREGROUND_MODE */
  } __attribute__((packed))
t_cell_run;

// we reallocate storage for the run buffer 1000 runs at a time
#define RUN_ALLOC_INCREMENT 1000

// a structure holding information about a run buffer

typedef struct
{
  t_dim 	num_rows;		/* number of rows in image */
  t_dim 	num_cols;		/* number of columns in image */
  t_dim_2 	num_cells;	        /* total number of cells in all patches (active or not, including singletons) */
  t_dim_2       num_runs;               /* number of runs in image, including singletons and two bogus runs */
  t_dim_2 	num_patches;		/* number of patches (including singletons) */
  t_dim_2 	num_singletons;		/* number of single-cell patches */
  t_dim_2 	num_active_cells;	/* number of cells in active patches (including active singletons) */
  t_dim_2 	num_active_runs;	/* number of runs in active patches (including active singletons) */
  t_dim_2 	num_active_patches;	/* number of active patches (including active singletons) */
  int 		runs_are_sorted;        /* flag: are runs sorted by increasing column within increasing row? */
} __attribute__((packed))  t_runbuf_info;
     

typedef struct
{
  int32_t		drop_singletons;  /* flag: if non-zero, keep singletons out of the patch list */
  int32_t		use_diags;	  /* flag: extend patches along diagonals too? */
  int32_t		vertical_wrap;	  /* flag: wrap patches across the bottom/top boundary */
  t_runbuf_info 	run_info;         /* geometry etc. of the run buffer */
  t_patch_cell 		fgd;  	          /* the foreground cell value */
  t_extmat 		runs;		  /* buffer of runs; runs are stored with this invariant:  the first run
					   with a given patch id must occur before the first run with any larger
					   patch id.  The first and last runs are bogus and used for convenience in
					   the code and to avoid bounds-checking errors.  The first run will point
					   to the first active patch.  Moreover, runs.length will not include these
					   two bogus runs.  FIXME: This is kludgy, since any attempt to use runs
					   as a normal extmat will include the first bogus run and miss the last
					   real run (and the following terminal bogus run).
				          */
  t_extmat 		scratch_row;	   /* a buffer into which we do compression and active-patch coalescence */
} __attribute__((packed))  t_image_struct;
     

/* a return-value type for the patch filters */

#define RADR_PF_DROP 1  // inactivate this patch
#define RADR_PF_QUIT 2  // stop enumerating patches
#define RADR_PF_SAME 4  // keep this patch's active/inactive status unchanged (overrides RADR_PF_DROP)

typedef enum {KEEP	    = 0,
	      DROP	    = RADR_PF_DROP,
	      KEEP_AND_QUIT = RADR_PF_QUIT,
	      DROP_AND_QUIT = RADR_PF_DROP | RADR_PF_QUIT,
	      SAME	    = RADR_PF_SAME,
	      SAME_AND_QUIT = RADR_PF_SAME | RADR_PF_QUIT
} t_pf_rv;

/* the type defining a callback for patch enumeration */
typedef t_pf_rv (*patch_function)(t_cell_run *tr);

typedef t_image_struct *t_image;

#endif /* ifdef _PATCHIFY_H */
