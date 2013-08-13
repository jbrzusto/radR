/* svn $Id: patchify.c 671 2010-10-08 18:47:29Z john $

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

   patchify.c: create patches of identical cells in a buffer

   JMB 18 Jan, 2009: change PATCH_BINARY_MODE to PATCH_BACKGROUND_MODE
   and add new PATCH_FOREGROUND_MODE which works in opposite sense:
   patches are made only from cells with value equal to the foreground
   value, (as opposed to cells with value NOT equal to the background
   value).  This is to allow for the new class "excluded", which
   corresponds to regions not processed.

   JMB Aug 24, 2006: reworked with a much simpler run-loop merging algorithm.
   Rather than using callbacks to enumerate patches as they are found,
   we link all runs into loops containing all runs in a patch,
   and then explicitly enumerate patches from this structure.

   Advantages:

   - the algorithm is much simpler (and faster)

   - vertical wrapping (i.e. treating the top and bottom border of the image
   as adjacent) is now straightforward

   - there is a well-defined state in which a complete representation
   of all patches is available, for saving from, loading into, or
   filtering.

   JMB May 2, 2006:

   patchify.c - extract patches of contiguous (in either the 4 or 8
   neighbour sense) equi-coloured cells from a run-length encoded image.
   Only non-background patches are extracted, although this can be
   circumvented by picking a non-existing cell value for background_val.
   If compile-time flag PATCH_BACKGROUND_MODE is defined, the image is treated
   as background (i.e. cells with background_val) vs foreground
   (all other cells).  In PATCH_BACKGROUND_MODE, patches consist of contiguous
   non-background cells. 

   Compiled from various changes I made to fragstats in 1998 to have it
   run-length-encode images and perform patch extraction and statistics
   directly on the RLE image.  None of this code is derived from the
   original fragstats code, however.

   TODO: once the bitmap has been compressed, changing a cell is done
   by temporarily decompressing the corresponding row into a buffer.
   Because the buffer holds only limited rows (otherwise, why bother
   compressing), eventually some of these must be discarded.  If they
   have been changed, these changes are not recorded back into
   compressed storage, because the number of runs might have changed.
   Mainly, the function save_buff_row() and save_row() need changing.
   Most (all?) of the infrastructure for allowing this appears to be in
   place.
*/

#include "patchify.h"
#include "radRprot.h"
#include "radRvars.h"

/* forward declarations */

int ensure_image (t_image image);
void free_image (t_image img);
int save_row (t_image image, t_dim row, t_patch_cell *raw, t_patch_cell background_val, unsigned skip);
t_dim compress_row (t_image image, t_patch_cell *row, t_dim row_num, t_patch_cell background_val, unsigned skip);

/* static variables */

t_dim_2 num_patches;
t_dim_2 num_singletons;
t_dim_2 num_cells;
t_dim_2 num_patch_runs;

int
ensure_image (t_image image)
{
  // ensure that the various image-related buffers hold enough space to 
  // compress an image with size num_rows by num_cols; and mark
  // the structure as empty

  (*pensure_extmat)(&image->runs, RUN_ALLOC_INCREMENT, 1);
  (*pensure_extmat)(&image->scratch_row, 1 + image->run_info.num_cols, 1);
  image->run_info.num_runs = 0;
  return TRUE;
}
  
void
free_image (t_image image)
{
  if (image) {
    (*pfree_extmat) (&image->runs);
    (*pfree_extmat) (&image->scratch_row);
  }
}

int
save_row (t_image image, t_dim row, t_patch_cell *raw, t_patch_cell background_val, unsigned skip) 
{
  /* Save a raw row into compressed storage
     Rows must be saved in order from first to last.
  */

  t_dim_2 num_old_runs, num_new_runs;

  num_old_runs = image->run_info.num_runs;
  num_new_runs = compress_row (image, raw, row, background_val, skip);
  (*pensure_extmat)(&image->runs, num_old_runs + num_new_runs + 2, 1);

  /* copy the runs for this row from scratch_row.  Note: we keep a bogus first run to point to the first patch
     which keeps bounds-checking code from complaining about creating a pointer to nonexistent storage */
  memcpy (((t_cell_run *) image->runs.ptr) + 1 + num_old_runs, ((t_cell_run *) image->scratch_row.ptr), num_new_runs * sizeof (t_cell_run));
  image->run_info.num_runs = num_old_runs + num_new_runs;
  return TRUE;
}


t_dim
compress_row (t_image image, t_patch_cell *row, t_dim row_num, t_patch_cell foreground_val, unsigned skip)
{
  /* compress the given row into scratch_row, returning the number of runs */
  /* row_num gives the physical row number, for recording in cell-runs */

  register t_patch_cell *rb = row;
#ifndef PATCH_FOREGROUND_MODE
  register t_patch_cell val;
#endif /* not PATCH_FOREGROUND_MODE */
  register t_cell_run *run = ((t_cell_run *) image->scratch_row.ptr);
  register t_patch_cell *old_rb;
  t_dim col = skip;
  t_dim num_runs = 0;
  t_dim num_cols = image->run_info.num_cols;  /* number of columns in input image    */

  /* set up a dummy cell value, for which space has been allocated,
     so that a run automatically ends at the end of the real data.
     (i.e. the dummy cell has a value different from the last real cell)
     This means we don't have to worry about running over the end
     of the buffer in the inner loop below.  We are using the same
     extra end-of-row cell as we use to mark whether a compressed
     row has been changed */

#ifdef PATCH_FOREGROUND_MODE
  rb[num_cols] = 1 + foreground_val; /* any value not equal to foreground_val */
#else
  rb[num_cols] = PATCH_INVALID_CELL;
#endif /* PATCH_FOREGROUND_MODE */
  rb += skip;

  do 
    {
#ifdef PATCH_FOREGROUND_MODE
      // skip past any background cells
      while (col < num_cols && *rb != foreground_val) {
	++col;
	++rb;
      }
      if (col >= num_cols)
	break;
      // record the start of a non-background run
      old_rb = rb;
      // scan to the end of the run (protected by the non-foreground val in column num_cols)
      while (*rb == foreground_val)
	++rb;
#else
      val = *(old_rb = rb++);
      while (*rb == val)  /* the dummy cell protects this loop from buffer overrun */
	++rb;
      run->value = val;
#endif /* PATCH_FOREGROUND_MODE */
      if (1 == (run->length = rb - old_rb))
	// count this run as a singleton
	// When we merge runs, we will reduce the count each time
	// a singleton loop is merged with another loop.

	++ num_singletons;
	
      run->row = row_num;
      run->col = col;
      col += run->length;
      num_cells += run->length;
      ++run;	                /* the number of runs allocated in
		                   scratch row equals the number of
		                   columns, so we are protected from
		                   overrunning the run buffer */
      ++num_runs;
    }
  while (col < num_cols);
  return num_runs;
}

t_cell_run *
get_patch_from_rc (t_image image, t_dim row, t_dim col)
{
  /*
    get the first run in the patch that includes the cell at row, col
    Return NULL if the cell is not in a patch.  

    If image->run_info.runs_are_sorted is TRUE, we can use a fast bisection algorithm:

    Because each patch is stored as a loop of runs, the entire
    patch can be enumerated by knowing one of its runs.
    We use bisection search to find a run containing the given cell.
    If the given cell is not in a patch, return NULL.

    Otherwise, we assume that the first run for each patch (i.e. the one reached
    by following the next_patch_offset fields) is the earliest (lowest column
    within lowest row) run for its patch, and use a much slower search.

  */

  t_cell_run *run_lo, *run_hi, *run_mid, *runs, *first_run;

  /*   if (! image || !image->run_info.num_runs) */
  /*     return NULL; */

  runs = ((t_cell_run *) image->runs.ptr) + 1;
  if (image->run_info.runs_are_sorted) {
    run_mid = run_lo = runs + 1;
    run_hi = run_lo + image->run_info.num_runs - 1; // the last real run

    while (run_hi >= run_lo) {
      /* loop invariant: (row, col) is no earlier than run_lo and no later than run_hi
	 and run_lo <= run_hi */

      run_mid = run_lo + (run_hi - run_lo) / 2;

      if (row < run_mid->row || (row == run_mid->row && col < run_mid->col)) {
	/* (row, col) is earlier than run_mid */
	run_hi = run_mid - 1;
      } else if (row > run_mid->row || (row == run_mid->row && col >= run_mid->col + run_mid->length)) {
	/* (row, col) is later than run_mid */
	run_lo = run_mid + 1;
      } else {
	/* (row, col) is within run_mid */
	break;
      }
    }
    if (run_hi < run_lo)
      return NULL;
    run_lo = run_mid;
  } else {
    // use the slow brute-force algorithm:

    for (run_lo = ((t_cell_run *) image->runs.ptr) + 1, run_hi = run_lo + image->run_info.num_runs; run_lo < run_hi; ++run_lo) {
      if (run_lo->row == row && run_lo->col <= col && run_lo->col + run_lo->length > col)
	break;
    }
    if (run_lo == run_hi)
      return NULL;
    run_mid = run_lo;
  }

  // both run_mid and run_lo now point to a run in the 
  // patch containing the sample at (row, col)
  
  // find the first run in this patch, namely the one
  // earliest in the run buffer

  first_run = run_lo;
  do {
    if (run_lo  < first_run)
      first_run = run_lo;
    run_lo += run_lo->next_run_offset;
  } while (run_lo != run_mid);

  return first_run;
}

t_dim_2 inline
get_patch_id (t_cell_run *r, t_cell_run *runs)
{
  /* return the true patch id of run r;
     "runs" points to the start of the run buffer */

  t_cell_run *old_r = r;
  t_dim_2 rv, tmp;

  /* trace this patch back along relabellings to the
     true patch id; i.e. find a run whose patch_id
     is equals its index in the run array, meaning it
     has not been relabelled.  Note that the run
     at index 0 in runs is bogus, and not part of any
     patch.
  */

  while ((rv = iabs(r->patch_id)) != r - runs)
    r = runs + rv;

  /* to speed up lookups next time, flatten the lookup structure by
     pointing each intermediate patch_id to the true patch_id  */

  while ((tmp = iabs(old_r->patch_id)) != rv) {
    old_r->patch_id = rv;
    old_r = runs + tmp;
  }
  return rv;
}

void inline
merge_patches (t_cell_run *r1, t_cell_run *r2, t_cell_run *runs)
{
  /* Merge the two patches represented by runs r1 and r2, which
     are in the full run array runs.

     If the patches are the same, do nothing.

     If the patches are distinct, relabel the patch_id field in the
     smallest run of the patch whose smallest run is the larger.

     The result is to maintain a collection of patches such that
     each patch has a unique id equal to the smallest patch_id field
     of all runs in the patch, and the runs in a patch form a circular
     linked list.
  */

  t_dim_2 pid1, pid2;
  int tmp;

  pid1 = get_patch_id(r1, runs);
  pid2 = get_patch_id(r2, runs);

  if (pid1 != pid2) {
    /* r1 and r2 represent different patches, so merge them by swapping next_run_offset pointers */

    /* if r1 is a singleton, reduce the count of these */
    
    if (r1->next_run_offset == 0 && r1->length == 1) 
      -- num_singletons;

    /* if r2 is a singleton, reduce the count of these */
    
    if (r2->next_run_offset == 0 && r2->length == 1) 
      -- num_singletons;

    tmp = r1 + r1->next_run_offset - r2;
    r1->next_run_offset = r2 + r2->next_run_offset - r1;
    r2->next_run_offset = tmp;

    /* change the larger patch_id to the smaller one; CAUTION: this must be done for the
       run which actually supplies the value of patch_id in get_patch_id, which is why
       we do 
       (runs + pidX)->patch_id = pidY  
       rather than
       rX->patch_id = pidY

       A simple inductive proof shows this yields a correct collection of runs; i.e. get_patch_id(r) = id_p
       for every run r in the patch p.  id_p is defined as min(r->patch_id such that r is a run in patch p)

       - inductive hypothesis: for all patches p, get_patch_id(r_p) = id_p for all runs r_p in patch p, and
       id_p == id_q => p == q

       - the inductive hypothesis is initially true, since each patch is a single run with unique id.

       - let p and q be two patches, and let pq be the merged patch.
       Then for any run r_pq in the merged patch pq,
       get_patch_id(r_pq) = min(id_p, id_q) which is the id of r_pq,
       and id_pq != id_P for any other patch P != pq
         
       Hence the collection of patches after a patch merger still satisfies the inductive
       hypothesis.
    */


    if (pid1 < pid2)
      (runs + pid2)->patch_id = pid1;
    else
      (runs + pid1)->patch_id = pid2;

    /* because we have merged two patches into one, decrease the number of patches */

    -- num_patches;
  }
}
          
void
patchify_image (t_image image, int use_diags, int vertical_wrap)
{

  t_dim i, j;
  t_dim_2 id;

  t_cell_run *run_ptr_old, *run_ptr_new; /* current runs in the old and new rows */
  t_cell_run *runs = (t_cell_run *) image->runs.ptr;  /* pointer to run buffer */
  t_dim right_joint_type;
  
  if (!image->run_info.num_runs)		// in case no runs were found, there are no patches 
    return;

  /* set each run to point to itself, making a singleton run-loop, and assign it
     a unique positive id */

  for (id = 1, run_ptr_new = ((t_cell_run *) image->runs.ptr) + 1; id <= image->run_info.num_runs; ++id, ++run_ptr_new) {
    run_ptr_new->next_run_offset = 0;
    run_ptr_new->patch_id = id;
  }

  /* 
     This loop merges run-loops in row i with run-loops in row i-1.
     We set up for an extra iteration, in case vertical wrapping is enabled.

  */
  for (i = 0, j = 1, run_ptr_old = run_ptr_new = ((t_cell_run *) image->runs.ptr) + 1; i < image->run_info.num_rows; ++i, ++j)
    {
      /* 
	 set pointers to the first runs of rows i and j;
	 if there are no runs in the required row, point to the
	 first run with a larger row at least as large.
      */

      while (run_ptr_old->row < i)
	++run_ptr_old;

      if (j < image->run_info.num_rows) {
	while (run_ptr_new->row < j)
	  ++run_ptr_new;
      } else {
	/* if vertical wrapping is not enabled, we are finished */
	if (! vertical_wrap)
	  break;

	/* set up pointers to merge adjacent runs in the last and first rows */
	run_ptr_new = ((t_cell_run *) image->runs.ptr) + 1;
	j = 0;
      }

      /* for each run in the new row, see if it is adjacent to any runs
         of the same cell type in the old row */

      while(run_ptr_old->row == i && run_ptr_new->row == j)
	{
	  /* if the old run is entirely left of the new run, then use the next old run */
	  if (run_ptr_old->col + run_ptr_old->length + use_diags <= run_ptr_new->col) {
	    ++run_ptr_old;
	    continue;
	  }

	  /* if the new run is entirely left of the old run, then use the next new run */
	  if (run_ptr_new->col + run_ptr_new->length + use_diags <= run_ptr_old->col) {
	    ++run_ptr_new;
	    continue;
	  }

	  /* there is overlap (or maybe only diagonal adjacency, if
	     use_diags == 1) between runs so a merger will be
	     necessary if they have the same cell value or if we are in binary mode */

	  /* compute a number whose signum indicates the situation at
	     the right ends of the two runs, so we'll know which run
	     pointer(s) to update later */

	  right_joint_type = run_ptr_old->col + run_ptr_old->length - (run_ptr_new->col + run_ptr_new->length);
#ifndef PATCH_FOREGROUND_MODE
	  if (run_ptr_new->value == run_ptr_old->value)
#endif /* not PATCH_FOREGROUND_MODE */

	    /* assign this run in the new row its correct id, and update the
	       count for the patch of this run's type */
	    merge_patches (run_ptr_old, run_ptr_new, runs);

#ifndef PATCH_FOREGROUND_MODE
	  else if (use_diags && (right_joint_type == 0))
	    {
	      /* we are using diagonals, and have just looked at runs A and B in
	         a situation like this:

	         old row:  ...AAAAAAAAACCCCCCCC...
	         new row:  ...BBBBBBBBBDDDDDDDD...
	         ^

	         where the runs in the old and new rows end at the same point.
	         (the caret marks the position calculated as j, below)
	         We have found that A != B
	         We now compare B to C, and A to D, remembering that
		 either C or D might be an unrecorded run of background cells
	      */
	      /* compare B to C:  check first whether C exists, then whether
	         it is actually adjacent to A (i.e. no intervening background) */
	      if ((run_ptr_old + 1)->row == i
		  && (run_ptr_old + 1)->col == run_ptr_old->col + run_ptr_old->length
		  && (run_ptr_old + 1)->value == run_ptr_new->value
		  )
		merge_patches (run_ptr_old, run_ptr_new, runs);

	      /* and now compare A to D the same way: */

	      if ((run_ptr_new + 1)->row == j
		  && (run_ptr_new + 1)->col == run_ptr_new->col + run_ptr_new->length
		  && (run_ptr_new + 1)->value == run_ptr_old->value
		  )
		merge_patches (run_ptr_old, run_ptr_new, runs);
	    }

#endif /* not PATCH_FOREGROUND_MODE */
	  /* we can now safely move on to new runs */

	  /* now move to new run(s), depending on overlap type */

	  if (right_joint_type >= 0)
	    ++run_ptr_new;
	  if (right_joint_type <= 0)
	    ++run_ptr_old;
	}
    }	/* end of loop scanning through rows  */
}

void 
link_patches(t_image image) 
{
  // set the next_patch_offset field of the first run in each patch to
  // "point" to the first run of the next patch.  This function is only
  // called immediately after patches have been created;
  // it requires all runs to have a positive patch_id.
  //
  // Side effect: the patch_id field for each run will be given
  // the correct value.

  t_cell_run *runs = ((t_cell_run *) image->runs.ptr) ;
  t_cell_run *run = runs + 1; // skip bogus first run
  t_cell_run *prev_run = runs;
  t_cell_run *last_run = run + image->run_info.num_runs - 1;
  t_dim_2 max_pid = 0, pid;
  char ds = image->drop_singletons;
  if (!image->run_info.num_runs)
    return;

  for(;;) {
    /* if this is the first run in a patch (i.e. its true id is 
       larger than that of any patch seen so far)
       and drop_singletons is FALSE or it is not a singleton */

    pid = get_patch_id(run, runs);

    if (pid > max_pid && (!ds || run->length > 1 || run->next_run_offset != 0)) {
      /* adjust the next_patch_offset from the previous patch to
	 point to this run */
      max_pid = pid;
      if (prev_run)
	prev_run->next_patch_offset = run - prev_run;
      prev_run = run;
    }
    if (run == last_run)
      break;
    ++run;
  }
  prev_run->next_patch_offset = 0;
}

  
int
patchify_buffer(t_patch_cell *buf, t_image image, unsigned skip)
{
  // find contigous patches in buf based on current patch parameters
  // buf must hold image->nrows * image->ncols values of type t_patch_cell

  // skip: a non-negative number of cells to skip at the start of each row.

  // No dummy cell is required at the end of each row, but the storage allocated 
  // for buf must have one extra cell at the end of the whole storage.

  t_dim i;
  t_patch_cell save_first;

  if (!ensure_image(image))
    return FALSE;

  num_patches 		= 0;
  num_singletons 	= 0;
  num_cells 	        = 0;
  num_patch_runs 	= 0;
  
  // store the classified image into compressed storage
  // one row at a time

  for (i=0; i < image->run_info.num_rows; ++i) {

    // save the first cell of the next row, since compress_row needs
    // to use that slot (i.e. a dummy extra cell at the end of this
    // row)

    save_first = buf[image->run_info.num_cols];
    if (! save_row(image, i, buf, image->fgd, skip))
      return FALSE;
    
    // restore the saved cell value

    buf[image->run_info.num_cols] = save_first;
    buf += image->run_info.num_cols;
  }

  // give the bogus extra run a row of num_rows for simplifying
  // the patchify algorithm (num_runs counts only real runs,
  // and not the first and last runs, which are bogus)

  ((t_cell_run *) image->runs.ptr)[image->run_info.num_runs + 1].row = image->run_info.num_rows;

  // record the true number of runs in the run buffer (don't count the 2 bogus runs)
  image->run_info.num_runs = image->runs.length - 2;

  // initially, each run is its own patch
  num_patches = image->run_info.num_runs;

  patchify_image (image, image->use_diags, image->vertical_wrap);
  link_patches (image);

  image->run_info.num_singletons = num_singletons;
  image->run_info.num_patches = num_patches - (image->drop_singletons ? num_singletons : 0);

  /* the number of active patches is, so far, the number of patches */
  image->run_info.num_active_patches = image->run_info.num_patches;
  image->run_info.num_cells = num_cells;
  image->run_info.num_active_runs = image->run_info.num_runs - (image->drop_singletons ? num_singletons : 0);
  image->run_info.num_active_cells = image->run_info.num_cells - (image->drop_singletons ? num_singletons : 0);

  /* indicate that runs are sorted by increasing column within increasing row */
  image->run_info.runs_are_sorted = TRUE;
  return TRUE;
}
  
void
shut_down_patchify(t_image image)
{
  /* free resources associated with the patchifying algorithm */
  if (image) {
    free_image(image);
    Free(image);
  }
}

void 
enumerate_patches(t_image image, patch_function f)
{
  /* for each patch in the linked list of active patches, call f with a pointer
     to the first run in that patch (the runs are in looped lists) 

     f returns one of four values: 
     KEEP - keep this patch active
     DROP - deactivate this patch
     QUIT_AND_KEEP - keep this patch active and quit the enumeration
     QUIT_AND_DROP - deactivate this patch and quit the enumeration.

     For deletions from the singly-linked list of active patches, we need to keep track
     of the location of the previous active patch, in order to adjust its next_patch_offset.
     For the first active patch, we make a pointer to a fake t_cell_run that has the next_patch_offset
     field at the correct offset. */

  t_cell_run *prev_patch_first_run;
  t_cell_run *run, *run2;
  int offset;
  t_pf_rv rv;

  if (!image->run_info.num_runs || !image->run_info.num_active_patches) 
    return;

  prev_patch_first_run = &((t_cell_run *) image->runs.ptr)[0];
  run = prev_patch_first_run + prev_patch_first_run->next_patch_offset;

  for ( ; ; ) {

    /* if the patch function returns false, drop this patch from the
       linked list of active patches. Note that if this is the last
       patch (i.e. run->next_patch_offset == 0), then we force
       prev_patch_first_run->next_patch_offset to be 0.  The first run
       of an inactive patch is also marked by negating its patch_id
       (and only runs which are the first run of an inactive patch
       will have a negative id); this is to simplify finding the patch
       containing a given cell in get_patch_from_rc() */

    offset = run->next_patch_offset;  

    rv = (*f)(run);
    if (rv & RADR_PF_DROP) {
      prev_patch_first_run->next_patch_offset = offset ? run + offset - prev_patch_first_run : 0;
      -- image->run_info.num_active_patches;
      run->patch_id = - run->patch_id;
      // remove the count of runs and cells in this patch from appropriate totals
      run2 = run;
      do {
	image->run_info.num_active_cells -= run2->length;
	-- image->run_info.num_active_runs;
	run2 += run2->next_run_offset;
      } while (run2 != run);
    } else {
      // otherwise, update the pointer to the previous patch's first run.
      prev_patch_first_run = run;
    }
    // if we've reached the end of the linked list of active patches, break
    if ((rv & RADR_PF_QUIT) || !offset)
      break;
    run += offset;
  }
}
  
t_cell_run *
enumerate_all_patches(t_image image, patch_function f, int *npat, int *patch_selector) 
{
  // enumerate all patches, whether active or inactive, and set their
  // active/inactive status according to the value returned by f.

  // This does not enumerate singletons.

  // It will quit early if f(...) & RADR_PF_QUIT is non-zero.

  // We scan the entire list of runs, looking for the first
  // run in each patch.  If (and only if) the patch is deactivated,
  // the first run has a negative patch_id.  If the patch is active,
  // its first run is pointed to by the first run of its preceding active patch,
  // and that's how we recognize it.

  // We call linked_patch_function on each patch, active or deactivated,
  // passing the patch's first run as the first parameter, and the 
  // first run of the preceding active patch as the second paramter.

  // Returns: pointer to the first run of the last active patch seen (which might
  // not be the last active patch if f(...) & RADR_PF_QUIT is true for some patch.

  // The number of patches is returned in npat, if it is not NULL.

  t_cell_run *run, *prev_apatch, *next_apatch, *last_run, *run2;
  int is_active;
  t_pf_rv rv;
  int unused;

  if (!npat)
    npat = &unused;

  *npat = 0;
  if (!image->run_info.num_runs || !image->run_info.num_patches)
    return NULL;

  run = ((t_cell_run *) image->runs.ptr) + 1;
  prev_apatch = & ((t_cell_run *) image->runs.ptr)[0];
  next_apatch = prev_apatch->next_patch_offset ? prev_apatch + prev_apatch->next_patch_offset : NULL;
  last_run = run + image->run_info.num_runs - 1;

  while (run <= last_run) {
    is_active = (run == next_apatch);
    if (is_active)
      next_apatch = run->next_patch_offset ? run + run->next_patch_offset : NULL;
    if (is_active || run->patch_id < 0) {
      ++*npat;
      if (!patch_selector || patch_selector[*npat - 1]) {
	rv = (*f)(run);
	// we need to relink/delink the patch and update counters if 
	// f(...) wants its state changed
	if (! (rv & RADR_PF_SAME)) {
	  if (is_active && (rv & RADR_PF_DROP)) {
	    // the patch is being deactivated
	    // unlink it from the active list
	    prev_apatch->next_patch_offset = next_apatch ? next_apatch - prev_apatch : 0;
	    // reduce the count of active patches
	    -- image->run_info.num_active_patches;
	    // mark its first run with a negative ID
	    run->patch_id = - run->patch_id;
	    // remove its count of runs and cells from counters
	    run2 = run;
	    do {
	      image->run_info.num_active_cells -= run2->length;
	      -- image->run_info.num_active_runs;
	      run2 += run2->next_run_offset;
	    } while (run2 != run);
	  } else if (!is_active && !(rv & RADR_PF_DROP)) {
	    // the patch is being activated
	    // insert it into the active list, preserving a pointer to what used
	    // to be the next active patch
	    run->next_patch_offset = next_apatch ? next_apatch - run : 0;
	    prev_apatch->next_patch_offset = run - prev_apatch;
	    // increase the count of active patches
	    ++ image->run_info.num_active_patches;
	    // mark its first run with a positive ID
	    run->patch_id = - run->patch_id;
	    // add its count of runs and cells from counters
	    run2 = run;
	    do {
	      image->run_info.num_active_cells += run2->length;
	      ++ image->run_info.num_active_runs;
	      run2 += run2->next_run_offset;
	    } while (run2 != run);
	  }
	}
	// if the patch is active (newly or originally), update the prev_apatch pointer
	if (run->patch_id > 0)
	  prev_apatch = run;

	// if f(...) wants a quit, do so
	if (rv & RADR_PF_QUIT)
	  break;
      }
    }    
    ++run;
  }
  return prev_apatch;
}


t_pf_rv 
pf_reactivate_all (t_cell_run *run) 
{
  // make sure the patch pointed to by run is active.

  return KEEP;
}

void 
reactivate_all_patches(t_image image) 
{
  // given that some patches may have been deactivated,
  // reactivate them by reconstructing the entire
  // linked list of patches

  int npat = 0;

  if (!image->run_info.num_runs || !image->run_info.num_patches)
    return;

  enumerate_all_patches(image, pf_reactivate_all, &npat, NULL);

  image->run_info.num_active_patches = npat;
  image->run_info.num_active_runs = image->run_info.num_runs - (image->drop_singletons ? image->run_info.num_singletons : 0);
  image->run_info.num_active_cells = image->run_info.num_cells - (image->drop_singletons ? image->run_info.num_singletons : 0);
}


SEXP
get_active_runbuf (SEXP patchessxp)
{
  // return a newly allocated RAWSXP containing a runbuf_info header followed by a
  // sorted set of runs corresponding only to active patches, followed
  // by the number of active patch cells.  This buffer can be saved
  // and then restored into an image's runs buffer.  As for the
  // rawarch plugin, we use a CHARSXP rather than a RAWSXP because R's
  // serialization of RAWSXP is very slow.

  // patchessxp: an EXTPTRSEXP pointing to a t_image structure.
  //
  // The algorithm works in two passes:
  //   1. calculate the new index of each run in an active patch, once runs in inactive
  //      patches have been removed
  //   2. copy the runs from active patches to the new buffer, correcting the next_patch_offset 
  //      and next_run_offset using the array calculated in pass one.

  t_image image;
  int *new_index; // the new index of each active run in the original buffer;
                  // Because a patch id is also the index of the first run in the patch,
                  // we use this to record whether a patch is active or not, by setting
                  // it to -1 for each index corresponding to the first run in an inactive patch.

  int i, *ni; // indexe, pointer to new index buffer
  int j;
  int num_active_runs; // counter of active cell runs
  t_cell_run *p, *ep, *q;  // pointer and end pointer for all runs
  int id;
  t_runbuf_info *rbi;
  t_dim_2 max_patch_id_seen;
  SEXP rv;

  image = (t_image) EXTPTR_PTR(patchessxp);
  if ( ! image->run_info.num_active_patches)
    return R_NilValue;

  // allocate return value

  PROTECT(rv = allocVector(RAWSXP, sizeof(t_runbuf_info) + (image->run_info.num_active_runs + 2) * sizeof(t_cell_run))); 

  // allocate temporary buffer

  new_index = Calloc(image->run_info.num_runs + 2, int); // "+2" counts the bogus first and last runs

  // copy the image structure fields

  

  // Pass 1:   to calculate the new index for a run, we need to know how many previous runs are active
  //           We scan through all runs, keeping track of which patch IDs correspond to active patches.
  //           A patch is active iff the patch_id field in its first run is positive and the patch
  //           is not a singleton.

  num_active_runs = 0;

  p = 1 + (t_cell_run *) image->runs.ptr;
  ep = p + image->run_info.num_runs;

  for (ni = new_index + 1; p < ep; ++p, ++ni) {
    id = iabs(get_patch_id(p, (t_cell_run *) image->runs.ptr));
#ifdef RADR_DEBUG
    if (id > image->run_info.num_runs)
      printf("Bad run ID: %d\n", id);
#endif
    if (new_index[id] == 0)
      // the patch with this ID has not been seen yet, so this is
      // its first run; provide it with its new index, or mark it as inactive
      // The test p->patch_id > 0 is valid since this is the patch's first run.
      *ni = (p->patch_id > 0 && (p->next_run_offset != 0 || p->length > 1)) ? ++num_active_runs : -1;
    else if (new_index[id] > 0)
      // the patch with this ID has been seen, and is active, 
      // so record the new index for this run
      *ni = ++num_active_runs;
  }

#ifdef RADR_DEBUG
  if (num_active_runs != image->run_info.num_active_runs)
    printf("get_active_runbuf: mismatch between num_active_runs = %d and image->run_info.num_active_runs = %d\n", num_active_runs, image->run_info.num_active_runs);
#endif

  // Pass 2:   copy the active runs, correcting their offset fields

  rbi = (t_runbuf_info *) RAW(rv);
  q = (t_cell_run *) (rbi + 1);

  // make the bogus first run point to the first real run

  memset(q, 0, sizeof(*q));
  q++->next_patch_offset = 1;

  j = 1;
  max_patch_id_seen = 0;
  for (i = 1, p = 1 + (t_cell_run *) image->runs.ptr, ep = p + image->run_info.num_runs; p < ep; ++p, ++i) {
    id = iabs(get_patch_id(p, (t_cell_run *) image->runs.ptr));
#ifdef RADR_DEBUG
    if (id > image->run_info.num_runs || id < 1)
      printf("Bad run ID: %d\n", id);
#endif
    if (new_index[id] > 0) {
      // copy the run to the new buffer, correcting offsets to next
      // run in patch, first run in next patch
#ifdef RADR_DEBUG
      if (i + p->next_run_offset > image->run_info.num_runs ||  i + p->next_run_offset < 1)
	printf("Bad next run ID: %d\n", i + p->next_run_offset);
#endif
      q->next_run_offset = new_index[i + p->next_run_offset] - j;

      if (id > max_patch_id_seen) {
	// this is the first run in a patch (and is a reliable test even if the run
	// buffer was created by blipmoviearch (which sorts runs within blips, but orders
	// blips by column within row).

	max_patch_id_seen = id;
#ifdef RADR_DEBUG
	if (i + p->next_patch_offset > image->run_info.num_runs || i + p->next_patch_offset < 1)
	  printf("Bad next patch ID: %d\n", i + p->next_patch_offset);
#endif
	q->next_patch_offset = new_index[i + p->next_patch_offset] - j;
      } else {
	q->next_patch_offset = 0;  // it's uninitialized storage
      }
      q->patch_id = new_index[id];
      q->length = p->length;
      q->row = p->row;
      q->col = p->col;
      ++q;
      ++j;
    }
  }
#ifdef RADR_DEBUG
  if (j != num_active_runs + 1)
    printf("Wrong number of 'active patch runs' found: %d instead of %d\n", j, num_active_runs);
#endif

  // make the bogus last run have row value just past the last real row

  memset(q, 0, sizeof(*q));

  q->row = image->run_info.num_rows;

  // save required metadata 

  * rbi = image->run_info;
  rbi->num_cells  = rbi->num_active_cells;
  rbi->num_runs = rbi->num_active_runs;
  rbi->num_patches = rbi->num_active_patches;
  rbi->num_singletons = 0;
  rbi->runs_are_sorted = 1;

  Free(new_index);

  UNPROTECT(2);
  
  return (rv);
}

SEXP
set_runbuf (SEXP patchessxp, SEXP runsxp)
{
  // Set the run/patch buffer of the image wrapped by patchessxp to the
  // value contained in the raw vector runsxp.
  // If runsxp == NULL, then we mark the image's run buffer as empty.
  //
  // patchessxp: EXTPTRSEXP pointing to a t_image struct
  // runssxp:    RAWSXP of run data

  t_image image;
  t_cell_run *run;
  t_runbuf_info *rbi;

  image = (t_image) EXTPTR_PTR(patchessxp);
  if (runsxp != R_NilValue) {
    rbi = (t_runbuf_info *) RAW(runsxp);
    
    run = (t_cell_run *) (rbi + 1);
    
    (*pensure_extmat) (&image->runs, rbi->num_runs + 2, 1);
    memcpy (image->runs.ptr, run, (rbi->num_runs + 2) * sizeof(t_cell_run));
    
    // copy the metadata
    
    image->run_info = * rbi;
  } else {
    // mark the image's run buffer as empty

    memset(& image->run_info, 0, sizeof(t_runbuf_info));
  }

  return R_NilValue;
}

SEXP
get_indexes_from_runbuf (SEXP runsxp)
{
  // Get the integer vector of indexes of all slots in all
  // patches in the runbuf; the slots are sorted by samples within pulses
  // This vector is origin 1, so it can be used to index an extmat.
  
  t_runbuf_info *rbi;
  t_cell_run *run, *runmax;
  SEXP rv;
  int num_cols;
  int *p;
  int i;

  rbi = (t_runbuf_info *) RAW(runsxp);
  run = 1 + (t_cell_run *) (rbi + 1); // skip first bogus run
  num_cols = rbi->num_cols;

  PROTECT(rv = allocVector(INTSXP, rbi->num_cells));

  for (p = INTEGER(rv), runmax = run + rbi->num_runs; run < runmax; ++run) {
    // index of first cell in the run is computed explicitly
    *p++ = run->row * num_cols + run->col + 1;
    for (i = run->length; i > 1;--i, ++p)
      // each subsequent index of a cell in this run is one larger than its predecessor
      *p = *(p-1) + 1;
  }
  
  UNPROTECT(1);
  return(rv);
}

SEXP
get_runbuf_info (SEXP runsxp)
{
  // Get the number of patches and samples represented by a run buffer
  t_runbuf_info *rbi;
  SEXP rv;

  rbi = (t_runbuf_info *) RAW(runsxp);
  rv = allocVector(INTSXP, 2);
  INTEGER(rv)[0] = rbi->num_patches;
  INTEGER(rv)[1] = rbi->num_cells;
  return (rv);
}

SEXP
index_extmat_by_runbuf (SEXP matsxp, SEXP runsxp)
{
  // extract items from an extmat according to the runs
  // in a run buffer, and return them as a single packed
  // string
  //
  // matsxp: EXTPTRSXP wrapping an extmat
  // runsxp: RAWSXP containing the results of a call to get_active_runbuf
  //
  // returns: raw vector containing the raw data from the extmat selected
  //          by the runs in the run buffer

  SEXP rv;
  t_runbuf_info *rbi;
  t_cell_run *run, *runmax;
  t_extmat *mat;
  int ncol;
  int n, m;
  char *p, *q;

  mat = SEXP_TO_EXTMAT(matsxp);
  rbi = (t_runbuf_info *) RAW(runsxp);
  if (mat->cols != rbi->num_cols || mat->rows != rbi->num_rows)
    error("index_extmat_by_runbuf: dimensions of matrix (%d, %d) and runbuf (%d, %d) do not match", mat->rows, mat->cols, rbi->num_rows, rbi->num_cols);

  run = 1 + (t_cell_run *) (rbi + 1); // skip first bogus run
  m = mat->size;
  ncol = mat->cols;

  // allocate return value
  PROTECT(rv = allocVector(RAWSXP, rbi->num_cells * m));

  p = (char *) RAW(rv);
  q = (char *) mat->ptr;

  for (runmax = run + rbi->num_runs; run < runmax; ++run) {
    n = run->length * m;
    memcpy (p, q + (run->row * ncol + run->col) * m, n);
    p += n;
  }
  UNPROTECT(1);
  return(rv);
}

SEXP
assign_extmat_by_runbuf (SEXP matsxp, SEXP runsxp, SEXP valsxp)
{
  // extract items from an extmat according to the runs
  // in a run buffer, and return them as a single packed
  // string
  //
  // matsxp: EXTPTRSXP wrapping an extmat
  // runsxp: RAWSXP raw vector containing the results of a call to get_active_runbuf
  // valsxp: RAWSXP raw vector containing the raw data to be stored in the slots of the extmat
  //         determined by runsxp
  // 
  // returns NULL

  t_extmat *mat;
  t_runbuf_info *rbi;
  t_cell_run *run, *runmax;
  int ncol;
  int n, m;
  char *p, *q;


  mat = SEXP_TO_EXTMAT(matsxp);
  rbi = (t_runbuf_info *) RAW(runsxp);

  if (mat->cols != rbi->num_cols || mat->rows != rbi->num_rows)
    error("assign_extmat_by_runbuf: dimensions of matrix (%d, %d) and runbuf (%d, %d) do not match", mat->rows, mat->cols, rbi->num_rows, rbi->num_cols);

  run = 1 + (t_cell_run *) (rbi + 1); // skip first bogus run
  m = mat->size;
  ncol = mat->cols;

  if (LENGTH(valsxp) != m * rbi->num_cells)
    error("assign_extmat_by_runbuf: RHS holds %d bytes but runbuf specifies %d cells of size %d = %d bytes total", LENGTH(valsxp), rbi->num_cells, m, m * rbi->num_cells);

  p = (char *) RAW(valsxp);
  q = (char *) mat->ptr;

  for (runmax = run + rbi->num_runs; run < runmax; ++run) {
    n = run->length * m;
    memcpy (q + (run->row * ncol + run->col) * m, p, n);
    p += n;
  }
  return R_NilValue;
}
