#include <stdio.h>
#include "patchify.h"
#include <windows.h>
#include <wingdi.h>

#define err(X) {puts(#X); exit(1);}



char *buf;
short rows, cols, colp;

/* running sums for a single blip */

static int num_patches = 0;
static int num_patch_cells = 0;

#define CB_STOP 0	   /* stop callbacks on this patch */
#define CB_CONTINUE 1	   /* continue with callbacks on this patch */
#define PATCH_ACCEPTED 1


t_pf_rv
pf_patch_info (t_cell_run *r)
{
  t_cell_run *first_run = r;
  int blip_num_samples = 0;
  unsigned short row_lo = ~0; // big unsigned number
  unsigned short row_hi = 0;
  unsigned short col_lo = ~0; // big unsigned number
  unsigned short col_hi = 0;

  do {
    row_hi = max(row_hi, r->row);                 
    row_lo = min(row_lo, r->row);
    col_hi = max(col_hi, r->col + r->length - 1);    
    col_lo = min(col_lo, r->col);
    blip_num_samples += r->length;
    r += r->next_run_offset;
  } while (r != first_run);

  printf(

#ifndef PATCH_BINARY_MODE
	 "Color %d; "
#endif
	 "samples: %d; (%d, %d)-(%d, %d)\n",
#ifndef PATCH_BINARY_MODE
	 r->value,
#endif

	 blip_num_samples, col_lo, rows - 1 - row_hi, col_hi, rows - 1 - row_lo);

  ++num_patches;
  num_patch_cells += blip_num_samples;
return KEEP;
}

int
pf_patch_filter_odd (t_cell_run *r)
{
  t_cell_run *first_run = r;
  unsigned short row_lo = ~0; // big unsigned number
  unsigned short row_hi = 0;
  unsigned short col_lo = ~0; // big unsigned number
  unsigned short col_hi = 0;
  int ns = 0;

  do {
    row_hi = max(row_hi, r->row);                 
    row_lo = min(row_lo, r->row);
    col_hi = max(col_hi, r->col + r->length - 1);    
    col_lo = min(col_lo, r->col);
    ns += r->length;
    r += r->next_run_offset;
  } while (r != first_run);

  // drop patches with number of samples not divisible by 3
  return (ns % 3) ? FALSE : TRUE;
}

char bg = 0;

t_image_struct image;

int
main (int argc, char *argv[]) {
  FILE *f;
  BITMAPINFOHEADER bmh;
  BITMAPFILEHEADER bfh;
  image.use_diags = TRUE;
  image.vertical_wrap = FALSE;
  if (argc < 2) 
    err(must specify filename);
  if (argc > 2)
    bg = atoi(argv[2]);
  if (argc > 3)
    image.use_diags = atoi(argv[3]);
  if (argc > 4)
    image.vertical_wrap = atoi(argv[4]);
  f = fopen(argv[1], "rb");
  if (!f)
    err(unable to open file);
  if (1 != fread(&bfh, sizeof(bfh), 1, f))
    err(unable to read file header);
  if (1 != fread(&bmh, sizeof(bmh), 1, f))
    err(unable to read bitmap header);
  rows = bmh.biHeight;
  cols = bmh.biWidth;
  colp = (cols + 3) / 4 * 4;
  printf("Got bitmap with width %d->%d and height %d, expanded total of %d cells.\n", cols, colp, rows, colp * rows);
  cols = colp;

  buf = (char *) calloc(1, max(1024, rows * cols));
  if (!buf)
    err(unable to allocate file buffer);
  if (1 != fread(buf, 1024, 1, f))
    err(unable to read palette);
  if (1 != fread(buf, rows * cols, 1, f))
    err(unable to read image);

  image.bkgd = bg;
  image.num_rows = rows;
  image.num_cols = cols;
  image.drop_singletons = TRUE;
    
  patchify_buffer(buf, &image);
  enumerate_patches(&image, pf_patch_info);
  printf("Got %d patches with a total of %d cells and %d singletons.\n", num_patches, num_patch_cells, image.num_singletons);
  printf("*** FILTERING PATCHES WITH A NUMBER OF SAMPLES != 0 mod 3 ***\n");
  enumerate_patches(&image, pf_patch_filter_odd);
  num_patches = num_patch_cells = 0;
  enumerate_patches(&image, pf_patch_info);
  printf("Got %d patches with a total of %d cells and %d singletons.\n", num_patches, num_patch_cells, image.num_singletons);
  printf("*** RESTORING ALL PATCHES ***\n");
  reactivate_all_patches(&image);
  num_patches = num_patch_cells = 0;
  enumerate_patches(&image, pf_patch_info);
  printf("Got %d patches with a total of %d cells and %d singletons.\n", num_patches, num_patch_cells, image.num_singletons);
  fflush(stdout);
  exit(0);
}
  
