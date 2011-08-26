/*================================================================

  sscan.c - scan and possibly fix the truncation problem in 
  a seascan archive.  The truncation problem occurs when a machine
  crashes while recording to a seascan archive.  This leaves the table
  of contents at the end of file unwritten.  

==================================================================*/

#include "sscan.h"

// variables and default values for options

static int read_angles           = FALSE;
static int force_rebuild         = FALSE;
static int help                  = FALSE;
static int confirm               = TRUE;
static int progress              = TRUE;
static int quiet                 = FALSE;
static int save_tail             = TRUE;

// static variables

static t_ssa ssa;  // the archive structure

static char zero_buffer[SEASCAN_DISK_CHUNK_SIZE];

static long long offs;  // the current file offset
static long long toc_offs;  // the offset at which we should write the toc
static long long apparent_toc_offs; // the offset at which there is supposed to be a toc

#define MSG(x, ...) {if (!quiet) {printf(x, ## __VA_ARGS__ ); fflush(stdout);}}
#define FORCE_MSG(x, ...) {printf(x, ## __VA_ARGS__ ); fflush(stdout);}
#define ERR(x, ...) {printf(x, ## __VA_ARGS__ ); fflush(stdout); exit(1);}

static char data_buffer[2 * 1024 * 1024];

int
ensure_archive_dir_buff(t_ssa *me, int i)
{
  // make sure there's enough space for the growing archive directory buffer

  if (i <= me->archive_dir_buff_alloc) 
    return TRUE;
  if (!me->dir_buff) {
    i = ROUND_UP_TO(i, 1024);
    me->dir_buff = calloc (i, sizeof(DISK_DIRECTORY_ENTRY));
  } else {
    me->dir_buff = realloc (me->dir_buff, i * sizeof(DISK_DIRECTORY_ENTRY));
  }
  if (!me->dir_buff) {
    ERR("Out of memory allocating buffer for %d directory entries.\n", i);
  }
  me->archive_dir_buff_alloc = i;
  return TRUE;
}

int
ensure_lut_buffer(t_ssa *me, int i)
{
  // make sure there's enough space for the angle lut table
  if (i <= me->lut_buff_alloc) 
    return TRUE;
  if (!me->lut_buff) {
    me->lut_buff = (RSI_LUT *) calloc (i, sizeof(RSI_LUT));
  } else {
    me->lut_buff = (RSI_LUT *) realloc (me->lut_buff, i * sizeof(RSI_LUT));
  }
  if (!me->lut_buff) {
    ERR("Out of memory allocating buffer for %d angle lookup entries.\n", i);
  }
  me->lut_buff_alloc = i;
  return TRUE;
}

int
read_archive_scan_hdr (t_ssa *me) 
{
  // read the scan header from the archive at the current
  // file location.  Return true if we succeed, 0 otherwise.

  return TRY_READ_CHUNK(me, brh);
}


int
skip_scan_data (t_ssa *me) 
{
  // skip over the data for this scan.
  //
  // Assumptions
  // 
  // - the me->file file pointer is at the start of the Data part of
  // the Data Object for the  scan
  //
  // - the structure me->brh holds the headers for this Data Object
  //
  // This function returns TRUE if it is succesful in seeking past the quadrant
  // data, FALSE otherwise.  It leaves the file pointer immediately
  // after the block of data (rounded up to the seascan disk chunk size).

  return 1 == fread(data_buffer, ROUND_UP_TO(me->brh.yExtent * me->brh.rAxisBytes, SEASCAN_DISK_CHUNK_SIZE), 1, me->file);
  //  return ! bigfseek(me->file, ROUND_UP_TO(me->brh.yExtent * me->brh.rAxisBytes, SEASCAN_DISK_CHUNK_SIZE), SEEK_CUR);
}

int
skip_angle_block (t_ssa *me) 
{
  // advance the file pointer past the angle block corresponding
  // to the most recently read scan
  // return TRUE on success, FALSE otherwise.

  return ! bigfseek(me->file, ROUND_UP_TO(me->brh.AssociatedAzimuths * sizeof(RSI_LUT), SEASCAN_DISK_CHUNK_SIZE
), SEEK_CUR);
}

int
read_angle_block (t_ssa *me) 
{
  // read the angle block corresponding
  // to the most recently read scan, leaving
  // the file pointer at the start of the first following block.
  // return TRUE on success, FALSE otherwise.

  ensure_lut_buffer(me, me->brh.AssociatedAzimuths);
  return TRY_READ_CHUNKS(me, lut_buff, me->brh.AssociatedAzimuths);
}

#define TAIL_BUFF_SIZE 512 * 128
static char tail_buff[TAIL_BUFF_SIZE];

void 
rebuild_archive_directory (t_ssa *me)
{
  int i; // counts quadrants
  int j; // counts segments
  int k;
  int ninv;  // number of invalid data headers
  char resp[80];
  char t1_buf[40], t2_buf[40];  // buffers for formatting date/times
  int new_run;
  int done;
  char fname_buff[2048];
  long long fsize;

  // seek past the archive label to the first data chunk

  memset(&me->toc, 0, sizeof(me->toc));

  bigfseek(me->file, SEASCAN_DISK_CHUNK_SIZE, SEEK_SET);

  // read each data chunk until there are no more
  // recording the timestamp and file offset
  // check whether a data chunk is in fact an archive label instead,
  // which indicates the start of a new run

  new_run = FALSE;
  for (i = 0, j = 0, ninv=0; ; ) {
    offs = bigftell(me->file);
    if (! read_archive_scan_hdr(me)) {
      MSG("Unable to read scan header: offset %.0f\n", (double) offs);
      break;
    }

    if (0 == strcmp((char *) &me->brh, ARCHIVE_LABEL_TEXT)) {
      new_run = TRUE;
      toc_offs = offs;
      if (progress)
	MSG("Found start of new segment: offset %0.f\n", (double) offs);
      // read the data header which follows this archive label
      offs = bigftell(me->file);
      if (! read_archive_scan_hdr(me)) {
	MSG("Unable to read scan header after archive label: offset %.0f.\n", (double) offs);
	break;
      }
    }
    // if the scan header is valid skip over rest of scan
    // and record the timestamp and offset
    if (me->brh.TimeStamp != 0) {
      toc_offs = offs;
      if (! skip_scan_data(me)) {
	MSG("Unable to skip scan data: offset %.0f\n", (double) offs);
	break;
      }
      if (read_angles) {
	// this can be used to examine the angle block when
	// running under a debugger, but nothing is done with
	// the angle block in this program
	if (! read_angle_block(me)) {
	  MSG("Unable to read angle block: offset %.0f\n", (double) offs);
	  exit(1);
	}
      } else {
	if (! skip_angle_block(me)) {
	  MSG("Unable to skip angle block: offset %.0f\n", (double) offs);
	  break;
	}
      }
      // since we had data and an angle block
      // advance the potential toc location
      toc_offs = bigftell (me->file);
      ensure_archive_dir_buff(me, i + 1);
      me->dir_buff[i].Position.QuadPart = offs;
      me->dir_buff[i].TimeStamp = me->brh.TimeStamp;
      
      // check whether this is the start of a new segment
      if (i > 0 && new_run) { 
	me->toc.StopTime[j] = me->dir_buff[i-1].TimeStamp;
	++j;
	if (j == MAX_SEGMENTS)
	  break;
	me->toc.StartTime[j] = me->dir_buff[i].TimeStamp;
	me->toc.NumImages[j] = 0;
	new_run = FALSE;
      } else if (i == 0) {
	me->toc.StartTime[j] = me->dir_buff[i].TimeStamp;
      }
      ++ me->toc.NumImages[j];
      if (progress && ! (i % 100)) {
	MSG("Scanned %d images, %d segments\r", i+1, j+1);
      }
      ++i;
    } else {
      if (progress)
	MSG("Skipping invalid quadrant:  offset %.0f\n", (double)offs);
      ++ninv;
    }
  }
   
  me->toc.StopTime[j] = me->dir_buff[i - 1].TimeStamp;
  ++j;
  me->toc.TotalImages = i;
  me->toc.NumSegments = j;
  // we've read past i valid scan entries,
  // so record the file TOC, starting past the end of the
  // last complete scan data block

  MSG ("Got %d valid, %d invalid quadrants.\n", i, ninv);
  MSG ("\n                   REBUILT Table of Contents\n");
  MSG ("\nSegment         Start                       End             Num images\n");
  for (k = 0; k < j; ++k) {
    strftime(t1_buf, 26, "%Y/%m/%d %H:%M:%S", gmtime(&me->toc.StartTime[k]));
    strftime(t2_buf, 26, "%Y/%m/%d %H:%M:%S", gmtime(&me->toc.StopTime[k]));
    MSG("%3d %26s %26s %10d\n", k+1, t1_buf, t2_buf, me->toc.NumImages[k]);
  }

  bigfseek(me->file, 0, SEEK_END);
  fsize = bigftell(me->file);
  sprintf(fname_buff, "%s_tail_at_%.0f", me->filename, (double) toc_offs);

  if (confirm || !quiet)
    FORCE_MSG ("About to write TOC at offset %.0f\n", (double) toc_offs);
  if (toc_offs == apparent_toc_offs) {
    MSG ("This matches the original location of the TOC.\n");
  } else if (apparent_toc_offs != 0) {
    MSG ("This differs from the original location of the TOC: offset %.0f\n", (double) apparent_toc_offs);
  }
  if (fsize > toc_offs)
    if (save_tail && confirm)
      MSG ("The original remaining %.0f bytes of the file will be saved in \nfile '%s'\n", fsize - (double) toc_offs, fname_buff);
  if (confirm) {
    FORCE_MSG ("*** Are you sure you want me to do this (y/n)? ***\n");
    for (done = FALSE; ! done;) {
      fgets(resp, 79, stdin);
      switch (resp[0]) {
      case 'n':
      case 'N':
	ERR ("Operation cancelled.\n");
      case 'y':
      case 'Y':
	done = TRUE;
	break;
      default:
	MSG ("\nPlease answer y or n:\n");
      }
    }
  }
  fclose(me->file);
  me->file = fopen (me->filename, "rb+");
  bigfseek (me->file, toc_offs, SEEK_SET);
  if (save_tail && fsize > toc_offs) {
    FILE *f_save = fopen (fname_buff, "wb");
    int n = fsize - toc_offs;
    int m;
    if (!f_save) {
      ERR ("\nUnable to open file %s\n for saving original file's tail.  Rebuild cancelled.\n", fname_buff);
    }
    while (n > 0) {
      m = fread (tail_buff, 1, TAIL_BUFF_SIZE, me->file);
      if (m != fwrite (tail_buff, 1, m, f_save)) {
	ERR ("\nError writing to file %s\nRebuild cancelled.\n", fname_buff);
      }
      n -= m;
    }
    fclose (f_save);
    MSG ("Saved %.0f bytes to file %s\n", offs - (double) toc_offs, fname_buff);
    bigfseek (me->file, toc_offs, SEEK_SET);
  }
  
  if (!TRY_WRITE_CHUNK(me, arlab)) {
    ERR ("Error writing archive label: offset %.0f\nRebuild cancelled.\n", (double) toc_offs);
  }

  offs = bigftell(me->file);
  if (!TRY_WRITE_CHUNK(me, toc)) {
    ERR ("Error writing segment table of contents: offset %.0f\nRebuild cancelled.\n", (double) offs);
  }
  offs = bigftell(me->file);
  if (!TRY_WRITE_BUFFER(me, me->dir_buff, i)) {
    ERR ("Error writing image directory: offset %.0f\nRebuild cancelled.\n", (double) offs);
  }
  bigfseek (me->file, 0, SEEK_SET);
  me->arlab.directoryPosition.QuadPart = toc_offs;
  if (!TRY_WRITE_CHUNK(me, arlab)) {
    ERR ("Error writing initial archive label with pointer to TOC: offset 0\nRebuild cancelled.\n");
  }
  fflush (me->file);
  MSG ("Table of contents written.\n");
}  


void
read_archive_contents (t_ssa *me, TAPE_CONTENTS *toc) 
{
  // read the tape contents and scan directories of
  // the currently open archive
  // returns TRUE on success

  if (bigfseek(me->file, 0, SEEK_SET))
    ERR("Unable to seek to start of file '%s'\n", me->filename);

  if (! TRY_READ_CHUNK(me, arlab)
      || strcmp(me->arlab.SystemName, ARCHIVE_LABEL_TEXT))
    ERR("Can't find initial label record in file '%s'\n", me->filename);

  apparent_toc_offs = me->arlab.directoryPosition.QuadPart;

  if (apparent_toc_offs == 0
      || bigfseek(me->file, SEASCAN_DISK_CHUNK_SIZE + apparent_toc_offs, SEEK_SET)
      || ! TRY_READ_CHUNK(me, toc)
      || ! ensure_archive_dir_buff(me, toc->TotalImages)
      || 1 != fread(me->dir_buff, toc->TotalImages * sizeof(DISK_DIRECTORY_ENTRY), 1, me->file)) {
    MSG("Seascan archive directory is missing or corrupt.\nRebuilding:\n");
    rebuild_archive_directory(me);
  } else if (force_rebuild) {
    MSG ("Archive appears to have a directory at offset %.0f\nbut forced rebuilding begins:\n", (double) apparent_toc_offs);
    rebuild_archive_directory(me);
  } else {    
    MSG ("This Seascan archive appears to have an intact directory.\nUse the -f (--force) option to force rebuilding of the directory.\n");
  }
}

int
shut_down(t_ssa *me) {
  if (!me)
    return TRUE;

  if (me->file) {
    fclose(me->file);
    me->file = NULL;
  }
  return TRUE;
}

void
start_up(t_ssa *me)
{
  TAPE_CONTENTS *toc;

  toc = &me->toc;
  if (! (me->file = bigfopen(me->filename, "rb"))) {
    ERR ("Couldn't open file '%s' for reading.\n", me->filename);
  }
  read_archive_contents (me, toc);
}

void
verify_archive (t_ssa *me) 
{
  start_up(me);
  shut_down(me);
}


int
main (int argc, char *argv[])
{
  int c;
  while (1) {
    int option_index = 0;
    static struct option long_options[] = {
      {"angles",      0, 0, 'a'},
      {"force",       0, 0, 'f'},
      {"help",        0, 0, 'h'},
      {"no-progress", 0, 0, 'p'},
      {"quiet",       0, 0, 'q'},
      {"no-tail",     0, 0, 't'},
      {"no-confirm",  0, 0, 'y'},
      {0, 0, 0, 0}
    };

    c = getopt_long (argc, argv, "afhpqty",
		     long_options, &option_index);
    if (c == -1)
      break;

    switch (c) {
    case 'a':
      read_angles = TRUE;
      break;

    case 'f':
      force_rebuild = TRUE;
      break;

    case 'h':
      help = TRUE;
      break;

    case 'p':
      progress = FALSE;
      break;

    case 'q':
      quiet = TRUE;
      break;

    case 't':
      save_tail = FALSE;
      break;

    case 'y':
      confirm = FALSE;
      break;

    default:
      break;
    }
  }

  if (help || optind == argc) {
    ERR("\
Usage:  sscan [OPTIONS] <filename> \n\
\n\
where <filename> is a seascan archive file\n\
\n\
Attempts to read the seascan archive file, and rebuild the table of\n\
contents if it is missing or corrupt.  The rebuilt TOC is printed\n\
and the user is asked to confirm the operation before it is written\n\
If the rebuilt TOC will overwrite the tail of the original file,\n\
that tail is saved in the file <filename>_tail_at_<offset>\n\
where <offset> is the byte offset of the tail in the original file.\n\
\nOPTIONS:\n\n\
-a  --angles      read angle block for each scan (useful when using gdb)\n\
-f, --force       force a rebuild of the table of contents even if one is found\n\
-h, --help        print this message\n\
-p, --no-progress don't print any progress messages\n\
-q, --quiet       print only error messages\n\
-t  --no-tail     don't save the tail of the original file\n\
-y, --no-confirm  answer 'yes' to the 'are you sure' prompt\n\
\n\
Author: John Brzustowski  john AT brz DOT ca\n\
");
  }
  strcpy (ssa.filename, argv[optind]);

  verify_archive(&ssa);
  exit(1);
}
