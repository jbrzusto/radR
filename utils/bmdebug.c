#ifndef Win32
#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>
#include <math.h>
#include <getopt.h>
#include "radR.h"
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>

#define ERR(x, ...) {printf(x, ## __VA_ARGS__ ); if (*filenames[0]) printf("errno=%d while processing file '%s'.\n", errno, filenames[0]); fflush(stdout); exit(1);}

/* bmdebug.c: check a blipmovie archive for consistency, reporting problems, and correcting them where possible */

typedef struct {
  char preamble[4096];
} __attribute__((packed)) t_scan_preamble;

typedef struct {
  int num_scans;
  time_t first_scan_timestamp;
  time_t last_scan_timestamp;
}  __attribute__((packed)) t_scan_header;

typedef struct {
  double timestamp;
  short duration;
  short pulses;
  short samples_per_pulse;
  short rotation_direction;
  short bits_per_sample;
  double sample_dist;
  double first_sample_dist;
  double north_offset;
  double first_blip;
  int num_blips;
} __attribute__((packed)) t_scan_record;

typedef struct {
  double num_blips;
} t_blip_header;

typedef struct {
  double first_sample;
  int num_samples;
}  __attribute__((packed)) t_blip_record;

typedef struct {
  double num_samples;
} t_samp_header;

typedef struct {
  double num_samples;
} __attribute__((packed)) t_scor_header;

/* 
   NOTE: the sample records for a blip are actually stored on disk as
   three consecutive arrays: unsigned short pulse_num[num_samples],
   sample_num[num_samples], value[num_samples].  So each sample
   requires 6 bytes (2 bytes for each of pulse_num, sample_num, and
   value), but these are usually not contiguous in the file.
*/
  
#define BYTES_PER_SAMPLE_RECORD 6

unsigned short *samp_buff[4] = {NULL, NULL, NULL, NULL};
int samp_buff_alloc = 0;

/* a structure to hold corrections for the file,
   which are collected through the scan phase, then
   applied later;
   The "COPY" patch is special: it says to copy a chunk
   of the file from one location to another.  The difference between
   the two locations and the size of the chunk must both fit
   in ints.
 */


/* structure for info on a "COPY" patch */

typedef struct {
  int dstoffset;
  int length;
} t_copy_patch_info;

/* sizes of each type of patch */

int patch_sizes[] = {sizeof(char), sizeof(short), sizeof(int), sizeof(time_t), sizeof(double), sizeof(t_copy_patch_info)};

/* maximum value of elements of patch_sizes[] */

#define MAX_PATCH_SIZE 8

typedef struct {
  int file;
  long long offset;
  enum {CHAR=0, SHORT, INT, TIME_T, DOUBLE, COPY} kind;
  char bytes[MAX_PATCH_SIZE];
} t_file_patch;

t_file_patch *file_patches = NULL;
int num_file_patches = 0;
int num_file_patches_alloc = 0;

// kinds of patches to be made to files
typedef enum {
  SEASCAN_TIMESTAMP, 
  REPEATED_TIMESTAMP,
  DURATION, 
  TOC_FIRST_TIMESTAMP, 
  TOC_LAST_TIMESTAMP, 
  SCAN_COUNT,
  BLIP_COUNT,
  SAMPLE_COUNT,
  SCORE_COUNT,
  USER_SET_TIMESTAMP,
  USER_SET_DURATION,
  SCAN_MISSING_BLIPS,
  BLIP_MISSING_SAMPLES,
} patch_category;

// lables of patch categories
char *patch_category_name[] = { "buggy radR Seascan timestamp", "repeated timestamp", "scan duration", "TOC first timestamp", "TOC last timestamp", "total scan count", "total blip count", "total sample count", "total score count", "user set timestamp", "user set duration", "scan missing blips", "blip missing samples"};

// initial counts of patches
int num_patches_by_category[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

char filenames[4][1024] = {"", "", "", ""};
char *suffixes[4] = {".bma", ".blp", ".smp", ".scr"};
FILE *files[4];

#define SCAN_FILE 0
#define BLIP_FILE 1
#define SAMP_FILE 2
#define SCOR_FILE 3

#define F_SCAN files[SCAN_FILE]
#define F_BLIP files[BLIP_FILE]
#define F_SAMP files[SAMP_FILE]
#define F_SCOR files[SCOR_FILE]

t_scan_header scan_header;
t_scan_record scan_record;
t_scan_record scan_record_unfixed;
t_scan_record prev_scan_record;
t_scan_record prev_scan_record_unfixed;
t_scan_record first_scan_record;

t_blip_header blip_header;
t_blip_record blip_record;
t_blip_record prev_blip_record;

t_samp_header samp_header;
t_scor_header scor_header;

unsigned int max_copy_patch_size = 0;

#define SCOR_BUFF_SIZE 1024
t_score score_buff[SCOR_BUFF_SIZE];

int skip_scan_preamble();
void check_scan_file();
void check_blip_file();
void check_samp_file();
void check_scor_file();
void check_blip_samps();
void dump_scan_records();
void truncate_blipmovie();
void set_timestamp();
void set_duration();

double scan_num_blips;
double blip_num_samps;
int apparent_num_scans;
int i_segment;
double apparent_num_blips;
double apparent_num_samps;
double apparent_num_scores;

// variables and default values for options

static int opt_fix                   = FALSE;
static int opt_timestamps            = TRUE;
static int opt_help                  = FALSE;
static int opt_max_warnings          = 5;
static int opt_max_patch_info        = 5;
static int opt_quiet                 = FALSE;
static int opt_confirm               = TRUE;
static int opt_dump_sr               = FALSE;
static int opt_multiseg              = FALSE;
static int opt_toc                   = FALSE;
static int opt_truncate              = 0;
static int opt_skip_contig           = FALSE;
static int opt_scan_index            = 0;
static double opt_set_timestamp      = 0;
static short opt_set_duration        = 0;

// static variables

char time_buff[256];
char time_buff_prev[256];
time_t t1, t2;

struct stat st;

#define MSG(x, ...) {printf(x, ## __VA_ARGS__ ); fflush(stdout);}

#define DOWARN(string, ...) {if (!opt_quiet) { if (opt_max_warnings != 0) {printf("Warning: " string, __VA_ARGS__); fflush(stdout); if (--opt_max_warnings == 0) {printf("Maximum number of warnings reached.\n");}}}}


void
ensure_samp_buff(int num_samples, int num_presabs) 
{
  int size = num_samples * 3 + num_presabs;
  if (samp_buff_alloc < size) {
    if (samp_buff[0]) {
      free(samp_buff[0]);
      samp_buff[0] = NULL;
    }
    samp_buff[0] = (typeof(samp_buff[0])) calloc(size * sizeof(samp_buff[0]), 1);
    if (!samp_buff[0]) {
      MSG("Out of memory allocating a buffer for %d samples.\n", size);
      exit(7);
    }
    samp_buff_alloc = size;
  }
  samp_buff[1] = samp_buff[0] + num_samples;
  samp_buff[2] = samp_buff[1] + num_samples;
  samp_buff[3] = samp_buff[2] + num_samples;
}

#define FILE_PATCH_ALLOC_INCREMENT 100

void
ensure_file_patches() {
  if (num_file_patches_alloc == num_file_patches) {
    file_patches = realloc(file_patches, (num_file_patches_alloc += FILE_PATCH_ALLOC_INCREMENT) * sizeof(t_file_patch));
    if (!file_patches) {
      if (!opt_quiet)
	puts("Out of memory reallocating file patch buffer.\n");
      exit(10);
    }
  }
}


#define MAKE_FILE_PATCH(FILENO, OFFSET, TYPE, _VALUE) \
{ \
    typeof(_VALUE) VALUE = _VALUE; \
    ensure_file_patches(); \
    file_patches[num_file_patches].file = FILENO;     \
    file_patches[num_file_patches].offset = OFFSET;     \
    file_patches[num_file_patches].kind = TYPE; \
    *(typeof(&(VALUE))) &file_patches[num_file_patches].bytes = VALUE; \
    ++num_file_patches; \
}


#define NOTE_FILE_PATCH(WHICH, FILENO, OFFSET, ITEMOFFSET, TYPE, OLDVALUE, VALUE) \
{ \
    MAKE_FILE_PATCH(FILENO, OFFSET, TYPE, VALUE); \
    if (!opt_quiet) { \
      if (opt_max_patch_info < 0 || (opt_max_patch_info > 0 && num_patches_by_category[WHICH] < opt_max_patch_info)) \
        printf("Patching '%s', %s, %.0f, %.0f, %0.f->%.0f\n", patch_category_name[WHICH], filenames[FILENO], (double) (OFFSET), (double) (ITEMOFFSET), (double) OLDVALUE, (double) VALUE); \
      else if (num_patches_by_category[WHICH] == opt_max_patch_info) \
        printf("***Further '%s' patch messages dropped\n", patch_category_name[WHICH]); \
    } \
    ++num_patches_by_category[WHICH]; \
}

#define NOTE_FILE_COPY_PATCH(WHICH, FILENO, _OFFSET, _DSTOFFSET, _LENGTH) \
{ \
    double OFFSET = _OFFSET, DSTOFFSET = _DSTOFFSET; \
    int LENGTH = _LENGTH; \
    t_copy_patch_info cpi = {.dstoffset = DSTOFFSET, .length = LENGTH}; \
    MAKE_FILE_PATCH(FILENO, OFFSET, COPY, cpi); \
    if (!opt_quiet) { \
      if (opt_max_patch_info < 0 || (opt_max_patch_info > 0 && num_patches_by_category[WHICH] < opt_max_patch_info)) \
        printf("Patching '%s', %s: copying %d bytes from %.0f to %.0f\n", patch_category_name[WHICH], filenames[FILENO], LENGTH, OFFSET, OFFSET + DSTOFFSET); \
      else if (num_patches_by_category[WHICH] == opt_max_patch_info) \
        printf("***Further '%s' patch messages dropped\n", patch_category_name[WHICH]); \
    } \
    ++num_patches_by_category[WHICH]; \
    max_copy_patch_size = (max_copy_patch_size < LENGTH) ? LENGTH : max_copy_patch_size; \
}
			     

/* patch the file with the recorded patches */

int
confirm (char *msg) {
  // if confirmation is enabled, print a message and get user confirmation
  // return TRUE for confirmed, FALSE otherwise
  char resp[3];
  
  if (!opt_confirm)
    return TRUE;
  printf("%s\nyes (y) or no (n) ? ", msg);
  fflush(stdout);
  fflush(stdin);
  *resp = 0;
  while (!*resp)
    fgets(resp, 3, stdin);
  return tolower(*resp) == 'y';
}

int
patch_files() {
  int i;
  t_file_patch *fp;
  char * copy_buff = NULL;
  t_copy_patch_info cpi;

  if (max_copy_patch_size)
    copy_buff = calloc(1, max_copy_patch_size);

  if (num_file_patches == 0)
    return TRUE;

  if (!opt_quiet) {
    printf ("Fixing will require changes to blipmovie %s:\n", filenames[0]);
    for (i=0; i < sizeof(num_patches_by_category) / sizeof(int); ++i) {
      if (num_patches_by_category[i] > 0)
	printf ("  %6d change(s) of type '%s'\n", num_patches_by_category[i], patch_category_name[i]);
    }
  }
  if (opt_fix && !confirm("Should I make these changes")) {
    printf("\n ** Cancelled! **\n");
    return FALSE;
  }
  for (i = 0; i < 3; ++i)
    bigfseek(files[i], 0LL, SEEK_SET);

  if (F_SCOR)
    bigfseek(F_SCOR, 0LL, SEEK_SET);

  for (i = 0; i < num_file_patches; ++i) {
    fp = &file_patches[i];
    if (fp->kind == COPY) {
      // copy patch
      cpi = *(t_copy_patch_info *) &fp->bytes;
      if (opt_fix) {
	bigfseek(files[fp->file], fp->offset, SEEK_SET);
	if (1 != fread(copy_buff, cpi.length, 1, files[fp->file])) {
	  printf("Couldn't read %d bytes from offset %.0f of file %s.\nSkipping this patch.\n", cpi.length, (double) fp->offset, filenames[fp->file]);
	} else {
	  if (bigfseek(files[fp->file], fp->offset + cpi.dstoffset, SEEK_SET) || 1 != fwrite(copy_buff, cpi.length, 1, files[fp->file])) {
	    printf("Couldn't seek/write %d bytes to offset %.0f of file %s.\nSkipping this patch.\n", cpi.length, (double) fp->offset + cpi.dstoffset, filenames[fp->file]);
	  }
	}
      } else {
	printf("Would copy %d bytes from %s:%.0f to %.0f\n", cpi.length, filenames[fp->file], (double) fp->offset, (double) fp->offset + cpi.dstoffset);
      }
    } else {
      // simple patch
      if (opt_fix) {
	bigfseek(files[fp->file], fp->offset, SEEK_SET);
	fwrite(& fp->bytes, patch_sizes[fp->kind], 1, files[fp->file]);
      } else {
	if (!opt_quiet) {
	  printf("Would set %d bytes at %s:%0.f\n", patch_sizes[fp->kind], filenames[fp->file], (double) fp->offset);
	}
      }
    }
  }
  return TRUE;
}

static int option_index = 0;
static struct option long_options[] = {
  {"dump-scans",     	0, 0, 'd'	},
  {"toc",            	0, 0, 0xff01	},
  {"fix",            	0, 0, 'f'	},
  {"help",           	0, 0, 'h'	},
  {"max-warnings",   	1, 0, 'w'	},
  {"max-patch-info", 	1, 0, 'p'	},
  {"multi-segment",  	0, 0, 'm'	},
  {"quiet",          	0, 0, 'q'	},
  {"truncate",       	1, 0, 'r'	},
  {"no-blip-contig", 	0, 0, 'c'	},
  {"no-timestamp-fix",  0, 0, 't'	},
  {"no-confirm",     	0, 0, 'y'	},
  {"scan-index",        1, 0, 'i'	},
  {"set-timestamp",  	1, 0, 's'	},
  {"set-duration",   	1, 0, 'u'	},
  {NULL,                0, 0, 0		}
};


int
main (int argc, char *argv[])
{
  int i;
  int c;

  /* setenv("TZ", "GMT", 1); */
  tzset();
  ensure_samp_buff(4096, 1024);


  while (1) {
    c = getopt_long (argc, argv, "cdfhi:mp:s:qr:tuw:y",
		     long_options, &option_index);
    if (c == -1)
      break;

    switch (c) {
    case 0xff01:
      opt_toc = TRUE;
      break;

    case 'i':
      opt_scan_index = atoi(optarg);
      if (opt_scan_index <= 0) {
	ERR("Parameter for scan index option must be >= 1.");
	break;

      case 's':
	opt_set_timestamp = atof(optarg);
	if (optarg < 0)
	  ERR("Parameter for set timestamp option must be >= 0.");
	break;

      case 'u':
	opt_set_duration = atoi(optarg);
	if (opt_set_duration <= 0)
	  ERR("Parameter for set duration option must be >= 1 (milliseconds).");
	break;

      case 'c':
	opt_skip_contig = TRUE;
	break;

      case 'd':
	opt_dump_sr = TRUE;
	break;

      case 'f':
	opt_fix = TRUE;
	break;

      case 'h':
	opt_help = TRUE;
	break;

      case 'w':
	opt_max_warnings = atoi(optarg);
	break;

      case 'p':
	opt_max_patch_info = atoi(optarg);
	break;

      case 'q':
	opt_quiet = TRUE;
	break;

      case 'r':
	opt_truncate = atoi(optarg);
	if (opt_truncate < 1) 
	  ERR("Error: parameter for truncate option must be >= 1.\n");
	break;

      case 'm':
	opt_multiseg = TRUE;
	break;

      case 't':
	opt_timestamps = FALSE;
	break;

      case 'y':
	opt_confirm = FALSE;
	break;

      default:
	break;
      }
    }
  }

  if (opt_help || optind == argc)
    ERR("\
Usage:  bmdebug [OPTIONS] XXX \n\
\n\
where XXX represents the blipmovie stored in files XXX.bma, XXX.blip, and XXX.smp \n\
\n\
Attempts to read the blipmovie and correct any problems found. \n\
\nOPTIONS:\n\n\
-c,   --no-blip-contig    skip the (slow) blip contiguity check\n\
-d,   --dump-scans        dump all scan records from file, then exit\n\
-f,   --fix               fix problems found\n\
-h,   --help              print this message\n\
\n\
-i I, --scan-index=I      set the scan index (starting at 1) for the set operations:\n\
-s t, --set-timestamp=t   set the timestamp for scan I (numbered from 1) to t, a floating point timestamp\n\
-u d, --set-duration=d    set the duration for scan I (numbered from 1) to d, an integer # of milliseconds\n\
                          To use either -s or -u, you must also specify -i.\n\
-w M, --max-warnings=M    maximum warnings and fix messages printed; M=-1 means print all\n\
-p M  --max-patch-info=M  maximum patches (of each type) to print info for; M=-1 means print all\n\
-q,   --quiet             don't print warnings and errors\n\
-r M, --truncate=M        truncate blipmovie by removing all scans starting at index M,\n\
                          with M >= 1; also remove the associated blips and samples\n\
-m,   --multi-segment     file may have more than one segment\n\
-t,   --no-timestamp-fix  DO NOT fix timestamps produced by buggy radR seascan code\n\
      --toc               dump the table of contents (you probably want to specify '-s' too)\n\
-y    --no-confirm        if fixing, do not prompt user for confirmation\n\
\n\
Author: John Brzustowski  john AT brz DOT ca\n\
");

  for(i=0; i < 4; ++i) {
    strcpy(filenames[i], argv[optind]);
    strcat(filenames[i], suffixes[i]);
    files[i] = bigfopen(filenames[i], "r+b");
    // the score file is optional
    if (i < 3 && !files[i])
      ERR("Error: could not open file '%s'\n", filenames[i]);
  }

  if (!skip_scan_preamble())
    ERR("Error: couldn't skip over scan file preamble.");
  
  if (opt_dump_sr || opt_toc) {
    dump_scan_records(opt_toc);
    exit(0);
  }
  
  if (opt_truncate) {
    truncate_blipmovie();
    exit(0);
  }

  if (opt_set_timestamp) {
    if (!opt_scan_index)
      ERR("Error: must specify which scan to set timestamp for using -i option.");
    set_timestamp();
    patch_files();
    exit(0);
  }
  
  if (opt_set_duration) {
    if (!opt_scan_index)
      ERR("Error: must specify which scan to set duration for using -i option.");
    set_duration();
    patch_files();
    exit(0);
  }
  
  check_scan_file();
  check_blip_file();
  check_samp_file();
  if (F_SCOR)
    check_scor_file();

  if (!opt_skip_contig)
    check_blip_samps();
  
  patch_files();
  
  for(i=0; i < 3; ++i)
    fclose(files[i]);
  if (F_SCOR)
    fclose(F_SCOR);
  
  exit(0);
}

#define CHECK_SCAN(OP, FIELD, CODE, I) \
   if (scan_record. FIELD OP prev_scan_record. FIELD) { \
    DOWARN("scan record %d has " #FIELD " " #CODE " " #OP " " #CODE " for record %d\n", I+1, scan_record. FIELD, prev_scan_record. FIELD, I); \
   }

#define CHECK_BLIP(OP, FIELD, CODE, I) \
   if (blip_record. FIELD OP prev_blip_record. FIELD) { \
    DOWARN("blip record %.0f has " #FIELD " " #CODE " " #OP " " #CODE " for record %.0f\n", I+1, blip_record. FIELD, prev_blip_record. FIELD, I); \
   }

void
dump_scan_records (int toc_only)
{
  int i;
  int seg = 0;
  int dur;
  struct tm *tptr1, *tptr2;

  for(seg = 0; seg == 0 || opt_multiseg; ++seg) {
    if (1 != fread(&scan_header, sizeof(scan_header), 1, F_SCAN))
      ERR("Error: couldn't read header from scan file.\n");
    fstat(fileno(F_SCAN), &st);
    t1 = (time_t) scan_header.first_scan_timestamp;
    tptr1 = localtime(&t1);
    t1 = (time_t) scan_header.last_scan_timestamp;
    tptr2 = localtime(&t1);
    if (tptr1)
      strftime(time_buff, 255, "%Y/%m/%d,%H:%M:%S", tptr1);
    else
      strcpy(time_buff, "BAD TIMESTAMP");
    if (tptr2)
      strftime(time_buff_prev, 255, "%Y/%m/%d,%H:%M:%S", tptr2);
    else
      strcpy(time_buff_prev, "BAD TIMESTAMP");

    printf("Segment %d: %6d scans from %s to %s\n", seg + 1, scan_header.num_scans, time_buff, time_buff_prev);
    if (!opt_multiseg) {
      apparent_num_scans = (st.st_size - sizeof(scan_header) - sizeof(t_scan_preamble)) / sizeof(scan_record);
      if (apparent_num_scans != scan_header.num_scans)
	printf("apparent number of scans based on file size: %d\n", apparent_num_scans);
    } else {
      apparent_num_scans = scan_header.num_scans;
    }
    if (seg == 0 && !toc_only)
      printf(" scan#    Date        Time       timestamp      dur prev_dur 1st_blip num_blips\n");
    if (toc_only) {
      // skip scan records
      bigfseek(F_SCAN, apparent_num_scans * sizeof(t_scan_record), SEEK_CUR);
    } else {
      for (i=0; i < apparent_num_scans; ++i) {
	if (1 != fread(&scan_record, sizeof(scan_record), 1, F_SCAN))
	  ERR("Error: couldn't read scan record %d.\n", i + 1);
	t1 = (time_t) scan_record.timestamp;
	tptr1 = localtime(&t1);
	if (tptr1)
	  strftime(time_buff, 255, "%Y/%m/%d %H:%M:%S.%%03d", tptr1);
	else
	  strcpy(time_buff, "BAD TIMESTAMP");
	sprintf(time_buff_prev, time_buff, (int) floor(0.5 + 1000 * (scan_record.timestamp - floor(scan_record.timestamp))));
	if (i > 0)
	  dur = (int) floor(0.5 + 1000 * (scan_record.timestamp - prev_scan_record.timestamp));
	else
	  dur = scan_record.duration;
	printf("%5d\t%s\t%.3f\t%4d\t%4d\t%.0f\t%d\n", i+1, time_buff_prev, scan_record.timestamp, scan_record.duration, dur, scan_record.first_blip, scan_record.num_blips);
	prev_scan_record = scan_record;
      }
    }
    if (feof(F_SCAN))
      break;
  }
}
 
void
get_scan_by_index(int ind)
{
  if (bigfseek(F_SCAN, sizeof(t_scan_preamble) + sizeof(t_scan_header) + (ind - 1) * sizeof(t_scan_record), SEEK_SET))
    ERR("Couldn't seek to scan record %d.\n", ind);
  if (1 != fread(&scan_record, sizeof(t_scan_record), 1, F_SCAN))
    ERR("Error: couldn't read scan record %d.\n", ind);
}

void
set_timestamp ()
{
  get_scan_by_index(opt_scan_index);
  NOTE_FILE_PATCH(USER_SET_TIMESTAMP, SCAN_FILE, bigftell(F_SCAN) - sizeof(t_scan_record) + ((char *) &scan_record.timestamp - (char *) &scan_record), opt_scan_index, DOUBLE, scan_record.timestamp, opt_set_timestamp);
}


void
set_duration ()
{
  get_scan_by_index(opt_scan_index);
  NOTE_FILE_PATCH(USER_SET_DURATION, SCAN_FILE, bigftell(F_SCAN) - sizeof(t_scan_record) + ((char *) &scan_record.duration - (char *) &scan_record), opt_scan_index, SHORT, scan_record.duration, opt_set_duration);
}


void
truncate_blipmovie ()
{
  double nb, ns;
  // truncate the blipmovie, so that the scan at opt_truncate (numbered from 1)
  // is the last remaining scan.
  // the blip and sample files are truncated appropriately
  // FIXME: doesn't work for multisegment files

  fstat(fileno(F_SCAN), &st);
  if (1 != fread(&scan_header, sizeof(t_scan_header), 1, F_SCAN))
    ERR("Error: couldn't read header from scan file.\n");
  apparent_num_scans = (st.st_size - sizeof(t_scan_header)) / sizeof(t_scan_record);
  if (opt_truncate > apparent_num_scans)
    ERR("Error: can't truncate starting at scan %d; blipmovie has only %d scans.\n", opt_truncate, apparent_num_scans);
  if (1 != fread(&blip_header, sizeof(blip_header), 1, F_BLIP))
    ERR("Error: couldn't read header from blip file.\n");
  if (1 != fread(&samp_header, sizeof(samp_header), 1, F_SAMP))
    ERR("Error: couldn't read header from sample file.\n");
  if (F_SCOR && 1 != fread(&scor_header, sizeof(scor_header), 1, F_SCOR))
    ERR("Error: couldn't read header from score file.\n");
  if (opt_truncate > 0) {
    get_scan_by_index(opt_truncate);
    nb = scan_record.first_blip + scan_record.num_blips;
    if(bigfseek(F_BLIP, sizeof(t_blip_header) + sizeof(t_blip_record) * (nb - 1), SEEK_SET))
      ERR("Couldn't seek to blip record %.0f.\n", nb);
    if (1 != fread(&blip_record, sizeof(t_blip_record), 1, F_BLIP))
      ERR("Error: couldn't read blip record %.0f\n", nb);
    ns = blip_record.first_sample + blip_record.num_samples;
  } else {
    nb = ns = 0;
  }
  NOTE_FILE_PATCH(SCAN_COUNT, SCAN_FILE, ((char *) &scan_header.num_scans - (char *) &scan_header), 0, INT, scan_header.num_scans, opt_truncate);
  NOTE_FILE_PATCH(BLIP_COUNT, BLIP_FILE, ((char *) &blip_header.num_blips - (char *) &blip_header), 0, DOUBLE, blip_header.num_blips, nb );
  NOTE_FILE_PATCH(SAMPLE_COUNT, SAMP_FILE, ((char *) &samp_header.num_samples - (char *) &samp_header), 0, DOUBLE, samp_header.num_samples, ns );
  if (F_SCOR)
    NOTE_FILE_PATCH(SCORE_COUNT, SCOR_FILE, ((char *) &scor_header.num_samples - (char *) &scor_header), 0, DOUBLE, scor_header.num_samples, ns );
  if (patch_files()) {
    // user has confirmed fix, so truncate the files
    ftruncate(fileno(F_SCAN), sizeof(t_scan_preamble) + sizeof(t_scan_header) + opt_truncate * sizeof(t_scan_record));
    ftruncate(fileno(F_BLIP), sizeof(t_blip_header) + sizeof(t_blip_record) * nb);
    ftruncate(fileno(F_SAMP), sizeof(t_samp_header) + BYTES_PER_SAMPLE_RECORD * ns);
    if (F_SCOR)
      ftruncate(fileno(F_SCOR), sizeof(t_scor_header) + sizeof(t_score) * ns);
  }
}


void 
check_scan_file ()
{
  int i;
  int dur;
  int del;
  int first_timestamp_buggy;
  int total_scans;
  long long anchor = 0;
  long long segment_header_offset; // location of the header of the current segment in absolute file offsets
  int repeated_ts = 0; // number of times current timestamp has been repeated
  struct tm *tptr1, *tptr2;

  total_scans = 0;
  fstat(fileno(F_SCAN), &st);
  for (i_segment = 0; !feof(F_SCAN) && (opt_multiseg || i_segment == 0); ++i_segment) { 
    // loop over segments

    segment_header_offset = bigftell(F_SCAN);
    if (1 != fread(&scan_header, sizeof(scan_header), 1, F_SCAN))
      ERR("Error: couldn't read header for segment %d from scan file.\n", 1 + i_segment);
    apparent_num_scans = scan_header.num_scans;
    if (!opt_multiseg) {
      if ( scan_header.num_scans != (apparent_num_scans = (st.st_size - sizeof(scan_header) - sizeof(t_scan_preamble)) / sizeof(scan_record))) {
	DOWARN("(FIXED) scan file claims to have %d scans, but file size implies %d.\n", scan_header.num_scans, 
	       apparent_num_scans);
	if ((i = (st.st_size - sizeof(scan_header) - sizeof(t_scan_preamble)) % sizeof(scan_record))) {
	  DOWARN("%d extra bytes or partial scan record at end of scan file.\n", i);
	}
	NOTE_FILE_PATCH(SCAN_COUNT, SCAN_FILE, segment_header_offset + ((char *) &scan_header.num_scans - (char *) &scan_header), 0, INT, scan_header.num_scans, apparent_num_scans);
      }
    }

    scan_num_blips = 0;

    // See the note below about my buggy interpretation of seascan
    // timestamps.  We need to check whether the first timestamp is in
    // fact already buggy (i.e. 1 seconds later than it should be).  We
    // test for that by looking for the first aberrant duration, i.e. an
    // interscan timestamp difference < 2000 msecs or > 3000 msecs in
    // the first 100 scans.  (we ignore the slim possibility of a
    // missing scan among the first 100 scans).  If the duration is
    // aberrantly small, then the initial timestamp is already buggy.
    // Otherwise, the bugginess begins later.

    first_timestamp_buggy = FALSE;
    for (i=0; i < 100 && i < apparent_num_scans; ++i) {
      if (1 != fread(&scan_record, sizeof(scan_record), 1, F_SCAN))
	ERR("Error: couldn't read scan record %d in segment %d.\n", i + 1, i_segment + 1);
      if (i > 0) {
	dur = (int) floor(0.5 + 1000 * (scan_record.timestamp - prev_scan_record.timestamp));
	if (dur > 3000) 
	  break;
	if (dur < 2000) {
	  // the initial timestamp is buggy; decrease it by one, and record the fact for later
	  DOWARN("(FIXED)Based on the apparent duration=%d of scan %d\nit appears the timestamp for the 1st scan in segment %d is buggy.\n", dur, i, i_segment + 1);
	  NOTE_FILE_PATCH(SEASCAN_TIMESTAMP, SCAN_FILE, segment_header_offset + sizeof(t_scan_header) + ((char *) &scan_record.timestamp - (char *) &scan_record), 1, DOUBLE, first_scan_record.timestamp, first_scan_record.timestamp - 1.0);
	  first_scan_record.timestamp -= 1.0;
	  break;
	}	
      }
      prev_scan_record = scan_record;
      if (i == 0) {
	first_scan_record = scan_record;
	anchor = bigftell(F_SCAN);
      }
    }

    // seek to the start of the 2nd scan record in this segment, since
    // we're already holding the first in first_scan_record

    bigfseek(F_SCAN, anchor, SEEK_SET);

    for (i=0; i < apparent_num_scans; ++i) {
      if (i == 0)
	scan_record_unfixed = scan_record = first_scan_record;
      else if (1 != fread(&scan_record, sizeof(scan_record), 1, F_SCAN))
	ERR("Error: couldn't read scan record %d in segment %d.\n", i + 1, i_segment + 1);
      scan_record_unfixed = scan_record;
      if (i == 0) {
	if (scan_header.first_scan_timestamp != (time_t) floor(scan_record.timestamp)) {
	  NOTE_FILE_PATCH(TOC_FIRST_TIMESTAMP, SCAN_FILE, segment_header_offset + ((char *) &scan_header.first_scan_timestamp - (char *) &scan_header), 0, TIME_T, scan_header.first_scan_timestamp, floor(scan_record.timestamp));
	  t1 = (time_t) scan_header.first_scan_timestamp;
	  t2 = (time_t) floor(scan_record.timestamp);
	  strcpy(time_buff, ctime(&t1));
	  DOWARN("(FIXED) scan file header claims first scan timestamp is\n%sbut first scan record has timestamp of\n%s\n", time_buff, ctime(&t2));
	}
      }
      if (i > 0) {
	// Note: in some configurations, seascan.c returns identical timestamps for 2 or more consecutive
	// scans.  The repeated timestamp appears to be the correct one for the last scan among those
	// sharing it.  I haven't yet determined whether this is a Seascan Server or radR seascan plugin bug.
	// For now, we detect runs of repeated timestamps.  When we find the end of a run with 
	// length = n > 1, we correct the timestamps of the first n-1 scans in it.

	if (prev_scan_record_unfixed.timestamp == scan_record_unfixed.timestamp) {
	  ++ repeated_ts;
	}
	else {
	  if (repeated_ts) {
	    int jj;
	    // previous timestamps were duplicated
	    // correct all but the last, by subtracting a constant 2.5 seconds for
	    // each repetition.
	    for (jj = 1; jj <= repeated_ts; ++jj) {
	      NOTE_FILE_PATCH(REPEATED_TIMESTAMP, SCAN_FILE, bigftell(F_SCAN) - (jj + 2) * sizeof(t_scan_record) + ((char *) &scan_record.timestamp - (char *) &scan_record), i + 1 - (jj + 1), DOUBLE, prev_scan_record_unfixed.timestamp, prev_scan_record_unfixed.timestamp - jj * 2.5);
	    }
	    repeated_ts = 0;
	  } 
	}
	t1 = (time_t) scan_record.timestamp;
        tptr1 = localtime(&t1);
	if (!tptr1) {
	  double last_good_blip = prev_scan_record.first_blip + prev_scan_record.num_blips - 1;
	  double last_good_sample;
	  DOWARN("(FIXED) scan record %d is corrupted.  This and remaining %d scans will be deleted.\n", i + 1, apparent_num_scans - (i + 1));
	  fseek(F_BLIP, sizeof(t_blip_header) + sizeof(t_blip_record) * last_good_blip, SEEK_SET);
	  if (1 != fread(&blip_record, sizeof(blip_record), 1, F_BLIP))
	    ERR("Error: couldn't read blip record %.0f\n", last_good_blip);
	  last_good_sample = blip_record.first_sample + blip_record.num_samples - 1;
	  ftruncate(fileno(F_SCAN), bigftell(F_SCAN) - sizeof(scan_record));
	  NOTE_FILE_PATCH(SCAN_COUNT, SCAN_FILE, segment_header_offset + ((char *) &scan_header.num_scans - (char *) &scan_header), 0, INT, apparent_num_scans, i);
	  ftruncate(fileno(F_BLIP), sizeof(t_blip_header) + sizeof(t_blip_record) * (last_good_blip + 1));
	  bigfseek(F_BLIP, 0, SEEK_SET);
	  ftruncate(fileno(F_SAMP), sizeof(t_samp_header) + BYTES_PER_SAMPLE_RECORD * (last_good_sample + 1));
	  bigfseek(F_SAMP, 0, SEEK_SET);
	  if (F_SCOR)
	    ftruncate(fileno(F_SCOR), sizeof(t_scor_header) + sizeof(t_score)  * (last_good_sample + 1));

	  // because we truncated, copy the previous scan record to the current one.
	  scan_record = prev_scan_record;
	  // set the apparent number of scans to record the truncation
	  apparent_num_scans = i;
	  break;
	}

	t1 = (time_t) prev_scan_record.timestamp;
        tptr2 = localtime(&t1);

	if (tptr1) 
	  strftime(time_buff, 255, "%Y/%m/%d %H:%M:%S", tptr1);
	else 
	  strcpy(time_buff, "BAD TIMESTAMP");
	if (tptr2)
	  strftime(time_buff_prev, 255, "%Y/%m/%d %H:%M:%S", tptr2);
	else
	  strcpy(time_buff_prev, "BAD TIMESTAMP");
	if (opt_timestamps) {
	  // Note:  in versions of seascanarch.c and seascan.c up to and including revision 594,
	  // radR was sometimes obtaining an incorrect timestamp from seascan archives / servers.
	  // The incorrect timestamps are exactly 1.0 second later than they should be.
	  // We detect this by checking whether the time difference from the last timestamp
	  // is closer to 2.564 (the approximate antenna rotation time for setups used with radR
	  // revisions 594 and earlier)
	  // However, it's also possible a scan is simply missing.  We detect that by a time
	  // difference of 4500 milliseconds or greater, since the other problem causes a time difference
	  // of around 3.5 seconds.  When found, we leave the timestamp as is, but adjust the duration
	  // of the previous scan to the mean value of 2.564

	  if (scan_record.timestamp - prev_scan_record.timestamp >= 4500e-3) {
	    if (prev_scan_record.duration != 2564) {
	      // if we haven't already corrected the previous scan's duration, do so
	      DOWARN("There is at least one scan missing between\nscan %6d at %s and\nscan %6d at %s\n", i, time_buff_prev, i + 1, time_buff);
	      dur = 2564;
	      NOTE_FILE_PATCH(DURATION, SCAN_FILE, bigftell(F_SCAN) - 2 * sizeof(scan_record) + ((char *) &scan_record.duration - (char *) &scan_record), i, SHORT, prev_scan_record.duration, dur);
	      prev_scan_record.duration = dur;
	    }
	  } else {
	    if (fabs (2.564 - (scan_record.timestamp - prev_scan_record.timestamp)) > 
		fabs (2.564 - (scan_record.timestamp - 1.0 - prev_scan_record.timestamp))) {
	      NOTE_FILE_PATCH(SEASCAN_TIMESTAMP, SCAN_FILE, bigftell(F_SCAN) - sizeof(scan_record) + ((char *) &scan_record.timestamp - (char *) &scan_record), i + 1, DOUBLE, scan_record.timestamp, scan_record.timestamp - 1.0);
	      scan_record.timestamp -= 1.0;
	    } else if (scan_record.timestamp < prev_scan_record.timestamp - 2.5) {
	      double last_good_blip = prev_scan_record.first_blip + prev_scan_record.num_blips - 1;
	      double last_good_sample;
	      DOWARN("scan record %d, has timestamp\n%.3f=%s.%03d, which is earlier than the previous scan's timestamp of\n%.3f=%s.%03d\n", i+1, scan_record.timestamp, time_buff, (int) (0.5 + 1000 * (scan_record.timestamp - (int) scan_record.timestamp)), prev_scan_record.timestamp, time_buff_prev,  (int) (0.5 + 1000 * (prev_scan_record.timestamp - (int)prev_scan_record.timestamp)));
	      printf("The final %d scans starting at scan %d may be bogus.\n", apparent_num_scans - i, i + 1);
	      if (confirm("Should I delete these remaining scans")) {
		fseek(F_BLIP, sizeof(t_blip_header) + sizeof(t_blip_record) * last_good_blip, SEEK_SET);
		if (1 != fread(&blip_record, sizeof(blip_record), 1, F_BLIP))
		  ERR("Error: couldn't read blip record %.0f\n", last_good_blip);
		last_good_sample = blip_record.first_sample + blip_record.num_samples - 1;
		ftruncate(fileno(F_SCAN), bigftell(F_SCAN) - sizeof(scan_record));
		NOTE_FILE_PATCH(SCAN_COUNT, SCAN_FILE, segment_header_offset + ((char *) &scan_header.num_scans - (char *) &scan_header), 0, INT, apparent_num_scans, i);
		ftruncate(fileno(F_BLIP), sizeof(t_blip_header) + sizeof(t_blip_record) * (last_good_blip + 1));
		bigfseek(F_BLIP, 0, SEEK_SET);
		ftruncate(fileno(F_SAMP), sizeof(t_samp_header) + BYTES_PER_SAMPLE_RECORD * (last_good_sample + 1));
		bigfseek(F_SAMP, 0, SEEK_SET);
		// because we truncated, copy the previous scan record to the current one.
		scan_record = prev_scan_record;
		// set the apparent number of scans to record the truncation
		apparent_num_scans = i;
		break;
	      }
	    } else if (scan_record.timestamp < prev_scan_record.timestamp) {
	      // there's a small time inversion, which can probably be corrected by running bmdebug.exe again
	      DOWARN("scan record %d, has a small timestamp inversion of %.3f seconds.\nThis can probably be fixed by re-running bmdebug\n", i + 1, prev_scan_record.timestamp - scan_record.timestamp);
	    }
	    // adjust the duration of the previous scan record to match the difference in timestamps
	    dur = (short) floor(0.5 + 1000 * (scan_record.timestamp - prev_scan_record.timestamp));
	    if (dur != prev_scan_record.duration)
	      NOTE_FILE_PATCH(DURATION, SCAN_FILE, bigftell(F_SCAN) - 2 * sizeof(scan_record) + ((char *) &scan_record.duration - (char *) &scan_record), i, SHORT, prev_scan_record.duration, dur );
	    prev_scan_record.duration = dur;
	  }
	}
	CHECK_SCAN(!=, bits_per_sample, %d, i);
	CHECK_SCAN(!=, pulses, %d, i);
	CHECK_SCAN(!=, samples_per_pulse, %d, i);
	CHECK_SCAN(<, first_blip, %f.0, i);
	del = (int) (scan_record.first_blip - (prev_scan_record.first_blip + prev_scan_record.num_blips));
	if (del < 0) {
	  // the previous scan is missing some blips; adjust the record accordingly.
	  DOWARN("(FIXED) scan record %d:%d is missing %d blips; num_blips will be reduced.", i_segment + 1, i, - del);
	  NOTE_FILE_PATCH(SCAN_MISSING_BLIPS, SCAN_FILE, bigftell(F_SCAN) - 2 * sizeof(scan_record) + ((char *) &scan_record.num_blips - (char *) &scan_record), i, INT, prev_scan_record.num_blips, (int)(scan_record.first_blip - prev_scan_record.first_blip) );
	  scan_num_blips += del; 
	} else if (scan_record.first_blip > prev_scan_record.first_blip + prev_scan_record.num_blips) {
	/* there are some unclaimed blips after those for the previous
	   scan and before those for the current scan.  we assume it's
	   because of a fix we've made to the file on a previous run,
	   so issue no warning and do nothing except note that there
	   are some extra un-owned blip records, which causes no
	   problem for the blipmoviearch code */
	  scan_num_blips += del;
	}
	if (scan_record.first_blip != floor(scan_record.first_blip)) {
	  DOWARN("scan record %d:%d, timestamp %s, has non-integer value %f for first_blip\n", i_segment + 1, i+1, time_buff,  scan_record.first_blip);
	}
      }
      scan_num_blips += scan_record.num_blips;
      prev_scan_record = scan_record;
      prev_scan_record_unfixed = scan_record_unfixed;
    }
    total_scans += apparent_num_scans;
    if (scan_header.last_scan_timestamp != (time_t) scan_record.timestamp) {
      NOTE_FILE_PATCH(TOC_LAST_TIMESTAMP, SCAN_FILE, segment_header_offset + ((char *) &scan_header.last_scan_timestamp - (char *) &scan_header), 0, TIME_T, scan_header.last_scan_timestamp, scan_record.timestamp);
      t1 = (time_t) scan_header.last_scan_timestamp;
      t2 = (time_t) scan_record.timestamp;
      strcpy(time_buff, ctime(&t1));
      DOWARN("(FIXED) scan file header segment %d claims last scan timestamp is\n%sbut last scan record has timestamp of\n%s\n", i_segment + 1, time_buff, ctime(&t2));
    }
    if (!opt_multiseg)
      break;
  } /* loop over segments */
  MSG("Checked %d scan records.\n", total_scans);
}

void
check_blip_file ()
{
  double i;

  /* we pass through the scan file again in parallel, so we can correct scan file entries
     if we find any blips which are missing samples */

  int iscan = 0;
  int nscan = 0;
  int n_del_blips = 0; // number of delete blips for current scan
  int del;

  double i_scan_last_blip = -1;

  bigfseek(F_SCAN, sizeof(t_scan_preamble), SEEK_SET);
  
  if (1 != fread(&blip_header, sizeof(blip_header), 1, F_BLIP))
    ERR("Error: couldn't read header from blip file.\n");
  fstat(fileno(F_BLIP), &st);
  if ( blip_header.num_blips != (apparent_num_blips = (st.st_size - sizeof(blip_header)) / (double) sizeof(blip_record))) {
    DOWARN("(FIXED) blip file claims to have \n%.0f blips, but file size implies \n%.0f blips.\n", blip_header.num_blips, 
	   apparent_num_blips);
    NOTE_FILE_PATCH(BLIP_COUNT, BLIP_FILE, ((char *) &blip_header.num_blips - (char *) &blip_header), 0, DOUBLE, blip_header.num_blips, apparent_num_blips);
    blip_header.num_blips = apparent_num_blips;
    if ((i = (st.st_size - sizeof(blip_header)) % sizeof(blip_record))) {
      DOWARN("%.0f extra bytes or partial blip record at end of blip file.\n", i);
    }
  }
  if ( apparent_num_blips != scan_num_blips ) {
    if (scan_num_blips < apparent_num_blips) {
      DOWARN("(FIXED)scan file records report a total of only \n%.0f blips, but blip file appears to hold \n%.0f blips.\n", scan_num_blips, apparent_num_blips);
      if (confirm("Should I delete these unused blips")) {
	ftruncate(fileno(F_BLIP), sizeof(blip_header) + scan_num_blips * sizeof(blip_record));
	NOTE_FILE_PATCH(BLIP_COUNT, BLIP_FILE,  ((char *) &blip_header.num_blips - (char *) &blip_header), 0, DOUBLE, apparent_num_blips, scan_num_blips);
	
	blip_header.num_blips = apparent_num_blips = scan_num_blips;
      }
    } else {
      DOWARN("scan file records report a total of \n%.0f blips, but blip file appears to hold only \n%.0f blips.\n", scan_num_blips, apparent_num_blips);
      printf("*** Error: bmdebug doesn't yet know how to handle this situation. ***\n");
      exit(2);
    }
  }

  blip_num_samps = 0;

  for (i = 0; i < apparent_num_blips; ++i) {
    if (iscan == nscan) {
      // read the header for a new (possibly the first) segment in the scan file
      fread(&scan_header, sizeof(scan_header), 1, F_SCAN);
      nscan = scan_header.num_scans;
    }
    if (i > i_scan_last_blip) {
      // read the header for a new scan in the scan file
      prev_scan_record = scan_record;
      fread(&scan_record, sizeof(scan_header), 1, F_SCAN);
      i_scan_last_blip = i + scan_record.num_blips - 1;
      n_del_blips = 0; // number of blips deleted from this scan
      ++ iscan;
    }

    if (1 != fread(&blip_record, sizeof(blip_record), 1, F_BLIP))
      ERR("Error: couldn't read blip record %.0f\n", i + 1);
    if (i > 1) {
      CHECK_BLIP(<, first_sample, %.0f, i);
      del = (int) (blip_record.first_sample - (prev_blip_record.first_sample + prev_blip_record.num_samples)); 
      if (del < 0 && del == - prev_blip_record.num_samples) {
	/* the previous blip is missing some samples.  We delete that
	   blip by copying remaining blip records for its scan down
	   one slot, and decrementing the scan's blip count.  The
	   leftover blip slot at the end will be ignored by
	   blipmoviearch code, since it fseeks to the next scan's first blip.

	   To allow for deleting more than one defective blip within a scan,
	   we keep track of the number of blips deleted for the current scan,
	   so that we can adjust the 2nd and subsequent COPY patches down a slot
	   for each already deleted blip.
	*/

	  ++n_del_blips;

	  DOWARN("(FIXED)blip %.0f is missing all %d of its samples and will be deleted.\nScan record %d will be adjusted accordingly.\n", i, -del, iscan);
	  NOTE_FILE_PATCH(BLIP_MISSING_SAMPLES, SCAN_FILE, bigftell(F_SCAN) - sizeof(scan_record) + ((char *) &scan_record.num_blips - (char *) &scan_record), i, INT, scan_record.num_blips - (n_del_blips - 1), scan_record.num_blips - n_del_blips);

	/* if this is not the last blip for this scan, copy the remaining blip records down, because the
	   blipmoviearch code requires that blip records for a given scan be contiguous in the blip file */

	if (i < i_scan_last_blip)
	  NOTE_FILE_COPY_PATCH(BLIP_MISSING_SAMPLES,
			       BLIP_FILE, 
			       bigftell(F_BLIP) - (n_del_blips - 1) * sizeof(blip_record),
			       - ((int)sizeof(blip_record)),
			       sizeof(blip_record) * (int)(i_scan_last_blip - i));
	blip_num_samps += del;

      } else if (del > 0) {
	blip_num_samps += del;
      } else if (del != 0) {
	  DOWARN("blip %.0f in scan %d is missing some but not all of its samples.  bmdebug can't handle this situation yet\n",  i, iscan);
	  exit(2);
      }
    }
    blip_num_samps += blip_record.num_samples;
    if (blip_record.first_sample != floor(blip_record.first_sample)) {
      DOWARN("blip record %.0f has non-integer value %f for first_sample_index\n", i+1, blip_record.first_sample);
    }

    prev_blip_record = blip_record;
  }
  MSG("Checked %.0f blip records.\n", apparent_num_blips);
}

void 
check_samp_file ()
{
  int i;
  
  if (1 != fread(&samp_header, sizeof(samp_header), 1, F_SAMP))
    ERR("Error: couldn't read header from sample file.\n");
  fstat(fileno(F_SAMP), &st);
  if ( samp_header.num_samples != (apparent_num_samps = (st.st_size - sizeof(samp_header)) / (double) BYTES_PER_SAMPLE_RECORD)) {
    DOWARN("sample file claims to have\n%.0f samples, but file size implies\n%.0f samples.\n", samp_header.num_samples, 
	   apparent_num_samps);
    NOTE_FILE_PATCH(SAMPLE_COUNT, SAMP_FILE, ((char *) &samp_header.num_samples - (char *) &samp_header), 0, DOUBLE, samp_header.num_samples, apparent_num_samps);
    samp_header.num_samples = apparent_num_samps;
    if ((i = (st.st_size - sizeof(samp_header)) % BYTES_PER_SAMPLE_RECORD)) {
      DOWARN("%d extra bytes or partial sample record at end of sample file.\n", i);
    }
  }
  if ( apparent_num_samps != blip_num_samps ) {
    if (blip_num_samps < apparent_num_samps) {
      DOWARN("(FIXED)blip file records report a total of only \n%.0f samples, but sample file appears to hold \n%.0f samples.\n", blip_num_samps, apparent_num_samps);
      if (confirm("Should I delete these unused samples")) {
	ftruncate(fileno(F_SAMP), sizeof(samp_header) + blip_num_samps * BYTES_PER_SAMPLE_RECORD);
	NOTE_FILE_PATCH(BLIP_COUNT, SAMP_FILE,  ((char *) &samp_header.num_samples - (char *) &samp_header), 0, DOUBLE, apparent_num_samps, blip_num_samps);
	samp_header.num_samples = apparent_num_samps = blip_num_samps;
      }
    } else {
      DOWARN("blip file records report a total of \n%.0f samples, but sample file appears to hold only \n%.0f samples.\n", blip_num_samps, apparent_num_samps);
      printf("*** Error: bmdebug doesn't yet know how to handle this situation. ***\n");
      exit(2);
    }
  }
}


void 
check_scor_file ()
{
  int i;
  
  if (1 != fread(&scor_header, sizeof(scor_header), 1, F_SCOR))
    ERR("Error: couldn't read header from score file.\n");
  fstat(fileno(F_SCOR), &st);
  if ( scor_header.num_samples != (apparent_num_scores = (st.st_size - sizeof(scor_header)) / sizeof(t_score))) {
    DOWARN("score file claims to have\n%.0f scores, but file size implies\n%.0f scores.\n", scor_header.num_samples, 
	   apparent_num_scores);
    NOTE_FILE_PATCH(SCORE_COUNT, SCOR_FILE, ((char *) &scor_header.num_samples - (char *) &scor_header), 0, DOUBLE, scor_header.num_samples, apparent_num_scores);
    scor_header.num_samples = apparent_num_scores;
    if ((i = (st.st_size - sizeof(scor_header)) % sizeof(t_score))) {
      DOWARN("%d extra bytes or partial score record at end of score file.\n", i);
    }
  }
  if ( apparent_num_scores != blip_num_samps ) {
    if (blip_num_samps < apparent_num_scores) {
      DOWARN("(FIXED)blip file records report a total of only \n%.0f scores, but score file appears to hold \n%.0f scors.\n", blip_num_samps, apparent_num_scores);
      if (confirm("Should I delete these unused scores")) {
	ftruncate(fileno(F_SCOR), sizeof(scor_header) + blip_num_samps * sizeof(t_score));
	NOTE_FILE_PATCH(BLIP_COUNT, SCOR_FILE,  ((char *) &scor_header.num_samples - (char *) &scor_header), 0, DOUBLE, apparent_num_scores, blip_num_samps);
	scor_header.num_samples = apparent_num_scores = blip_num_samps;
      }
    } else {
      DOWARN("blip file records report a total of \n%.0f scores, but sample file appears to hold only \n%.0f scores.\n", blip_num_samps, apparent_num_scores);
      printf("*** Error: bmdebug doesn't yet know how to handle this situation.  Consider truncating the blipmovie. ***\n");
      exit(2);
    }
  }
}

int 
skip_scan_preamble ()
{
  // skip over the scan file preamble.  Return FALSE on failure.
  return !bigfseek(F_SCAN, sizeof(t_scan_preamble), SEEK_SET);
}

void
check_blip_samps ()
{
  int scan, i, j, ns, np, spp, nb, minx, maxx, miny, maxy, maxval, i_segment;
  long long segment_header_offset;

  bigfseek(F_SCAN, (long long)sizeof(t_scan_preamble), SEEK_SET);
  bigfseek(F_BLIP, (long long)sizeof(blip_header), SEEK_SET);
  bigfseek(F_SAMP, (long long)sizeof(samp_header), SEEK_SET);

  /* For each blip, ensure that its pulse and sample numbers
     do not have any missing values between their minimum and
     maximum.  This is necessary, but not sufficient, for
     the blip samples to be contiguous, and should be a strong
     enough test to detect any systematic file corruption.
     We overwrite the sample values in memory while doing this test.
  */

  for (i_segment = 0; (opt_multiseg || i_segment == 0); ++i_segment) {
    segment_header_offset = bigftell(F_SCAN);
    if (1 != fread(&scan_header, sizeof(scan_header), 1, F_SCAN))
      ERR("Error: couldn't read header for segment %d from scan file.\n", 1 + i_segment);
    for (scan = 0; scan < scan_header.num_scans; ++scan) {
      if (1 != fread(&scan_record, sizeof(scan_record), 1, F_SCAN))
	ERR("Error: couldn't read scan record %d:%d.\n", i_segment + 1, scan + 1);
      bigfseek(F_BLIP, sizeof(blip_header) + sizeof(blip_record) *  (long long) scan_record.first_blip, SEEK_SET);
      nb = scan_record.num_blips;
      np = scan_record.pulses;
      spp = scan_record.samples_per_pulse;
      for (i=0; i < nb; ++i ) {
	if (1 != fread(&blip_record, sizeof(blip_record), 1, F_BLIP))
	  ERR("Error: couldn't read blip record %d\n", i + 1);
	bigfseek(F_SAMP, sizeof(samp_header) + BYTES_PER_SAMPLE_RECORD *  (long long) blip_record.first_sample, SEEK_SET);
	ns = blip_record.num_samples;
	
	ensure_samp_buff(ns, max(np, spp));

	if (1 != fread(samp_buff[0], ns * BYTES_PER_SAMPLE_RECORD, 1, F_SAMP))
	  ERR("Error: couldn't read the %d samples for blip %d.\n", ns, i + 1);
    
	/* get the min and max x and y (pulse and sample #) */

	minx = maxx = samp_buff[0][0];
	miny = maxy = samp_buff[1][0];

	for (j=1; j < ns; ++j) {
	  if (samp_buff[0][j] < minx) 
	    minx = samp_buff[0][j];
	  if (samp_buff[0][j] > maxx)
	    maxx = samp_buff[0][j];
	  if (samp_buff[1][j] < miny) 
	    miny = samp_buff[1][j];
	  if (samp_buff[1][j] > maxy)
	    maxy = samp_buff[1][j];
	}

	/* check that pulse and sample numbers are within range for this scan,
	   remembering that these are stored origin 1 in the file */
	if (maxx > scan_record.pulses) {
	  t1 = (time_t) scan_record.timestamp;
	  DOWARN("in scan %d at %sblip %d (= absolute blip # %.0f) has a pulse number (%d) which is larger than the maximum allowed (%d) for this scan\n", scan + 1, ctime(&t1), i+1, i+scan_record.first_blip + 1, maxx, scan_record.pulses);
	  goto BAD_BLIP;
	}

	if (maxy > scan_record.samples_per_pulse) {
	  t1 = (time_t) scan_record.timestamp;
	  DOWARN("in scan %d at %sblip %d (= absolute blip # %.0f) has a sample number (%d) which is larger than the maximum allowed (%d) for this scan\n", scan + 1, ctime(&t1), i+1, i+scan_record.first_blip + 1, maxy, scan_record.samples_per_pulse);
	  goto BAD_BLIP;
	}


	/* check that all sample values are within range */
	maxval = 1 << scan_record.bits_per_sample;

	for (j=0; j < ns; ++j) {
	  if (samp_buff[2][j] >= maxval) {
	    t1 = (time_t) scan_record.timestamp;
	    DOWARN("in scan %d at %sblip %d (= absolute blip # %.0f) has a sample value (%d) which is larger than the maximum allowed (%d) for this scan\n", scan + 1, ctime(&t1), i+1, i+scan_record.first_blip + 1, samp_buff[2][j], maxval - 1);
	    goto BAD_BLIP;
	  }
	}
      
	/* we use the extra sample buffer to flag which x (i.e. pulse) coordinates
	   have been used by samples in this blip */

	memset(samp_buff[3], 0, (maxx - minx + 1) * sizeof(samp_buff[3][0]));
	for (j=0; j < ns; ++j)
	  samp_buff[3][samp_buff[0][j] - minx] = 1;
    
	/* check for missing pulse values, which indicate a discontiguous blip;
	   Note that for a pulse crossing the zero-cut, we do a different test. */

        if (minx == 1 && maxx == np) {
	  // check that the pulses present form a contiguous set
	  for (j = 0; j < np && samp_buff[3][j]; ++j);  // initial "present" segment
	  for (/**/ ; j < np && !samp_buff[3][j]; ++j); // possible "absent" segment
	  for (/**/ ; j < np && samp_buff[3][j]; ++j);  // final "present" segment
	  if (j != np) {
	    t1 = (time_t) scan_record.timestamp;
	    DOWARN("in scan %d at %sblip %d (= absolute blip # %.0f) is non-contiguous: the set of its pulse numbers has more than one gap\n", scan + 1, ctime(&t1), i+1, i+ 1 + scan_record.first_blip);
	    goto BAD_BLIP;
	  }
	} else {
	  for (j=0; j <= maxx - minx; ++j) {
	    if (!samp_buff[3][j]) {
	      t1 = (time_t) scan_record.timestamp;
	      DOWARN("in scan %d at %sblip %d (= absolute blip # %.0f) is non-contiguous: its pulse numbers go from %d to %d but there is no sample with pulse # %d\n", scan + 1, ctime(&t1), i+1, i+ 1 + scan_record.first_blip, minx, maxx, minx + j);
	      goto BAD_BLIP;
	    }
	  }
	}
    
	/* do the same with sample numbers */
    
	memset(samp_buff[3], 0, (maxy - miny + 1) * sizeof(samp_buff[3][0]));
	for (j=0; j < ns; ++j)
	  samp_buff[3][samp_buff[1][j] - miny] = 1;
	for (j=0; j <= maxy - miny; ++j) {
	  if (!samp_buff[3][j]) {
	    t1 = (time_t) scan_record.timestamp;
	    DOWARN("in scan %d at %sblip %d (= absolute blip # %.0f) is non-contiguous: its sample numbers go from %d to %d but there is no sample with sample # %d\n", scan + 1,ctime(&t1), i + 1, i+1 + scan_record.first_blip, miny, maxy, miny + j);
	    goto BAD_BLIP;
	  }
	}
      }
    BAD_BLIP:
      continue; /* go to the next scan */
    }
  } /* go to the next segment */
  MSG("Checked %.0f blips for coordinate and value ranges and internal contiguity\n", apparent_num_blips);
}
