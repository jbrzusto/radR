/*

  blipmoviearch.h - definitions for the blipmovie archive reader/writer

*/

#ifndef Win32
#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64
#endif
#include <stdlib.h>
#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include "radRmodule.h"
#include "patchify.h"
#include <sys/types.h>
#include <unistd.h>
#include "radR.h"

/* definition of the blipmovie archive file format */

#define SCANFILE_PREAMBLE_LENGTH 4096
#define EARLIEST_VALID_TIMESTAMP 315550800 // 1980-01-01 00:00:00 GMT 
typedef struct {
  char text[SCANFILE_PREAMBLE_LENGTH];
}  __attribute__((packed)) t_scan_preamble;

/* the preamble is followed by a header */

typedef struct {
  int num_scans;
  time_t first_scan_timestamp;
  time_t last_scan_timestamp;
}  __attribute__((packed)) t_scan_header;

/* in memory, headers are saved in a structure that
   also records their offset from the start of the scan
   file */

typedef struct {
  long long offset;
  t_scan_header header;
}  __attribute__((packed)) t_scan_header_mem;

/* the header is followed by num_scans scan records */

typedef struct {
  double timestamp;          // time_t value plus fractional number of seconds; time at top of scan    
  short duration;	     // duration of scan in milliseconds                                       
  short pulses;		     // number of pulses in scan                                               
  short samples_per_pulse;   // samples per pulse (                                                    
  short orientation;         // +1 = clockwise, -1 = counterclockwise                                  
  short bits_per_sample;     // bits per sample                                                        
  double sample_dist;	     // range increment covered by one sample                                  
  double first_sample_dist;  // range to the start of the first sample                                 
  double bearing;	     // how many degrees from north, clockwise, is first pulse                 
  double first_blip;	     // index (starting from 0) of first blip for this scan in the blips file  
  int num_blips;	     // number of blips in this scan                                           
} __attribute__((packed)) t_scan_record;

/* there can be another header, and another set of scans, repeatedly, until
   the end of the scan file */

typedef struct {
  double num_blips;
} __attribute__((packed)) t_blip_header;

typedef struct {
  double first_sample;
  int num_samples;
}  __attribute__((packed)) t_blip_record;

typedef struct {
  double num_samples;
} __attribute__((packed)) t_samp_header;

typedef struct {
  double num_samples;
} __attribute__((packed)) t_scor_header;

/* 
   NOTE: the sample records for a blip are stored on disk as
   three consecutive arrays: unsigned short pulse_num[num_samples],
   sample_num[num_samples], value[num_samples].  So each sample
   requires 6 bytes (2 bytes for each of pulse_num, sample_num, and
   value), but these are interleaved.
*/
  
#define BYTES_PER_SAMPLE_RECORD (sizeof(t_sample) + 2 * sizeof(t_scan_dim))
#define BYTES_PER_SCORE_RECORD (sizeof(t_score))

/* types for maintaining an open blipmovie archive */

typedef enum {
  BMA_NONE = -1,                /* no port */
  BMA_READER = 0,		/* the archive reader */
  BMA_WRITER = 1,		/* the archive writer */
} t_bma_port;

// indexes into the t_bma.files array

#define BMA_SCAN_FILE 0
#define BMA_BLIP_FILE 1
#define BMA_SAMP_FILE 2
#define BMA_SCOR_FILE 3
    
typedef struct {
  t_bma_port port;		    /* what port am I?  if ssa->port == x, then ports[x] == ssa */
  int is_started;		    /* has start_up been called on this port? */
  char basename[1024];              /* full path to archive file (without ".bma" extension */
  FILE *files[4];		    /* file handles for scan, blip, and sample files */
  int num_runs;		            /* the number of runs in this file */
  int run_index;                    /* index of run which includes next image to be read */
  int scan_index;                   /* index of next archive image to be read, among all images in the archive, not
				    just the ones in the current; segment zero-based */
  int have_scan_data;               /* is there data available for a call to get_scan_data */
  t_scan_preamble scan_preamble;    /* the scan preamble */
  t_extmat scan_headers;	    /* an array of t_scan_header_mem */
  t_scan_record scan_record;	    /* record from the scan file */
  t_blip_header blip_header;        /* header of the blip file */
  t_blip_record blip_record;	    /* record from the blip file */
  t_samp_header samp_header;	    /* header of the sample file */
  t_scor_header scor_header;	    /* header of the scores file */
  int have_toc;
  int have_scores;
} t_bma;

char *bma_file_suffixes[] = {".bma", ".blp", ".smp", ".scr"};

// macros to access the n'th scan header and its file offset

#define SCAN_HEADER_MEM(ME, N) (((t_scan_header_mem *) ME->scan_headers.ptr)[N])
#define SCAN_HEADER(ME, N) (SCAN_HEADER_MEM(ME, N).header)
#define SCAN_HEADER_OFFSET(ME, N)(SCAN_HEADER_MEM(ME, N).offset)

// these macros all return non-zero on error
// seek to the next scan header, assuming we have just read a scan header
// from the scan file

// seek to the Nth scan header, once the headers and their offsets have been loaded
// into memory

#define SEEK_SCAN_HEADER(ME, N) bigfseek(ME->files[BMA_SCAN_FILE], ((N) > 0) ? SCAN_HEADER_OFFSET(ME, N) : sizeof(t_scan_preamble), SEEK_SET)
#define SEEK_BLIP_HEADER(ME) bigfseek(ME->files[BMA_BLIP_FILE], 0LL, SEEK_SET)
#define SEEK_SAMP_HEADER(ME) bigfseek(ME->files[BMA_SAMP_FILE], 0LL, SEEK_SET)
#define SEEK_SCOR_HEADER(ME) bigfseek(ME->files[BMA_SCOR_FILE], 0LL, SEEK_SET)
#define SEEK_BLIP(ME, N)     bigfseek(ME->files[BMA_BLIP_FILE], sizeof(t_blip_header) + ((long long)(N)) * sizeof(t_blip_record), SEEK_SET)
#define SEEK_SAMP(ME, N)     bigfseek(ME->files[BMA_SAMP_FILE], sizeof(t_samp_header) + ((long long)(N)) * BYTES_PER_SAMPLE_RECORD, SEEK_SET)
#define SEEK_SCOR(ME, N)     bigfseek(ME->files[BMA_SCOR_FILE], sizeof(t_scor_header) + ((long long)(N)) * BYTES_PER_SCORE_RECORD, SEEK_SET)

#define READ_SCAN_PREAMBLE(ME) (1 != fread(&ME->scan_preamble, sizeof(t_scan_preamble), 1, ME->files[BMA_SCAN_FILE]))
#define READ_SCAN_HEADER(ME, N)  (1 != fread(&SCAN_HEADER(ME, N), sizeof(t_scan_header), 1, ME->files[BMA_SCAN_FILE]))
#define READ_BLIP_HEADER(ME)  (1 != fread(&ME->blip_header, sizeof(t_blip_header), 1, ME->files[BMA_BLIP_FILE]))
#define READ_SAMP_HEADER(ME)  (1 != fread(&ME->samp_header, sizeof(t_samp_header), 1, ME->files[BMA_SAMP_FILE]))
#define READ_SCOR_HEADER(ME)  (1 != fread(&ME->scor_header, sizeof(t_scor_header), 1, ME->files[BMA_SCOR_FILE]))
#define READ_SCAN(ME)         (1 != fread(&ME->scan_record, sizeof(t_scan_record), 1, ME->files[BMA_SCAN_FILE]))
#define READ_BLIP(ME)         (1 != fread(&ME->blip_record, sizeof(t_blip_record), 1, ME->files[BMA_BLIP_FILE]))
#define READ_SAMPLES(ME, PTR, N)  (N != fread(PTR, BYTES_PER_SAMPLE_RECORD, N, ME->files[BMA_SAMP_FILE]))
#define READ_SCORES(ME, PTR, N)  (N != fread(PTR, BYTES_PER_SCORE_RECORD, N, ME->files[BMA_SCOR_FILE]))

#define WRITE_SCAN_PREAMBLE(ME) (void) ((1 != fwrite(&ME->scan_preamble, sizeof(t_scan_preamble), 1, ME->files[BMA_SCAN_FILE])))
#define WRITE_SCAN_HEADER(ME, N) (1 != fwrite(&SCAN_HEADER(ME, N), sizeof(t_scan_header), 1, ME->files[BMA_SCAN_FILE]))
#define WRITE_BLIP_HEADER(ME) (1 != fwrite(&ME->blip_header, sizeof(t_blip_header), 1, ME->files[BMA_BLIP_FILE]))
#define WRITE_SAMP_HEADER(ME) (1 != fwrite(&ME->samp_header, sizeof(t_samp_header), 1, ME->files[BMA_SAMP_FILE]))
#define WRITE_SCOR_HEADER(ME) (1 != fwrite(&ME->scor_header, sizeof(t_scor_header), 1, ME->files[BMA_SCOR_FILE]))
#define WRITE_SCAN(ME)        (1 != fwrite(&ME->scan_record, sizeof(t_scan_record), 1, ME->files[BMA_SCAN_FILE]))
#define WRITE_BLIP(ME)        (1 != fwrite(&ME->blip_record, sizeof(t_blip_record), 1, ME->files[BMA_BLIP_FILE]))
#define WRITE_SAMPLES(ME, PTR, N)  (N != fwrite(PTR, BYTES_PER_SAMPLE_RECORD, N, ME->files[BMA_SAMP_FILE]))
#define WRITE_SCORES(ME, PTR, N)  (N != fwrite(PTR, BYTES_PER_SCORE_RECORD, N, ME->files[BMA_SCOR_FILE]))

#define GET_SCAN_HEADER(ME, N) (SEEK_SCAN_HEADER(ME, N) || READ_SCAN_HEADER(ME, N))
#define GET_BLIP_HEADER(ME)    (SEEK_BLIP_HEADER(ME)    || READ_BLIP_HEADER(ME))
#define GET_SAMP_HEADER(ME)    (SEEK_SAMP_HEADER(ME)    || READ_SAMP_HEADER(ME))
#define GET_SCOR_HEADER(ME)    (SEEK_SCOR_HEADER(ME)    || READ_SCOR_HEADER(ME))
#define GET_BLIP(ME, N)        (SEEK_BLIP(ME, N)        || READ_BLIP(ME))

#define PUT_SCAN_HEADER(ME, N) (void) ((SEEK_SCAN_HEADER(ME, N) || WRITE_SCAN_HEADER(ME, N)))
#define PUT_BLIP_HEADER(ME)    (void) ((SEEK_BLIP_HEADER(ME)    || WRITE_BLIP_HEADER(ME)))
#define PUT_SAMP_HEADER(ME)    (void) ((SEEK_SAMP_HEADER(ME)    || WRITE_SAMP_HEADER(ME)))
#define PUT_SCOR_HEADER(ME)    (void) ((SEEK_SCOR_HEADER(ME)    || WRITE_SCOR_HEADER(ME)))
#define PUT_BLIP(ME, N)        (void) ((SEEK_BLIP(ME, N)        || WRITE_BLIP(ME)))

// these macros all use the current run index, so we use "CURRENT" in the name

#define CURRENT_NUM_SCANS(ME)  (SCAN_HEADER(ME, ME->run_index).num_scans)
#define CURRENT_FIRST_SCAN_TIMESTAMP(ME) (SCAN_HEADER(ME, ME->run_index).first_scan_timestamp)
#define CURRENT_LAST_SCAN_TIMESTAMP(ME) (SCAN_HEADER(ME, ME->run_index).last_scan_timestamp)
// seek to the Nth scan of the current scan run
#define SEEK_SCAN_CURRENT_RUN(ME, N)     bigfseek(ME->files[BMA_SCAN_FILE], SCAN_HEADER_OFFSET(ME, ME->run_index) + ((long long)(N)) * sizeof(t_scan_record) + sizeof(t_scan_header), SEEK_SET)

#define SEEK_PAST_CURRENT_RUN(ME) bigfseek(ME->files[BMA_SCAN_FILE], SCAN_HEADER_OFFSET(ME, ME->run_index) + sizeof(t_scan_header) + sizeof(t_scan_record) * CURRENT_NUM_SCANS(ME), SEEK_SET)
#define GET_SCAN_CURRENT_RUN(ME, N) (SEEK_SCAN_CURRENT_RUN(ME, N) || READ_SCAN(ME))
#define PUT_SCAN_CURRENT_RUN(ME, N) (SEEK_SCAN_CURRENT_RUN(ME, N) || WRITE_SCAN(ME))

// forward declaration
SEXP start_up(SEXP portsxp);

static char SCANFILE_PREAMBLE_TEXT[] = 
"## radR blipmovie scan file v 00.10  Binary data - do not edit!\n\
## This is file 1 of 3:  you also need the XXX.blp and XXX.smp files\n\
\n\
## sample R code for reading the scan information stored in this file:\n\
\n\
## utility functions:\n\
\n\
DOUBLE  <- function(f, n=1) readBin(f, \"double\", n, size=8, endian=\"little\")\n\
INT     <- function(f, n=1) readBin(f, \"int\",    n, size=4, endian=\"little\")\n\
SHORT   <- function(f, n=1) readBin(f, \"int\",    n, size=2, endian=\"little\")\n\
USHORT  <- function(f, n=1) readBin(f, \"int\",    n, size=2, signed=FALSE, endian=\"little\")\n\
DATE    <- function(x) {class(x)<-\"POSIXct\"; x}\n\
\n\
## constants:\n\
scan.preamble.size <- 4096\n\
scan.header.size <- 12\n\
scan.record.size <- 54  ## 5 DOUBLE + 5 SHORT + 1 INT\n\
blip.header.size <- 8\n\
blip.record.size <- 12\n\
samp.header.size <- 8\n\
samp.record.size <- 6\n\
## set the timezone to GMT\n\
Sys.setenv(TZ=\"GMT\")\n\
\n\
openFiles <- function(f) {\n\
  ## f is the basename of the blip movie files\n\
  ## i.e. the part before the \".bma\"\n\
\n\
  f1 <<- file(paste(f, \".bma\", sep=\"\"), \"rb\")\n\
  f2 <<- file(paste(f, \".blp\", sep=\"\"), \"rb\")\n\
  f3 <<- file(paste(f, \".smp\", sep=\"\"), \"rb\")\n\
\n\
  ## skip the preamble (this text)\n\
  seek(f1, scan.preamble.size)\n\
\n\
  ## read the header details\n\
  ## from the scan file:\n\
  return(list(\n\
              num.scans = INT(f1),\n\
              first.scan.timestamp = DATE(INT(f1)),\n\
              last.scan.timestamp = DATE(INT(f1)),\n\
              ## from the blip file:\n\
              total.num.blips = DOUBLE(f2),\n\
              ## and from the samples file:\n\
              total.num.samples = DOUBLE(f3)))\n\
}\n\
\n\
## a function to read data for the nth scan, n=1, 2, ..., num.scans\n\
getScan <- function(n) {\n\
## seek to the nth scan, skipping the descriptive and machine headers\n\
  seek(f1, scan.preamble.size + scan.header.size + (n-1) * scan.record.size)\n\
  return(list(\n\
   timestamp           = DATE(floor(ts <- DOUBLE(f1))),\n\
   time.offset         = ts - floor(ts),\n\
   duration            = SHORT(f1),\n\
   pulses              = SHORT(f1),\n\
   samples.per.pulse   = SHORT(f1),\n\
   orientation  = SHORT(f1),\n\
   bits.per.sample     = SHORT(f1),\n\
   sample.dist         = DOUBLE(f1),\n\
   first.sample.dist   = DOUBLE(f1),\n\
   north.offset        = DOUBLE(f1),\n\
   first.blip          = DOUBLE(f1),\n\
   num.blips           = INT(f1)\n\
 ))\n\
}\n\
\n\
## a function to read data for the nth blip, n=1, 2, ..., total.num.blips\n\
getBlip <- function(n) {\n\
## seek to the nth blip, skipping the header\n\
  seek(f2, blip.header.size + (n-1) * blip.record.size)\n\
  return(list(first.sample=DOUBLE(f2),\n\
              num.samples=INT(f2)\n\
    ))\n\
}\n\
\n\
## a function to read the samples data for the nth blip, n=1, 2, ... total.num.blips\n\
## returns a three column matrix: pulse, sample, value\n\
getBlipSamples <- function(n) {\n\
## get the nth blip\n\
  b <- getBlip(n)\n\
\n\
## seek to the first sample, skipping the header\n\
\n\
  seek(f3, samp.header.size + b$first.sample * samp.record.size)\n\
## read the pulse and samples numbers and the\n\
## values of the corresponding samples\n\
  pulse.num <- USHORT(f3, b$num.samples)\n\
  sample.num <- USHORT(f3, b$num.samples)\n\
  value <- USHORT(f3, b$num.samples)\n\
  return(cbind(pulse.num, sample.num, value))\n\
}\n\
\n\
cat(\"\n\nFunctions for blipmovie archives: \n\n\
openFiles(f)      opens the three blip movie archive files with basename f\n\n\
getScan(n)        gets info on the n-th scan in the archive\n\n\
getBlip(n)        gets info on the n-th blip in the archive\n\n\
getBlipSamples(n) returns the raw sample data for the n-th blip\n\n\
\n\
e.g.\n\
\n\
openFiles(\"bmovie\")\n\
\n\
## to get the samples for the all blips in the 100th scan:\n\
\n\
scan <- getScan(100)\n\
samps <- data.frame()\n\
for(i in 1:scan$num.blips)\n\
  samps <- rbind(samps, cbind(i, getBlipSamples(scan$first.blip + i -1)))\n\
\n\
names(samps) <- c(\"blip.num\", \"pulse.num\", \"sample.num\", \"value\")\n\
samps$theta <- pi / 2 - pi * (scan$north.offset + scan$orientation * 360 * (samps$pulse.num - 1) / scan$pulses) / 180\n\
samps$range <- scan$first.sample.dist + scan$sample.dist * samps$sample.num\n\
samps$x <- samps$range * cos(samps$theta)\n\
samps$y <- samps$range * sin(samps$theta)\n";
