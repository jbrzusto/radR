/*

decompressInrad.c - extract gated data from an lz4-compressed inrad image

(C) 2015, John Brzustowski
Licence: GPL V2+

*/

#include <lz4.h>
#include <stdint.h>
#include <stdlib.h>
#include "radRmodule.h"

// from inrad radR plugin

typedef struct bscan_info_record_t
{
	uint32_t header_size;	      // bytes
        uint32_t rev_number;          // formatted as (MAJOR << 16) | (MINOR << 8) | (REVISION); e.g. 0x0001 00 01 = 1.0.1
	uint32_t samples_per_line;
	uint32_t num_output_lines;
	uint32_t data_size;	      // bytes
	uint32_t compression_state;
	uint32_t compressed_size;     // bytes
	uint32_t acp_count_range;
	uint32_t num_source_lines;
	uint32_t time_stamp_seconds;  // seconds since unix epoch, 00:00 UTC 1/1/1970
	uint32_t time_stamp_useconds; // microseconds since unix epoch, 00:00 UTC 1/1/1970
	uint32_t scan_duration;	      // microseconds
	uint32_t rotation_period;     // milliseconds
	uint32_t prf;		      // milliHz
	uint32_t sample_rate;	      // samps per second * 10
	uint32_t range_per_sample;    // mm
	uint32_t start_range;	      // mm
	uint32_t end_range;	      // mm
	uint32_t adc_gain;	      // dB * 100
	uint32_t adc_offset;	      // code, 0-1024
	uint32_t heading;	      // degress * 1e6
	int32_t	 time_zone;
	uint32_t antenna_elevation;   // degress * 1e6
	int32_t latitude;	      // +/- degrees.decimal * 1e7
	int32_t longitude;	      // +/- degrees.decimal * 1e7
	int32_t altitude;	      // mm from datum
	uint32_t pl_mode;	      // pulse length table entry record number
	uint8_t pl_name[16];	      // pulse length table entry name
        uint32_t magic;               // inrad format magic number: 0x07fe03fa
        uint16_t azimuths[0];         // for version 1.1.0 and above, an azimuth for each output line (i.e. size is sizeof(uint16_t)*num_output_lines
} bscan_info_record;

#define MAX_RADARCAM_AZIMUTH 8192

typedef uint16_t sample_t;

char *decomp_buff = 0;
int decomp_buff_samples = 0;

uint16_t *azi_buff = 0;
int azi_buff_samples = 0;

// Is integer azimuth azi_src1 at least as good as azi_src2 (both are
// in the range [0, max_azi_src])  "Better" means as an approximation to
// azi_dst, which is in the range [0, max_azi_dst]
int azi_better(int azi_src_1, int azi_src_2, int max_azi_src, int azi_dst, int max_azi_dst) {
  // return abs(azi_src_1 / max_azi_src - azi_dst / max_azi_dst) <= abs(azi_src_2 / max_azi_src - azi_dst / max_azi_dst)
  return abs(max_azi_dst * azi_src_1 - azi_dst * max_azi_src) <= abs(max_azi_dst * azi_src_2 - azi_dst * max_azi_src);
}

SEXP
decompress_sweep (SEXP rawvec, SEXP extptr, SEXP npsxp) {
  // rawvec: raw vector with entire file contents
  // extptr: external pointer to extmat storage area, already
  // large enough for data in rawvec

  // returns TRUE if decompression succeeded and uncompressed data
  // match size advertised in header; FALSE otherwise

  char *p;
  bscan_info_record *hdr;
  int numSamples;
  int numDecoded;
  int np;
  int aziSize = 0;

  if (TYPEOF(rawvec) != RAWSXP)
    return ScalarLogical(0);

  if (LENGTH(rawvec) < 128)
    return ScalarLogical(0);

  np = INTEGER(npsxp)[0];
  p = (char *) &RAW(rawvec)[0];
  hdr = (bscan_info_record*) p;
  if(hdr->rev_number >= 0x00010100) {
    aziSize = hdr->num_output_lines * sizeof(uint16_t);
  }
  numSamples = hdr->samples_per_line * hdr->num_output_lines;
  if (numSamples > decomp_buff_samples) {
    if (decomp_buff) {
      Free(decomp_buff);
    }
    decomp_buff = Calloc(numSamples * sizeof(sample_t), char);
    decomp_buff_samples = numSamples;
  }
  hdr->compressed_size = LENGTH(rawvec) - 128 - aziSize;
  numDecoded = LZ4_decompress_safe(p + 128 + aziSize, decomp_buff, hdr->compressed_size, hdr->data_size);
#ifdef RADR_DEBUG
  printf("input: %p, output: %p, size:%d, decompsize: %lu, azisize: %d, samples_per_line:%d, num_output_lines: %d, numDecoded: %d sizeof(t_sample): %d\n",
         p+128 + aziSize,
         (char *) decomp_buff,
         hdr->compressed_size,
         hdr->data_size,
	 aziSize,
         hdr->samples_per_line, hdr->num_output_lines, numDecoded, sizeof(t_sample));
#endif
  if (numDecoded != numSamples * sizeof(sample_t))  {
    if (numDecoded == numSamples) {
      // samples in source are 1-byte, so expand them in-place
      uint8_t *src = ((uint8_t *) decomp_buff) + numSamples - 1;
      sample_t *dst = ((sample_t *) decomp_buff) + numSamples - 1;
      int i;
      for (i = 0; i < numSamples; ++i) {
        *dst-- = *src--;
      }
    } else {
      return ScalarLogical(0);
    }
  }
  // copy pulses from decompressed buff to extmat;
  // if this is an older file version without an azimuth table,
  // generate a bogus one with equally-spaced azimuths.
  int max_azi;
  uint16_t *azis;
  if (hdr->rev_number < 0x00010100) {
    max_azi = hdr->num_output_lines;
#ifdef RADR_DEBUG
    printf("creating azimuth table with max_azi=%d\n", max_azi);
#endif
    // no azimuth table
    if (azi_buff_samples < hdr->num_output_lines) {
      if (azi_buff) {
	Free(azi_buff);
      }
      azi_buff = Calloc(hdr->num_output_lines, uint16_t);
      azi_buff_samples = hdr->num_output_lines;
    }
    for (int i = 0; i < hdr->num_output_lines; i++) {
      azi_buff[i] = i;
    }
    azis = azi_buff;
  } else {
    max_azi = MAX_RADARCAM_AZIMUTH;
    azis = &hdr->azimuths[0];
  }
  sample_t *src = ((sample_t *) decomp_buff);
  sample_t *dst = (sample_t *)EXTPTR_PTR(extptr);
  int span = hdr->samples_per_line;
  for (int i=0, j=0; i < np; i++) {
    // advance to best source pulse for this target azimuth
    while (j < hdr->num_output_lines - 1 &&
	   azi_better(azis[j+1], azis[j], max_azi, i, np)) {
      j++;
      src += span;
    }
    memcpy(dst, src, span);
    dst += span;
  }
  return ScalarLogical(1);
}

R_CallMethodDef inradarch_call_methods[]  = {
  MKREF(decompress_sweep, 3),
  {NULL, NULL, 0}
};

void
R_init_inradarch(DllInfo *info)
{
  /* Register routines, allocate resources. */

  R_registerRoutines(info, NULL, inradarch_call_methods, NULL, NULL);
  //  R_useDynamicSymbols(info, FALSE);
}

void
R_unload_inradarch(DllInfo *info)
{
  /* Release resources. */
}
