/*

decompressInrad.c - extract gated data from an lz4-compressed inrad image

(C) 2015, John Brzustowski
Licence: GPL V2+

*/

#include <lz4.h>
#include <stdint.h>
#include "radRmodule.h"

// from inrad radR plugin

typedef struct bscan_info_record_t
{
	uint32_t header_size;	      // bytes
	uint32_t rev_number;
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

} bscan_info_record;

typedef uint16_t sample_t;

SEXP
decompress_sweep (SEXP rawvec, SEXP extptr) {
  // rawvec: raw vector with entire file contents
  // extptr: external pointer to extmat storage area, already
  // large enough for data in rawvec

  // returns TRUE if decompression succeeded and uncompressed data
  // match size advertised in header; FALSE otherwise

  char *p;
  bscan_info_record *hdr;
  int numSamples;
  int numDecoded;

  if (TYPEOF(rawvec) != RAWSXP)
    return ScalarLogical(0);

  if (LENGTH(rawvec) < 128)
    return ScalarLogical(0);

  p = (char *) &RAW(rawvec)[0];
  hdr = (bscan_info_record*) p;

  numSamples = hdr->samples_per_line * hdr->num_output_lines;
  hdr->compressed_size = LENGTH(rawvec) - 128;
  numDecoded = LZ4_decompress_safe(p + 128, (char *) EXTPTR_PTR(extptr), hdr->compressed_size, hdr->data_size);
#ifdef RADR_DEBUG
  printf("input: %p, output: %p, size:%d, decompsize: %lu, samples_per_line:%d, num_output_lines: %d, numDecoded: %d\n",
         p+128,
         (char *) EXTPTR_PTR(extptr),
         hdr->compressed_size,
         hdr->data_size,
         hdr->samples_per_line, hdr->num_output_lines, numDecoded);
#endif
  if (numDecoded != numSamples * sizeof(sample_t))  {
    if (numDecoded == numSamples) {
      // samples in source are 1-byte, so expand them in-place
      uint8_t *src = ((uint8_t *) EXTPTR_PTR(extptr)) + numSamples - 1;
      sample_t *dst = ((sample_t *) EXTPTR_PTR(extptr)) + numSamples - 1;
      int i;
      for (i = 0; i < numSamples; ++i) {
        *dst-- = *src--;
      }
    } else {
      return ScalarLogical(0);
    }
  }

  return ScalarLogical(1);
}

R_CallMethodDef inradarch_call_methods[]  = {
  MKREF(decompress_sweep, 2),
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
