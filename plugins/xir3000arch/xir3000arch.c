/* xir3000arch.c -  for reading files recorded by Russell Technologies Inc. Software */

#include "radRmodule.h"
#include "xir3000arch.h"


void
process_REC_buffer (unsigned char *p, unsigned int len, double *si, t_sample *dat, int ri)
{
  // process an RTI .REC file, saving sensor and/or scan data
  //
  // p:   pointer to the first byte of the .REC file buffer
  // len: number of bytes in the .REC file buffer
  // si:  pointer to a double buffer for sensor and metadata (or NULL if this is not to be extracted)
  // dat: pointer to a buffer for raw radar data (or NULL if this is not to be extracted)
  // ri:  range index to which data will be resampled, if collecting data; must be at least
  //      the maximum range index for any pulse in the sweep

  unsigned char *ep = p + len;

  int i, j;
  int samp_rate = XIR3_DEFAULT_SAMPLE_RATE;
  double utc_sweeptime = NA_REAL;
  unsigned int first_pulse_ticks = 0, last_pulse_ticks = 0;

  int max_rangeind = 0, min_rangeind = 8;
  int np = XIR3_STANDARD_PULSES_PER_SWEEP;
  int spp = XIR3_NOMINAL_SAMPLES_PER_PULSE;
  int np_found = 0;
  
  t_recording_type rec_type;
  int shift; // how much do sums of data from the current pulse need to be right shifted 

  // macro to return p cats as a pointer to a particular type, and post-increment
  // p to advance it past the item of that type.

#define GET_ITEM(_TYPE_) ({_TYPE_ *__x = ((_TYPE_ *)p); p += sizeof(_TYPE_); __x;})

  // grab the recording type from the main header; signature was checked at R level
  rec_type = GET_ITEM(t_rec_header) -> type;

  // read file headers
  if (rec_type >= REC_TYPE_RLC_3) {
    t_RLC_3_header *p3 = GET_ITEM(t_RLC_3_header);
    samp_rate = p3->freq;
    spp       = p3->spp;
    np        = p3->pulses;
    if (rec_type >= REC_TYPE_RLC_4) {
      t_RLC_4_header *p4 = GET_ITEM(t_RLC_4_header);
      np            = p4->ext_pulses;
      //      printf("Sweep time: 0x%x  0x%x\n", p4->sweep_time.dwLowDateTime, p4->sweep_time.dwHighDateTime);
      utc_sweeptime = FILETIME_TO_UTC(p4->sweep_time);
      first_pulse_ticks = p4->ticks;
    } else {
      samp_rate = samp_rate * 100.0;  // weird - from data recorded by M. D'entremont, it appears
				      // that a sampling rate of X Hz was recorded as (X/100) Hz in REC_TYPE_RLC_3
      GET_ITEM(t_RLC_4_header); // this is allocated space in the file, but not filled in
    }
  }
  // read segments
  while (p < ep) {
    t_seg_info *segi = GET_ITEM(t_seg_info);
    if (segi->type == RS_SCAN) {
      if (rec_type >= REC_TYPE_RLC_4) {
	t_RLC_4_pulse_header *ph4 = GET_ITEM(t_RLC_4_pulse_header);
	last_pulse_ticks = ph4->ticks;
	if (np_found != ph4->index)
	  printf("xir3000arch: pulse count mis-match: header says %d, count says %d\n", ph4->index, np_found);
      }
      
      if (segi->rangeind > max_rangeind)
	max_rangeind = segi->rangeind;
      else if (segi->rangeind < min_rangeind)
	min_rangeind = segi->rangeind;

      if (dat) {
	// grab data
	if (segi->is_compressed) {
	  for (j = spp; j > 0; /**/) {
	    t_RLC_encoded_run *run;
	    if (*p == 0xff) {
	      run = GET_ITEM(t_RLC_encoded_run);
	      j -= 1 + run->len;
	      for (i = 1 + run->len; i > 0; --i)
		*dat++ = run->val;
	    } else {
	      --j;
	      *dat++ = *p++;
	    }
	  }
	} else {
	  for (i = spp; i > 0; --i)
	    *dat++ = *p++;
	}
	shift = ri - segi->rangeind;
	if (shift > 0) {
	  // This pulse has a smaller range than the final data.
	  // Luckily, the ranges are all related by factors of a power of 2,
	  // so we can do quick arithmetic to coarsen the data
	  
	  // copy over data from either the raw file or the decompression buffer,
	  // coarsening it by averaging across cells
	  
	  t_sample *tmp;         // temporary pointer to the current pulse's data
	  unsigned int cellsum;  // sums of groups of consecutive data from the current pulse
	  unsigned int spc;      // samples per cell
	  unsigned int roundoff; // for rounding

	  spc = 1 << shift;  // the number of samples to be averaged into a single one
	  roundoff = (spc / 2) - 1;

	  dat -= spp;
	  tmp = dat;
	  
	  for (j = spp / spc; j > 0; --j) {
	    cellsum = 0;
	    for (i = 0; i < spc; ++i)
	      cellsum += *tmp++;
	    *dat++ = (cellsum + roundoff) >> shift;
	  }
	  // fill the remainder of the return value for this pulse with zeros
	  memset (dat, 0, sizeof(t_sample) * (spp - (spp >> shift)));
	  // move to start of storage fornext pulse
	  dat = tmp;
	}
      } else {
	// skip data
	if (segi->is_compressed) {
	  for (j = spp; j > 0; /**/) {
	    t_RLC_encoded_run *run;
	    if (*p == 0xff) {
	      run = GET_ITEM(t_RLC_encoded_run);
	      j -= 1 + run->len;
	    } else {
	      --j;
	      ++p;
	    }
	  }
	} else {
	  p += spp;
	}
      }
      ++np_found;
    } else if (segi->type == RS_SENSOR) {
      t_sensor_data *dp = GET_ITEM(t_sensor_data);
      if (si) {
	i = 0;
#define COPY_IF_VALID(X) if (dp->valid.X) si[i] = dp->X; ++i;
	COPY_IF_VALID(Longitude);
	COPY_IF_VALID(Latitude);
	COPY_IF_VALID(Depth);
	COPY_IF_VALID(DistTransToWaterLine);
	COPY_IF_VALID(DistTransToKeel);
	COPY_IF_VALID(TrueHeading);
	COPY_IF_VALID(MagnHeading);
	COPY_IF_VALID(MagnVariation);
	COPY_IF_VALID(MagDeviation);
	COPY_IF_VALID(TrueTrackGround);
	COPY_IF_VALID(MagnTrackGround);
	COPY_IF_VALID(SpeedWater);
	COPY_IF_VALID(SpeedGround);
	COPY_IF_VALID(DriftSpeed_Water);
	COPY_IF_VALID(DriftSpeedGround);
	COPY_IF_VALID(UTC);
	COPY_IF_VALID(WaterTemperature);
	COPY_IF_VALID(GPSHorizontalDiluation);
	COPY_IF_VALID(GPSAntennaAltitude);
	COPY_IF_VALID(GPSGeoidalSeparation);
	COPY_IF_VALID(GPSAgeOfDifferentialData);
	COPY_IF_VALID(GPSNumberOfSatellites);
	COPY_IF_VALID(GPSDifferentialReferenceStationID);
	COPY_IF_VALID(AntennaPulseLength);
	COPY_IF_VALID(AntennaPowerInPort);
	COPY_IF_VALID(AntennaPowerOutPort);
	COPY_IF_VALID(AntennaMagnetronCurrent);
	COPY_IF_VALID(AntennaRMonitor);
	COPY_IF_VALID(AntennaState);
	COPY_IF_VALID(AntennaAlarm);
	COPY_IF_VALID(SpeedWind);
	COPY_IF_VALID(WindAngleRel);
	COPY_IF_VALID(WindAngleTrueInDeg);
      }
    } else {
      printf ("xir3000arch: unknown segment type %d at offset %d\n", segi->type, len - (ep - p));
    }
  }
  if (si) {
    if (np_found != np)
      printf("xir3000arch: pulse count mismatch: format says %d, file has %d\n", np, np_found);
    
    si[NUM_SENSOR_DATA_ITEMS    ] = max_rangeind;
    si[NUM_SENSOR_DATA_ITEMS + 1] = np;
    si[NUM_SENSOR_DATA_ITEMS + 2] = spp;
    si[NUM_SENSOR_DATA_ITEMS + 3] = utc_sweeptime;
    
    // for REC_TYPE_RLC_4 and above, we can get a precise duration by looking at ticks, which appear
    // to be from a 20 MHz clock; (in milliseconds)
    if (rec_type >= REC_TYPE_RLC_4)
      si[NUM_SENSOR_DATA_ITEMS + 4] = (last_pulse_ticks - first_pulse_ticks) * (np / (np - 1.0)) / XIR3_TICK_RATE * 1000;
    si[NUM_SENSOR_DATA_ITEMS + 5] = VELOCITY_OF_LIGHT / (2.0 * samp_rate / (1 << max_rangeind));
  }
}
  
SEXP
get_scan_info (SEXP rawfiledat) {
  // return a real vector of scan info metadata
  // from a raw .REC file

  // scan the file for all SENSORDATA segments,
  // and accumulate those items which are valid, 
  // so that the last valid version of an item is the 
  // one returned.  NAs are returned in slots for which
  // no data were found.

  // We also return extra fields (depending on the REC_TYPE):
  //
  //    RangeIndex : the maximum range index for any pulse in this scan
  //    Pulses: the number of pulses found in this scan
  //    Samples: the number of samples per pulse
  //    TimeStamp: the UTC of the first pulse in the scan
  //    Duration: the duration of the scan, in milliseconds
  //    SampleDist: the size of range cell, in metres

  int i;
  SEXP rv;
  double *rvp;
  
  // allocate return vector of doubles
 
  rv = allocVector(REALSXP, NUM_SENSOR_DATA_ITEMS + NUM_EXTRA_DATA_ITEMS);
  rvp = REAL(rv);

  // mark info items as NA

  for (i = 0; i < NUM_SENSOR_DATA_ITEMS + NUM_EXTRA_DATA_ITEMS; ++i) 
    rvp[i] = NA_REAL;

  process_REC_buffer ((unsigned char *) RAW(rawfiledat), LENGTH(rawfiledat), rvp, NULL, 0);
  return rv;
}
  
SEXP
get_scan_data (SEXP rawfiledat, SEXP maxrangeind, SEXP extmat) {
  // fill the extmat with raw scan data from a RAW vector file.
  // from a raw .REC file

  // rawfiledat: a RAWSXP holding the complete .REC file contents
  // maxrangeind: an INTSXP holding the maximum range index found in the file,
  //              as returned by get_scan_info
  // extmat: an EXTPTR pointer to an extmat to hold scan data.  It must already
  //         have been redimensioned to sufficient size.

  // In case the range changed during the scan, we return a matrix of maximum range.
  // So data from pulses where the range was smaller are averaged and placed appropriately.

  process_REC_buffer ((unsigned char *) RAW(rawfiledat), LENGTH(rawfiledat), NULL, (t_sample *) (SEXP_TO_EXTMAT(extmat)->ptr), INTEGER(AS_INTEGER(maxrangeind))[0]);

  RETBOOL(TRUE);
}
    
R_CallMethodDef xir3000arch_call_methods[] = {
  MKREF(get_scan_info, 1),
  MKREF(get_scan_data, 3),
  {NULL, NULL, 0}
};

void
R_init_xir3000arch(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, xir3000arch_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

void
R_unload_xir3000arch(DllInfo *info)
{
  /* Release resources. */
}

