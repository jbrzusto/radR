/* xenexarch.h - definitions for reading files recorded by Russell Technologies' IntegRadar/WinHorizon */

// types of recorded data formats

typedef enum {
  REC_TYPE_UNKNOWN = 0,
  REC_TYPE_DEFAULT = 1,
  REC_TYPE_RLC_DOS = 2,
  REC_TYPE_RLC_1 = 3,
  REC_TYPE_RLC_2 = 4,
  REC_TYPE_RLC_3 = 5,
  REC_TYPE_RLC_4 = 6,
  
} t_recording_type;


#define NUM_SENSOR_DATA_ITEMS 33
#define NUM_EXTRA_DATA_ITEMS 6

#define XIR3_NOMINAL_SAMPLES_PER_PULSE      512
#define XIR3_ACTUAL_SAMPLES_PER_PULSE       506
#define XIR3_STANDARD_PULSES_PER_SWEEP     1024
#define XIR3_DEFAULT_SAMPLE_RATE       54000000 // rate of the A/D clock
#define XIR3_TICK_RATE                 20000000 // rate of the clock used to indicate pulse start times 

#ifdef Win32
#define MS_STRUCT_PREFIX __attribute__((__packed__, __ms_struct__))
#define MS_STRUCT_POSTFIX __attribute__((__packed__, __ms_struct__))
#else
#define MS_STRUCT_PREFIX
#define MS_STRUCT_POSTFIX
#endif

// flags for validity of data returned by sensors 

//#pragma ms_struct on

typedef struct MS_STRUCT_PREFIX {
  unsigned int Longitude:1;
  unsigned int Latitude:1;
  unsigned int Depth:1;
  unsigned int DistTransToWaterLine:1;
  unsigned int DistTransToKeel:1;
  unsigned int TrueHeading:1;
  unsigned int MagnHeading:1;
  unsigned int MagnVariation:1;
  unsigned int MagDeviation:1;
  unsigned int TrueTrackGround:1;
  unsigned int MagnTrackGround:1;
  unsigned int SpeedWater:1;
  unsigned int SpeedGround:1;
  unsigned int DriftSpeed_Water:1;
  unsigned int DriftSpeedGround:1;
  unsigned int UTC:1;
  unsigned int WaterTemperature:1;
  unsigned int GPSHorizontalDiluation:1;
  unsigned int GPSAntennaAltitude:1;
  unsigned int GPSGeoidalSeparation:1;
  unsigned int GPSAgeOfDifferentialData:1;
  unsigned int GPSNumberOfSatellites:1;
  unsigned int GPSDifferentialReferenceStationID:1;
  unsigned int AntennaPulseLength:1;
  unsigned int AntennaPowerInPort:1;
  unsigned int AntennaPowerOutPort:1;
  unsigned int AntennaMagnetronCurrent:1;
  unsigned int AntennaRMonitor:1;
  unsigned int AntennaState:1;
  unsigned int AntennaAlarm:1;
  unsigned int SpeedWind:1;
  unsigned int WindAngleRel:1;
  unsigned int WindAngleTrueInDeg:1;
#ifdef Win32
  unsigned int _UnusedPadding_: 31;
#endif
} MS_STRUCT_POSTFIX t_sensor_valid;

// data returned by sensors

typedef struct MS_STRUCT_PREFIX {
  t_sensor_valid valid;
  long Longitude;
  long Latitude;
  double Depth;
  double DistTransToWaterLine;
  double DistTransToKeel;
  double TrueHeading;
  double MagnHeading;
  double MagnVariation;
  double MagDeviation;
  double TrueTrackGround;
  double MagnTrackGround;
  /* Removed to make structure have observed size (see below) 
  double Reserved01; // not in documentation, but in CANStar.h
  */ 
  double SpeedWater;
  double SpeedGround;
  double DriftSpeed_Water;
  double DriftSpeedGround;

  double UTC;
  double WaterTemperature;

  double GPSHorizontalDiluation;
  double GPSAntennaAltitude;
  double GPSGeoidalSeparation;
  double GPSAgeOfDifferentialData;

  int GPSNumberOfSatellites;
  int GPSDifferentialReferenceStationID;

  int AntennaPulseLength;
  int AntennaPowerInPort;
  int AntennaPowerOutPort;

  // this is not documented; it brings the structure
  // up to the 240 byte length seen in actual files

  int UnusedPadding; 
  
  double AntennaMagnetronCurrent;
  double AntennaRMonitor;
  int AntennaState;
  int AntennaAlarm;

  double SpeedWind;
  double WindAngleRel;
  double WindAngleTrueInDeg;

} MS_STRUCT_POSTFIX t_sensor_data;

typedef struct MS_STRUCT_PREFIX {
  int signature;
  t_recording_type type;
} MS_STRUCT_POSTFIX t_rec_header;

typedef enum {
  RS_SCAN = 0,
  RS_SENSOR = 1
} t_seg_type;

// the segment information descriptor
typedef struct MS_STRUCT_PREFIX {
  unsigned char type:3;
  unsigned char rangeind:4;
  unsigned char is_compressed:1;
} MS_STRUCT_POSTFIX t_seg_info;

// the extra header for segment types RLC_3 and above
typedef struct MS_STRUCT_PREFIX {
  int freq; // sampling frequency in Hz
  int spp;  // samples per pulse
  int pulses; // pulses per sweep
} MS_STRUCT_POSTFIX t_RLC_3_header;

// the Microsoft filetime structure (as found in cygwin's winbase.h)
typedef struct _FILETIME {
        int dwLowDateTime;
        int dwHighDateTime;
} FILETIME,*PFILETIME,*LPFILETIME;

// the extra header for segment types RLC_4 and above
typedef struct MS_STRUCT_PREFIX {
  int  __attribute__ ((packed)) ext_pulses; // number of pulses per sweep in extended mode
  FILETIME __attribute__ ((packed)) sweep_time; // time at start of sweep, in MS FILETIME format (see below)
  unsigned int ticks; // tick count at start of sweep
  unsigned int unused; // padding?
} MS_STRUCT_POSTFIX t_RLC_4_header;

// the per-pulse header for segment types RLC_4 and above
typedef struct MS_STRUCT_PREFIX {
  unsigned index; // pulse number (from 0 to n-1)
  unsigned ticks; // clock ticks since sweep start
} MS_STRUCT_POSTFIX  t_RLC_4_pulse_header;

// the run-length encoding structure
typedef struct MS_STRUCT_PREFIX {
  unsigned char prefix;  // the byte 0xff indicates a run-length encoding
  unsigned char len;     // 1+len = the length of the run
  unsigned char val;     // the value repeated in the run
} MS_STRUCT_POSTFIX t_RLC_encoded_run;

//#pragma ms_struct off

// MS FILETIMEs are 64 bit counts of 100 nanosecond intervals since Jan 1, 1601
// How many such intervals separate the UTC time origin from the MS FILETIME origin?
// according to R (version 2.10.1, *not* version 2.5.1): 
//
//  > -as.numeric(ISOdatetime(1601, 1, 1, 0, 0, 0, tz="UTC")) * 1e7
//  [1] 1.16444736e+17
//
// Note: this ignores leap seconds.

#define UTC_FILETIME_ORIGIN_DIFF  1.16444736e+17

// interconvert these times (ignoring leap seconds)

#define FILETIME_TO_UTC(FT)  ((((unsigned)((FT).dwLowDateTime) + (((unsigned long long) (FT).dwHighDateTime) << 32)) - UTC_FILETIME_ORIGIN_DIFF) / 1E7)
#define UTC_TO_FILETIME(X, FT) ({* (unsigned long long *) &FT = (unsigned long long) (((X) * 1e7) + UTC_FILETIME_ORIGIN_DIFF);})
