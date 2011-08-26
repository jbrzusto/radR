/*********************************************************************

	IDLTYPES.h	-	Common Declarations for SeaScan clients

	(c) 1995-2003 Sigma Engineering Limited.

*********************************************************************/

#ifndef _IDLTYPES
#define _IDLTYPES

// *****************************************************************
//  Radar A/D Module

typedef struct _PROCESSING_PARAMETERS { 
  BOOL ScanAveragingEnabled; 
  USHORT ScansToAverage; // number of scans to average
  USHORT Reserved1; 
  BOOL MotionCompEnabled; 
  BOOL ScanConversionEnabled; 
  BOOL CFAREnabled; 
  USHORT CFARWindowLength; // window length for CFAR
  SHORT CFAROffset; // offset for CFAR
  USHORT CFARRank; // rank value to use (from 0 to 100 percent)
  USHORT Reserved2; 
  BOOL PulseFilterEnabled; 
  USHORT FTCIndex; // index of FTC filter; 0 to disable
} MS_STRUCT  PROCESSING_PARAMETERS; 


// Structure returned by GetADStatus.

typedef struct _A2D_STATUS { 
  USHORT Dbg; // The last error condition processed into this structure
  USHORT Sequence; // message count
  USHORT Rotation; // Antenna rotation count
  USHORT Pulse; // Pulse period count
  USHORT Hdg; // Heading 0 to 8192
  USHORT Quadrant;   // quadrant number 0(NE),1(SE),2(SW),3(NW)
  USHORT Hardware; // hardware ID
  USHORT Month; // 1-12
  USHORT Day; // 1-31
  USHORT Year; // 1995
  BOOL Fatal; 
  BOOL Valid; 
  BOOL Configured; 
  BOOL Overrun; 
  BOOL Fault; 
  BOOL Synchro; 
  BOOL Online; 
  BOOL Running; 
  BOOL Started; 
  USHORT ErrorCode; 
  USHORT ErrorParm; 
} MS_STRUCT  A2D_STATUS; 


typedef struct _SEASCAN_REALTIME {
  USHORT Year; 
  USHORT Month; 
  USHORT DayOfWeek; 
  USHORT Day; 
  USHORT Hour; 
  USHORT Minute; 
  USHORT Second; 
  USHORT Milliseconds; 
} MS_STRUCT  SEASCAN_REALTIME; 

typedef struct _TIME_POS_INFO { 
  SEASCAN_REALTIME Time; 
  float Latitude; // in degrees (45° 25” 15’ = 45.4208333)
  float Longitude; // in degrees
} MS_STRUCT  TIME_POS_INFO; 

typedef struct _DATA_HEADER { 
 // time and position information
  TIME_POS_INFO TimePosStamp; 
  time_t   GPSTimeStamp;
  float Heading; // in degrees (0 to 360)
  float SpeedFromGPS; // in knots
  float CourseFromGPS; // in degrees (0 to 360)
  float SpeedFromLOG; // in knots
  float CourseFromLOG; // in degrees (0 to 360)
  float SpeedFromMAN; // in knots
  float CourseFromMAN; // in degrees (0 to 360)
 // image information
  ULONG Lines; // Lines*SamplesPerLine gives
  ULONG SamplesPerLine; // size of data to transmit/display
  BOOL SingleQuadrant; // FALSE if data is a full scan
  ULONG QuadrantNumber; // ignore if SingleQuadrant is FALSE
  BOOL ScanConverted; // FALSE if data is B-scan format
  float StartBearing;  // bearing of start of data, in degrees
  float AngleCoverage; // angular coverage of data, in degrees
  ULONG StartRange; // range of start of data, in meters
  ULONG RangeCoverage;  // range coverage of data, in meters
  int originX; // screen center if scan converted
  int originY; // screen center if scan converted
 // A/D information
  USHORT PRF;   // to nearest Hz, calculated from radar
  USHORT AntennaSpeed; // to nearest RPM, calculated from radar
  BOOL NorthAligned; // TRUE if data is north aligned
  float RangePerSample; // in meters
  USHORT SelectedModeNdx; // desired mode index 
  USHORT ModeNdx; // actual mode index
  USHORT SelectedPlenNdx;// desired pulse length index 
  USHORT PlenNdx; // actual pulse length index
  USHORT PulseLength; // to nearest ns, from config file
 // processing information
  MS_STRUCT_FILLER(1, 2);
  BOOL CFARed; 
  BOOL PulseFiltered; 
  BOOL MotionCompensated;
  USHORT ScansAveraged;
  USHORT FTCNdx; // FTC index, 0 if disabled
  USHORT State; // state of data (OK, TRANSITION, etc.)
  BYTE XpolImage; // 0=normal, otherwise cross-polarized
  BYTE reserved; 
} MS_STRUCT  DATA_HEADER; 

typedef struct _MOTION_OFFSETS {
  int x; 
  int y; 
} MS_STRUCT  MOTION_OFFSETS; 



// *****************************************************************
//  Tape Module


typedef struct _TAPE_CONTENTS { 
  int TotalImages; // total number of images on tape
  int NumSegments; // number of data blocks
  time_t StartTime[MAX_DATA_BLOCKS]; // time of first image in this block
  time_t StopTime[MAX_DATA_BLOCKS]; // time of last image in this block
  int NumImages[MAX_DATA_BLOCKS]; // number of images in this block
  char TapeLabel[80]; // current tape label
} MS_STRUCT  TAPE_CONTENTS; 

// Tape status

typedef struct _TAPE_STATUS { 
  BOOL device_present; // tape drive installed on system
  BOOL media_present; // media present in device
  BOOL media_WP; // media is write protected
  BOOL media_initialized; // media is ready for access
  BOOL device_locked; // media is locked in device
  USHORT access_mode; // current access mode
  USHORT startup_mode; // startup access mode
  USHORT data_mode; // current data mode
  USHORT overall_status; // overall status
  int tape_remaining; // remaining tape, in bytes
  char TapeLabel[80]; // current tape label
} MS_STRUCT  TAPE_STATUS; 



// *****************************************************************
//  GPS Module

typedef struct _GPS_STATUS { 
  USHORT Sentence;
  USHORT Baud;
  BYTE Port; 
  BYTE padding[3]; // Added Dec 1, 1995
  float Long; 
  float Lat; 
  SEASCAN_REALTIME TimeStamp;
} MS_STRUCT  GPS_STATUS,*PGPS_STATUS; 


// *****************************************************************
//  AGC Module

// AGC parameters structure

typedef struct _AGC_PARAMETERS {
  BOOL AutomaticMode; 
  int NewGain;    // the new desired gain (approx for automatic mode)
  int NewOffset; // the new desired offset (approx for automatic mode)
  int Reserved[14]; 
  UCHAR MaxDeltaGain; // the maximum variation from NewGain (auto mode)
  UCHAR MaxDeltaOffset; // the maximum variation from NewOffset (auto mode)
  UCHAR DynamicRange; // the maximum allowable dynamic range (auto mode)
  UCHAR NoiseThreshold; // noise will be suppressed below this value (auto mode)
  UCHAR BloomThreshold; // blooms will be saturated above this value (auto mode)
  UCHAR BloomStart; // the range (in pixels) for sampling bright targets
  UCHAR BloomLength; // the range extent (in pixels) for sampling bright targets
  UCHAR NoiseLength; // the range extent (measured inward from 512) for sampling noise
} MS_STRUCT  AGC_PARAMETERS; 

// AGC status structure:

typedef struct _AGC_STATUS { 
  BOOL Enabled; 
  BOOL Active; 
  int CurrentGain; 
  int CurrentOffset; 
} MS_STRUCT  AGC_STATUS; 


// *****************************************************************
//  Plot Extraction Module

typedef struct _EXTRACTION_STATUS {
  BOOL Enabled; 
  BOOL Active; 
  int NumTargets; 
} MS_STRUCT  EXTRACTION_STATUS; 


typedef struct _EXTRACTION_PARAMETERS { 
  USHORT Threshold; // data must be above this value to be a potential plot
  USHORT  MinBlobSize; // minimum plot size - smaller ones are rejected
  USHORT MaxBlobSize; // maximum plot size - larger ones are rejected
  USHORT ScansToAverage; // number of scans to average
  BOOL ScanAveragingEnabled; 
  BOOL ClutterMapEnabled; 
  USHORT ClutterMapTimeConstant; // used to compute the filter delay and gain values
  USHORT CFARWindowLength; // window length for CFAR
  SHORT CFAROffset; // offset for CFAR
  USHORT CFARRank; // rank value to use (from 0 to 100 percent)
  BOOL MaskingEnabled; 
  int MaxNumOfPlots; 
} MS_STRUCT  EXTRACTION_PARAMETERS; 

typedef struct _PLOT_EXTRACTION_SETTINGS {
  EXTRACTION_PARAMETERS Parameters;
  BOOL SettingsHaveChanged; // set when a new command has been received
  BOOL ExtractionEnabled;
  BOOL NewMaskOnServer; // the server has a new mask, so fetch it
} MS_STRUCT  PLOT_EXTRACTION_SETTINGS; 


typedef struct _PLOT { 
  float Range;     // range of center of intensity, in meters
  float Bearing; // bearing of center of intensity, in degrees
  BYTE MajorAxisRun; // 0 to 255
  BYTE MinorAxisRise; // 0 to 255
  USHORT MaximumIntensity; // 0 to 4095
  USHORT Orientation;
  USHORT Reserved2;
} MS_STRUCT  PLOT; 

typedef struct _PLOT_HDR { 
  DATA_HEADER DataHeader; 
  USHORT DataThreshold;
  USHORT NumTargets;
  BOOL Masked; 
  BOOL ClutterMapUsed; 
  USHORT MaskMode;
  MS_STRUCT_FILLER(2, 2);
  int CfarOffset;
  int Reserved[2];
} MS_STRUCT  PLOT_HDR; 

typedef struct _PLOTS { 
  union { 
    PLOT_HDR Header; 
    struct { 
      DATA_HEADER DataHeader; 
      USHORT DataThreshold;
      USHORT NumTargets;
      BOOL Masked; 
      BOOL ClutterMapUsed; 
      USHORT MaskMode;
  MS_STRUCT_FILLER(3, 2);
      int CfarOffset;
      int Reserved[2];
    }; 
  }; 
  PLOT PlotArray[MAX_PLOTS]; 
} MS_STRUCT  PLOTS; 

typedef struct _HISTOGRAM { 
  DATA_HEADER Header; 
  BOOL Masked; 
  int DataBin[4096]; 
} MS_STRUCT  HISTOGRAM; 

typedef struct _BLANK_AREA { 
  USHORT startRange; // start range in meters
  USHORT startBearing; // start bearing in degrees
  USHORT stopRange; // stop range in meters
  USHORT stopBearing; // stop bearing in degrees
} MS_STRUCT  BLANK_AREA; 

// Added for Raytheon AIT support; this will provide 
// the Raytheon tracker more control over the plot extractor
typedef struct _DIRECTION_SETTINGS
{ 
  USHORT Threshold; // data must be above this value to be a potential plot
  USHORT CFARWindowLength; // window length for CFAR
  USHORT CFARRank; // rank value to use (from 10 to 90 percent)
  SHORT CFAROffset; // offset for CFAR
} MS_STRUCT  DIRECTION_SETTINGS; 

typedef struct _EXTENDED_EXTRACTION_PARAMETERS
{ 
  DIRECTION_SETTINGS UpWind; // settings for up-wind
  DIRECTION_SETTINGS CrossWind; // settings for cross-wind
  DIRECTION_SETTINGS DownWind; // settings for down-wind
  USHORT WindDirection; // true direction of wind
  USHORT UpWindSector; // 10-350 
  USHORT DownWindSector; // 10-350 
  USHORT enabled; // 0=disabled, else enabled
} MS_STRUCT  EXTENDED_EXTRACTION_PARAMETERS;

// Added October 25, 2002 by Andrew Menchions
// Add support for new Plot extraction mask format (Klein)
typedef struct _MASK_COORD 
{ 
  float Range; 
  float Bearing; 
} MS_STRUCT  MASK_COORD; 



// *****************************************************************
//  Tracker Module Module


typedef struct _TRACK 
{ 
  unsigned TargetID; // target id
  unsigned char OriginatorID; // zero indicates target was automatically acquired
  unsigned char ScansSinceConfirmed; // indicates scnas since last confirmation
  char TrackStatus; // target status.. see IDLCONST.H for possible values
  char UpdateStatus; // update status.. see IDLCONST.H for possible values
  float mRange; // range in meters
  float degBearing; // bearing in degrees
  float degCourse; // course in degrees
  float knotsSpeed; // speed in knots
  float mCPA; // closest point of approach in meters
  float minTCPA; // time to closest point of approach in minutes
} MS_STRUCT  TRACK; 

typedef struct _TRACK_HDR { 
  DATA_HEADER DataHeader; 
  USHORT NumTracks;
  USHORT reserved;
} MS_STRUCT  TRACK_HDR; 

typedef struct _TRACKS { 
  TRACK_HDR Header; 
  TRACK TrackArray[MAX_TRACKS]; 
} MS_STRUCT  TRACKS; 

typedef struct _SRVTRACK_V2{ 
  unsigned  TrackId; 
  float Range;     // range of center of intensity, in meters
  float Bearing; // bearing of center of intensity, in degrees
  BYTE MajorAxisRun; // 0 to 255
  BYTE MinorAxisRise; // 0 to 255
  USHORT MaximumIntensity; // 0 to 4095
  USHORT Orientation; // 0 to 360
  USHORT PlotSize;
  float  rmsXY;  
  float rmsV; 
} MS_STRUCT  SRVTRACK_V2; 

typedef struct SRVTRACKS_V2 { 
  TRACKS Tracks; 
  SRVTRACK_V2 TracksV2[MAX_TRACKS];
} MS_STRUCT  SRVTRACKS_V2; 

// The structure of the Extended Track is such that the first 32 bytes (up to minTCPA) are a TRACK and the 
// remaining bytes are a SRVTRACK_V2. 
typedef struct _EXTENDED_TRACK{ 
  unsigned  TargetID; // target id
  unsigned char OriginatorID; // zero indicates target was automatically acquired
  unsigned char ScansSinceConfirmed; // indicates scans since last confirmation
  char TrackStatus; // target status
  char UpdateStatus; // update status
  float mRange; // range in meters
  float degBearing; // bearing in degrees
  float degCourse; // course in degrees
  float knotsSpeed; // speed in knots
  float mCPA; // closest point of approach in meters
  float minTCPA; // time to closest point of approach in minutes
  unsigned  reserved1; 
  float reserved2; 
  float reserved3; 
  BYTE MajorAxisRun; // 0 to 255
  BYTE MinorAxisRise; // 0 to 255
  USHORT MaximumIntensity; // 0 to 4095
  USHORT Orientation; // 0 to 360
  USHORT PlotSize;
  float rmsXY;  
  float rmsV; 
} MS_STRUCT  EXTENDED_TRACK; 

typedef struct EXTENDED_TRACKS {
  TRACK_HDR Header; 
  EXTENDED_TRACK Tracks[MAX_TRACKS];
} MS_STRUCT  EXTENDED_TRACKS; 

// *****************************************************************
//  Server Setup Module

typedef struct _SERVER_STATUS { 
  char SiteName[16]; // From configuration file
  BOOL AtoDActive; 
  BOOL TapeRecording; 
  BOOL TapePlayback; 
  BOOL PlotExtractionActive; 
  BOOL AGCActive; 
  BOOL RPCEnabled; 
  BOOL SignalProcessingActive; 
} MS_STRUCT  SERVER_STATUS; 


typedef struct _CFG_INFO { 
  USHORT NumOfValidCfgs; 
  char CfgNames[10][16]; 
  USHORT DefaultCfg; 
  SERVER_STATUS Status; 
} MS_STRUCT  CFG_INFO; 


typedef struct _RPC_COMMAND { 
  USHORT Command; 
  USHORT DestServer; 
  BOOL Success; 
} MS_STRUCT  RPC_COMMAND; 

typedef struct _RPC_COMMAND_STATUS {
  RPC_COMMAND commands[10]; 
} MS_STRUCT  RPC_COMMAND_STATUS; 


// Header for TCP/IP data

typedef struct _MessageHeader { 
  ULONG sequenceNumber; // Nice to have...
  ULONG messageID;    // Request, Command, Response or Notification ID
  ULONG size; // number of data bytes transmitted (header plus data)
  ULONG fullID; 
  ULONG returnValue; 
  long arg1; 
  long arg2; 
  long arg3; 
} MS_STRUCT  MessageHeader; 

typedef struct _DATA_RESPONSE { 
  MessageHeader msgHeader; 
  DATA_HEADER dataHeader; 
} MS_STRUCT  DATA_RESPONSE; 


// *****************************************************************
//  Client-server arbitration Module

typedef struct _CLIENT_ID { 
  union { 
    ULONG FullID; 
    struct { 
      USHORT ServerAssignedID; // shared by multiple threads of a client
      USHORT ClientThreadID; // used to identify a specific channel
    }; 
  }; 
} MS_STRUCT  CLIENT_ID; 


typedef struct _CLIENT_REQUEST {
  CLIENT_ID CurrentClient; 
  ULONG ProcessID; 
  char ComputerName[MAX_COMPUTER_NAME_LENGTH]; 
  time_t  LastAccessTime; 
  union { 
    BOOL RpcRequests; 
    struct { 
      BOOL RpcDataRequest : 1;
      BOOL RpcPlotRequest : 1;
      BOOL RpcAscopeRequest : 1;
      BOOL RpcQuadDataRequest : 1; 
      BOOL RpcTrackRequest : 1;
      BOOL RpcRequestTypeF : 1;
      BOOL RpcRequestTypeG : 1;
      BOOL RpcRequestTypeH : 1;
    }; 
  }; 

  union { 
    BOOL NewForRpc; 
    struct { 
      BOOL DataNewForRpc : 1;
      BOOL PlotsNewForRpc : 1;
      BOOL AscopeNewForRpc : 1;
      BOOL QuadDataNewForRpc : 1;
      BOOL TracksNewForRpc : 1;
      BOOL TypeFNewForRpc : 1;
      BOOL TypeGNewForRpc : 1;
      BOOL TypeHNewForRpc : 1;
    }; 
  }; 
  HANDLE hevData;   // assumes all three channels are available
 // simultaneously; if not, add additional handles
  HANDLE hevAscopeData; 
  HANDLE hevPlotData; // separate handle required because plot data is
 // not synchronous with other data
  HANDLE hevQuadData; 
  HANDLE hevTrackData; 
  HANDLE hevTypeF; 
  HANDLE hevTypeG; 
  HANDLE hevTypeH; 
  long lastQuadrantRetrieved; 
} MS_STRUCT  CLIENT_REQUEST; 


typedef struct _STC_PARAMETERS {
  BOOL Enabled; 
  float MaximumAppliedRange; 
  float MaximumAttenuation; 
  USHORT Reserved1; 
  USHORT Reserved2; 
  USHORT Reserved3; 
  USHORT Reserved4; 
} MS_STRUCT  STC_PARAMETERS; 



// New structures to support client updates of time and position


typedef struct _SEASCAN_GPS_FIX {
  float Latitude;  // in degrees (45° 25” 15’ N = 45.4208333)
  float Longitude; // in degrees
  USHORT Hour; // 0 to 23
  USHORT Minute; // 0 to 59
  USHORT Second; // 0 to 59
  USHORT Milliseconds; // 0 to 999
} MS_STRUCT  SEASCAN_GPS_FIX; 


typedef struct _SEASCAN_TIME_FIX {
  USHORT Year; // 1 to 10000
  USHORT Month; // 1 to 12
  USHORT Day; // 1 to 31
  USHORT Hour; // 0 to 23
  USHORT Minute; // 0 to 59
  USHORT Second; // 0 to 59
  USHORT Milliseconds; // 0 to 999
  USHORT Reserved; 
} MS_STRUCT  SEASCAN_TIME_FIX; 

typedef struct _TERMA_REPORT { 
  time_t LastUpdate;   // time that this structure was last updated
  int LinkGood; // 0 = error, 1 = OK
  int MainsOn; // 0 = OFF, 1 = ON (fn 1)
  int TxOn; // 0 = OFF, 1 = ON, 2 = WAIT (fn 2)
  int ActiveRxTx; // 1 or 2 (fn 3)
  int MotorNum; // 1 = low, 2 = high (fn 4)
  int ShortPulseOn; // 0 = OFF, 1 = ON (fn 5)
  int LogVideoOn; // 0 = OFF, 1 = ON (fn 6)
  int VideoProcessingOn; // 0 = OFF, 1 = ON (fn 7)
  int FTCOn; // 0 = OFF, 1 = ON (fn 8)
  int STCMode; // 1 = Manual, 2 = Clutter map (fn 9)
  int Polarization; // 1 = Linear, 2 = Circular (fn 10)
  int PRF;     // 400 to 4400 (fn 17)
  int SectorTxOn; // 0 = OFF, 1 = ON (fn 18)
  int STCOffset; // 1 to 5 (fn 20)
  int Stagger; // 0 = OFF, 1 = 2%, 2 = 4%, 3 = 8% (fn 27)
  int ScannerOn; // 0 = OFF, 1 = ON (fn 38)
  int DiversityMode;   // 0 = Add / And, 1 = Add / Add (fn 41)
  int DiversityLogOn; // 0 = OFF, 1 = ON (fn 42)
  int DiversityShiftRange; // 0 to 15 --> (N+1)*M; short M=2, medium M=4 (fn 43)
  int DiversitySweepDelay; // 0 to 31 --> (N*2) + 1 (fn 44)
  int DiversityCorrelationLaw; // 0 to 36, from 0 of 1 to 8 of 8 (fn 46)
  int STCCurveType; // 0 to 4 (fn 50)
  int MainBangSupressionOn; // 0 = OFF, 1 = ON (fn 51)
  int CFARWidth; // 1, 2, 4 or 8 pulses (fn 36)
  int CFARLength; // 4, 8, 16, 32 or 64 samples (at 80 MHz) (fn 37)
  int Tune;        // 0 to 100, in percent (fn 12)
  int VideoOffsetLog; // 0 to 100, in percent (fn 21)
  int CorrelationLawLog; // 0 to 36, from 0 of 1 to 8 of 8 (fn 22)
  int DiversityVideoOffset; // 0 to 100, in percent (fn 45)
  int DiversityWindowLength; // 0 to 15 (fn 48)
  int DiversityWindowOffset; // 0 to 15 (fn 49)
  int SectorBearing; // 0 to 359, in degrees (fig 4)
  int SectorWidth; // 10 to 350, in degrees (fig 5)
  BYTE BarStatus[12]; // error codes 0 = OK 2 = ERROR
} MS_STRUCT TERMA_REPORT; 

typedef struct _CLUTTER_MAP_PARAMETERS {
  BOOL enableClutterMap; // if set, enable the clutter map
  BOOL immediateRefresh; // if set, update the clutter map immediately
  BOOL useCFAR;  // it set, include CFAR in the clutter map processing
  ULONG scansToProcess; // number of scans used to generate the clutter map
  ULONG automaticRefreshInterval; // automatic update period, in minutes
  ULONG reserved[4]; 
} MS_STRUCT  CLUTTER_MAP_PARAMETERS; 

typedef struct _ADV_PLOT_SETTINGS {
  BOOL enableDualMode; // if set, enable plot extraction on the raw data
  float azimuthTolerance; // in degrees, how close a raw plot must be to one from the processed channel to be deleted
  float rangeTolerance; // in samples, how close a raw plot must be to one from the processed channel to be deleted
  ULONG threshold;    // threshold for plot extraction on the raw data
  ULONG dynamicRange; // in dB, used to calculate the real radar signal level
  BOOL scanAveragingEnabled; 
  USHORT scansToAverage; // number of scans to average for the second mode (no longer just raw data) - August 12, 2004
  MS_STRUCT_FILLER(4, 2);
  int reserved[4]; 
} MS_STRUCT  ADV_PLOT_SETTINGS; 

// Disk format The default access block size of the disk archives is
// 1024 bytes. All read/write accesses to the archive must be
// performed in multiples of 1024 bytes.

// Overall data format The format of the data stored on the second
// partition of the tape is shown below. 
// ARCHIVE_LABEL | Data object | Data object | ... | Archive directory

typedef struct _ARCHIVE_LABEL { 
  char SystemName[80]; // set as Version 3 - MRI data tape, see below 
  time_t TimeStamp;   // initial time stamp, used for recognition 
  MS_STRUCT_FILLER(4b, 4);
  LARGE_INTEGER directoryPosition; // used for disk recording to locate directory 
} MS_STRUCT  ARCHIVE_LABEL;  

// Additional segments may be appended to the archive, beginning
// immediately following the last image from the previous segment. The
// Directory format contains provisions for storing the start and stop
// times and segment sizes.

// Data object format:
// BASE_RADAR_HDR | Data | Angles

typedef UCHAR ucBOOL; 
typedef struct _IMAGE_OPTIONS { 
  UCHAR FTC; // device-specific 
  ucBOOL PulseFiltering; 
  ucBOOL ScanConversion; 
  ucBOOL MotionCompensation; // only if scan converted
  UCHAR ScanAveraging; // 1 to 16
  ucBOOL CFAR; 
  UCHAR CfarOffset;    // The CFAR Offset used/to be used for CFAR
} MS_STRUCT  IMAGE_OPTIONS; 

typedef struct _BANDWIDTH_RELATED_INPUTS {
  USHORT RotationTime; // in msecs 
  USHORT PRF; // index of current PRF
  USHORT ModeNdx; // mode index
  USHORT PlenNdx; // plen index
  USHORT SelectedModeNdx; // the desired range
  USHORT SelectedPlenNdx; // the desired range
  USHORT PulsePeriod; // in usecs
  USHORT PulseLength; // from configuration file
  float ConfiguredCh2WidthNMI; // from configuration file
} MS_STRUCT  BANDWIDTH_RELATED_INPUTS; 

typedef struct _RADAR_BANDWIDTH_MODE {
  float MaximumRangeNMI; 
  ULONG PixelConfiguration; 
  ULONG SamplesCollectable; 
  ULONG MinAzimuthDecimation; 
  ULONG NomimalThroughput; 
  ULONG SourceChannelBufferHorzPitch; // width of buffer
  ULONG SourceChannelAzimuthCount; // height of buffer
} MS_STRUCT  RADAR_BANDWIDTH_MODE; 

typedef struct _BASE_RADAR_HDR {
  ULONG SourceId; 
  ULONG RawAtoDmode; 
  FLOAT StartRange; // in nautical miles
  FLOAT usecRangeCell; // stored sample resolution
  ULONG Samples;    //number of valid samples (containing data)
  USHORT BitsPerSample; 
  MS_STRUCT_FILLER(5, 2);
  ULONG rAxisBytes; // may included padded bytes at end of all samples
  BOOL NorthStabilized; 
  FLOAT Heading; // most recent ship heading at time of capture
  FLOAT StartAngle; // angle of first pulse stored.
  FLOAT Degrees; 
  ULONG yExtent; //in rasters stored
  USHORT AssociatedAzimuths; 
  MS_STRUCT_FILLER(6, 2);
  BOOL Gated;  // if gated or scan-converted, no LUT is used
  BOOL ScanConvert512x512; // TITAN only
  ULONG reserved[2]; 
  SYSTEMTIME PCTimeStamp; // PC time stamp
  time_t TimeStamp; // GPS time stamp
  float Latitude;      // in degrees (45° 25' 15'' = 45.4208333)
  float Longitude; // in degrees
  IMAGE_OPTIONS current; 
  MS_STRUCT_FILLER(7, 1);
  BANDWIDTH_RELATED_INPUTS BandwidthStatus;
  RADAR_BANDWIDTH_MODE BandwidthMode;
} MS_STRUCT  BASE_RADAR_HDR; 

typedef union _RSI_LUT 
{ 
  unsigned long AsULong; 
  struct { 
    unsigned long Gyro : 12; 
    unsigned long TickNibbleHi : 4;
    unsigned long Bearing : 12; 
    unsigned long TickNibbleLo : 4;
  }; 
} MS_STRUCT  RSI_LUT; 

typedef struct _DISK_DIRECTORY_ENTRY {
  LARGE_INTEGER Position; // queried from storage device
  time_t TimeStamp;    // copied from DataHeader.Time in current image
  MS_STRUCT_FILLER(10, 4);
} MS_STRUCT  DISK_DIRECTORY_ENTRY; 

#endif // #ifndef _IDLTYPES
