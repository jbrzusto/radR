/*
 *
 * Portions of IRIS definitions required for reading RAW product files
 * see Sec. 3.5.4 of Iris Programmer's Manual, March 2007.
 *
 * Extracted from header files bearing this copyright notice:
 *
 * COPYRIGHT (c) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
 *                               2007 BY 
 *             VAISALA INC., WESTFORD MASSACHUSETTS, U.S.A.  
 * 
 * THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY BE USED AND  COPIED
 * ONLY  IN  ACCORDANCE WITH  THE  TERMS  OF  SUCH  LICENSE  AND WITH THE
 * INCLUSION OF THE ABOVE COPYRIGHT NOTICE.  THIS SOFTWARE  OR  ANY OTHER
 * COPIES  THEREOF MAY NOT BE PROVIDED OR OTHERWISE MADE AVAILABLE TO ANY
 * OTHER PERSON.  NO TITLE TO AND OWNERSHIP OF  THE  SOFTWARE  IS  HEREBY
 * TRANSFERED. 
 */ 

#include <stdint.h>

  /* - - - - - - - - - - - System-Wide Variables   */
  /* Do not change this without changing the event flags defined in event_flags.h */
#define MAX_NETWORK_LINKS     (18)       /*Max # of incoming network links*/


/* - - - - - - - - - - - DSP and Data related parameters   */
#define MAX_BINS            (3072)       /*Max # bins in any ray at any time*/
#define MAX_MOMENTS           (16)       /*Max # moments recordable at once*/

/* - - - - - - - - - - - INGEST definitions */
#define MAX_INGEST_RAYS     (1024)       /*Max # rays per INGEST data file*/

#define LATE_SKIP_TIME        (60)       /*Grace period (in seconds) for late*/
                                         /* skip tasks.*/

/* - - - - - - - - - - - Task related parameters */
#define MAX_TSC_SLOTS      (100)         /*Max # tasks in schedule*/
#define MAX_SWEEPS          (40)         /*Max # sweeps in any scan mode    */
#define MAX_TSC_TASKS_MAJ    (8)           
#define MAX_TSC_TASKS_MIN   (26)

/* - - - - - - - - - - - Product related parameters
 * Note that MAX_PSC_SLOTS should be increased along with num_def.. to keep
 * space for users, because one slot is used up for each defined product.
 */
#define NUM_DEFINED_PRODUCTS  (34)                /*# different product types   */

  /* Check that the array is dimensioned properly.  It must be a multiple
   * of 4, and be larger than NUM_DEFINED_PRODUCTS.
   */
#if defined(NUM_DEFINED_PRIORITIES) && (NUM_DEFINED_PRIORITIES < NUM_DEFINED_PRODUCTS)
#error Please manually maintain the dimension of iprod_priorities[] in setup.h
#endif
#define MAX_PSC_SLOTS (NUM_DEFINED_PRODUCTS+2000) /*Max # products in schedule*/

#define MAX_PSC_TASKS_MINOR (26)  /* Max number of minor tasks */



/* - - - - - - - - - - - Output and Formatting parameters */
#define TAPE_RECORD_LEN      (6144)      /*Tape record length in bytes*/

  /* - - - - - - - - - - - File I/O Parameters
   * The INGEST and PRODUCT directories are permitted to temporarily grow
   * beyond their advertised limits.  This gives the watchdog time to
   * respond, without actually hiting the hard file count limit.
   */
#define INGEST_FILES_CUSHION  (200)
#define MAX_INGEST_FILES      (2000 + INGEST_FILES_CUSHION)

  /* To save network bandwidth, we default to a minimum scroll size if
   * not specified elsewhere.
   */

#define MIN_SCROLL_SIZE ( 10 )
  /* The maximum number of lines in any of the scrolled windows of
   * data supplied by the server to the client.
   */
#define MAX_SCROLL_SIZE ( 1000 )


/* Use this if the system call has a (char *) argument which
 * should be (const char *).
 */
#define SHOULD_BE_CONST( VALUE ) ((char *)(VALUE))

typedef    int8_t   SINT1;
typedef   uint8_t   UINT1;
typedef    int16_t  SINT2;
typedef   uint16_t  UINT2;
typedef    int32_t  SINT4;
typedef   uint32_t  UINT4;

typedef float           FLT4;
typedef double          FLT8;

typedef   uint16_t  BIN2;
typedef   uint32_t  BIN4;

typedef UINT4 MESSAGE;

/* Max signed, Min signed, and Max unsigned integer bounds
 */
#define MAXSINT4 ((SINT4)0x7FFFFFFF)
#define MINSINT4 ((SINT4)0x80000000)
#define MAXUINT4 ((UINT4)0xFFFFFFFF)

#define MAXSINT2 ((SINT2)0x7FFF)
#define MINSINT2 ((SINT2)0x8000)
#define MAXUINT2 ((UINT2)0xFFFF)

#define MAXSINT1 ((SINT1)0x7F)
#define MINSINT1 ((SINT1)0x80)
#define MAXUINT1 ((UINT1)0xFF)

/* Macros for general use
 */
#define BIT( ibit ) ( 1 << (ibit) )
#define BTEST( ivalue, ibit ) (((ivalue) &  BIT(ibit)) != 0 )
#define IBSET( ivalue, ibit ) ( (ivalue) |  BIT(ibit) )
#define IBCLR( ivalue, ibit ) ( (ivalue) & ~BIT(ibit) )
#define IBINV( ivalue, ibit ) ( (ivalue) ^  BIT(ibit) )

#define NINT(fvalue) ((SINT4)floor(0.5+(double)(fvalue)))
#define SQUARE(value) ((value)*(value))

#define STR_UPPER(str,size) \
   {int _ii; for(_ii=0;_ii<size;_ii++) (str)[_ii]=toupper((str)[_ii]);}

#define STR_LOWER(str,size) \
   {int _ii; for(_ii=0;_ii<size;_ii++) (str)[_ii]=tolower((str)[_ii]);}

#define SCALARSWAP( ITEMX, ITEMY, TYPE ) \
  do { TYPE __swapVal = ITEMX ; ITEMX = ITEMY ; ITEMY = __swapVal ; } while (0)

#ifndef FALSE  
#define TRUE  (1)
#define FALSE (0)
#endif

struct BOGUS 
{
  char me[128];
};

struct ymds_time
{
  UINT4 isec;
  UINT2 imills;		/* Fractions of seconds in milliseconds, in low 10 bits */
  UINT2 iyear, imon, iday;
};


/* Structure used to define the color scale conversion */

struct color_scale_def
{
#define COLOR_SCALE_VARIABLE (0x0100)
#define COLOR_SCALE_OVERRIDE (0x0200)  /* Not in file */
#define COLOR_SCALE_TOP_SAT  (0x0400)  /* All values above top shown as top */
#define COLOR_SCALE_BOT_SAT  (0x0800)  /* All values below bottom shown as bottom */
  UINT4 iflags;
  SINT4 istart;
  SINT4 istep;
  SINT2 icolcnt;
  UINT2 iset_and_scale;   /* Set number in low byte, scale number in high */
  UINT2 ilevel_seams[16];

};

/* -------------------- Structure Header --------------------
 * The following five words appear at the beginning of many IRIS structures
 * to help identify and use them.
 */
#define STRUCT_HEADER_SIZE   12          /* Size of header */

struct structure_header 
{
  SINT2 id;                             /*Structure identifier */
#define ST_TASK_CONF       (22)    /* Task_Configuration */
#define ST_INGEST_HDR      (23)    /* ingest_header */
#define ST_INGEST_DATA     (24)    /* INGEST_data_header */
#define ST_TAPE_INVEN      (25)    /* Tape inventory */
#define ST_PRODUCT_CONF    (26)    /* Product_configuration */
#define ST_PRODUCT_HDR     (27)    /* Product_hdr */
#define ST_TAPE_HEADER     (28)    /* Tape_header_record */
#define ST_VERSION_STEP    (20)    /* Step between version 1 and 2 */

  SINT2 ivers;                         /*Format version # */
#define SVER_TASK_CONF_P     5     /* Task_Configuration */
#define SVER_INGEST_HDR_P    4     /* ingest_header */
#define SVER_INGEST_DATA_P   3     /* ingest_data_header */
#define SVER_PRODUCT_CONF_P  6     /* Product_configuration */
#define SVER_PRODUCT_HDR_P   8     /* Product_hdr */
  SINT4 ibytes;                        /*# of bytes in entire structure */

  SINT2 reserved;
  SINT2 iflags;                        /* Flag Bits */
#define HD_STRUCTURE_COMPLETE  (0x0001)      /*  Structure Complete */
};

/*-------------------- Product Specific Information --------------------------
 * This gives certain information that is specific to the type of product being
 * considered.  This structure is used in product.h and output.h, hence
 * its inclusion here.  Note that scale number of products using the same
 * projection should be in the same place.  The projection is given in () after
 * the product name.
 */
#define PSI_SIZE (80)

/* For immediate products that ordinarily run on one particular angle, the
 * following wildcard value is taken to mean "run on all angles".
 */
#define IMP_WILD_ANGLE2 ((BIN2)0x8001)
#define IMP_WILD_ANGLE4 ((BIN4)0x80010000)
#define WARN_MAX_INPUTS 3

struct beam_psi_struct
{
  UINT4 irmin,         irmax;          /* Range limits in centimeters */  
  BIN4  iaz_left,      iaz_right;      /* Azimuth limits */  
  BIN4  iel_low,       iel_high;       /* Elevation limits */  
  BIN4  iaz_smooth,    iel_smooth;     /* Smoothing angles */  
  BIN4  iaz_sun_start, iel_sun_start;  /* Sun starting position */  
  BIN4  iaz_sun_end,   iel_sun_end;    /* Sun ending position */  
  char  ipad_end[PSI_SIZE-48];
};

struct cappi_psi_struct		/*CAPPI [cappi] */
{
  UINT4 iShearFlags;              /* Same flags as in shear product */
  SINT4 icappi_hgt;               /* Height (cm above reference) */
  UINT2 icappi_flags;		  /* Option flags */
#define CAPPI_FLG_PSEUDO (0x0001)   /* Make a pseudo CAPPI (extend single tilts) */
#define CAPPI_FLG_HORVEL (0x0002)   /* Modify velocity to be horizontal winds */
#define CAPPI_FLGS_ALL (CAPPI_FLG_PSEUDO|CAPPI_FLG_HORVEL)
  BIN2  iAzSmooth;                /* Smoothing for azimuth shear */
  
  char  svvp_name[12];            /* Name of VVP task to use */
  UINT4 ivvp_max_age;             /* Maximum age of VVP in seconds */
  char ipad_end[PSI_SIZE-28];
};

#define CATCH_MAX_HOURS    (96)  /* Max number of hours in a CATCH product */
/* Note separate definition of CATCH_MAX_HOURS_STORED in product.h, not fully separated */
struct catch_psi_struct		/*For CATCH [earth] */
{
  UINT4 iFlags ;		  /* Flags in lower 16-bits, RAIN1 flags in upper */
#define CATCH_FLG_WARNINGS_ON (0x0001)
#define CATCH_FLGS_ALL (CATCH_FLG_WARNINGS_ON)
  UINT4 iHours ;		  /* Number of hours to accumulate */
  SINT4 iThresholdOffset;	  /* Add this Offset to all thresholds (1/1000 of mm)*/
  SINT4 iThresholdFactor;	  /* Multiply this Factor with all thresholds (1/1000)*/
  char  sProduct[12] ;		  /* Name of RAIN1 pco, space padded */
  char  sSubCatchment[16];	  /* Name of subcatchment file, null terminated */
  UINT4 iSpanSeconds;             /* Seconds of accumulate (in lower 16-bits) */
  UINT4 iRain1MinZ;               /* Copied from RAIN1 input */
  UINT4 iRain1Span;               /* Span of the RAIN1 inputs in seconds */
  UINT4 iGageFactor;              /* Gage Correction Factor in DB_DBZ2 (lower 16-bits)*/
  char  ipad_end[PSI_SIZE-60];
};

#define COMP_MAX_INPUTS 16
struct comp_psi_struct
{
  UINT1 iPTypeIn;           /* Type of product used as input */
  UINT1 iCombineScheme;     /* Scheme used to combine inputs */
#define COMP_SCHEME_MAXIMUM   (0)
#define COMP_SCHEME_AVERAGE   (1)
#define COMP_SCHEME_PRIORITY  (2)
  /* The following schemes are used just for warning products */
#define COMP_SCHEME_AND_MAX   (0)
#define COMP_SCHEME_AND_AVG   (1)
#define COMP_SCHEME_OR_MAX    (2)
#define COMP_SCHEME_OR_AVG    (3)
  UINT2 iTimeSpan;          /* Time span in seconds, all inputs must be within */
  char  sPNameIn[12];       /* Name of product used as input */
  char  sSiteOut[4];        /* Site of output product */
  char  sSiteInList[COMP_MAX_INPUTS][3]; /* Site list of desired inputs */
  char  ipad_end[PSI_SIZE-68];
} ;

struct cross_psi_struct		/*Cross Section, User Section [cross] */
{
  BIN2 iazimuth;			/* Azimuth Bearing binary angle */
  char ipad2x10[10];
  SINT4 icenterx, icentery;		/* Coodinate in cm of center of section */
  SINT4 ivuser_misc[(PSI_SIZE-20)/4];	/* Unspecified contents */
};

#define DWELL_FLG_DATA       (0x01) /* Make output data rather then time */
#define DWELL_FLG_CONTRASTOR (0x02) /* Apply contrast filter */
#define DWELL_FLG_DATA_MIN   (0x04) /* Apply min data threshold */
#define DWELL_FLG_DATA_MAX   (0x08) /* Apply max data threshold */
#define DWELL_FLG_DIAGNOSTIC (0x10) /* Make diagnostic product */
#define DWELL_FLG_BIRD       (0x20) /* Apply bird algorithm */
#define DWELL_FLG_USE_VVP    (0x40) /* Use VVP to determine wind speed */
#define DWELL_FLG_ADD_WIND   (0x80) /* Add mean wind to the bird airspeed */

#define DWELL_FLG2_VVP_USED  (0x01) /* VVP was used to determine wind speed */
#define DWELL_FLG2_PPI_CONT  (0x02) /* Apply contrastor separately for each PPI angle */
#define DWELL_FLG2_AREA_MAX  (0x04) /* Apply max area threshold to warn */
#define DWELL_FLGS2_ALL (DWELL_FLG2_VVP_USED|DWELL_FLG2_PPI_CONT|DWELL_FLG2_AREA_MAX) /* All valid flags */

struct dwell_target_psi
{
  BIN2  iDirLeft;        /* Target direction limits */
  BIN2  iDirRight;     
  UINT2 iSpeedMin;       /* Target speed limits in 1/100 m/s */
  UINT2 iSpeedMax;  
  UINT4 iProtectMask;    /* Bit flags to choose protected areas */
  char  sWarnSymbol[12]; /* Symbol to display, space padded */
  SINT2 iPileThreshold;  /* Target confidence threshold */
  UINT1 iMergeRange;     /* Target merge range in 1/10 of km */
  char  ipad27x1;    
  UINT4 iFlags2;         /* Secondary flag word */
  UINT4 iAreaMax;        /* Maximum area of the target in 1/100 of square km */
  UINT2 iSpeedStep;      /* Target search speed step in 1/100 m/s, 0=default 4 m/s */
  BIN2  iDirStep;        /* Target search direction step in bin2, 0=default 5 deg*/
  char  ipad_end[PSI_SIZE-68];
};
struct dwell_warn_psi
{
  char  sWarnSuffix1[4];    /* Suffix string, null terminated */
  char  sWarnSuffix2[4];
  SINT4 iWarnAge2;          /* Age in seconds to transition to second suffix */
  char  ipad12x16[16];
  UINT4 iFlags2;            /* Secondary flag word */
  char  ipad_end[PSI_SIZE-60];
};

union dwell_psi_union
{
  struct dwell_target_psi Target;
  struct dwell_warn_psi   Warn;
};

struct dwell_psi_struct
{
  SINT4 iDataMin;	    /* Data value limits [in 1/100] */
  SINT4 iDataMax;
  SINT2 iContrastCoverage;  /* Contrastor coverage percentage [in 1/100] */
  SINT2 iContrastThreshold; /* Contraster threshold [in 1/100] */
  UINT2 iTimeSpan;          /* Dwell time all inputs are within */
  UINT1 iPTypeIn;           /* Type of product used as input */
  UINT1 iFlags;
  char  sPNameIn[12];       /* Name of product used as input */
  union dwell_psi_union u;

} ;

/*Use for forecast */
struct fcast_psi_struct
{
  UINT4 iCorrThreshold;           /* Correlation threshold */
  SINT4 iDataThreshold;           /* Data threshold levels [in 1/100] */
  SINT4 iMeanSpeed;               /* Mean speed in cm/hour */
  BIN4  iMeanDirection;           /* Mean speed in cm/hour */
  UINT4 imax_time_step;           /* Maximum age between products for motion calc. */
  SINT4 imax_velocity;            /* Maximum velocity allowed in motion calc. */
  UINT4 iflags;  
  SINT4 iOutputResolution;        /* Output resolution in cm */
  UINT4 iptype;                   /* Product type being used as input */
  char  spname[12];               /* Name of product file, space padded */
  char  ipad_end[PSI_SIZE-48];
};

struct gage_psi_struct		/*For GAGE [earth] */
{
  UINT4 iSpan;                    /* Number of seconds of data recorded in the file */
  UINT4 iFlag;
#define GAGE_PFLG_DISDRO (0x001)  /* File contains Z/R numbers (Disdrometer) */
#define GAGE_PFLG_NORAIN (0x002)  /* File does not contain a Rainrate (is not a gauge) */
#define GAGE_PFLGS_ALL (GAGE_PFLG_DISDRO|GAGE_PFLG_NORAIN)
  char  ipad_end[PSI_SIZE-8];

};

struct maximum_psi_struct	/*Maximum with side panels */
{
  char ipad0x4[4];
  SINT4 ibottom;                  /* Bottom of interval in cm */
  SINT4 itop;                     /* Top of interval in cm */
  SINT4 iside_size;               /* Number of pixels in side panels */
  SINT2 ihorz_smooth;             /* Horz. smoother in side panels */
  SINT2 ivert_smooth;             /* Vert. smoother in side panels */
  char  ipad_end[PSI_SIZE-20];
};

struct ndop_input
{
  char sTName[12] ;		/* Task names, space padded */
  char sSiteId[3] ;		/* Site ID, all NULLs if site is not used */
  UINT1 iflags ;		/* Flag bits (must follow sSiteId) */
#define NDOP_INFLG_VC      (0x01)  /*   Input is based on Vc, else V */
#define NDOP_INFLG_INFILE  (0x02)
#define NDOP_INFLGS_ALL    (NDOP_INFLG_VC|NDOP_INFLG_INFILE)
} ;
struct ndop_psi_struct
{
  struct ndop_input Inputs[WARN_MAX_INPUTS] ;
  SINT4 iTimeWindow;		/* Time window in seconds */
  SINT4 iCappiHeight;		/* Height of lowest cappi (cm above reference) */
  SINT4 iOutputResolution;	/* Output resolution in cm */
  BIN4  iMinCrossingAngle;	/* Minimum permitted ray crossing angle */
  UINT4 iFlags;                 /* Flag bits */
#define NDOP_FLG_DIAGNOSTIC (0x0001) /* Make diagnostic product */
#define NDOP_FLGS_ALL (NDOP_FLG_DIAGNOSTIC)
  char  sSiteOut[4];            /* Site of output product (4th char not used) */
  char  ipad_end[PSI_SIZE-72];
} ;

struct ppi_psi_struct		/* PPI [ppi] */
{
  BIN2  ippi_el;                /* Elevation binary angle */
  char  ipad2x2[2];
  SINT4 iMaxRange;              /* max range in cm (zero=none)*/
  SINT4 iMaxHeight;             /* max height in cm above reference (zero=none)*/
  char ipad_end[PSI_SIZE-12];
};

#define RAIN_SPAN_MULTIPLE (900)  /* All rain product spans are a multiple of 15 minutes*/
#define RAINN_MAX_HOURS    (168)  /* Max number of hours in a rainn span */

#define RAIN_FLG_CLT_MAP    (0x0001) /* Use clutter map */
#define RAIN_FLG_CLT_MAP_OK (0x0004) /* Clutter map was actually used */
#define RAIN_FLG_GAG_COR    (0x0002) /* Apply raingage correction */
#define RAIN_FLG_GAG_COR_OK (0x0008) /* Raingage correction was actually used */
#define RAIN_FLG_DIAGNOSTIC (0x0010) /* Make diagnostic product */
#define RAIN_FLG_SRI_INPUT  (0x0020) /* Input is SRI type, else CAPPI */
#define RAIN1_FLGS_USED (RAIN_FLG_CLT_MAP|RAIN_FLG_GAG_COR|RAIN_FLG_DIAGNOSTIC|RAIN_FLG_SRI_INPUT)  /* Used in the RAIN1 PCO */
#define RAIN_FLG_USED RAIN1_FLGS_USED
#define RAINN_FLGS_KEPT (RAIN_FLG_CLT_MAP|RAIN_FLG_CLT_MAP_OK|RAIN_FLG_GAG_COR|RAIN_FLG_GAG_COR_OK|RAIN_FLG_DIAGNOSTIC|RAIN_FLG_SRI_INPUT)  /* Kept in the RAINN PCO */
struct rain_psi_struct		/*For RAIN1 and RAIN-N [earth] */
{
  UINT4 irain1_minz;              /* Assume no rain below this level DB_DBZ2 format */
  UINT2 iAverageGageFactor;       /* Gage Correction Factor in DB_DBZ2 format */
  UINT2 iSeconds;                 /* Seconds of time interval (multiples of 15 min) */
  UINT2 irain_flags ;		  /* Flags */
  SINT2 irainn_hours ;		  /* RAINN number of hours to accumulate */
  char  sproduct[12] ;		  /* Name of CAPPI/SRI/RAIN1 pco, space padded */
  UINT4 iRain1Span;               /* Span of the RAIN1 inputs in seconds (only for RAINN) */
  char  ipad_end[PSI_SIZE-28];
};

struct raw_psi_struct		/*RAW [void] */
{
  UINT4 iDataMask0;               /* First word of Data bit mask */
  SINT4 iraw_bin_last;            /* Range of last bin in cm */
  UINT4 iraw_convert;		  /* Format conversion instructions */
#define IRAW_CONVERT_PRESERVE 0	     /* Preserve all INGEST data */
#define IRAW_CONVERT_8_TO_16  1	     /* Convert 8-bit data to 16-bit data */
#define IRAW_CONVERT_16_TO_8  2	     /* Convert 16-bit data to 8-bit data */

  UINT4 iflags;			  /* Control flags */
#define RAW_FLG_SWEEP    (0x00000001) /* Make sweep-by-sweep product files */
#define RAW_FLG_DATAMASK (0x00000002) /* Mask the data types recorded */
#define RAW_FLGS_ALL (RAW_FLG_SWEEP|RAW_FLG_DATAMASK) /* All bits in use */

  SINT4 isweep ;		  /* Individual sweep number, if RAW_FLG_SWEEP is set
				   *  in iflags; else zero. */
  UINT4 iXhdrType;                /* Extended header type */
  UINT4 iDataMask1;               /* Data bit mask */
  UINT4 iDataMask2;               /* Data bit mask */
  UINT4 iDataMask3;               /* Data bit mask */
  UINT4 iDataMask4;               /* Data bit mask */
  UINT4 iPlayback;                /* Playback version in low 16-bits */
  char ipad_end[PSI_SIZE-44];
};

struct rhi_psi_struct		/*RHI [rhi] */
{
  BIN2 irhi_az ;                  /* Azimuth binary angle */
  char ipad_end[PSI_SIZE-2];
};

struct rti_psi_struct		/*RTI [rti] */
{
  BIN4  iSweepAngle;              /* Nominal sweep angle */
  UINT4 iStartOffset;             /* Starting and ending time offsets from sweep time */
  UINT4 iEndOffset;               /* Times are in ms */
  BIN4  iStartAzimuth;            /* Direction of the first ray in the file */
  BIN4  iStartElevation;
  char  ipad_end[PSI_SIZE-20];
};

struct shear_psi_struct		/*SHEAR [earth] */
{
  BIN4  iaz_smooth;               /* Azimuthal smoothing angle, 0=none */
  BIN2  ishear_el;                /* Elevation binary angle */
  char  ipad6x2[2];
  UINT4 iflags;
#define SHEAR_FLG_RADIAL    (0x00000001)
#define SHEAR_FLG_AZIMUTHAL (0x00000002)
#define SHEAR_FLG_USE_VVP   (0x00000004)
#define SHEAR_FLG_VVP_USED  (0x00000008)
#define SHEAR_FLG_UNFOLDING (0x00000010)
#define SHEAR_FLG_ELEVATION (0x00000020)
#define SHEAR_FLG_DPRS      (0x00000040) /* Discard positive radial shear */
#define SHEAR_FLGS_ALL (SHEAR_FLG_RADIAL|SHEAR_FLG_AZIMUTHAL|SHEAR_FLG_USE_VVP|SHEAR_FLG_VVP_USED|SHEAR_FLG_UNFOLDING|SHEAR_FLG_ELEVATION|SHEAR_FLG_DPRS)
  char  svvp_name[12];            /* Name of VVP task to use */
  UINT4 ivvp_max_age;             /* Maximum age of VVP in seconds */
  char  ipad_end[PSI_SIZE-28];
};

struct sline_psi_struct		/*Shear Line [earth] */
{
  SINT4 iarea;                    /* Area in square meters */
  SINT4 ishear_threshold;         /* Shear threshold (cm/sec/km) */
  UINT4 iprotect_mask;            /* Bit flags to choose protected areas */
  SINT4 iforecast_time;           /* Maximum forecast time in seconds */
  UINT4 imax_time_step;           /* Maximum age between products for motion calc. */
  SINT4 imax_velocity;            /* Maximum velocity allowed in motion calc. */
  UINT4 iflags;                   /* Shear flags in low bits */
#define SLINE_FLG_2ANGLES    (0x00000100)
#define SLINE_FLG_DIAGNOSTIC (0x00000200)
#define SLINE_FLG_MAXWRN     (0x00000400)
#define SLINE_FLG_2FCAST     (0x00000800)  /* Display with 2 forecast lines */
#define SLINE_FLGS_ALL     (SLINE_FLG_2ANGLES|SLINE_FLG_DIAGNOSTIC|SLINE_FLG_MAXWRN|SLINE_FLG_2FCAST|SHEAR_FLGS_ALL)
  BIN4  iaz_smooth;               /* Azimuthal smoothing angle, 0=none */
  BIN4  iel1;                     /* Elevation binary angle */
  BIN4  iel2;                     /* Elevation binary angle */
  char svvp_name[12];             /* Name of VVP task to use */
  UINT4 ivvp_max_age;             /* Maximum age of VVP in seconds */
  SINT4 ifit_threshold;           /* Curve fit variance threshold */
  UINT4 iMaxs;                    /* Low byte: Max sline length in 1/10 km (unsigned) */
  char  ipad_end[PSI_SIZE-64];
};

#define IC_MELTING_UNKNOWN   (0x0000)  /* Melting layer height unknown */
#define iMeltFromData(_DATA) ((SINT2)( 0x8000 ^ (SINT2)(_DATA)))
#define iDataFromMelt(_MELT) ((SINT2)( 0x8000 ^ (SINT2)(_MELT)))
struct sri_psi_struct	    /*SRI [earth] */
{
  UINT4 iPsiFlags;	    /* Option flags */
  SINT4 iBinCount;          /* Total number of bins inserted */
  SINT4 iDataCount;         /* Total number of bins with data in them */
  SINT4 iCorrCount;         /* Number of data bins passing convection test */
  SINT2 iSurfaceHeight;     /* Height of surface to correct to (m above reference) */
  SINT2 iMaxHeight;         /* Max height to consider (m above reference) */
  SINT2 iMeltHeight;        /* Melting height (m above MSL), MSB complemented */
  SINT2 iMeltThickness;     /* Melting level thinkness in m (info only)*/
  SINT2 iMeltIntensity;     /* Melting level intensity (1/100 of dB) (info only)*/
  SINT2 iGradientAbove;     /* Gradient above melting level( 1/100 dB/km) (info only)*/
  SINT2 iGradientBelow;     /* Gradient below melting level( 1/100 dB/km) (info only)*/
  SINT2 iConvectiveHeight;  /* Height of convective check (m above Melting) */
  UINT2 iConvectiveZ;       /* dBZ of convective check (DB_DBZ2 format) */
  char  ipad_end[PSI_SIZE-34];
  
#define SRI_FLG_PROFILE_CORRECTION (0x01) /* Apply the profile correction */
#define SRI_FLG_DIAGNOSTIC         (0x02) /* Make diagnostic product */
#define SRI_FLG_TERRAIN_MAP        (0x04) /* Use terrain map file */
#define SRI_FLG_MELT_SOURCE0       (0x08) /* Melting source: */
#define SRI_FLG_MELT_SOURCE1       (0x10) /* 0:Ingest, 1:Setup, 2:TypeIn */
#define SRI_FLG_CHECK_CONVECTION   (0x20) /* Check for convection */
#define SRI_FLG_NO_CLUTTER         (0x40) /* Clutter correction not applied */
#define SRI_FLGS_ALL   (SRI_FLG_PROFILE_CORRECTION|SRI_FLG_DIAGNOSTIC|SRI_FLG_TERRAIN_MAP|SRI_FLG_MELT_SOURCE0|SRI_FLG_MELT_SOURCE1|SRI_FLG_CHECK_CONVECTION|SRI_FLG_NO_CLUTTER)
};

/* No server structure defined yet because no Product Configuration
 * Menu
 */
struct tdwr_psi_struct		/*TDWR [overlay] */
{
  UINT4 iFlags;
#define TDWR_FLG_LLWAS    (0x0001)
#define TDWR_FLG_WARN     (0x0002)
#define TDWR_FLG_SLINE    (0x0004)
#define TDWR_FLG_2FCAST   (0x0008) /* Display with 2 forecast lines */
  UINT4 iRMax;                    /* Maximum range in centimeters */  
  char  sSourceID[4];             /* LLWAS source ID, space padded */
  char  sCFDirection[3];
  UINT1 ipad7x1;
  char  sCFSpeed[2];
  char  sCFGustSpeed[2];
  UINT4 iprotect_mask;		  /* Mask showing which protected areas were checked */
  UINT4 iCentroidCount;		  /* Number of centroids also in file */
  UINT4 iSlineCount;		  /* Number of shearlines also in file */
  SINT4 iForecastTime;            /* Forecast time in seconds */
};

struct top_psi_struct		/*Echo tops and base [earth] */
{
  char ipad0x4[4];
  SINT2 iZThreshold;            /* Z threshold in 1/16 dbZ */
  char ipad_end[PSI_SIZE-6];
};

/*Use for track */
struct track_psi_struct
{
  SINT4 iarea;                    /* Area in square meters */
  SINT4 ithreshold;               /* Data threshold levels [in 1/100] */
  UINT4 iprotect_mask;            /* Bit flags to choose protected areas */
  SINT4 iforecast_time;           /* Maximum forecast time in seconds */
  UINT4 imax_time_step;           /* Maximum age between products for motion calc. */
  SINT4 imax_velocity;            /* Maximum velocity allowed in motion calc. */
  UINT4 iflags;  
#define TRACK_FLG_DIAGNOSTIC (0x00000200)
#define TRACK_FLGS_ALL (TRACK_FLG_DIAGNOSTIC)
  SINT4 ispan_time;               /* Maximum time span for points in product */
  UINT4 iptype;                   /* Product type being used as input */
  char  spname[12];               /* Name of product file, space padded */
  SINT4 iconnect_error;           /* Point connecting error allowance */
  char  ipad_end[PSI_SIZE-52];
};

struct user_psi_struct		/*For both User Map [earth], and Other */
{
  SINT4 imisc[PSI_SIZE/4];        /* Unspecified contents */
};

struct vil_psi_struct		/*VIL [earth] */
{
  char ipad0x4[4];
  SINT4 ivil_bottom;              /* Bottom of interval (cm above reference) */
  SINT4 ivil_top;                 /* Top of interval in (cm above reference) */
  char  ipad_end[PSI_SIZE-12];
};

struct vvp_psi_struct		/*Volume Velocity Processing [VVP] */
{
  SINT4 ivvp_rmin, ivvp_rmax;	        /* Range limits in centimeters */
  SINT4 ivvp_hmin, ivvp_hmax;		/* Height limits (cm above reference)*/
  SINT2 ivvp_intervals;			/* Number of height intervals  */
  char ipad18x2[2];
  SINT4 ivvp_quota;			/* Target # bins per interval  */
  UINT4 ivvp_switch;			/* Wind parameters to compute  */
					/*  (defined in product.h)   */
  char  ipad_end[PSI_SIZE-28];
};

struct warn_psi_struct		/*Warning [overlay] */
{
  SINT4 iwarn_area;			  /* Area in square meters */
  SINT4 iwarn_threshs[WARN_MAX_INPUTS];	  /* Data threshold levels [in 1/100] */
  SINT2 iwarn_seconds[WARN_MAX_INPUTS];	  /* Data valid times in seconds */
  char ipad22x2[2];
  char  swarn_symbol[12];		  /* Symbol to display, space padded */
  char  swarn_names[WARN_MAX_INPUTS][12]; /* Names of product files, space padded */
  UINT1 iwarn_prods[WARN_MAX_INPUTS];     /* Product types being used as input */
  UINT1 iflags ;		          /* Control Flags */
#define WARN_FLG_CMPLT0  0x01	          /*   Perform a "less than" comparison each */
#define WARN_FLG_CMPLT1  0x02	          /*   of these inputs if the bit is set. */
#define WARN_FLG_CMPLT2  0x04
#define WARN_FLG_DIAGNOSTIC (0x08)        /* Make diagnostic product */
#define WARN_FLGS_ALL (WARN_FLG_CMPLT0|WARN_FLG_CMPLT1|WARN_FLG_CMPLT2|WARN_FLG_DIAGNOSTIC)
  UINT4 iprotect_mask;			  /* Bit flags to choose protected areas */
};

struct wind_psi_struct		/* Horizontal Wind Field */
{
  SINT4 ihmin, ihmax;		/* Height limits (centimeters above reference) */
  SINT4 irmin, irmax ;          /* Range limits in centimeters */  
  SINT4 irange_count ;		/* Number of points in range */
  SINT4 iazimuth_count ;       	/* Number of points in azimuth */
  SINT4 isector_length ;       	/* Sector length in centimeters */
  BIN4  isector_width ;		/* Sector width as binary angle */
  UINT4 iflags ;                /* Control flags */
  UINT4 ivvp_switch ;	        /* Contents of embedded vvp_results */
  char  ipad_end[PSI_SIZE-40];
} ;
#define WIND_MAX_RANGE_COUNT   (40)
#define WIND_MAX_AZIMUTH_COUNT (36)

union product_specific_info
{
  struct    beam_psi_struct beam;
  struct   cappi_psi_struct cappi;
  struct   catch_psi_struct Catch;  /* Note that "catch" is a C++ keywork */
  struct    comp_psi_struct comp;
  struct   cross_psi_struct cross;
  struct   dwell_psi_struct Dwell;
  struct   fcast_psi_struct fcast;
  struct    gage_psi_struct Gage;
  struct maximum_psi_struct max;
  struct    ndop_psi_struct ndop;
  struct     ppi_psi_struct ppi;
  struct    rain_psi_struct rain;
  struct     raw_psi_struct raw;
  struct     rhi_psi_struct rhi;
  struct     rti_psi_struct rti;
  struct   shear_psi_struct shear;
  struct   sline_psi_struct sline;
  struct     sri_psi_struct Sri;
  struct    tdwr_psi_struct tdwr;
  struct     top_psi_struct top;
  struct   track_psi_struct track;
  struct    user_psi_struct user;
  struct     vil_psi_struct vil;
  struct     vvp_psi_struct vvp;
  struct    warn_psi_struct warn;
  struct    wind_psi_struct wind;
  char ipad[PSI_SIZE];
};

typedef union product_specific_info PSI;

struct product_configuration
{
  struct structure_header  hdr;	/* Generic Header */

  UINT2 iptype;			/* Product Type Code */
#define PROD_PPI        1	/*  PPI */
#define PROD_RHI        2	/*  RHI */
#define PROD_CAPPI      3	/*  CAPPI */
#define PROD_CROSS      4	/*  Cross section */
#define PROD_TOPS       5	/*  Echo tops */
#define PROD_TRACK      6	/*  Storm track */
#define PROD_RAIN1      7	/*  Precipitation 1 hour */
#define PROD_RAINN      8	/*  Precipitation n hour */
#define PROD_VVP        9	/*  Velocity Volume processing */
#define PROD_VIL        10	/*  Vertically Integrated Liquid */
#define PROD_SHEAR      11	/*  Wind shear */
#define PROD_WARN       12	/*  Warning (overlay) */
#define PROD_CATCH      13	/*  Rain catchments */
#define PROD_RTI        14	/*  Range-Time-Display */
#define PROD_RAW        15	/*  Raw data set (no display)*/
#define PROD_MAX        16	/*  Maximum with side panels */
#define PROD_USER       17	/*  Earth projection user product */
#define PROD_USERV      18	/*  Section projection user product */
#define PROD_OTHER      19	/*  Other user product (no display) */
#define PROD_STATUS     20	/*  Status product (no display) */
#define PROD_SLINE      21	/*  Shear Line Product */
#define PROD_WIND       22	/*  Horizontal wind field */
#define PROD_BEAM       23	/*  Beam pattern */
#define PROD_TEXT       24	/*  Text */
#define PROD_FCAST      25	/*  Forecast */
#define PROD_NDOP       26	/*  Multi-Doppler */
#define PROD_IMAGE      27	/*  Arbitrary graphics image */
#define PROD_COMP       28	/*  Composite */
#define PROD_TDWR       29	/*  TDWR Wind Alert */
#define PROD_GAGE       30	/*  Raingage product */
#define PROD_DWELL      31	/*  Dwell and bird detection product */
#define PROD_SRI        32	/*  Surface Rainfall Intensity */
#define PROD_BASE       33	/*  Echo bottoms */
#define PROD_HMAX       34	/*  Height of Max Reflectivity */

#define PROD33_RANK     0	/*  BASE */
#define PROD23_RANK     1	/*  BEAM */
#define PROD3_RANK      2	/*  CAPPI */
#define PROD13_RANK     3	/*  CATCH */
#define PROD28_RANK     4	/*  COMP */
#define PROD31_RANK     5	/*  DWELL */
#define PROD25_RANK     6	/*  FCAST */
#define PROD30_RANK     7	/*  GAGE */
#define PROD34_RANK     8	/*  HMAX */
#define PROD27_RANK     9	/*  IMAGE */
#define PROD16_RANK    10	/*  Max */
#define PROD26_RANK    11	/*  NDOP */
#define PROD1_RANK     12	/*  PPI */
#define PROD7_RANK     13	/*  Rain 1 */
#define PROD8_RANK     14	/*  Rain n */
#define PROD15_RANK    15	/*  RAW */
#define PROD2_RANK     16	/*  RHI */
#define PROD14_RANK    17	/*  RTI */
#define PROD11_RANK    18	/*  Shear */
#define PROD21_RANK    19	/*  SLINE */
#define PROD32_RANK    20	/*  SRI */
#define PROD20_RANK    21	/*  Status */
#define PROD29_RANK    22	/*  TDWR */
#define PROD24_RANK    23	/*  TEXT */
#define PROD5_RANK     24	/*  TOPS */
#define PROD6_RANK     25	/*  TRACK */
#define PROD10_RANK    26	/*  VIP */
#define PROD9_RANK     27	/*  VVP */
#define PROD12_RANK    28	/*  WARN */
#define PROD22_RANK    29	/*  Wind */
#define PROD4_RANK     30	/*  XSECT */
#define PROD17_RANK    31	/*  USER */
#define PROD18_RANK    32	/*  USERV */
#define PROD19_RANK    33	/*  OTHER */

#define PROD_RANK_LIST {\
    PROD1_RANK,  PROD2_RANK,  PROD3_RANK,  PROD4_RANK,  PROD5_RANK,  PROD6_RANK, \
    PROD7_RANK,  PROD8_RANK,  PROD9_RANK,  PROD10_RANK, PROD11_RANK, PROD12_RANK,\
    PROD13_RANK, PROD14_RANK, PROD15_RANK, PROD16_RANK, PROD17_RANK, PROD18_RANK,\
    PROD19_RANK, PROD20_RANK, PROD21_RANK, PROD22_RANK, PROD23_RANK, PROD24_RANK,\
    PROD25_RANK, PROD26_RANK, PROD27_RANK, PROD28_RANK, PROD29_RANK, PROD30_RANK,\
    PROD31_RANK, PROD32_RANK, PROD33_RANK, PROD34_RANK}

  /* Product licensing bits */
#define LBIT_BASIC    (0x00000001)  /* Basic product set */
#define LBIT_CAPPI    (0x00000002)  /* Full product set */
#define LBIT_MAX      (0x00000004)  /* Full product set */
#define LBIT_RAIN     (0x00000008)  /* Full product set */
#define LBIT_TOPS     (0x00000010)  /* Full product set */
#define LBIT_VIL      (0x00000020)  /* Full product set */
#define LBIT_VVP      (0x00000040)  /* Full product set */
#define LBIT_WIND     (0x00000080)  /* Full product set */
#define LBIT_COMP     (0x00000100)  /* Optional product set */
#define LBIT_SHEAR    (0x00000200)  /* Optional product set */
#define LBIT_IMAGE    (0x00000400)  /* Special product set */
#define LBIT_TEXT     (0x00000800)  /* Special product set */
#define LBIT_USER     (0x00001000)  /* Special product set */
#define LBIT_LLWAS    (0x00002000)  /* Special product set */
#define LBIT_HYDROMET (0x00004000)  /* Optional product set */
#define LBIT_DWELL    (0x00008000)  /* Optional product set */
#define LBIT_UNUSED   (0x10000000)  /* Unused product set */

#define PROD1_LBIT    LBIT_BASIC    /* PPI */
#define PROD2_LBIT    LBIT_BASIC    /* RHI */
#define PROD3_LBIT    LBIT_CAPPI    /* CAPPI (full) */
#define PROD4_LBIT    LBIT_BASIC    /* Cross section */
#define PROD5_LBIT    LBIT_TOPS     /* Tops */
#define PROD6_LBIT    LBIT_BASIC    /* Track */
#define PROD7_LBIT    LBIT_RAIN     /* Rain1 */
#define PROD8_LBIT    LBIT_RAIN     /* RainN */
#define PROD9_LBIT    LBIT_VVP      /* VVP */
#define PROD10_LBIT   LBIT_VIL      /* VIP */
#define PROD11_LBIT   LBIT_SHEAR    /* Shear */
#define PROD12_LBIT   LBIT_BASIC    /* Warning */
#define PROD13_LBIT   LBIT_HYDROMET /* CATCH */  
#define PROD14_LBIT   LBIT_BASIC    /* RTI */
#define PROD15_LBIT   LBIT_BASIC    /* RAW */
#define PROD16_LBIT   LBIT_MAX      /* Max */
#define PROD17_LBIT   LBIT_USER     /* USER */
#define PROD18_LBIT   LBIT_USER     /* USERV */
#define PROD19_LBIT   LBIT_USER     /* OTHER */
#define PROD20_LBIT   LBIT_BASIC    /* Status */
#define PROD21_LBIT   LBIT_SHEAR    /* SLine */
#define PROD22_LBIT   LBIT_WIND     /* Wind */
#define PROD23_LBIT   LBIT_BASIC    /* Beam */
#define PROD24_LBIT   LBIT_TEXT     /* Text */
#define PROD25_LBIT   LBIT_BASIC    /* Forecast */
#define PROD26_LBIT   LBIT_COMP     /* NDOP */
#define PROD27_LBIT   LBIT_IMAGE    /* Image */
#define PROD28_LBIT   LBIT_COMP     /* COMP */
#define PROD29_LBIT   LBIT_LLWAS    /* LLWAS */
#define PROD30_LBIT   LBIT_HYDROMET /* GAGE */
#define PROD31_LBIT   LBIT_DWELL    /* DWELL */
#define PROD32_LBIT   LBIT_CAPPI    /* SRI */
#define PROD33_LBIT   LBIT_BASIC    /* BASE */
#define PROD34_LBIT   LBIT_BASIC    /* HMAX */

#define PROD_LBIT_LIST {\
    PROD1_LBIT,  PROD2_LBIT,  PROD3_LBIT,  PROD4_LBIT,  PROD5_LBIT,  PROD6_LBIT, \
    PROD7_LBIT,  PROD8_LBIT,  PROD9_LBIT,  PROD10_LBIT, PROD11_LBIT, PROD12_LBIT,\
    PROD13_LBIT, PROD14_LBIT, PROD15_LBIT, PROD16_LBIT, PROD17_LBIT, PROD18_LBIT,\
    PROD19_LBIT, PROD20_LBIT, PROD21_LBIT, PROD22_LBIT, PROD23_LBIT, PROD24_LBIT,\
    PROD25_LBIT, PROD26_LBIT, PROD27_LBIT, PROD28_LBIT, PROD29_LBIT, PROD30_LBIT,\
    PROD31_LBIT, PROD32_LBIT, PROD33_LBIT, PROD34_LBIT}



  /* The following variables are used to schedule the running of this product
   * configuration.  Starting from EPOCH, either the next available data, or
   * all available data (as per ISCH) will be processed.  The various time
   * structures have the following meaning.
   *
   *    1) The "ing_time" time structure has different meaning depending on
   *  the type of product.  TZ determined by iMinutesWest and sTZName.
   *
   *  a) For "immediate" products (i.e., PPI, RHI, SHEAR, and single
   *     sweep RAW), the "ing_time" structure holds the starting time
   *     of the specific sweep that was used to make the product.
   *
   *  b) For "volume scan" products (i.e., all others), the "ing_time"
   *     structure holds the time of the start of the earliest INGEST
   *     volume scan that was used in making the product.  For example,
   *     if a TOPS product is made from two hybrid tasks "TASK_B" and
   *     "TASK_C", then the TOPS "ing_time" would be the starting time
   *     of the "B" volume scan.
   *
   *    2) The "epoch" time structure is used for scheduling the products,
   *  and always has the contents of "b)" above, regardless of whether
   *  the product is an "immediate" or "volume scan" type.  Thus, as an
   *  "immediate" product runs through the individual tilts of a volume
   *  scan, the "epoch" time remains fixed at the volume scan starting
   *  time, whereas the "ing_time" time marchs forward for each sweep
   *  processed.
   *
   *    3) The "prod_time" time structure refers to the time that the product
   *  file was produced on whatever machine originally generated it.
   *  While in the product scheduler, this is the next-data-time.
   *  TZ determined by the setup question.
   */
  UINT2  isch;			/* Scheduling */
#define PSC_HOLD      0		/*  Do not run at all. */
#define PSC_NEXT      1		/*  Run once on next available data */
#define PSC_ALL       2		/*  Run as data becomes available */
#define PSC_AGAIN     3		/*  Run again on data last used */
#define PSC_ONCE      4		/*  Run once then remove from schedule */

  SINT4 isch_skip;		/* # seconds between runs */

  struct ymds_time prod_time;	/* Time product was created */
  struct ymds_time ing_time;	/* Time of ingest data product was made from */
				/* For RAIN1 and RAINN, end of time interval */

  struct ymds_time epoch;	/* Beginning of scheduling time period */

				/* The following text strings must be in upper case: */

  char ipad56x6[6];
  char spname[12];		/* Product configuration name */

  char stname[12];		/* Task configuration name */

  UINT2 iflags;			/* Flags */
  /* Flags in the file, and in the inventory
   */
#define PF_KEEP        (0x0020)  /* Do not remove this file    */
#define PF_CLUTTER     (0x0040)  /* This is a clutter map      */
#define PF_COMPOSITED  (0x0800)  /* This product has been composited*/
#define PF_DWELL       (0x1000)  /* This product is a dwell product*/
#define PF_ZR_SOURCE0  (0x2000)  /* Source of Z/R numbers: */
#define PF_ZR_SOURCE1  (0x4000)  /* 0:TypeIn, 1:Setup, 2:Disdro, 3:Unused */

  /* Product type specific flags in the file, and in the inventory
   */
#define PF_WARN_TDWR   (0x0002)  /* Make TDWR style messages */
#define PF_WARN_SAY    (0x0080)  /* Speak warning messages */

  /* Bits stored in the file
   */
#define PF_IN_FILE (PF_KEEP|PF_CLUTTER|PF_WARN_TDWR|PF_WARN_SAY|PF_COMPOSITED|PF_DWELL|PF_ZR_SOURCE0|PF_ZR_SOURCE1)

  /* Bits controlled by PCF menu
   */
#define PF_IN_PCF  (PF_WARN_TDWR|PF_WARN_SAY|PF_ZR_SOURCE0|PF_ZR_SOURCE1)

  /* Flags in the product schedule
   */
#define PF_RUNNING    (0x0004)  /* This pcf is being run               */
#define PF_HEADER     (0x0008)  /* Header, placeholder only, dont execute  */

  /* Flags in the product inventory only
   */
#define PF_DELETE     (0x0010)  /* Delete this product        */
#define PF_REINGEST   (0x0100)  /* Tagged for reingesting     */
#define PF_TIMEOUT    (0x0200)  /* Status product has timed out*/
#define PF_PENDING    (0x0001)  /* Status product from before startup*/

  /* Flags in server-to-client report */
#define PF_CHECKED    (0x0400)  /* Status product checking enabled for this site*/

  SINT4 ixscale, iyscale, izscale ; /* Scale in cm/pixel  */
  SINT4 ixsize , iysize , izsize  ; /* Size of (up to 3) dimensions     */

#define MIN_PRODUCT_SIZE     (16)   /* Min/Max sizes of X & Y */
#define MAX_PRODUCT_SIZE   (3100)
#define MAX_PRODUCT_SURFACES (50)   /* CAPPI, MAX, etc., horizontal surface bound */

  SINT4 ixrad,  iyrad,  izrad;	/*  Location of radar in pixels*1000 */

  /* The following variable is only used in the version 2.0 structures with
   * version numbers 1 or less.
   */
  SINT4 irange_last_v20;	/* Range of last bin in cm (raw only) */
  char ipad128x2[2];
  UINT2 idata_out;		/* Data type of data generated by product gen */

  /* This section for version 2.1+ products
   */
  char  sprojection_name[12];	/* Projection name, null terminated */
  UINT2 idata_in;		/*  Data type used by the generator */
  UINT1 iprojection_type;	/* Projection type for map */
  char ipad147x1;
  SINT2 iradial_smooth;		/* Range in km*100 over which radial  */
				/* smoothing should be done.  0:none. */
  UINT2 iruns;			/* # of times this pcf has been run   */
  SINT4 izr_const;		/* Z-R or Z-W constant and exponent   */
  SINT4 izr_exp;		/*  in 1/1000 of integers             */
  SINT2 ix_smooth;		/* X-Y Smoothing parameters for 2D */
  SINT2 iy_smooth;		/*  products.  km*100,  0:none */

  /* ---- Product Specific Parameters ----
   * The following area conveys information needed for each specific product.
   */
  PSI psi;

#define PCF_TASK_MINOR_SIZE (16)
  char sTaskMinorList[PCF_TASK_MINOR_SIZE]; /* Null terminated list of hybrid extensions */
  char ipad260x12[12];
  struct color_scale_def colors;
} ;

typedef struct product_configuration PCF;

/* -------------------- Task Antenna Scan Information --------------------*/
#define TASK_PSCAN_INFO_SIZE (200)

/* --- RHI Azimuth List ---
 *   Starting and ending elevation angles, followed by
 *   AZ list of binary angles 
 */
struct task_rhi_scan_info
{
  UINT2 istartel;
  UINT2 iendel;
  UINT2 iazlst[MAX_SWEEPS];
#define TASK_RHI_SCAN_INFO_PAD (115)
  char ipad_end[TASK_RHI_SCAN_INFO_PAD];
#define TASK_SCAN_RHI_NEAREST  (0)
#define TASK_SCAN_RHI_LOWER    (1)
#define TASK_SCAN_RHI_UPPER    (2)
  UINT1 iStartEnd;            /* Which end of the sector to start at */
};


struct serv_task_rhi_scan_info
{
  UINT4 istartel;
  UINT4 iendel;
  UINT4 iazlst[MAX_SWEEPS];
  UINT4 iStartEnd;
};


/* --- PPI Elevation List ---
 * Actual # of items in IELLST followed by
 * EL list of binary angles
 */
struct task_ppi_scan_info
{
  UINT2 istartaz;
  UINT2 iendaz;
  UINT2 iellst[MAX_SWEEPS];
#define TASK_PPI_SCAN_INFO_PAD (115)
  char ipad_end[TASK_PPI_SCAN_INFO_PAD];
#define TASK_SCAN_PPI_NEAREST  (0)
#define TASK_SCAN_PPI_LEFT     (1)
#define TASK_SCAN_PPI_RIGHT    (2)
  UINT1 iStartEnd;  /* Which end of the sector to start at */
};

struct serv_task_ppi_scan_info
{
  UINT4 istartaz;
  UINT4 iendaz;
  UINT4 iellst[MAX_SWEEPS];
  UINT4 iStartEnd;
};

/* --- File Scan Info ---
 * First azimuth and elevation from the file, followed by
 * the file name.
 */
struct task_file_scan_info
{
  UINT2 ifirstaz;
  UINT2 ifirstel;
  char scan_file[12];		/* Space terminated */
  char ipad[184];
};   

/* --- Manual Scan Info --- 
 */
struct task_manual_scan_info
{
#define TCF_SCAN_MAN_CONT_MASK (0x0001)
#define TCF_SCAN_CONT_MASK     (0x0001)
#define TCF_SCAN_SET_AZ        (0x0002)
#define TCF_SCAN_SET_EL        (0x0004)
  UINT2 iman_flags;		/* bit 0: continuous */
#define TASK_MANUAL_SCAN_INFO_PAD (188)
  char  ipad2x2[2];
  BIN4  iStartAz;
  BIN4  iStartEl;
  char ipad_end[TASK_MANUAL_SCAN_INFO_PAD];
};

/* --- Exec Scan Info ---
 */
struct task_exec_scan_info
{
#define TASK_EXEC_SCAN_COMMAND_SIZE (160)
  char sCommand[TASK_EXEC_SCAN_COMMAND_SIZE];	/* Null terminated */
#define TASK_EXEC_SCAN_INFO_PAD (40)
  char ipad_end[TASK_EXEC_SCAN_INFO_PAD];
};   

struct serv_task_file_scan_info
{
  UINT4 ifirstaz;
  UINT4 ifirstel;
  char scan_file[12];		/* Space terminated */
};   

struct serv_task_manual_scan_info
{
  UINT4 iman_flags;		/* bit 0: continuous */
  BIN4  iStartAz;
  BIN4  iStartEl;
};


#define TASK_SCAN_INFO_SIZE  320
/* The following Union contains scan information, the format of which varies
 * according to the scan mode.
 */
union task_scan_info_u
{
  struct task_rhi_scan_info rhi;
  struct task_ppi_scan_info ppi;
  struct task_file_scan_info fil;
  struct task_manual_scan_info man;
  struct task_exec_scan_info exec;
} ;

/* -------------------- Task Range Selection Information --------------------*/

#define TASK_RANGE_INFO_SIZE  160
struct  task_range_info 
{
  SINT4 ibin_first;		/* Range of first (input) bin in cm */
  SINT4 ibin_last;		/* Range of last (input) bin in cm */
  SINT2 ibin_in_num;		/* Number of input bins */
  SINT2 ibin_out_num;		/* Number of output bins */

  SINT4 ibin_in_step;		/* Step between input bins in cm */
  SINT4 ibin_out_step;		/* Step between output bins in cm */

  /* If IBIN_VAR is zero the input bins are equally spaced, in which
   * case the input and output bin spacings and any bin averaging are
   * defined.  If IBIN_VAR is non-zero, then it indicates that some
   * type of variable input spacing has been selected.  In that case,
   * there are (up to) 48 bytes reserved to describe the variable
   * format specifically.
   */
  UINT2 ibin_var;		/* Non-Zero ==> variable resolution */

  SINT2 ibin_avg;		/* 0:No Avg,  1:Avg Pairs, ... */
  SINT2 ibin_smooth;		/* 0:No Avg,  1:Avg Pairs, ... */
#define TASK_RANGE_INFO_PAD (134)
  char ipad_end[TASK_RANGE_INFO_PAD];
};


struct task_scan_info
{
  UINT2 iscan;			/* Antenna Scan Mode (TASK_SCAN_xxx) */

  /* IRES is the desired angular resolution expressed as an integer
   * number of thousandths of degrees.  This format was chosen (rather
   * than binary angle) so that user-units of resolution could be
   * expressed exactly.  In manual scans, this is the number of rays
   * to record.
   */
  SINT2 ires1000;

  /* In the PPI scan modes, ISCAN_SPEED indicates the azimuth rotation
   * rate in binary angles per second.  If this value is initially
   * zero, then INGEST will calculate a scan rate based on the other
   * task parameters and overwrite the zero with the computed
   * value.
   */
  BIN2 iscan_speed ;

  SINT2 isweeps;		/* # Sweeps to perform */

  union task_scan_info_u u;

#define TASK_SCAN_INFO_PAD (112)
  char ipad_end[TASK_SCAN_INFO_PAD];
};

/* The following Union contains scan information, the format of which
 * varies according to the scan mode.
 */
union serv_task_scan_info_u
{
  struct serv_task_rhi_scan_info rhi;
  struct serv_task_ppi_scan_info ppi;
  struct serv_task_file_scan_info fil;
  struct serv_task_manual_scan_info man;
  struct      task_exec_scan_info exec;
} ;

struct serv_task_scan_info
{
  UINT4 iscan;			/* Antenna Scan Mode */

  SINT4 ires1000;
  BIN4  iscan_speed ;
  SINT4 isweeps;		/* # Sweeps to perform */

  union serv_task_scan_info_u u ;
};

struct product_end
{
  char sprod_sitename[16];	/* Name of product generator site, space padded */
  char sprod_version[8];	/* Product IRIS version, null terminated */
  char sing_version[8];		/* Ingest IRIS version, null terminated */
    
  struct ymds_time old_ing;	/* Oldest data in this file */
  char ipad44x28[28];

  SINT2 iLocalWest;             /* # minutes local standard time is west of GMT */
  char  sIngHardwareName[16];	/* Hardware name of ingest site, space padded */

  /* Information about the radar site.  Latitude and Longitude are stored as 
   * 32-bit binary angles, where 20000000 Hex is 45 degrees North Latitude or 
   * East Longitude.  For products of many tasks,
   * these are taken from the first site.  For track product, these are taken
   * from the site making the track.
   */
  char  sing_sitename[16];	/* Name of ingest site, space padded */
  SINT2 iMinutesWest;		/* # minutes recorded standard time is west of GMT */
  BIN4  ilat_center;		/* Latitude of center of product */
  BIN4  ilon_center;		/* Longitude of center of product */
  SINT2 ignd_hgt;		/* Ground height (m above sea level) */
  SINT2 irad_hgt;		/* Radar height (m above ground) */

  /* Signal processing related numbers.  These are mostly copied from the task
   * configuration structure.
   */
  SINT4 iprf;			/* PRF in hertz */
  SINT4 ipw;			/* Pulse width in 1/100 usec */
  UINT2 idsptype;
  UINT2 itrig;			/* Trigger rate scheme */
  SINT2 isamp;			/* # samples used */
  char  sfilter[12];		/* Clutter filter file name */
  UINT2 idop_filter_first;	/* Doppler filter used on first bin */
  SINT4 ilambda;		/* Wavelength in cm*100 */
  SINT4 itrunc;			/* Truncation height (cm above the radar) */
  SINT4 ibin_first;		/* Range of first bin in cm */
  SINT4 ibin_last;		/* Range of last bin in cm */
  SINT4 ibin_out_num;		/* Number of output bins */

  SINT2 iPEFlags;		/* Was iflags */

  /* If the Disdrometer was selected as the Z/R source, but it was not
   * available, then we switch to setup source, and set this bit.
   */
#define PEF_ZR_FALLBACK (0x0001)

  SINT2 input_cnt;              /* Number of ingest and product files
				 * processed to produce this product
				 * (only for accumulating products). */

  UINT2 ipolar;			/* Type of polarization, see dsp_lib.h */
  SINT2 iI0Horiz;               /* I0 number from calibration, 1/100 of dB */
  SINT2 iCalNoiseHoriz;         /* Noise at calibration time, 1/100 of dBm */
  SINT2 iRadarConstantHoriz;    /* Radar constant, 1/100 of dB */
  UINT2 iReceiverBandwidth;     /* Receiver bandwidth, in kHz */
  SINT2 iActNoiseHoriz;         /* Noise level, 1/100 of dBm */

  char ipad184x28[28];

  BIN4  iStdParallel1; /* Standard parallels used in Lambert Conformal Conic */
  BIN4  iStdParallel2;
  UINT4 iEarthRadius;  /* Earth's equitorial radius in cm */
  UINT4 iFlattening;   /* 1/Flattening in 1/1000000 */

  /* System fault status at the time the task was started.  See the
   * 'ICFB_..." macros in ingest.h for the definition of these bits.
   */
  UINT4 iFaultBits ;

  UINT4 iSiteMask;              /* Mask of sites in the product (if PF_COMPOSITED) */
  UINT2 ilog_filter_first;	/* Log filter used on first bin */
  UINT2 lclutmap_used ;		/* Clutter map applied to DSP data */

  BIN4  ilat_ref;		/* Latitude of projection reference point */
  BIN4  ilon_ref;		/* Longitude of projection reference point */

  /* The following sequence number can be used whenever product files
   * need to be ordered or indexed in some way.  For example, when the
   * Nth product is written to a tape, the value is filled in with N.
   */
  UINT2 isequence_num;

  /* Only applicable to picture products, will be zeroed in data products
   */
  UINT2 istep_colors[16];	/* Color #'s for above steps */

  SINT2 iMeltingHeight;         /* Melting height MSB complemented (m above MSL) */
  SINT2 iReferenceOffset;	/* Height of radar above Reference in meters */

  /* The number of results elements at the end of the file.  Used for
   * WARN, SLINE, STAT, TRACK only.  In STAT products, there is always one
   * status_results structure, followed by iresults_count-1 status_one_message
   * structures.
   */
  SINT2 iresults_count;

  /* Wind speed and direction, if available.  Set to zero if unknown.  If
   * the wind speed is known, but is zero, then set direction to 255.  Speed
   * is in nearest 0.5 m/s, and direction is binary angle.
   */
  UINT1 iWindSpeed;
  UINT1 iWindDirection;
  char  ipad290x2[2];
  char  sLocalTZName[8];         /* Name of timezone of local time */
  char  ipad300x8[8];
};

struct product_hdr
{
  struct structure_header      hdr;     /* Generic Header */
  struct product_configuration pcf;     /* Product Config Info */
  struct product_end           end;     /* Product end info */
};

typedef struct product_hdr PHD;

/* -------------------- Task Miscellaneous Information -------------------- */

#define TASK_MISC_INFO_SIZE  320
struct task_misc_info 
{
  SINT4 ilambda;		/* Radar wavelength in cm*100 */
  char str_id[16];		/* User's id of xmitter, receiver, etc, space term. */
  SINT4 ixmt_pwr;		/* Transmit power in watts */

  UINT2 iflags;
#define TASK_MSC_DSSIM       0x0001	/* Digital Signal Simulator in use */
#define TASK_MSC_PARTIAL     0x0002	/* Volume scan was halted prematurely */
#define TASK_MSC_KEEP        0x0010	/* Keep this file (WATCHDOG info) */
#define TASK_MSC_CLUTMAP     0x0020	/* File tagged as a clutter map (INGEST info) */

  UINT2 ipolar;			/* Type of polarization, see dsp_lib.h */
  SINT4 itrunc;			/* Truncation height (cm above radar) */
  char  ipad32x18[18];		/* Reserved for polarization description */

  char ipad50x12[12];

  /* Users comments are associated with tasks.  The comment buffer
   * itself is part of the Task Configuration Structure, and the
   * number of bytes inserted in that buffer so far is given below.
   */
  SINT2 icomnt_num;		/*# Bytes of comments */
  BIN4  iHorzBeamWidth;         /* Horizontal full beamwidth */
  BIN4  iVertBeamWidth;         /* Vertical full beamwidth */
  UINT4 iUser[10];              /* Space for users to put things */
#define TASK_MISC_INFO_PAD (208)
  char ipad_end[TASK_MISC_INFO_PAD];
};

/* -------------------- Task ending structure --------------------------- 
 * Remainder of structure (up to the comment buffer section) must consist of
 * no more than TASK_CONF_END_SIZE bytes. 
 */

#define TASK_CONF_END_SIZE  320 /* Size of struct's end area: */

struct task_end_info
{
  /* The task number consists of a "major" and "minor" number.  The
   * major number is what we usually refer to the task by.  The minor
   * number is used for "hybrid" tasks i.e., when more than one task
   * configuration is used to define an overall task.  If the minor
   * number is zero, then there are no additional tasks. Otherwise the
   * minor numbers represent an ordering of a set of task
   * configurations, all of which have the same major number. The
   * minor numbers always run from 1...N, where N is the total number
   * of tasks involved in this hybrid.
   */
  SINT2 id_major, id_minor;

  /* There are two character strings associated with a task.  One is
   * the task name, i.e., file name of the .TCO file.  The other is an
   * optional brief description of what the task does.  Both strings
   * are end-padded with spaces.
   */
  char stname[12]; 
  char sdscript[80];

  SINT4 ihybrid_count;		/* Number of tasks in this hybrid set */

  /* STATE is the current state of the task.  It can be modified by the menus
   * or INGEST processes.
   */
  UINT2 istate;
#define TASK_VOID         0	/* No task defined here (empty slot) */
#define TASK_MODIFY       1	/* Being modified by someone */
#define TASK_INACTIVE     2	/* Exists, but not used in any way */
#define TASK_SCHED        3	/* Waiting to run */
#define TASK_RUNNING      4	/* Running */

  char  ipad102x2[2];
  struct ymds_time ing_time;    /* Task time, set when running */

#define TASK_END_INFO_PAD (204)
  char ipad_end[TASK_END_INFO_PAD];

};

  /* -------------------- Task Scheduling Information --------------------
   */
#define TASK_SCHED_INFO_SIZE  120

struct task_sched_info
{
  /* There are six times stored for each task.  All times are in
   * seconds offset from the start of a 24-hour day.  Invalid times
   * are indicated by a value of -1.
   *
   * START and STOP are absolute times that define when the task can
   * be running at all.  SKIP is the desired time between individual
   * runs. Each time a task is run the beginning time is stored in
   * LAST_RUN.  When a run finishes, the time used is written to
   * TIME_USED.  LAST_RUN_DAY is a relative day number that is used to
   * resolve very old last run times.
   */
  SINT4 istart;			/* Start time for task */
  SINT4 istop;			/* Stop time for task */
  SINT4 iskip;			/* Desired skip between runs */
  SINT4 ilast_run_sec;		/* Time that task was last run (seconds within the day) */
  SINT4 itime_used_sec;		/* Time used on last run (seconds)*/
  
  SINT4 ilast_run_day;		/* Day that task was last run (days since 1970) */
  
  /* The flag word modifies the scheduling details.
   *  * ASAP indicates that the scheduling times should be ignored and that the
   *    task should be run once as soon as possible, after which the ASAP bit 
   *    will be cleared.
   *  * MAND indicates that the task can preempt a non-MAND task that is already
   *    running, and also defines a separate level of priority for normal time-
   *    based scheduling.
   *  * LSKIP is a late skip flag which indicates that the task must run within
   *    a certain tolerance of its expected starting times, else it will be 
   *    skipped.
   *  * STOP indicates to INGEST that the task should be descheduled after 
   *    running it, i.e., its state should go to INACTIVE, rather than SCHEDULED.
   */
  UINT2 iflag;
#define TASK_SCH_ASAP      (0x0001) /* Start as_soon_as_possible */
#define TASK_SCH_MAND      (0x0002) /* Task has mandatory status */
#define TASK_SCH_LSKIP     (0x0004) /* Late Skip */
#define TASK_SCH_MESTU     (0x0008) /* TIME_USED is measured, else estimated. */
#define TASK_SCH_DESCHED   (0x0010) /* Deschedule task after running it. */
#define TASK_SCH_INTR      (0x0020) /* Interrupt this task right now */
#define TASK_SCH_FLIP      (0x0040) /* Offer to flip control to other system */

#define TASK_SCHED_INFO_PAD (94)
  char ipad_end[TASK_SCHED_INFO_PAD];
};

/* -------------------- Task DSP Information -------------------- */

/* The following Union contains dsp configuration information, the format of 
 * which varies according to the major mode.
 */

#define TASK_DSP_MODE_SIZE  32
struct task_dsp_mode_batch
{
  UINT2 iLoPrf;          /* Low PRF in Hz */
  UINT2 iLoPrfFract;     /* Low PRF fractions of Hz */
  SINT2 iLoSampleSize;   /* Sample size for low PRF */
  SINT2 iLoAvgSide;      /* Averaging applied to low PRF */
  SINT2 idBOverRefl;     /* Reflectivity dB threshold (1/100 dB)*/
  SINT2 idBOverVel;      /* Velocity dB threshold (1/100 dB)*/
  SINT2 idBOverWid;      /* Width dB threshold (1/100 dB ) */
#define TASK_DSP_MODE_BATCH_PAD (18)
  char ipad_end[TASK_DSP_MODE_BATCH_PAD];
};
struct task_dsp_mode_other
{
  SINT2 imisc[TASK_DSP_MODE_SIZE/2];
};
union task_dsp_mode_u
{
  struct task_dsp_mode_batch batch;
  struct task_dsp_mode_other other;
} ;

/* Structure holding information about data types.  This is somewhat
 * awkward to deal with backwards compatibility.
 */
struct dsp_data_mask
{
  UINT4 iWord0;
  UINT4 iXhdrType;
  UINT4 iWord1;
  UINT4 iWord2;
  UINT4 iWord3;
  UINT4 iWord4;
};

#define TASK_DSP_INFO_SIZE  320
struct task_dsp_info
{
  UINT2 imajor_mode ;		/* DSP mode (from dsp_lib.h) */

  UINT2 idsptype ;		/* DSP type (from dsp_lib.h) */

  /* The type(s) of data being recorded by the DSP are summarized in
   * DataMask.  IDATA is an array of 160 bit positions,
   * indicating which types of data are to be ingested.
   * For DB_XHDR, the iXhdrType gives information about which type
   * of extended header data is recorded.
   * For definitions see the dsp_lib.h file.
   */
  /* UINT4 idata; */
  struct dsp_data_mask DataMask;

  /* We can optionally block recording of some data types when
   * making the RAW product.  This records the original data types.
   * Unused if all zero.  Added on 2-Nov-2004 in release 8.07.
   */
  struct dsp_data_mask OriginalDataMask;

  union task_dsp_mode_u u;
  char  ipad_84x52[52];

  /* Trigger and related Info 
   */
  SINT4 iprf;			/* PRF in hertz */
  SINT4 ipw;			/* Pulse Width in 1/100 usec. */

  UINT2 itrig;			/* Trigger rate flag, see dsp_lib.h */

  SINT2 idual_delay;		/* Stabilization # pulses for multi-PRF */
  UINT2 iagc_code;		/* Selected coefficient for AGC feedback */

  /* Miscellaneous
   */
  SINT2 isamp;			/* # Pulses used (Sample Size) */

  UINT2 igain;
#define GAIN_FIXED     0	/* Fixed receiver gain */
#define GAIN_STC       1	/* STC Gain */
#define GAIN_AGC       2	/* AGC Gain */

  /* File defining clutter filter.  If the file name is blank, then
   * the first filter code is used at all ranges.
   */
  char sfilter[12];		/* Space terminated */
  UINT1 idop_filter_first;	/* Doppler based filter used on first bin */
  UINT1 ilog_filter_first;	/* Z based filter (unused, for legacy RVP6 data only) */

  SINT2 ifixed_gain;		/* 1000 * fixed gain level (0-1) */

  UINT2 igas_atten ;		/* 100000 * db/km, up to 10000, thereafter 10 times slope */

  UINT2 lclutmap_used ;		/* Clutter map applied to DSP data */

  UINT2 iXmtPhaseSequence;      /* Phase sequence to use, if major_mode=PMODE_RPH */

  UINT4 iCfgHdrMask;            /* Mask sent to CFGHDR command in signal processor */
  UINT2 iFlagsTS;               /* Time series playback related flags, see OPTS_* */
  char  ipad_182x2[2];

  /* If the extended header number above is 2, then use this custom header name. */
  char  sCustomRayHeader[16];
 
#define TASK_DSP_INFO_PAD (120)
  char ipad_end[TASK_DSP_INFO_PAD];
};

/* ----------- Task Calibration and Thresholding Information ----------------- */

#define TASK_CALIB_INFO_SIZE  320
struct task_calib_info
{
  /* Various calibration slopes and thresholds
   */
  SINT2 iz_slope;		/* LOG slope in dB*4096/ A/D count */
  
  SINT2 izns_thr;		/* LOG noise threshold in dB*16 */
  SINT2 iccr_thr;		/* Clutter Correction threshold in dB*16 */
  SINT2 isqi_thr;		/* SQI threshold * 256 */
  SINT2 isig_thr;		/* Signal power threshold in dBm*16 */
  char  ipad10x8[8];
  SINT2 iz_calib;		/* Calibration reflectivity, dBZ*16 @ 1Km */

  /* Threshold control flags for various parameters.
   */
  UINT2 iuz_tcf;		/* UnCorrected Z flags */
  UINT2 icz_tcf;		/* Corrected Z flags */
  UINT2 ivl_tcf;		/* Velocity flags */
  UINT2 iwd_tcf;		/* Width flags */
  UINT2 izdr_tcf;		/* ZDR flags */
  char  ipad30x6[6];

  /* Miscellaneous processing flags.
   */
  UINT2 iflags;
#define TCF_CAL_ZSPECKLE      0x0001 /* Log channel speckle remover ON */
#define TCF_CAL_VSPECKLE      0x0004 /* Linear channel speckle remover ON */
#define TCF_CAL_RANGENORM     0x0010 /* Data is range normalized */
#define TCF_CAL_BEG_PULSE     0x0020 /* DSP issues pulse at beginning of rays */
#define TCF_CAL_END_PULSE     0x0040 /* DSP issues pulse at end of rays */
#define TCF_CAL_VAR_PULSES    0x0080 /* DSP varies # pulses in Dual PRF mode */
#define TCF_CAL_3LAG_WIDTHS   0x0100 /* Use 3-lag Doppler processing (else 2) */
#define TCF_CAL_SHIP_COR      0x0200 /* Velocities corrected for ship motion */
#define TCF_CAL_VC_UNFOLD     0x0400 /* Vc has unfolding via VVP */
#define TCF_CAL_CVEL_UNFOLD   0x0400 
#define TCF_CAL_VC_FALLSPD      0x0800 /* Vc has fallspeed correction */
#define TCF_CAL_CVEL_FALLSPD    0x0800
#define TCF_CAL_ZC_BEAMBLOCK    0x1000 /* Zc has Partial beam blockage correction */
#define TCF_CAL_BEAMBLOCK       0x1000
#define TCF_CAL_ZC_ATTENUATION  0x2000 /* Zc has Intervening attenuation correction */
#define TCF_CAL_ATTENUATION     0x2000 
#define TCF_CAL_ZC_TARGET_DET   0x4000 /* Zc has Target Detection correction */

  char ipad38x2[2] ;

  SINT2 ildr_bias;		/* LDR bias in dB*100 (XDR) */  
  SINT2 izdr_bias;		/* ZDR bias in dB*16  (GDR) */

  SINT2 iPointClutterThreshold; /* Threshold in dB*100 */
  UINT2 iPointClutterSkip;      /* Side skip in low 4 bits, 0=feature off */

  SINT2 iI0Horiz;               /* I0 number from calibration, 1/100 of dB */
  SINT2 iI0Vert;
  SINT2 iCalNoiseHoriz;         /* Noise at calibration time, 1/100 of dBm */
  SINT2 iCalNoiseVert;
  SINT2 iRadarConstantHoriz;    /* Radar constant, 1/100 of dB */
  SINT2 iRadarConstantVert;
  UINT2 iReceiverBandwidth;     /* Receiver bandwidth, in kHz */

#define TASK_CALIB_INFO_PAD (258)
  char ipad_end[TASK_CALIB_INFO_PAD];
};

struct serv_task_calib_info
{
  /* Various calibration slopes and thresholds
   */
  SINT4 iz_slope;		/* LOG slope in dB*4096/ A/D count */
  
  SINT4 izns_thr;		/* LOG noise threshold in dB*16 */
  SINT4 iccr_thr;		/* Clutter Correction threshold in dB*16 */
  SINT4 isqi_thr;		/* SQI threshold * 256 */
  SINT4 isig_thr;		/* Signal power threshold in dBm*16 */
  SINT4 iz_calib;		/* Calibration reflectivity, dBZ*16 @ 1Km */

    /* Threshold control flags for various parameters.
     */
  UINT4 iuz_tcf;		/* UnCorrected Z flags */
  UINT4 icz_tcf;		/* Corrected Z flags */
  UINT4 ivl_tcf;		/* Velocity flags */
  UINT4 iwd_tcf;		/* Width flags */
  UINT4 izdr_tcf;		/* ZDR flags */

  /* Miscellaneous processing flags.
     */
  UINT4 iflags;
  SINT4 iPointClutterThreshold; /* Threshold in dB*100 */
  UINT4 iPointClutterSkip;      /* Side skip in low 4 bits, 0=feature off */
};


/* -------------------- Task Configuration Structure -------------------- */

#define TASK_COMNT_SIZE     720          /* Size of Comment buffer */

#define TASK_CONFIG_SIZE ( STRUCT_HEADER_SIZE + TASK_SCHED_INFO_SIZE + \
   TASK_DSP_INFO_SIZE  + TASK_CALIB_INFO_SIZE + TASK_RANGE_INFO_SIZE + \
   TASK_SCAN_INFO_SIZE + TASK_MISC_INFO_SIZE  + TASK_CONF_END_SIZE   + \
   TASK_COMNT_SIZE )

struct task_configuration 
{
  struct structure_header    hdr; 
  struct task_sched_info     sch;
  struct task_dsp_info       dsp;
  struct task_calib_info     cal;
  struct task_range_info     rng;
  struct task_scan_info      scan;
  struct task_misc_info      misc;
  struct task_end_info       end;

  /* The comment buffer is the last part of the Task Configuration
   * Stucture.
   */
  UINT1  comnts[ TASK_COMNT_SIZE ];
};
typedef struct  task_configuration TCF;


struct gparm {
  UINT2 irev_ser ;		/* 01: Rev/Serial # */
  UINT2 ibin_out_num ;		/* 02: # range bins output by processor */
  UINT2 iprt_mes ;		/* 03: Current trigger period */
  UINT2 itaga ;			/* 04: Tag bits 00 - 15 */
  UINT2 itagb ;			/* 05: Tag bits 16 - 31 */
  UINT2 log_nse ;		/* 06: Log Channel Noise */
  SINT2 i_nse_ ;		/* 07: I Channel Noise */
  SINT2 q_nse_ ;		/* 08: Q Channel Noise */
  UINT2 istat_l ;		/* 09: Latched status */
  UINT2 istat_i ;		/* 10: Immediate status */
  UINT2 idiag_a ;		/* 11: Diagnostic Results A */
  UINT2 idiag_b ;		/* 12: Diagnostic Results B */
  UINT2 isamp ;			/* 13: # pulses / ray */
  UINT2 itrg_cnt_a ;		/* 14: Low 16 bits of trigger count */
  UINT2 itrg_cnt_b ;		/* 15: High 8 bits of trigger count */
  UINT2 iaqbins ;		/* 16: # properly acquired bins */
  UINT2 iprbins ;		/* 17: # properly processed bins */
  UINT2 istat_i2 ;		/* 18: Immediate Status (second word) */
  UINT2 inse_rng ;		/* 19: Noise range in Km */
  UINT2 inse_prt ;		/* 20: Noise period */
  UINT2 ipwmin_0 ;		/* 21: Pulse width 0 min trigger period */
  UINT2 ipwmin_1 ;		/* 22: Pulse width 1 min trigger period */
  UINT2 ipwmin_2 ;		/* 23: Pulse width 2 min trigger period */
  UINT2 ipwmin_3 ;		/* 24: Pulse width 3 min trigger period */
  UINT2 ipw_bits ;		/* 25: Pulse width bit patterns */
  UINT2 ipw_now ;		/* 26: Current pulsewidth choice */
  UINT2 iprt_gen ;		/* 27: Current trigger generator period */
  UINT2 iprt_des ;		/* 28: Desired trigger generator period */
  UINT2 iprt_start ;		/* 29: Measured PRT at start of last ray */
  UINT2 iprt_end ;		/* 30: Measured PRT at end of last ray */
  UINT2 iflags ;		/* 31: Processing/threshold flags */
  SINT2 iz_slope ;		/* 32: LOG conversion Slope */
  SINT2 izns_thr ;		/* 33: LOG noise threshold */
  SINT2 iccr_thr ;		/* 34: Clutter correction threshold */
  UINT2 isqi_thr ;		/* 35: SQI threshold */
  SINT2 isig_thr ;		/* 36: Signal Power threshold */
  SINT2 iz_calib ;		/* 37: Calibration Reflectivity */
  UINT2 iqi_now ;		/* 38: (Q,I) current A/D sample */
  UINT2 iz_now ;		/* 39: LOG current A/D sample */
  UINT2 ibin_avg ;		/* 40: Range averaging choice (0:None, 1:Pairs, etc) */

  UINT2 idiag_c ;		/* 41: Reserved for future diagnostic bits */
  UINT2 idiag_d ;		/* 42: Reserved for future diagnostic bits */

  UINT2 iproc_hdr0 ;		/* 43: Bits 0-15 defining header of PROC rays */

  UINT2 isq_lo ;		/* 44: Noise sample average value of I squared, */
  SINT2 isq_hi ;		/* 45:   expressed as a 32-bit quantity. */
  UINT2 qsq_lo ;		/* 46: Same for Q... */
  SINT2 qsq_hi ;		/* 47:  */

  SINT2 zlin_noise ;		/* 48: Linearized LOG power noise sample mean */
  SINT2 zlin_rms  ;		/* 49: Linearized LOG power noise root-mean-square */

  SINT2 inse_hv_ratio ;		/* 50: Horiz/Vert noise ratio in dB*100 */

  SINT2 iafclevel ;		/* 51: Signed 16-bit AFC level */

  UINT2 intflt ;		/* 52: Interference filter (bits  3:0) */
				/*            Minor Version (bits  7:4) */
				/*       IFD Sat.Power Code (bits 11:8) */
  SINT2 intflt_p1 ;		/* 53: Interference filter parameter #1 */
  SINT2 intflt_p2 ;		/* 54: Interference filter parameter #1 */

  UINT2 istat_i3 ;		/* 55: Immediate Status (third word) */
  SINT2 itrigslew ;		/* 56: Burst tracking slew (usec * 100) */

  UINT2 iPolFlags ;		/* 57: Polarization flags */
#define POLFLAG_TX_H     0x0001	/*       Use H transmissions for (T,Z,V,W) */
#define POLFLAG_TX_V     0x0002	/*       Use V transmissions for (T,Z,V,W) */
#define POLFLAG_RX_CO    0x0004	/*       Use Co-Pol reception for (T,Z,V,W) */
#define POLFLAG_RX_CX    0x0008	/*       Use Cross-Pol reception for (T,Z,V,W) */
#define POLFLAG_CNOISE   0x0010	/*       Correct all polar params for noise */
#define POLFLAG_FILTER   0x0020	/*       Use filtered data for all polar params */
#define POLFLAG_PHISIGN  0x0040	/*       Sign convention for PHIdp */

  UINT2 iMaskSpacingCM ;	/* 58: Range mask spacing (cm) for current pulsewidth */

  UINT2 istat_i4 ;		/* 59: Immediate Status (fourth word) */

  UINT2 unused_word_60 ;	/* 60:  */
  UINT2 unused_word_61 ;	/* 61:  */
  UINT2 unused_word_62 ;	/* 62:  */
  UINT2 unused_word_63 ;	/* 63:  */
  UINT2 unused_word_64 ;	/* 64:  */
} ;


#define INGEST_CONFIGURATION_SIZE  (480)    /* Size of Site Information */

struct ingest_configuration
{
  char sfile[80];                       /* Name of File on disk */

  /* IDFNUM indicates the current number of data files that are associated with 
   * this summary file.  As sweeps are performed and more data files are added
   * to the INGEST directory, IDFNUM is bumped accordingly.  Likewise, IDFSIZE
   * tallies up the total size in bytes of those data files.
   */
  SINT2 idfnum;                      /* # of associated data files extant */
  SINT2 isweeps_done ;		     /* # of sweeps that have been completed */
  SINT4 idfsize;                     /* Total size of those files */

  /* Date and time that the Volume Scan was started.  This is not necessarilly
   * equal to the starting time of the first sweep, but is always .LE. to it.
   */
  struct ymds_time VolumeYmds;
  char ipad100x12[12];

  /* Sizes of various things
   */
  SINT2 ib_rayhed;                   /* # bytes in ray headers */
  SINT2 ib_xhdr;                     /* # bytes in Extended Header */
  SINT2 ib_task;                     /* # bytes in task config table */
  SINT2 iPlaybackVersion;            /* Playback version number */
  char ipad120x4[4];
  char siris_version[8];             /* Null terminated */
  char sHardwareName[16];            /* Ingest hardware name */
  SINT2 iLocalWest;                  /* # minutes local standard time is west of GMT */

  /* Information about the radar site.  Latitude and Longitude are stored as 
   * 32-bit binary angles, where 20000000 Hex is 45 degrees North Latitude or 
   * East Longitude. 
   */
  char  sSitename[16];
  SINT2 iMinutesWest;                /* # minutes recorded standard time is west of GMT */
  BIN4  ilat, ilon;                  /* Latitude and Longitude */
  SINT2 ignd_hgt;                    /* Ground height (m above sea level) */
  SINT2 irad_hgt;                    /* Radar height (m above ground) */

  /* Information about the organization of the ray pointers in the data files
   * for this summary file.  The ray pointers at the beginning of each INGEST
   * data file point to IRTOTL rays whose nominal angles are taken from the set
   * of INREV equally spaced angles around the circle starting from zero degrees.
   * ISNDX is an index in the range [0 - INREV-1] that specifies the nominal angle
   * of the first ray.  For some types of scans the pointer table is not sorted
   * by angle, but rather by order of arrival of the rays.  In these cases INREV
   * and ISNDX will be zero.
   */
  SINT2 inrev;                       /* Number of rays in one revolution. */
  SINT2 isndx;                       /* Implies angle of first pointer */
  SINT2 irtotl;                      /* Total number of rays */

  SINT2 ib_gparm;                    /* # bytes in each GPARM */
  SINT4 ialtitude;                   /* Altitude of radar (cm above sea level) */
  SINT4 inu_vel[3];

  /* The offset between the INU and the radar pedestal on moving
   * platform system.
   */
  SINT4 inu_offset[3];

  /* System fault status at the time the task was started
   */
  UINT4 iFaultBits ;
#define ICFB_BITE_FAULT      (0x00000001) /* Normal BITE fault */
#define ICFB_BITE_CRITICAL   (0x00000002) /* Critical BITE fault */
#define ICFB_RCP_FAULT       (0x00000004) /* Normal RCP fault */
#define ICFB_RCP_CRITICAL    (0x00000008) /* Critical RCP fault */
#define ICFB_SYS_CRITICAL    (0x00000010) /* Critical system fault */
#define ICFB_PRODUCT_FAULT   (0x00000020) /* Product generator faulted */
#define ICFB_OUTPUT_FAULT    (0x00000040) /* Output master faulted */

  SINT2 iMeltingHeight;                /* Melting height (cm above MSL), msb complemented */
  char ipad222x2[2] ;
  char sLocalTZName[8];                /* Null terminated local Timezone name */
  UINT4 iIcfFlags;
#define ICF_FLG_ANGLE_EDGE   (0x00000001) /* First ray not centered on zero */
#define ICF_DPOLAPP_CONFIG_NAME_SIZE (16)
  char sDpolappConfigName[ICF_DPOLAPP_CONFIG_NAME_SIZE]; /* Null terminated configuration name */

  char ipad_end[228] ;
};
typedef struct ingest_configuration ICF;

struct  ingest_header
{
  struct structure_header hdr;
  struct ingest_configuration icf;
  struct task_configuration tcf;
  char ipad_3104x732[732];

  struct gparm GParm;  /* Read from the RVP just after configuration */
  char ipad_end[920];
  
};
typedef struct ingest_header IHD;

#define INGEST_DATA_HEADER_SIZE (STRUCT_HEADER_SIZE + 64)

struct ingest_data_header {

  struct structure_header hdr ;

  struct ymds_time time ;	/* Date and time that sweep was started */
            
  SINT2 isweep ;		/* Sweep number, origin 1 */

/* The following three variables are copies of variables of the same name from
 * the ingest_configuration structure.  There are included here for convenience when
 * processing data files.
 */
  SINT2 inrev ;			/* Number of rays in one revolution. */
  SINT2 isndx ;			/* Implies angle of first pointer */
  SINT2 irtotl ;		/* Total number of rays */

/* IWRITN gives the number of rays actually written in this file, i.e., the
 * number of non-zero ray pointers.  It is possible that not all of the pointers
 * are actually filled in, e.g., if the antenna scan speed was too fast, some
 * angles might have been missed.
 */
  SINT2 iwritn ;		/* # rays written [0 - IRTOTL] */
  BIN2 iangle ;		        /* Fixed angle for this sweep */
  SINT2 ibits_bin ;		/* bits/bin for these data */
  UINT2 idata ;			/* Data code (See Task_DSP_Info.IDATA) */

  char ipad40x36[36] ;
} ;

/* =========================================================================
 * =============== Miscellaneous Definitions ===============================
 * =========================================================================
 *
 * The following header is written at the beginning of each "sweep block" in
 * the Raw Product files.  It is included primarily for error recovery
 * in the context of retrieving those files from tape.
 */

#define RAW_PROD_BHDR_SIZE  (12)

struct raw_prod_bhdr
{
  SINT2 irec;                       /*  Record # */
  SINT2 isweep;                     /*  Sweep # */
  SINT2 iray_ptr;                   /*  Byte offset (from BHdr) of first ray */
  SINT2 iray_num;                   /*  Ray number of above first ray */
  UINT2 iflags;                     /*  Flags */
#define APB_NOGOOD    (0x0001)	    /*    Block has invalid data */
  UINT1 ipad10x2[2];
};

/* Blocks of sweep data in the Raw file look like the following */

#define RAW_PROD_SDATA_SIZE (TAPE_RECORD_LEN - RAW_PROD_BHDR_SIZE)

struct raw_prod_block
{
  struct raw_prod_bhdr hdr;
  UINT1 ibytes[ RAW_PROD_SDATA_SIZE ];
};

/* -------------------- Ray Headers --------------------
 *
 * All radar rays are prefixed with the following short header.  The idea is
 * that this is sufficient for most products, and thus takes up the least amount
 * of space.  Products which require additional ray information must arrange to
 * have the "Extended Header" recorded along with the rest of the data.  Note
 * that even the "Extended Header" has this ray header attached.
 */
#define RAY_HSIZE (12)

struct ray_header {

/* Antenna positions at the start and end of the ray are stored as 16-bit binary
 * angles.
 */
  BIN2 iaz_start, iel_start ;	/* AZ and EL at start of ray */
  BIN2 iaz_end  , iel_end   ;	/* AZ and EL at end of ray */

  SINT2 ibincount ;		/* Actual number of bins */
  UINT2 itime ;			/* Time offset in seconds, unsigned */

} ;

/* -------------------- Extended Header Format --------------------
 *
 * The "Extended Header" type can be selected just like any other radar data
 * type, and has one of the following formats.  Note that AZ and EL are not
 * included in the Extended Header, because they are already in the ray header
 * that accompanies it.  The reason for supporting several formats is to allow
 * a hook for a potentially large amount of additional information per ray.
 *
 * Extended Header rays are unlike Radar Data rays in that there is no range
 * index on the information, i.e., there is just one set of words regardless
 * of the amount of other data being collected per ray.  As a consequence, the
 * size of the structure is known at compile time.
 */

/* --- Version 0 Extended Header, and Extended Header Ray ---
 */
#define EXT_RHED_V0_SIZE (20)
struct extended_header_v0 {

/* The exact time that the ray was acquired is stored to the nearest 
 * millisecond.  The time is relative to the sweep starting time given 
 * in the Ingest_Data_Header.
 */
  SINT4 itimems ;		/* Time ray was initiated */
  SINT2 icallevel ;		/* Calibration signal level */
  char ipad6x14[14] ;
} ;

/* --- Version 1 Extended Header, and Extended Header Ray ---
 */
#define EXT_RHED_V1_SIZE (54)
struct extended_header_v1 {

/* Information about the radar site.  Latitude and Longitude are
 * stored as 32-bit binary angles, where 20000000 Hex is 45 degrees
 * North Latitude or East Longitude.  All heights are in meters; all
 * velocities are in cm/sec; all angles are binary angles, all angular
 * velocities are in binary angles/second.
 */

/* The exact time that the ray was acquired is stored to the nearest 
 * millisecond.  The time is relative to the sweep starting time given 
 * in the Ingest_Data_Header.
 */
  SINT4 itimems ;		/* Time ray was initiated */
  SINT2 icallevel ;		/* Calibration signal level */

  BIN2 iaz, iel ;
  BIN2 itrain, ielor ;
  BIN2 ipitch ;
  BIN2 iroll ;
  BIN2 iheading;

  BIN2 ivel_az ;
  BIN2 ivel_el ;
  BIN2 ivel_pitch ;
  BIN2 ivel_roll ;
         
  BIN4 ilatitude, ilongitude ;	/* Latitude and Longitude */
  BIN2 ivel_heading ;

  SINT2 ialtitude ;		/* Radar height (m above MSL) */
  SINT2 ivel_east ;		/* Radar Velocity East */
  SINT2 ivel_north ;		/* Radar Velocity North */
  SINT4 iupdate_age ;
  SINT2 ivel_up ;		/* Radar Velocity Up */

  UINT2 iflag ;
  SINT2 ivel_correction ;
} ;

/*
 * -------------------- Generic Ray Format --------------------
 *
 * Data rays of all types are accessable through this structure.  "Data_Ray"
 * is one of the few structures whose length is not known at compile-time.  For
 * this reason the data references in the structure are dimensioned to the max-
 * imum size in order not to generate bounds check errors.  The structure should
 * never be used to allocate storage, but rather, should become rooted at the
 * first byte of a ray in order to get at the ray's components.  This rooting can
 * be done, e.g., by passing the ray's beginning address to a routine that
 * declares its formal parameter to be a Data_Ray record.
 *
 * All data types that are not simple byte or word arrays must first be given
 * a structure definition so that the data can be indexed by range.
 */

/* Three byte time series, characteristic of PPP and RVP-5 */
struct  ppp_time_series 
{
  UINT1 iits;
  UINT1 iqts;
  UINT1 ilts;
  UINT1 izero;   /* (with one zero byte) */
};

union ray_data
{
  /* Generic byte data array */
  UINT1 iData1[MAX_BINS];
  UINT2 iData2[MAX_BINS];
  /* extended header version 0 */
  struct extended_header_v0 xh0;
  /* Extended Header Version 1 */
  struct extended_header_v1 xh1;
};

/* Finally, the Data Ray structure itself...
 */
struct  data_ray
{
  struct ray_header hdr;           /* Header goes on all data types */
  union  ray_data   data;
};

