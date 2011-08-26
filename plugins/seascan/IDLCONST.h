/*********************************************************************

	IDLCONST.h	-	Common Declarations for SeaScan clients

	(c) 1995-2003 Sigma Engineering Limited.

*********************************************************************/

#ifndef _IDLCONST
#define _IDLCONST

#ifndef _TIME_T_DEFINED
	typedef long time_t;        // time value 

#define _TIME_T_DEFINED     // avoid multiple def's of time_t
#endif

// The DLL functions report back a CLIENT_STATUS code which indicates success or failure
// of the call. "No error" indicates only that the command was successfully transmitted 
// across the network and does not necessarily imply that the server was able to 
// execute the request. The programmer should call GetServerLastError to determine
// the success or failure of the last request.

typedef ULONG CLIENT_STATUS;

#define		CLIENT_STATUS_NO_ERROR				0
#define		CLIENT_STATUS_INVALID_ARGUMENT			1
#define		CLIENT_STATUS_FUNCTION_NOT_SUPPORTED		2
#define		CLIENT_STATUS_NETWORK_ERROR			3
#define		CLIENT_STATUS_SERVER_BUSY			4
#define		CLIENT_STATUS_FUNCTION_FAILED			5
#define		CLIENT_STATUS_QUAD_ERROR			6
#define     	CLIENT_STATUS_MAX_CONNECTIONS           	7
#define     	CLIENT_STATUS_ERROR_CONNECTED               	8
#define     	CLIENT_STATUS_NO_CONNECTION			9
#define     	CLIENT_STATUS_INVALID_ID			10


#define		CLIENT_STATUS_ERR_GENERAL_FAILURE		0xFFFFFFFF

#define		MAX_CLIENTS		20
#define		MAX_SERVERS		4  // Used by ClientDLL and server
#define		MAX_SERVERS_EX		8  // Used by ClientDLL and server
#define		MAX_QUADRANTS		4
#define		MAX_SCANS_TO_AVERAGE	16
#define		METERS_PER_NMI		1852.0f

#define		MAX_BLANK_AREAS		100
#define		MAX_MASK_SIZE		(1024*1024)
#define		MAX_MASK_COORDS		1000

// Radar A/D Module Error conditions:

#define CLIENT_MODES_COUNT		21
#define CLIENT_MODE(x)	x
#define CLIENT_MODE_LBL0	"INVALID MODE"
#define CLIENT_MODE_LBL1	".25  nmi"
#define CLIENT_MODE_LBL2	".50  nmi"
#define CLIENT_MODE_LBL3	".75  nmi"
#define CLIENT_MODE_LBL4	"1.0  nmi"
#define CLIENT_MODE_LBL5	"1.5  nmi"
#define CLIENT_MODE_LBL6	"2.0  nmi"
#define CLIENT_MODE_LBL7	"3.0  nmi"
#define CLIENT_MODE_LBL8	"4.0  nmi"
#define CLIENT_MODE_LBL9	"   6  nmi"
#define CLIENT_MODE_LBL10	"   8  nmi"
#define CLIENT_MODE_LBL11	"   9  nmi"
#define CLIENT_MODE_LBL12	" 12  nmi"
#define CLIENT_MODE_LBL13	" 16  nmi"
#define CLIENT_MODE_LBL14	" 18  nmi"
#define CLIENT_MODE_LBL15	" 24  nmi"
#define CLIENT_MODE_LBL16	" 32  nmi"
#define CLIENT_MODE_LBL17	" 36  nmi"
#define CLIENT_MODE_LBL18	" 40  nmi"
#define CLIENT_MODE_LBL19	" 64  nmi"
#define CLIENT_MODE_LBL20	" 72  nmi"

#define CLIENT_MODE_RGE0	0.0f
#define CLIENT_MODE_RGE1	0.25f
#define CLIENT_MODE_RGE2	0.50f
#define CLIENT_MODE_RGE3	0.75f
#define CLIENT_MODE_RGE4	1.00f
#define CLIENT_MODE_RGE5	1.50f
#define CLIENT_MODE_RGE6	2.00f
#define CLIENT_MODE_RGE7	3.00f
#define CLIENT_MODE_RGE8	4.00f
#define CLIENT_MODE_RGE9	6.00f
#define CLIENT_MODE_RGE10	8.00f
#define CLIENT_MODE_RGE11	9.00f
#define CLIENT_MODE_RGE12	12.00f
#define CLIENT_MODE_RGE13	16.00f
#define CLIENT_MODE_RGE14	18.00f
#define CLIENT_MODE_RGE15	24.00f
#define CLIENT_MODE_RGE16	32.00f
#define CLIENT_MODE_RGE17	36.00f
#define CLIENT_MODE_RGE18	40.00f
#define CLIENT_MODE_RGE19	64.00f
#define CLIENT_MODE_RGE20	72.00f

#define CONSERVATION_MODES_COUNT	4

#define	A2D_VALID			0
#define	A2D_FATAL			101
#define	A2D_OFFLINE			102
#define	A2D_SYNCHRO			103
#define	A2D_SW_RESET			104
#define	A2D_OVERRUN			110
#define	A2D_MISS			111
#define	A2D_INVALID_DATA		120
#define	A2D_INVALID_MODE		121
#define	A2D_FAULT			122
#define	A2D_ERROR			123

// Valid data States in DATA_HEADER:

#define		OK			0
#define		TRANSITION		140
#define		CHARGING		141
#define		OVERRIDE		142
#define		OPTIONS_ERROR		143	// desired processing options are not compatible with
									// hardware configured options

//
// Definitions for tape module
//

#define		MAX_DATA_BLOCKS		50
#define		MAX_IMAGES_PER_TAPE	20000

// Access modes:

#define		BLANK_ONLY		0
#define		OVERWRITE		200
#define		APPEND			201
#define		NO_MODE			202

// Data modes:

#define		RECORD		210
#define		PLAYBACK	211

// Overall status values:

#define		TAPE_OK					0
#define		TAPE_ERROR				220
#define		TAPE_FULL				221
#define		TAPE_WAIT				222
#define		TAPE_LOST_DATA_OR_CORRUPTED_TAPE	223
#define		TAPE_INVALID_MEDIA_FOR_MODE		224

const USHORT REWINDING			=	130;
const USHORT WAITING_FOR_MEDIA		=	131;
const USHORT LOADING_CONTENTS		=	132;
const USHORT UPDATING_CONTENTS		=	133;
const USHORT FORMATTING			=	134;
const USHORT POSITIONING		=	135;
const USHORT INITIALIZING		=	136;
const USHORT DISK_ERROR			=	137;

//
// Definitions for GPS module
//
// GPS sentences:

#define		GPS_NONE	0
#define		GPS_GLL		400
#define		GPS_GGA		401
#define		GPS_RMA		402
#define		GPS_RMC		403
#define		GPS_AUTO	404

//
// Definitions for plot extraction module
//
#define MAX_PLOTS 20000


//
// Definitions for tracker module
//

#define MAX_TRACKS 4096
#define TRACK_STATUS_QUERYING		0
#define TRACK_STATUS_TRACKING		1
#define TRACK_STATUS_LOST		-1

#define UPDATE_STATUS_REFERENCE		  3
#define UPDATE_STATUS_STATIONARY	  2
#define UPDATE_STATUS_MOBILE		  1
#define UPDATE_STATUS_UNCONFIRMED	  0
#define UPDATE_STATUS_HIGHPFA		 -1
#define UPDATE_STATUS_HIGHESTPFA	 -2


//
// Definitions for client DLL module
//

#define NO_COMMAND          0
#define BIND_COMMAND        1
#define SETPROTOCOL_COMMAND 2
#define SETNAME_COMMAND     3
#define SETENDPOINT_COMMAND 4
#define CALL_COMMAND        5
#define MAX_COMPUTER_NAME_LENGTH 16


// Definitions for client arbitration
#define MAX_ACCESS_INTERVAL 600  // Maximum time (in sec) before an inactive client is flushed
                                // from the client list.


#endif  // ifndef _IDLCONST
