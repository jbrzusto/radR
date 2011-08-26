/*********************************************************************
 *								     *
 *  constants shared between radR R and C modules		     *
 *								     *
 *  USER WARNING: Do not modify or delete this file		     *
 *  unless you are recompiling the radR dynamic libraries.           *
 *  Values in this file must match those in the compiled             *
 *  dynamic libraries called by radR.                                *
 *                                                                   *
 *  The R function rss.get.shared.constants()                        *
 *  in radRutil.R reads this file, which must be distributed         *
 *  with radR.                                                       *
 *								     *
 *********************************************************************/

/* types used for radR external data matrices */

#define NUM_SAMPLE_CLASSES 5            // the number of sample classes, and so the number of palettes
#if BITS_PER_SAMPLE <= 8
#define T_SAMPLE_TYPE unsigned char	// the type used to store a single sample from a pulse
#define EXTMAT_TYPE_SAMPLE EXTMAT_TYPE_UCHAR // the same, but as a type index
#elif BITS_PER_SAMPLE <= 16
#define T_SAMPLE_TYPE short             
#define EXTMAT_TYPE_SAMPLE EXTMAT_TYPE_SHORT // the same, but as a type index
#else
#error "Sample sizes of > 16 bits are not supported."
#endif

#define T_SAMPLE_BITS_USED BITS_PER_SAMPLE // actual number of sample bits used

#define T_PIXEL_TYPE unsigned int 	// the type used to store a single pixel for a plot
#define T_PIXEL_DATA_BITS 8        	// number of pixel bits used for colour choice (these are the low-order bits)
#define T_CLASS_TYPE unsigned char  	// the type used to store the classification of a sample
#define T_PALETTE_TYPE unsigned int     // the type used to store a palette entry: i.e. an rgb value 

#define T_FRAME_CELL_TYPE int 		// the type used to store accumulated stats for sample cells

#define T_SCORE_TYPE short 		// the type used to store z-score for a sample; this is scaled by 2 << T_SCORE_FRACTIONAL_BITS
#define T_SCORE_BITS_USED 16    	// how many bits of T_SCORE_TYPE are used (including fractional ones)
#define T_SCORE_FRACTIONAL_BITS 8   	// how many fractional bits in the fixed point z-score representation
#define T_SCORE_INTERMEDIATE_TYPE int 	// the intermediate type used to calculate the score
                                        // this must hold at least T_SCORE_FRACTIONAL_BITS more
                                        // bits than t_score does

/* error codes for modules and the main radR program */

#define RADR_ERROR_NONE                        0
#define RADR_ERROR_NOMEM 		       1 
#define RADR_ERROR_BAD_SERVER 		       2 
#define RADR_ERROR_BAD_PORT 		       3 
#define RADR_ERROR_EOF_ON_PORT 		       4 
#define RADR_ERROR_FILE_ERROR_ON_PORT	       5 
#define RADR_ERROR_CANT_OPEN_ARCHIVE 	       6 
#define RADR_ERROR_INVALID_ARCHIVE 	       7 
#define RADR_ERROR_NO_ARCHIVE_DIR 	       8 
#define RADR_ERROR_NO_CURRENT_SRC 	       9 
#define RADR_ERROR_SOURCE_NOT_ARCHIVE 	      10 
#define RADR_ERROR_INVALID_RUN 		      11 
#define RADR_ERROR_PORT_NOT_SOURCE 	      12 
#define RADR_ERROR_PORT_NOT_SINK 	      13 
#define RADR_ERROR_SEEK_BEYOND_ARCHIVE 	      14 
#define RADR_ERROR_INVALID_SCAN 	      15 
#define RADR_ERROR_ARCHIVE_IN_USE 	      16 
#define RADR_ERROR_UNKNOWN_PORT_ERROR         17
#define RADR_ERROR_NOT_IMPLEMENTED            18
#define RADR_ERROR_UNABLE_TO_WRITE_TO_FILE    19
#define RADR_ERROR_FEATURE_NOT_AVAILABLE      20
#define RADR_ERROR_PLUGIN_ERROR               21

/* 

   hook codes, for passing to rss.call.hooks or call_R_hooks 

   !!! NOTE:  THE NUMERICAL ORDER MUST MATCH THE ALPHABETICAL ORDER  !!!

   Hook numbers are never stored externally, so no incompatibility
   arises if they change when you add/remove/modify hook names, provided
   all of radR is rebuilt.

   After changing this list, you can correctly reformat it by running
   the #define lines through this shell command:

     sort | gawk '{printf "%s %-40s %2d\n", $1, $2, ++i;}'

*/

#define RADR_HOOK_ANTENNA_CONFIG                  1
#define RADR_HOOK_BLIP                            2
#define RADR_HOOK_CLASSIFY                        3
#define RADR_HOOK_DONE_SCAN                       4
#define RADR_HOOK_END_SINK                        5
#define RADR_HOOK_END_SOURCE                      6
#define RADR_HOOK_FIND_PATCHES                    7
#define RADR_HOOK_FULL_SCAN                       8
#define RADR_HOOK_GET_SCAN_DATA                   9
#define RADR_HOOK_GET_SCAN_DATA_NAMES            10
#define RADR_HOOK_GET_SCAN_INFO                  11
#define RADR_HOOK_MARK_PLOT_POINT                12
#define RADR_HOOK_ONEXIT                         13
#define RADR_HOOK_ONPAUSE                        14
#define RADR_HOOK_ONPLAY                         15
#define RADR_HOOK_ONPLAY_CTS                     16
#define RADR_HOOK_ONPLAY_ONE                     17
#define RADR_HOOK_ONSTOP                         18
#define RADR_HOOK_PAINT_BACKGROUND               19
#define RADR_HOOK_PATCH                          20
#define RADR_HOOK_PATCH_STATS                    21
#define RADR_HOOK_PLOT_CURSOR_MOVED              22
#define RADR_HOOK_PLOT_PANNED                    23
#define RADR_HOOK_PLOT_ROTATED                   24
#define RADR_HOOK_PLOT_ZOOMED                    25
#define RADR_HOOK_POST_SCAN_CONVERT              26
#define RADR_HOOK_POST_STATS                     27
#define RADR_HOOK_PRE_GET_SCAN                   28
#define RADR_HOOK_PRE_PROCESS                    29
#define RADR_HOOK_PRE_SCAN_CONVERT               30
#define RADR_HOOK_PRE_STATS                      31
#define RADR_HOOK_PUT_SCAN_DATA                  32
#define RADR_HOOK_RAW_BLIPS                      33
#define RADR_HOOK_RESTART_LEARNING               34
#define RADR_HOOK_SCAN_INFO                      35
#define RADR_HOOK_SCAN_ROW                       36
#define RADR_HOOK_SCORES                         37
#define RADR_HOOK_START_SINK                     38
#define RADR_HOOK_START_SOURCE                   39
#define RADR_HOOK_STATS                          40
#define RADR_HOOK_TK_PLOT_MODE                   41
#define RADR_HOOK_TRACK                          42
#define RADR_HOOK_UPDATE_PARMS                   43
