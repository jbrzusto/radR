#ifndef _RADR_H
#define _RADR_H
//#define _LARGEFILE64_SOURCE
//#define _FILE_OFFSET_BITS 64
#include <stdint.h>
#include "R.h"
#define USE_RINTERNALS
#include "Rinternals.h"
#include "Rdefines.h"
#include "R_ext/Rdynload.h"
#include "extmat.h"

/* shared constants */

#include "radRshared.h"

#define ROUND_UP_TO(X, Y) (((X - 1) / Y + 1) * Y)
#define ROUNDED_DIV(X, Y) ({typeof(Y) __Y = Y; ((X) + (__Y) / 2) / (__Y)})

// get an attribute for an R object

#define ATTR(X, Y) GET_ATTR(X, mkString(##Y))

// fundamental radR types.  The T_XXX_TYPE definitions must use numbers corresponding
// to the EXTMAT_TYPE_XXX macros defined in extmat.h

typedef T_SAMPLE_TYPE t_sample;  // the type used to store a single sample from a pulse
#define T_SAMPLE_TYPE_SIZE sizeof(T_SAMPLE_TYPE)

typedef uint16_t t_scan_dim; // the type used to identify a single dimension of the scan
                                   // e.g. if unsigned short is used, then a scan can
                                   // be up to 65535 pulses by 65535 samples per pulse, memory
                                   // permitting.  FIXME:  this is used only occasionally
                                   // in the code, but should be used everywhere!

typedef T_PIXEL_TYPE t_pixel;    // the type used to store a single pixel for a plot
#define T_PIXEL_TYPE_SIZE sizeof(T_PIXEL_TYPE)

typedef T_PALETTE_TYPE t_palette;  // the type used to store a single palette entry (i.e. RGB value)
#define T_PALETTE_TYPE_SIZE sizeof(T_PALETTE_TYPE)

typedef T_CLASS_TYPE t_class;    // the type used to store the classification of a sample
#define T_CLASS_TYPE_SIZE sizeof(T_CLASS_TYPE)

typedef T_FRAME_CELL_TYPE t_frame_cell;// the type used to store accumulated stats for sample cells
#define T_FRAME_CELL_TYPE_SIZE sizeof(t_frame_cell)

typedef T_SCORE_TYPE         t_score;      // the type used to store z-score for a sample; this is scaled by 2 << T_SCORE_FRACTIONAL_BITS
typedef T_SCORE_INTERMEDIATE_TYPE  t_score_intermediate; // the intermediate type used to calculate the score
                                            // this must hold at least T_SCORE_FRACTIONAL_BITS more
                                            // bits than t_score does
#define T_SCORE_TYPE_SIZE sizeof(T_SCORE_TYPE)                                
#define T_SCORE_INT_BIT_MASK ((1 << (8 * sizeof(t_score))) - 1)  // if a score is computed as wider integer type, this masks off the useful part
#define T_SCORE_MAX  ((t_score_intermediate) ((((unsigned)1) << (8 * sizeof(t_score) - 1)) - 1))
#define T_SCORE_MIN  (-T_SCORE_MAX - 1)
#define T_SCORE_SCALE (1.0 / (1 << T_SCORE_FRACTIONAL_BITS))

// a type reflecting the options available to the radR_update_stats function
typedef enum {
  INIT_ACCUM = 1,
  CONT_ACCUM = 2,
  LAST_ACCUM = 3,
  UPDATE = 4
} t_stats_mode;

// constants for blip processing

#define RADR_BLIP_FILE_ID_STRING "radR version    0.1          \n"

#define CLASS_COLD 0       	/* sample is cold */
#define CLASS_HOT 1		/* sample is hot */
#define CLASS_BLIP 2		/* sample is in a blip */

// physical constants

#define METRES_PER_NMI 1852.0
#define VELOCITY_OF_LIGHT 2.99792458E8  /* in vacuum, m/s; FIXME: find a decent atmospheric value */


/* utility macros */

/* library types and constants */

/* we update moving averages with new data using:

   ma[t+1] <- k * ma[t] + (1-k) * data[t]

where 1 >= k >= 0.  To keep math in integers,
we represent k as the numerator of a fraction
whose denominator is 2^(number of bits in t_sample minus 1)
using the following constants.

*/

/* 
   - a blip is a contiguous (in [pulse,echo] space) set of samples each of which is hot
   - the score of a sample is (x - ma(x)) / madev(x)
   - a sample becomes hot if its score exceeds hot_score
   - a sample becomes cold if it was in a blip but its new score is less than cold_score
   - the weight of a sample is its index (i.e. what scan column it is in)
   - the weight of a blip is the sum of the weights of its samples
   - the weight of a blip can be converted to a physical area as follows
        physical_area = blip_weight * dR^2 * dTheta
     where
        dR = the range represented by one sample, in metres
	(we square it to convert 1 column to metres for both R and dR)
        dTheta = the angle covered by one pulse, in radians
   - we record only blips having areas between a_lo and a_hi, and number of samples
     between ns_lo and ns_hi;  a value < 0 for a_hi or ns_hi means there is no upper limit
 */

// create an R external pointer object (EXTPTR SEXP) from a pointer
#define PTR_TO_EXTPTR(_P_) R_MakeExternalPtr((void *)(_P_), R_NilValue, R_NilValue)

/* for holding a vector of each component of a t_hot_sample struct */

typedef struct {
  t_extmat row;
  t_extmat col;
  t_extmat val;
  t_extmat z;
} t_patch_coords;

// for building tables of R-accessible functions, this macro is useful:
// X is the name of the function, N is the number of parameters it takes

#define MKREF(FUN, N) {#FUN, (DL_FUNC) &FUN, N}
#define MKREFN(NAME, FUN, N) {NAME, (DL_FUNC) &FUN, N}

// error return codes for pass/fail .Call functions

#define LOGICAL_SEXP(V) ({SEXP __s__ = allocVector(LGLSXP, 1); LOGICAL(__s__)[0] = (V) ? 1 : 0; __s__;})

#define PASS_SEXP LOGICAL_SEXP(1)
#define FAIL_SEXP R_NilValue
#define TRUE_SEXP ScalarLogical(1)
#define FALSE_SEXP ScalarLogical(0)

#ifndef min
#define min(a,b) (a) <= (b) ? (a) : (b)
#endif
#ifndef max
#define max(a,b) (a) >= (b) ? (a) : (b)
#endif
#ifndef iabs
#define iabs(X) ({typeof(X) __X__ = X; (__X__ >= 0) ?  __X__ :  - (__X__);})
#endif

// functions for opening and seeking in large (> 2 Gig ?) files
#ifdef Win32
#ifdef __CYGWIN__
#define bigfopen(F, MODE) fopen(F, MODE)
#define bigfseek(F, OFF, WHENCE) fseeko(F, OFF, WHENCE)
#define bigftell(F) ftello(F)
#else
#define bigfopen(F, MODE) fopen64(F, MODE)
#define bigfseek(F, OFF, WHENCE) fseeko64(F, OFF, WHENCE)
#define bigftell(F) ftello64(F)
#endif /* Win32 */
#else /* unix */
// The same functions work on my linux system, but other *nixes
// will presumably vary.  Using (-1 != lseek64(fileno(F), OFF, WHENCE)) does not work on my system!
#define bigfopen(F, MODE) fopen64(F, MODE)
#define bigfseek(F, OFF, WHENCE) fseeko64(F, OFF, WHENCE)
#define bigftell(F) ftello64(F)
#endif

// a macro to call the R hooks associated with a particular hook number
// This packages the hook number into an INTSXP, prepends it to the variable list
// of parameters, and calls call_R_function with function name "rss.call.hooks" and 
// R_GlobalEnv.  The net effect is an R call to rss.call.hooks(hook, ...)

SEXP call_R_function(char *name, SEXP env, ...);
SEXP make_R_vector(int sxp_type, int n, ...);

// statements for returning results to R from .Call()'d functions

#define RETBOOL(x) return ScalarLogical(x)
#define RETINT(x) return ScalarInteger(x)
#define RETCHAR(x) return mkString(x)
#define RETDBL(x) return ScalarReal(x)

#define call_R_hooks(HOOK, ...) ({ SEXP _hk_, _rv_; PROTECT(_hk_ = allocVector(INTSXP, 1)); INTEGER(_hk_)[0] = HOOK; _rv_ = call_R_function("rss.call.hooks", R_GlobalEnv, _hk_, __VA_ARGS__, 0); UNPROTECT(1); _rv_;})

#define call_R_hooks_accum(HOOK, ...) ({ SEXP _hk_, _rv_; PROTECT(_hk_ = allocVector(INTSXP, 1)); INTEGER(_hk_)[0] = HOOK; _rv_ = call_R_function("rss.call.hooks.accum", R_GlobalEnv, _hk_, __VA_ARGS__, 0); UNPROTECT(1); _rv_;})

// avoid having to #ifdef debugging print statements
#ifdef RADR_DEBUG
#define dbgprintf(FMT, ...) printf(FMT, ## __VA_ARGS__)
#else
#define dbgprintf(FMT, ...)
#endif


#endif /* _RADR_H */
