/* 
   multiframecorr.h - definitions for multiframecorr.c

*/

#ifndef _MULTIFRAMECORR_H_
#define _MULTIFRAMECORR_H_

typedef double t_mfc_coord;	/* type for coordinates of points */
typedef int t_mfc_gain; 	/* type for gain between points */
#define T_MFC_COORD 	EXTMAT_TYPE_DOUBLE	/* must match t_mfc_coord */
#define T_MFC_COORD_SIZE EXTMAT_TYPE_DOUBLE_SIZE
#define T_MFC_GAIN      EXTMAT_TYPE_INT	/* must match t_mfc_gain */
#define T_MFC_GAIN_SIZE EXTMAT_TYPE_INT_SIZE
#define MFC_NUM_COORDS  4       /* number of coordinates per point; 4: x, y, z, t */
#define MFC_NUM_STATE_VARS 3    /* number of state variables per track */
#define TRACK_NO_TRACK   0       /* bogus TID meaning "not part of a track" */
// structure to hold a point in an mfc problem
// the x coordinate is NA_REAL when the state is invalid
typedef union {
  struct {
    t_mfc_coord x;
    t_mfc_coord y;
    t_mfc_coord z;
    t_mfc_coord t;
  };
  t_mfc_coord coords[MFC_NUM_COORDS];
} t_mfc_point;

// structure to hold the motion-model state of a point in an mfc problem
typedef union {
  struct {
    t_mfc_coord vx;
    t_mfc_coord vy;
    t_mfc_coord vz;
  };
  t_mfc_coord vars[MFC_NUM_STATE_VARS];
} t_mfc_state;

#define MFC_COPY_COORDS(dst, src)memcpy((double *) dst, (double *) src, MFC_NUM_COORDS * sizeof(double))
#define MFC_COPY_STATE(dst, src) memcpy((double *) dst, (double *) src, MFC_NUM_STATE_VARS * sizeof(double))

typedef struct {
  int 		nf; 		/* number of frames currently in this problem */
  int 		k;  		/* maximum number of frames allowed at a time */
  int           np;             /* total number of points in frames 0...(nf-1) */
  double        alpha;          /* weighting parameter for gain function */
  double        eps;            /* penalty parameter for gain function */
  double        max_dist;       /* maximum distance between two points (same units as point coordinates) */
  double        max_speed;      /* maximum speed between two points (units: point space coordinates divided by point time coordinates) */
  double        min_gain;       /* minimum gain for a blip to join a track; edges with smaller gain get weight zero */
  t_extmat 	n;  		/* an array k integers, giving the number of points in each current frame */
  t_extmat 	x; 		/* extmat holding x, y, z, t coordinates of points */
  t_extmat 	first;  	/* an array k integers; for each frame, this is the index into x[] of its first point */
  t_extmat 	state;  	/* for each point in the current dag, this is its state assuming a path has been followed
				   to reach the point; If the point has no predecessor, the state takes on a default value. */
  t_extmat 	succ;   	/* for each point, this is the index in x[] of its successor.  NA means no successor */
  t_extmat 	pred;   	/* for each point, this is the index in x[] of its predecessor.  NA means no predecessor */
  t_extmat 	osucc;   	/* for each point, this is the index in x[] of its successor in the previous path cover.  NA means no successor */
  t_extmat 	opred;   	/* for each point, this is the index in x[] of its predecessor in the previous path cover.  NA means no predecessor */
  t_extmat 	gain;   	/* for each point, this is the gain along the edge to its successor. */
  t_extmat      iblip;          /* for each point, its index in the tracker object all.blips */
  t_extmat      track;          /* the index of the tracker track object this point is on, if the track has been officially started; this is the index into TRACKER$tracks; i.e. it's an atid */
  t_DAG 	dag;       	/* DAG for the current problem */
  int           n_cover_edges;  /* number of edges in the current path cover */
  // the following are only used if they are not NULL
  // otherwise, internal versions are used
  // these are set by update() from the attributes() of the t_mfc_problem EXTPTR object
  SEXP          R_gain_pp;      /* SEXP of the R function for calculating point-to-point gains */
  SEXP          R_gain_tp;      /* SEXP of the R function for calculating track-to-point gains */
  SEXP          R_new_state;    /* SEXP of the R function for calculating new states of points added to tracks */

} t_mfc_problem;

// convenience access macros

#define MFC_N(p, i) 	(((int *)((p)->n.ptr))[i])
#define MFC_X(p, i) 	(((t_mfc_point *)((p)->x.ptr))[i])
#define MFC_FIRST(p, i) (((int *)((p)->first.ptr))[i])
#define MFC_STATE(p, i) (((t_mfc_state *)((p)->state.ptr))[i])
#define MFC_VALID_STATE(p, i) (!ISNA((((t_mfc_state *)((p)->state.ptr))[i]).vars[0]))
#define MFC_INVALIDATE_STATE(p, i) (((((t_mfc_state *)((p)->state.ptr))[i]).vars[0]) = NA_REAL)
#define MFC_SUCC(p, i) 	(((int *)((p)->succ.ptr))[i])
#define MFC_PRED(p, i) 	(((int *)((p)->pred.ptr))[i])
#define MFC_OSUCC(p, i) (((int *)((p)->osucc.ptr))[i])
#define MFC_OPRED(p, i) (((int *)((p)->opred.ptr))[i])
#define MFC_GAIN(p, i) 	(((int *)((p)->gain.ptr))[i])
#define MFC_IBLIP(p, i) (((int *)((p)->iblip.ptr))[i])
#define MFC_TRACK(p, i) (((int *)((p)->track.ptr))[i])
#define MFC_IS_REMNANT(p, i) (MFC_SUCC(p, i) == NO_NODE && MFC_PRED(p, i) == NO_NODE && MFC_TRACK(p, i) == TRACK_NO_TRACK)
#define R_STATE_VEC(p, i) make_R_vector(REALSXP, 3, MFC_STATE(p, i).vx, MFC_STATE(p, i).vy, MFC_STATE(p, i).vz)

#define MAX_GAIN_INT ((1 << 30) - 1)

#endif /* _MULTIFRAMECORR_H_ */
