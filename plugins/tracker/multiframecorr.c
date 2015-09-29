/*  svn $Id: multiframecorr.c 594 2010-05-21 19:51:30Z john $

    radR : an R-based platform for acquisition and analysis of radar data
    Copyright (C) 2006-2009 John Brzustowski        

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


  Implement the multiframe point correspondence method of

      [1] K. Shafique and M. Shah (2005).  "A Noniterative Greedy Algorithm
      for Multiframe Point Correspondence".  IEEE Transactions on Pattern
      Analysis and Machine Intelligence, Vol. 27(1), pp. 51 - 65.

  for the radR tracker plugin.  We skip the backtracking stage at frame k,
  and use non-recursive false hypothesis replacement.

*/

#include <math.h>
#include "radR.h"
#include "maxpathcover.h"
#include "multiframecorr.h"
#include "radRvars.h"

// SEXPs for names of the attributes used to hold R functions for gain and state calculations
SEXP MFC_GAIN_PP_ATTR;
SEXP MFC_GAIN_TP_ATTR;
SEXP MFC_NEW_STATE_ATTR;

SEXP
set_gain_pp (SEXP ep, SEXP gain_pp) {
  // set the point-to-point gain function for an MFC problem
  // ep: external pointer to an MFC problem, e.g. as returned by init
  // gain_pp: a CLOSXP for calculating the point-to-point gain
  //     this must be a function of two variables, where the first
  //     is a double vector of k coordinates for the first point,
  //     the second is a k x n matrix of coordinates for the set of
  //     second points, and the third indicates whether the set of points
  //     is in the next scan from the single point.
  if (gain_pp != R_NilValue && (TYPEOF(gain_pp) != CLOSXP || length(FORMALS(gain_pp)) != 3))
    error("multiframecorr: gain.pp must be a function of three variables");
  setAttrib(ep, MFC_GAIN_PP_ATTR, gain_pp);
  return PASS_SEXP;
}

SEXP
set_gain_tp (SEXP ep, SEXP gain_tp) {
  // set the track-to-point gain function for an MFC problem
  // ep: external pointer to an MFC problem, e.g. as returned by init
  // gain_tp: a CLOSXP for calculating the track-to-point gain
  //     this must be a function of three variables, where the first
  //     is a double vector of k coordinates for the first point,
  //     the second is a k x n matrix of coordinates for the set of second points
  //     the third is the state vector for the target (whose current track ends in the first point)
  //     and the third indicates whether the set of points
  //     is in the next scan from the single point.
  if (gain_tp != R_NilValue && (TYPEOF(gain_tp) != CLOSXP || length(FORMALS(gain_tp)) != 4))
    error("multiframecorr: gain.tp must be a function of four variables");
  setAttrib(ep, MFC_GAIN_TP_ATTR, gain_tp);
  return PASS_SEXP;
}

SEXP
set_new_state (SEXP ep, SEXP new_state) {
  // set the new state calculation function for an MFC problem
  // ep: external pointer to an MFC problem, e.g. as returned by init
  // new_state: a CLOSXP for calculating the new state of a target
  //     this must be a function of three variables, where the first
  //     is a double vector of k coordinates for the old position of the target,
  //     the second is the state vector for the target in the old position
  //     and the third is a double vector of k coordinates for the new target position

  if (new_state != R_NilValue && (TYPEOF(new_state) != CLOSXP || length(FORMALS(new_state)) != 3))
    error("multiframecorr: new.state must be a function of three variables");
  setAttrib(ep, MFC_NEW_STATE_ATTR, new_state);
  return PASS_SEXP;
}

static t_mfc_problem *
create_mfc_instance (int k, double alpha, double eps, double max_dist, double max_speed, double min_gain)
{
  t_mfc_problem *p = Calloc(1, t_mfc_problem);
  p->k		   = k;
  p->nf		   = 0;
  p->np		   = 0;
  p->n		   = CREATE_EXTMAT(EXTMAT_TYPE_INT, NULL);
  p->x		   = CREATE_EXTMAT(T_MFC_COORD, NULL);
  p->first	   = CREATE_EXTMAT(EXTMAT_TYPE_INT, NULL);
  p->state	   = CREATE_EXTMAT(T_MFC_COORD, NULL);
  p->succ	   = CREATE_EXTMAT(EXTMAT_TYPE_INT, NULL);
  p->pred	   = CREATE_EXTMAT(EXTMAT_TYPE_INT, NULL);
  p->osucc	   = CREATE_EXTMAT(EXTMAT_TYPE_INT, NULL);
  p->opred	   = CREATE_EXTMAT(EXTMAT_TYPE_INT, NULL);
  p->gain	   = CREATE_EXTMAT(T_MFC_GAIN, NULL);
  p->iblip	   = CREATE_EXTMAT(EXTMAT_TYPE_INT, NULL);
  p->track	   = CREATE_EXTMAT(EXTMAT_TYPE_INT, NULL);
  p->dag.n	   = p->dag.m = 0;
  p->n_cover_edges = 0;
  (*pensure_extmat)(&p->n, p->k, 1);
  (*pensure_extmat)(&p->first, p->k, 1);
  p->alpha	   = alpha;
  p->eps	   = eps;
  p->max_dist	   = max_dist;
  p->max_speed	   = max_speed;
  p->min_gain	   = min_gain;

  return (p);
}
		     
SEXP
init (SEXP k, SEXP alpha, SEXP eps, SEXP max_dist, SEXP max_speed, SEXP min_gain, SEXP gain_pp, SEXP gain_tp, SEXP new_state) {
  // create an empty instance of the multi frame correspondence problem
  // k: INTSXP; the number of frames used in the MFC algorithm

  SEXP rv;
  t_mfc_problem *p = create_mfc_instance(INTEGER(AS_INTEGER(k))[0],
					 REAL(AS_NUMERIC(alpha))[0],
					 REAL(AS_NUMERIC(eps))[0],
					 REAL(AS_NUMERIC(max_dist))[0],
					 REAL(AS_NUMERIC(max_speed))[0],
					 REAL(AS_NUMERIC(min_gain))[0]);

  PROTECT(rv = PTR_TO_EXTPTR(p));
  set_gain_pp(rv, gain_pp);
  set_gain_tp(rv, gain_tp);
  set_new_state(rv, new_state);
  UNPROTECT(1);
  return rv;
}

SEXP
set_alpha (SEXP ep, SEXP alpha) {
  // set the alpha parameter for an MFC problem
  // ep: external pointer to an MFC problem, e.g. as returned by init
  // alpha: a REALSXP with the value of alpha, from [1].
  t_mfc_problem *p = (t_mfc_problem *) EXTPTR_PTR(ep);
  p->alpha = REAL(AS_NUMERIC(alpha))[0];
  return PASS_SEXP;
}

SEXP
set_eps (SEXP ep, SEXP eps) {
  // set the epsilon parameter for an MFC problem
  // ep: external pointer to an MFC problem, e.g. as returned by init
  // eps: a REALSXP with the value of epsilon, from [1].
  t_mfc_problem *p = (t_mfc_problem *) EXTPTR_PTR(ep);
  p->eps = REAL(AS_NUMERIC(eps))[0];
  return PASS_SEXP;
}

SEXP
set_max_dist (SEXP ep, SEXP max_dist) {
  // set the maximum range for an MFC problem; this is the largest
  // possible distance between two points.
  // ep: external pointer to an MFC problem, e.g. as returned by init
  // max_dist: a REALSXP with the value of max_dist, 
  t_mfc_problem *p = (t_mfc_problem *) EXTPTR_PTR(ep);
  p->max_dist = REAL(AS_NUMERIC(max_dist))[0];
  return PASS_SEXP;
}

SEXP
set_max_speed (SEXP ep, SEXP max_speed) {
  // set the maximum speed between points; gain between points whose
  // locations imply a larger speed is set to zero
  // ep: external pointer to an MFC problem, e.g. as returned by init
  // max_speed: a REALSXP with the value of max_speed, 
  t_mfc_problem *p = (t_mfc_problem *) EXTPTR_PTR(ep);
  p->max_speed = REAL(AS_NUMERIC(max_speed))[0];
  return PASS_SEXP;
}

SEXP
set_min_gain (SEXP ep, SEXP min_gain) {
  // set the minimum gain required for an edge between two blips
  // to be considered
  // ep: external pointer to an MFC problem, e.g. as returned by init
  // min_gain: a REALSXP with the value of min_gain 
  t_mfc_problem *p = (t_mfc_problem *) EXTPTR_PTR(ep);
  p->min_gain = REAL(AS_NUMERIC(min_gain))[0];
  return PASS_SEXP;
}

static void
free_instance (t_mfc_problem *p) {
  // free the storage associated with an MFC instance
  p->nf = 0;
  p->np = 0;
  (*pfree_extmat)(&p->n);
  (*pfree_extmat)(&p->x);
  (*pfree_extmat)(&p->first);
  (*pfree_extmat)(&p->state);
  (*pfree_extmat)(&p->succ);
  (*pfree_extmat)(&p->pred);
  (*pfree_extmat)(&p->osucc);
  (*pfree_extmat)(&p->opred);
  (*pfree_extmat)(&p->gain);
  (*pfree_extmat)(&p->iblip);
  (*pfree_extmat)(&p->track);
  free_dag(&p->dag);
}

SEXP
destroy (SEXP ep) {
  // delete an instance of the multi frame correspondence problem
  // ep is the external pointer SEXP for the problem
  // e.g. as returned by init()

  t_mfc_problem *p = (t_mfc_problem *) EXTPTR_PTR(ep);
  free_instance(p);
  return PASS_SEXP;
}

t_mfc_state inline
calc_state_internal (t_mfc_point *u, t_mfc_state *ostate, t_mfc_point *v) {
  // calculate the object state at v given that it used to be
  // at u with state ostate

  // Currently, we don't use ostate.

  t_mfc_state rv;
  double dt = v->t - u->t;
  // use the direct estimate of velocity between u and v
#ifdef RADR_DEBUG
  if (dt == 0.0)
    printf("******* WARNING: calc_state_internal: got zero delta t\n");
#endif
  rv.vx = (v->x - u->x) / dt;
  rv.vy = (v->y - u->y) / dt;
  rv.vz = (v->z - u->z) / dt;
#ifdef RADR_DEBUG
  if (rv.vx == 0.0 && rv.vy == 0.0 && rv.vz == 0.0)
    printf("******* WARNING: calc_state_internal: got zero velocity\n");
#endif
  return rv;
}

double inline
SUMSQ(double x, double y, double z) {
  return x*x+y*y+z*z;
}

double inline 
MAG(double x, double y, double z) {
  return sqrt(x*x+y*y+z*z);
}

t_mfc_gain inline
GAIN_TO_INT(double x) {
  // convert floating point gain in [0, 1] to non-negative integer
  return (int) ((x * MAX_GAIN_INT + 0.5));
}

double inline
INT_TO_GAIN(int x) {
  // recover floating point gain in [0, 1] from non-negative integer representation
  return x / (double) MAX_GAIN_INT;
}

t_mfc_gain inline
calc_nn_gain (t_mfc_problem *p, t_mfc_point *u, t_mfc_point *v, int cons_scan) {
  // calculate the gain between two points using the
  // nearest neighbour gain function
  // (i.e. Eqn. (2) of [1], weighted by alpha so it is effectively the first term
  // of Eqn. (4) of [1].)
  // cons_scan is 1 if u and v are from consecutive scans, 0 otherwise.  

  // If the implied speed between two points is larger than p->max_speed, the gain
  // is set to zero, preventing matching.

  double dist = MAG(u->x - v->x, u->y - v->y, u->z - v->z);
  double rv;

  if (fabs(v->x) > 10000 || fabs(u->x) > 10000 || fabs(u->t) < 1e9 || fabs(v->t) < 1e9)
    printf("******* WARNING: likely wrong coordinate ordering in calc_nn_gain");

  if (fabs(v->t - u->t) < 0.001) {
#ifdef RADR_DEBUG
    printf("******* WARNING: calc_nn_gain: got zero delta t");
#endif
    return GAIN_TO_INT(0.0);
  }
  if (fabs(dist / (v->t - u->t)) > p->max_speed)
    return GAIN_TO_INT(0.0);

  rv = (1 - p->alpha) * (1 - dist / p->max_dist) - (cons_scan ? 0.0 : p->eps);

  if (rv < 0) {
#ifdef RADR_DEBUG
    printf("******** Warning: negative gain = %g set to 0\n", rv);
#endif
    rv = 0.0;
  }

  if (rv > 1.0) {
#ifdef RADR_DEBUG
    printf("******** Warning: overlarge gain = %g set to 1.0\n", rv);
#endif
    rv = 1.0;
  }

  return GAIN_TO_INT(rv);
}

t_mfc_gain inline
calc_gain (t_mfc_problem *p, t_mfc_state *state, t_mfc_point *u, t_mfc_point *v, int cons_scan) {
  /*
    calculate the gain in matching the object at u with the object at v, given
    the state at u is given by state. cons_scan is 1 if u and v are from consecutive scans,
    0 otherwise.

    We use Euclidean norms (p=2).  [1] does not indicate what p they use.
    To normalize distances, we use max_dist, which is the diameter of the radar's
    field of view.
    This model assumes constant velocity, and predicts the location of the point given
    its starting location, velocity, and the elapsed time.
    Note: the order of coords in rows of all.blips is t, x, y, z
    The ornate notation in equation 4 of [1] can be reduced to this:

    Let u be the position of a point observed in frame i
    Let w be the predicted position in frame j > i of the object represented by that point
    Let v be the position of a point observed in frame j > i

    The gain function g3(u, v, w) is given by

    alpha * (0.5 + (w-u).(v-u)/(2|w-u||v-u|)) + (1-alpha) * (1 - |v-w|/(2*max.range))

    where | | is vector magnitude and . is dot product
  */

  double dt = v->t - u->t;
  double w_x, w_y, w_z;
  double dist_uv = MAG(v->x - u->x, v->y - u->y, v->z - u->z);
  double dist_uw, dist_vw;
  double g1, g2;
  double rv;

#ifdef RADR_DEBUG
  if (dt == 0.0)
    printf("calc_gain: got zero delta t\n");
#endif

  // make sure the velocity is not excessive

  if (fabs(dist_uv / (v->t - u->t)) > p->max_speed)
    return GAIN_TO_INT(0.0);

  if (fabs(v->x) > 10000 || fabs(u->x) > 10000 || fabs(u->t) < 1e9 || fabs(v->t) < 1e9)
    printf("******* WARNING: likely wrong coordinate ordering in calc_gain");

  // predict a new point position assuming constant velocity

  w_x = u->x + dt * state->vx;
  w_y = u->y + dt * state->vy;
  w_z = u->z + dt * state->vz;

  // the predicted distance
    
  dist_uw = dt * MAG(state->vx, state->vy, state->vz);

  // the distance between predicted and observed

  dist_vw = MAG(w_x - v->x, w_y - v->y, w_z - v->z);

  // the closeness-to-predicted-location gain component

  g1 = 1.0 - dist_vw / p->max_dist;

  // a negative gain outside implies a predicted point outside the
  // visible radar range, so set this gain to zero

  if (g1 < 0.0)
    g1 = 0.0;
    
  // if both are non-zero, add the directional homogeneity gain component

  if (dist_uv > 0 && dist_uw > 0) 
    g2 = 0.5 + ((w_x - u->x) * (v->x - u->x) + (w_y - u->y) * (v->y - u->y) + (w_z - u->z) * (v->z - u->z))
      / ( 2 * dist_uv * dist_uw);
  else
    g2 = 0.0;
  
  rv = (1 - p->alpha) * g1 + p->alpha * g2 - (cons_scan ? 0.0 : p->eps);

  if (rv > 1.0) {
    // with alpha in [0, 1] and p->eps >=0, this can't happen
#ifdef RADR_DEBUG
      printf("Warning: gain = %g > 1\n", rv);
#endif
      rv = 1.0;
  }
#ifdef RADR_DEBUG
  else if (rv < 0) {
    printf("Warning: gain = %g < 0\n", rv);
    // no need to set to zero since this will be done below
  }
#endif
    // if the gain is less than the low threshold, force it to zero which
    // will effectively bar this track/blip pair from consideration
  if (rv < p->min_gain)
    rv = 0.0;

  return GAIN_TO_INT(rv);
}

int
calc_gains (t_mfc_problem *p, SEXP env, int i1, int i2, int n, int *i2p, int *resptr, int resspan, int is_cons)
{
  // calculate gains between a fixed point, possibly on a track, and a set of
  // other points.  Do this with user-defined R functions, if they exist, otherwise
  // use the internal versions.  Return the number of positive gains.
  //
  // p - pointer to the mfc problem instance
  // i1 - index within *p of the fixed point
  // i2 - index within *p of the first of the other points; the other points must
  //      be contiguous in *p, i.e. they are p[i2], p[i2+1], ..., p[i2 + n-1]
  // n - number of other points
  // i2p - if not NULL, a pointer to an array of indexes of other points
  //       (if i2p is not NULL, then i2 is not used)
  // resptr - where to store the first gain
  // resspan - how much to increment resptr (in bytes) for storing consecutive gains
  // is_cons - 1 if i1 and the points in i2p are from consecutive scans, 0 otherwise.

  SEXP sexp1, sexp2, sexp3, sexp4, R_call, rv;

  int use_track = MFC_VALID_STATE(p, i1);
  SEXP user_fun = (use_track ? p->R_gain_tp : p->R_gain_pp);
  int j;
  double *rp;
  int num_pos = 0;

  if (user_fun != R_NilValue) {
      t_mfc_point *u;
      /* set up R vectors with the point coordinates */
      PROTECT(sexp1 = allocVector(REALSXP, MFC_NUM_COORDS));
      PROTECT(sexp2 = allocMatrix(REALSXP, MFC_NUM_COORDS, n));
      PROTECT(sexp4 = allocVector(LGLSXP, 1));

      u = &MFC_X(p, i1);
      MFC_COPY_COORDS(REAL(sexp1), u);
      rp = REAL(sexp2);
      for (j = 0; j < n; ++j) {
	u = &MFC_X(p, i2p ? i2p[j] : i2 + j);
	MFC_COPY_COORDS(rp, u);
	rp += MFC_NUM_COORDS;
      }
      LOGICAL(sexp4)[0] = is_cons ? 1 : 0;
      if (!use_track) {
	PROTECT(R_call = lang4(user_fun, sexp1, sexp2, sexp4));
      } else {
	// set up a parameter with the first point's state
	PROTECT(sexp3 = allocVector(REALSXP, MFC_NUM_STATE_VARS));
	MFC_COPY_STATE(REAL(sexp3), &MFC_STATE(p, i1));
#ifdef RADR_DEBUG
	if (REAL(sexp3)[0] == 0.0 && REAL(sexp3)[1] == 0.0 && REAL(sexp3)[2] == 0.0)
	  printf("Warning: found 0 state vector");
#endif
	PROTECT(R_call = lang5(user_fun, sexp1, sexp2, sexp3, sexp4));
      }
      rv = eval(R_call, env);
      if (!IS_NUMERIC(rv))
	error("MFC: gain.%cp must return a vector of real numbers", (use_track ? 't' : 'p'));
      if (LENGTH(rv) != n)
	error("MFC: gain.%cp returned %d instead of the required %d values.", (use_track ? 't' : 'p'), LENGTH(rv), n);
      for (j = 0; j < n; ++j, resptr = (int *)(((char *) resptr) + resspan)) {
	if (REAL(rv)[j] < 0.0 || REAL(rv)[j] > 1.0)
	  error("MFC: gain.%cp returned %f, which is value outside the range 0.0 to 1.0", (use_track ? 't' : 'p'), REAL(rv)[j]);
	if ((*resptr = GAIN_TO_INT(REAL(rv)[j])) > 0)
          ++num_pos;
      }
      UNPROTECT(4 + use_track);
  } else {
    // use internal functions

    if (!use_track) {
      // isolated point:  we'll calculate point-to-point gains
      // use the internal nearest-neighbour gain function
      for (j = 0; j < n; ++j, resptr = (int *)(((char *) resptr) + resspan))
	if ((*resptr = calc_nn_gain(p, &MFC_X(p, i1), &MFC_X(p, i2p ? i2p[j] : i2 + j), is_cons)) > 0)
          ++num_pos;

    } else {
      // first point is on a track:  calculate track-to-point gains
      for (j = 0; j < n; ++j, resptr = (int *)(((char *) resptr) + resspan))
	if ((*resptr = calc_gain(p, &MFC_STATE(p, i1), &MFC_X(p, i1), &MFC_X(p, i2p ? i2p[j] : i2 + j), is_cons)) > 0)
          ++num_pos;
    }
  }
  return num_pos;
}

t_mfc_state 
calc_state (t_mfc_problem *p, t_mfc_point *u, t_mfc_state *ostate, t_mfc_point *v, SEXP env)
{
  // calculate the new state of a point
  // other points.  Do this with a user-defined R function, if it exists, otherwise
  // use the internal version.
  //
  // p - pointer to the mfc problem instance
  // u - pointer to old target location
  // ostate - pointer to old target state
  // v - pointer to new target location

  SEXP sexp1, sexp2, sexp3, R_call, rv;

  SEXP user_fun = p->R_new_state;
  t_mfc_state ns;

  if (user_fun != R_NilValue) {
      /* set up R vectors with the point coordinates */
      PROTECT(sexp1 = allocVector(REALSXP, MFC_NUM_COORDS));
      PROTECT(sexp2 = allocVector(REALSXP, MFC_NUM_STATE_VARS));
      PROTECT(sexp3 = allocVector(REALSXP, MFC_NUM_COORDS));
      MFC_COPY_COORDS(REAL(sexp1), u);
      MFC_COPY_STATE(REAL(sexp2), ostate);
      MFC_COPY_COORDS(REAL(sexp3), v);

      PROTECT(R_call = lang4(user_fun, sexp1, sexp2, sexp3));

      rv = eval(R_call, env);
      if (!IS_NUMERIC(rv))
	error("MFC: new.state must return a vector of real numbers");
      if (LENGTH(rv) != MFC_NUM_STATE_VARS)
	error("MFC: new_state returned %d instead of the required %d values.", LENGTH(rv), MFC_NUM_STATE_VARS);
      MFC_COPY_STATE(ns.vars, REAL(rv));
      UNPROTECT(4);
  } else {
    ns = calc_state_internal(u, ostate, v);
  }
  return(ns);
}

#define MFC_COPY_EXTMAT(DST, SRC, MAT) {(*pensure_extmat)(&DST->MAT, SRC->MAT.rows, SRC->MAT.cols); memmove((char *) DST->MAT.ptr, (char *) SRC->MAT.ptr, SRC->MAT.rows * SRC->MAT.cols * SRC->MAT.size);}

SEXP
update (SEXP ep, SEXP TR, SEXP X, SEXP Y, SEXP Z, SEXP T, SEXP IND, SEXP TIME_NOW, SEXP IS_PREVIEW)
{
  // update an instance of the multiframe correspondence problem with a new frame
  // of points; if TR is not NULL, then (possibly) call functions in the tracker
  // plugin to update its list of tracks.
  // 
  // ep:         EXTPTR;  the external pointer SEXP to the problem instance
  // TR:         ENVSXP;  the tracker plugin object (a strictenv)
  // x, y, z, t: REALSXP; the x, y, z, and t coordinates of new points
  // ind:        INTSXP;  the indexes in TR$all.blips of the new points
  // time_now:   REALSXP; the current time

  t_mfc_problem *p = (t_mfc_problem *) EXTPTR_PTR(ep);
  t_mfc_problem *p2 = NULL;
  t_mfc_point *pc;
  int np = (X == R_NilValue) ? 0 : LENGTH(X);
  int nkeep;
  int edges_dropped; // number of edges dropped when the first frame is deleted
  int i, ii, j, jj, n, m, ri, rii, f, f2;
  t_edge *k;
  t_DAG rem_dag;
  int rem_n, rem_n2; // numbers of remnant nodes in frames i, i+1
  int *rem_which;    // indexes in full frame point set of remnant points
  int n_rem_cover_edges; // number of edges in remnant cover
  t_node_handle *rem_pred, *rem_succ; // predecessor and successor arrays for remnant DAG
  int preview = LOGICAL(AS_LOGICAL(IS_PREVIEW))[0]; // is this a preview scan
  SEXP sexp, sexp2, sexp3; // SEXPs for use as parameters to R functions
  int i_s1; // index of first blip from previous scan; lets us tell whether a given blip is 
            // in the scan immediately before the new scan
  
  int num_pos; // number of positive-gain edges

  if (preview) {
    p2 = create_mfc_instance(p->k, p->alpha, p->eps, p->max_dist, p->max_speed, p->min_gain);
    MFC_COPY_EXTMAT(p2, p, n);
    MFC_COPY_EXTMAT(p2, p, x);
    MFC_COPY_EXTMAT(p2, p, first);
    MFC_COPY_EXTMAT(p2, p, state);
    MFC_COPY_EXTMAT(p2, p, succ);
    MFC_COPY_EXTMAT(p2, p, pred);
    MFC_COPY_EXTMAT(p2, p, osucc);
    MFC_COPY_EXTMAT(p2, p, opred);
    MFC_COPY_EXTMAT(p2, p, gain);
    MFC_COPY_EXTMAT(p2, p, iblip);
    MFC_COPY_EXTMAT(p2, p, track);

    p2->dag = empty_dag();
    p2->n_cover_edges = p->n_cover_edges;
    p2->nf = p->nf;
    p2->np = p->np;

    // now use this copy in the rest of what follows
    p = p2;
  }
  
  // if necessary, delete the first frame's worth of points
  
  edges_dropped = 0;
  
  // set internal copies of the R-level gain/state calculation functions
  
  p->R_gain_pp = getAttrib(ep, MFC_GAIN_PP_ATTR);
  p->R_gain_tp = getAttrib(ep, MFC_GAIN_TP_ATTR);
  p->R_new_state = getAttrib(ep, MFC_NEW_STATE_ATTR);
  
  if (p->nf == p->k) {
    
    // If a point without a successor will be shifted out of the first frame,
    // end its track, if any.

    for (i = 0; i < MFC_N(p, 0); ++i) {
      if (MFC_SUCC(p, i) == NO_NODE) {
	if (MFC_TRACK(p, i) != TRACK_NO_TRACK) {
	  PROTECT(sexp = ScalarInteger(MFC_TRACK(p, i)));
	  call_R_function ("end.tracks", TR, sexp, NULL);
	  UNPROTECT(1);
	}
      } else {
	// note that a path-cover edge is being shifted away
	++ edges_dropped;
      }
    }

    // delete the first frame's points and associated data

    nkeep = p->np - MFC_N(p, 0); // number of points to keep (all but those in first frame)
      
    memmove(p->x.ptr,     ((t_mfc_point *) p->x.ptr)     + MFC_N(p, 0), nkeep * sizeof (t_mfc_point));
    memmove(p->state.ptr, ((t_mfc_state *) p->state.ptr) + MFC_N(p, 0), nkeep * sizeof (t_mfc_state));
    memmove(p->succ.ptr,  ((int *)         p->succ.ptr)  + MFC_N(p, 0), nkeep * sizeof (int));
    memmove(p->pred.ptr,  ((int *)         p->pred.ptr)  + MFC_N(p, 0), nkeep * sizeof (int));
    memmove(p->gain.ptr,  ((t_mfc_gain *)  p->gain.ptr)  + MFC_N(p, 0), nkeep * sizeof (t_mfc_gain));
    memmove(p->iblip.ptr, ((int *)         p->iblip.ptr) + MFC_N(p, 0), nkeep * sizeof (int));
    memmove(p->track.ptr, ((int *)         p->track.ptr) + MFC_N(p, 0), nkeep * sizeof (int));

    // adjust nf, np, n and first to account for deletion of first frame

    -- (p->nf);
    p->np -= MFC_N(p, 0);

    // adjust pred and succ to account for deleted first frame
    // pred[i] will become NO_NODE if it was in the first frame
    for (i = 0; i < p->np; ++i) {
      if (MFC_SUCC(p, i) != NO_NODE)
	MFC_SUCC(p, i) -= MFC_N(p, 0);
      if (MFC_PRED(p, i) != NO_NODE) {
	MFC_PRED(p, i) -= MFC_N(p, 0);
	if (MFC_PRED(p, i) < 0)
	  MFC_PRED(p, i) = NO_NODE;
      }
    }

    // adjust n, first to account for deletion of first frame

    memmove(p->n.ptr, ((int *) p->n.ptr) + 1, p->nf * sizeof(int));
    for (i = 1; i < p->nf; ++i) 
      MFC_FIRST(p, i) = MFC_FIRST(p, i-1) + MFC_N(p, i-1);
  }
  
  // ensure storage for points and associated data
  
  (*pensure_extmat)(&p->x,     p->np + np, sizeof(t_mfc_point) / sizeof(t_mfc_coord));
  (*pensure_extmat)(&p->state, p->np + np, sizeof(t_mfc_state) / sizeof(t_mfc_coord));
  (*pensure_extmat)(&p->succ,  p->np + np, 1);
  (*pensure_extmat)(&p->pred,  p->np + np, 1);
  (*pensure_extmat)(&p->osucc, p->np + np, 1);
  (*pensure_extmat)(&p->opred, p->np + np, 1);
  (*pensure_extmat)(&p->gain,  p->np + np, 1);
  (*pensure_extmat)(&p->iblip, p->np + np, 1);
  (*pensure_extmat)(&p->track, p->np + np, 1);

  // add points to end of current list
  for (i = 0, ii = p->np, pc = &MFC_X(p, p->np); i < np; ++i, ++ii, ++pc) {
    pc->x = REAL(X)[i];
    pc->y = REAL(Y)[i];
    pc->z = REAL(Z)[i];
    pc->t = REAL(T)[i];
    MFC_INVALIDATE_STATE(p, ii);
    MFC_SUCC(p, ii) = NO_NODE;
    MFC_PRED(p, ii) = NO_NODE;
    MFC_GAIN(p, ii) = 0;
    MFC_IBLIP(p, ii) = INTEGER(IND)[i];
    MFC_TRACK(p, ii) = TRACK_NO_TRACK;
  }

  i_s1 = p->np - MFC_N(p, p->nf - 1); // index of first point in penultimate scan (assumes k >= 2)
  MFC_FIRST(p, p->nf) = p->np;
  MFC_N(p, p->nf) = np;
  p->np += np;
  ++ (p->nf);

  // we now have p->nf frames of points

  // if this is the first frame, there's nothing 
  // left to do

  if (p->nf == 1)
    goto done;

  // create the DAG consisting of "old" track edges between points in frames 1, 2, ..., k-1
  // and "correction" and "extension" edges from all points in frames 1, 2, ... k-1 to all
  // points in frame k

  m = (p->n_cover_edges - edges_dropped) + (p->np - np) * np;
  n = p->np;

  if (m == 0 || np == 0) 
    // no new edges
    goto done;

  // free any old dag
  free_dag(&p->dag);

  // allocate a new dag
  p->dag = create_dag(n, m);

  num_pos = 0;
  // add edges to each node's edge list
  for (i = 0, k = p->dag.edges; i < n - np; ++i) {
    p->dag.nodes[i].e = k;
    // first add "old" edge, if there is one
    if (MFC_SUCC(p, i) != NO_NODE) {
      ++p->dag.nodes[i].deg;
      k->nh = MFC_SUCC(p, i);
      k->wt = MFC_GAIN(p, MFC_SUCC(p, i));
      ++k;
      ++num_pos;
    }
    // now add a new edge from this point to each new point
    for (j = 0, ii = MFC_FIRST(p, p->nf - 1); j < np; ++j, ++ii, ++k)
      k->nh =  ii;
    
    // calculate the gains from this point to the new points
    num_pos += calc_gains(p, TR, i, MFC_FIRST(p, p->nf - 1), np, NULL, &((k-np)->wt), sizeof(*k), i >= i_s1);
    p->dag.nodes[i].deg += np;
    
#ifdef RADR_DEBUG
    {
      t_edge *kk = k - np;
      for (j = 0, ii = MFC_FIRST(p, p->nf - 1); j < np; ++j, ++kk, ++ii) {
        if (kk->wt > 0)
          printf("%d,%d,%d\n", MFC_IBLIP(p, i), MFC_IBLIP(p, ii), kk->wt);
      }
    }
#endif
  }
  // the DAG is built; find a maximum path cover
#ifdef RADR_DEBUG
  printf("Doing max_path_cover on DAG with %d nodes (%d new) and %d edges (%d have +ve gain).\n", p->dag.n, np, p->dag.m, num_pos);
#endif
  // save the current pred and succ relationship 
  memmove(p->opred.ptr, p->pred.ptr, p->dag.n * sizeof(t_node_handle));
  memmove(p->osucc.ptr, p->succ.ptr, p->dag.n * sizeof(t_node_handle));
  do_max_path_cover (& p->dag, (t_node_handle *) p->pred.ptr, (t_node_handle *) p->succ.ptr, &p->n_cover_edges);
#ifdef RADR_DEBUG
  printf("Done; found %d matches\n", p->n_cover_edges);
  printf("Done\n");
  
  printf("Cover:\n");
  for (i = 0; i < p->dag.n; ++i) {
    ii = MFC_SUCC(p, i);
    if (ii != NO_NODE) {
      printf("succ: %d,%d (%g,%g,%g,%g)->(%g,%g,%g,%g) = %d\n", MFC_IBLIP(p, i), MFC_IBLIP(p, ii), 
             MFC_X(p, i).x,  MFC_X(p, i).y,  MFC_X(p, i).z,  MFC_X(p, i).t,
             MFC_X(p, ii).x,  MFC_X(p, ii).y,  MFC_X(p, ii).z,  MFC_X(p, ii).t, p->dag.nodes[i].e[p->dag.nodes[i].mate].wt);
    }
  }
#endif

  // inform the tracker plugin of new point->track assignments
    
  // There are four types of edges in the path cover:
  //
  // 
  // extension edges: edges from a terminal node in the previous cover to a node in frame k
  //                  MFC_OSUCC(p, i) == NO_NODE && MFC_SUCC(p, i) != NO_NODE
  //
  // correction edges: edges from a NON-terminal node in the previous cover to a node in frame k
  //                  MFC_OSUCC(p, i) != NO_NODE && MFC_SUCC(p, i) != NO_NODE && MFC_OSUCC(p, i) != MFC_SUCC(p, i)
  //
  // false hypothesis edges: edges reachable from a node i such that MFC_OPRED(p, i) is the tail of a correction edge
  //
  // old edges:  all other edges

  // we process nodes from earliest to latest, calculating gains and states whenever a node has
  // a new predecessor; note that we don't need to examine nodes in frame k since they have no out-edges 

  for (f = 0, i = 0; f < p->nf - 1; ++f ) { /* f enumerates frames, i enumerates points */
    for (n = MFC_N(p, f); n > 0; --n, ++i) { /* n counts points in frame f */
      ii = MFC_SUCC(p, i);
      if (ii != NO_NODE && ii != MFC_OSUCC(p, i)) {
	// ** POINT HAS A SUCCESSOR IN NEW COVER
	if (MFC_OSUCC(p, i) == NO_NODE) {
	  // ******* EXTENSION EDGE (no old successor)
	  
	  // start a new track, if necessary
	  if (MFC_TRACK(p, i) == TRACK_NO_TRACK) {
	    PROTECT(sexp = ScalarInteger(MFC_IBLIP(p, i)));
	    MFC_TRACK(p, i) = INTEGER(call_R_function ("start.new.track", TR, sexp, NULL))[0];
	    UNPROTECT(1);
	  }
	  // add the successor blip to this track
	  PROTECT(sexp = ScalarInteger(MFC_IBLIP(p, ii)));
	  PROTECT(sexp2 = ScalarInteger(MFC_TRACK(p, i)));

	  MFC_TRACK(p, ii) = MFC_TRACK(p, i);
	  call_R_function ("add.blip.to.track", TR, sexp, sexp2, NULL);
	  UNPROTECT(2);
	  
	} else if (MFC_OSUCC(p, i) != ii) {
	  // ******* CORRECTION EDGE
	  
	  if (MFC_TRACK(p, i) != TRACK_NO_TRACK) {
	    // current node is on a track:  truncate track starting at the old successor blip
	    PROTECT(sexp = ScalarInteger(MFC_TRACK(p, i)));
	    PROTECT(sexp2 = ScalarInteger(MFC_IBLIP(p, MFC_OSUCC(p, i))));
	    call_R_function ("truncate.track.tail", TR, sexp, sexp2, NULL);
	    UNPROTECT(2);
	  } else {
	    // current node is not on a track: start a new one
	    PROTECT(sexp = ScalarInteger(MFC_IBLIP(p, i)));
	    MFC_TRACK(p, i) = INTEGER(call_R_function ("start.new.track", TR, sexp, NULL))[0];
	    UNPROTECT(1);
	  }
	  // add the successor blip to this track
	  PROTECT(sexp = ScalarInteger(MFC_IBLIP(p, ii)));
	  PROTECT(sexp2 = ScalarInteger(MFC_TRACK(p, i)));
	  MFC_TRACK(p, ii) = MFC_TRACK(p, i);
	  call_R_function ("add.blip.to.track", TR, sexp, sexp2, NULL);
	  UNPROTECT(2);
	  
	  // FALSE HYPOTHESIS REMOVAL:

	  // Remove any edge in the old or new cover that depended on the
	  // original edge from this point in the old cover.  "Depended"
	  // means used the state information from this node's successor
	  // in the old cover.  That state information is now deemed
	  // wrong, because this point's old successor no longer has a
	  // predecessor (i.e. this point has a new successor).

	  // So, starting with the successor to this node in the old cover,
	  // trace along its path in the old cover, removing each edge from
	  // the cover.  Also, if any of the nodes along the way has
	  // itself been given a different successor in the new cover
	  // (i.e. has been rejoined to a point in the latest frame), 
	  // remove that new successor from the new cover.

	  // remove its successor path from the new cover.  Also,
	  // remove the successor path from the old cover.  

	  rii = MFC_OSUCC(p, i);
	  while (rii != NO_NODE) {
	    // remember the old-cover successor of node rii
	    jj = MFC_OSUCC(p, rii);

	    if ((j = MFC_SUCC(p, rii)) != jj && j != NO_NODE)
	      // this node has a different successor in the new cover,
	      // so drop that new edge (which is to a point in the latest
	      // frame, and so is the last edge in its path)
	      MFC_PRED(p, j) = NO_NODE;

	    // drop this point from the old and new covers
	    MFC_OSUCC(p, rii) = MFC_OPRED(p, rii) = MFC_SUCC(p, rii) = MFC_PRED(p, rii) = NO_NODE;
	    // mark this point as not on a track
	    MFC_TRACK(p, rii) = TRACK_NO_TRACK;
	    // set this point as having invalid state (since it has no predecessor)
	    MFC_INVALIDATE_STATE(p, rii);
	
	    // continue with the old-cover successor of rii
	    rii = jj;
	  }
	}
	// CALCULATION OF STATE FOR NEW SUCCESSOR
	// get the gain and state for this new edge, and add it to the
	// tracker track object
	
	// To avoid recalculating gain, we find the index of this edge 
	// among those leaving point i; note that a point in frame f
	// has edges to its successor in the old path cover, if any,
	// and then edges to all points in the last frame 
	
	j = ii - MFC_FIRST(p, p->nf - 1) + (MFC_OSUCC(p, i) != NO_NODE);

#ifdef RADR_DEBUG
	if (ii != p->dag.nodes[i].e[j].nh)
	  printf("****** Error: full dag: miscalculation of location of edge between nodes %d and %d\n", i, ii);
#endif
	MFC_GAIN(p, ii) = p->dag.nodes[i].e[j].wt;            
	MFC_STATE(p, ii) = calc_state(p, &MFC_X(p, i), &MFC_STATE(p, i), &MFC_X(p, ii), TR);
	PROTECT(sexp = ScalarInteger(MFC_TRACK(p, ii)));
	PROTECT(sexp2 = ScalarReal(INT_TO_GAIN(MFC_GAIN(p, ii))));
	PROTECT(sexp3 = allocVector(REALSXP, 3));
	REAL(sexp3)[0] = MFC_STATE(p, ii).vx;
	REAL(sexp3)[1] = MFC_STATE(p, ii).vy;
	REAL(sexp3)[2] = MFC_STATE(p, ii).vz;
	call_R_function ("add.track.info", TR, sexp, sexp2, sexp3, NULL);
	UNPROTECT(3);
      } else { 
	// either no successor or successor has not changed
#ifdef RADR_DEBUG
	if (MFC_OSUCC(p, i) != NO_NODE && MFC_SUCC(p, i) == NO_NODE) {
	  printf("****** Error: unhandled case in MFC update: node that had a successor no longer has one\n");
	}
#endif
      }
    } /* for(i...) */
  } /* for (f...) */

  if (p->nf < 3) 
    goto done;

  // "Non-Recursive False Hypothesis Replacement":
  // As defined in [1], for each pair of consecutive frames, find a maximum weight
  // matching between points not in the path cover, using the nearest-neighbour
  // gain function (i.e. no velocity information).  If a point in frame i is matched
  // to a point in frame i-1, then that point is not used the matching between
  // frames i and i+1, so that all paths created by this method have only two points.

  // A point i is "remnant" if it is not in the maximum weight path cover and not on a track
  // and this is true iff p->pred[i] == p->succ[i] == NO_NODE && p->track[i] == TRACK_NO_TRACK

  // We build the remnant DAG with three passes 
  // 1: count remnant nodes and edges in each frame of the full dag
  // 2: gather indexes in the full DAG of points in the remnant dag
  // 3: calculate edge weights in the remnant dag
  // We skip passes 2 and 3 if the remnant dag has no edges

  rem_n2 = -1; /* -Wall */

  for (f = 0; f < p->nf; ++f) { /* f: enum frames */

    // PASS 1: count the remnant nodes in frame f

    rem_n = rem_n2; /* number of remnant points in frame f-1 */
    rem_n2 = 0; /* number of remnant points in frame f */
    for (j = MFC_N(p, f), i = MFC_FIRST(p, f); j > 0; --j, ++i) /* j: count points in frame f; i: enum points */
      if (MFC_IS_REMNANT(p, i))
	++ rem_n2;
    if (f == 0)
      continue; // first frame has been counted, loop again for the second
    
    n = rem_n + rem_n2; // total number of remnant nodes in frames f-1 and f
    m = rem_n * rem_n2; // add count of all potential edges between remnant nodes in frames f-1 and f
    
#ifdef RADR_DEBUG
    printf("In frames %d and %d, found %d remnant nodes, %d remnant edges.\n", f, f+1, n, m);
#endif
    if (m == 0)
      continue; // no potential edges between frames f-1 and f
    
    // create the remnant DAG
    rem_dag = create_dag(n, m);
    rem_pred = Calloc(n, t_node_handle);
    rem_succ = Calloc(n, t_node_handle);

    // PASS 2: get the indexes in p->x.ptr[] of the remnant points
    rem_which = Calloc(n, int);

    for (f2 = f-1, i = MFC_FIRST(p, f-1), ri = 0; f2 <= f; ++f2) /* f2: enum frames; i: enum points; ri: enum remnant points */
      for (j = MFC_N(p, f2); j > 0; --j, ++i) /* j: count points in frame f */
	if (MFC_IS_REMNANT(p, i)) {
	  // add this node's index to the list of remnant points
#ifdef RADR_DEBUG
	  if (ri >= n) {
	    printf("***** Error: mismatch in number of remnant nodes; rem_n = %d, rem_n2 = %d\n", rem_n, rem_n2);
	  }
#endif
	  rem_which[ri++] = i;
	}

    // PASS 3: populate the remnant DAG

    ri = 0; // index into remnant nodes
    k = rem_dag.edges; // pointer to dag edges
    for (j = rem_n; j > 0; --j, ++ri) {
      // add remnant node from this frame and 
      // node with possible successors
      rem_dag.nodes[ri].e = k;
      rem_dag.nodes[ri].deg = rem_n2;
      // add edges to all points in next frame
      for (jj = rem_n; jj < n; ++jj, ++k)
	k->nh = jj;
      calc_gains(p, TR, rem_which[ri], -1, rem_n2, rem_which + rem_n, &(k - rem_n2)->wt, sizeof(*k), 1);
    }
    // add remnant nodes from frame f
    for (j = 0; j < rem_n2; ++j, ++ri) { // ri continues enumerating remnant points
      // node in last frame
      rem_dag.nodes[ri].e = NULL;
      rem_dag.nodes[ri].deg = 0;
    }

    // calculate a maximum path cover for the remnant DAG:
#ifdef RADR_DEBUG
    printf("Doing remnant max_path_cover for frames %d and %d\non DAG with %d nodes and %d edges.\n", f, f+1, rem_dag.n, rem_dag.m);
#endif      
    do_max_path_cover (&rem_dag, rem_pred, rem_succ, &n_rem_cover_edges);
#ifdef RADR_DEBUG
    printf("Done; found %d matches\n", n_rem_cover_edges);
#endif

    if (n_rem_cover_edges) {
      // there are some matched points

      // add count of edges to cover
      p->n_cover_edges += n_rem_cover_edges;
      
      // add new edges to the main DAG, and inform the tracker plugin of new
      // blip-to-track assignments
      
      for (ri = 0; ri < rem_n; ++ri) {
	if ((rii = rem_succ[ri]) != NO_NODE) {
	  i = rem_which[ri];
	  ii = rem_which[rii];
	  MFC_SUCC(p, i) = ii;
	  MFC_PRED(p, ii) = i;
	  // calculate the index (within rem_dag) for the edge between
	  // points i and ii:
	  j = rii - rem_n;
#ifdef RADR_DEBUG
	  if (rem_dag.nodes[ri].e[j].nh != rii)
	    printf("****** Error: rem dag: miscalculation of location of edge between nodes %d and %d\n", ri, rii);
#endif

	  MFC_GAIN(p, ii) = rem_dag.nodes[ri].e[j].wt;
	  MFC_STATE(p, ii) = calc_state(p, &MFC_X(p, i), &MFC_STATE(p, i), &MFC_X(p, ii), TR);

	  // inform the tracker plugin of this new edge, which starts a track
	  PROTECT(sexp = allocVector(INTSXP, 2));
	  INTEGER(sexp)[0] = MFC_IBLIP(p, i);
	  INTEGER(sexp)[1] = MFC_IBLIP(p, ii);
#ifdef RADR_DEBUG
	  if (MFC_TRACK(p, i) != TRACK_NO_TRACK || MFC_TRACK(p, ii) != TRACK_NO_TRACK)
	    printf("***** Error: rem dag: a remnant point already has a track value; ti=%d, tii=%d\n", MFC_TRACK(p, i), MFC_TRACK(p, ii));
#endif
	  MFC_TRACK(p, ii) = MFC_TRACK(p, i) = INTEGER(call_R_function ("start.new.track", TR, sexp, NULL))[0];
	  UNPROTECT(1);
	  PROTECT(sexp = ScalarInteger(MFC_TRACK(p, ii)));
	  PROTECT(sexp2 = ScalarReal(INT_TO_GAIN(MFC_GAIN(p, ii))));
	  PROTECT(sexp3 = allocVector(REALSXP, 3));
	  REAL(sexp3)[0] = MFC_STATE(p, ii).vx;
	  REAL(sexp3)[1] = MFC_STATE(p, ii).vy;
	  REAL(sexp3)[2] = MFC_STATE(p, ii).vz;
	  call_R_function ("add.track.info", TR, sexp, sexp2, sexp3, NULL);
	  UNPROTECT(3);
	}
      }
    }
    Free(rem_succ);
    Free(rem_pred);
    Free(rem_which);
    free_dag(&rem_dag);
    rem_n2 -= n_rem_cover_edges;  // adjust number of remnant points in frame f, to be used in next iteration
  }

 done:
  if (preview) {
    free_instance(p);
    Free(p);
  }
  return PASS_SEXP;
}    

SEXP
end_all_tracks (SEXP ep, SEXP TR)
{
  // end all tracks.  This just deletes all saved data
  // of points; if TR is not NULL, then (possibly) call functions in the tracker
  // plugin to update its list of tracks.
  // 
  // ep:         EXTPTR;  the external pointer SEXP to the problem instance
  // TR:         ENVSXP;  the tracker plugin object (a strictenv)

  t_mfc_problem *p;
  int i;
  if (ep == R_NilValue)
    return PASS_SEXP;
  p = (t_mfc_problem *) EXTPTR_PTR(ep);
  if (!p) 
    return PASS_SEXP;
  if (p->first.ptr) {
    for (i = 0; i < p->nf; ++i) {
      MFC_FIRST(p, i) = 0;
      MFC_N(p, i) = 0;
    }
  }
  p->nf = p->np = 0;
  return PASS_SEXP;
};

SEXP
gain_from_track_to_point (SEXP ep, SEXP trackpoint, SEXP info, SEXP point, SEXP cons_scan) {
  /* 
     calculate the gain from trackpoint, with given state, to point.
     Differs from calc_gains in that neither the trackpoint nor the
     isolated point need be in the current MFC instance, and this function
     always uses the internal versions of gain functions, since it's assumed
     the caller knows how to call the user's R gain functions.
     
     ep:      EXTPTR;  the external pointer SEXP to the problem instance
     tpoint:  real vector giving coordinates of point on track
     info:    real vector giving dynamic state of point on track
     point:   real vector giving coordinates of the point for calculating gain
     cons_scan: LOGICAL vector: TRUE if the track point and free point are from
                consecutive scans, FALSE otherwise
     if LENGTH(info) == 0, we use the nearest neighbour gain, otherwise the standard gain.
  */

  t_mfc_problem *p;
  t_mfc_point p1, p2;
  t_mfc_state s;
  int gain;
  int cons;

  if (ep == R_NilValue)
    return FAIL_SEXP;
  p = (t_mfc_problem *) EXTPTR_PTR(ep);
  if (!p) 
    return FAIL_SEXP;

  cons = LOGICAL(cons_scan)[0];

  memmove(p1.coords, REAL(trackpoint), MFC_NUM_COORDS * sizeof(double));
  memmove(p2.coords, REAL(point),      MFC_NUM_COORDS * sizeof(double));

  if (LENGTH(info)) {
    memmove(s.vars, REAL(info), MFC_NUM_STATE_VARS * sizeof(double));
    gain = calc_gain(p, &s, &p1, &p2, cons);
  } else {
    gain = calc_nn_gain(p, &p1, &p2, cons);
  }
  return ScalarReal(INT_TO_GAIN(gain));
}
  
  

/*================================================================

multiframecorr.dll method registration, initialization, and destruction

==================================================================*/


R_CallMethodDef multiframecorr_call_methods[]  = {
  // R hook functions
  MKREF(destroy         , 1),
  MKREF(init            , 9),
  MKREF(set_alpha       , 2),
  MKREF(set_eps         , 2),
  MKREF(set_max_dist    , 2),
  MKREF(set_max_speed   , 2),
  MKREF(set_min_gain    , 2),
  MKREF(set_gain_pp     , 2),
  MKREF(set_gain_tp     , 2),
  MKREF(set_new_state   , 2),
  MKREF(update		, 9),
  MKREF(end_all_tracks  , 2),
  MKREF(gain_from_track_to_point, 5),
  {NULL, NULL, 0}
};

void
R_init_multiframecorr(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, multiframecorr_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  pfree_extmat = (typeof(free_extmat)*) R_GetCCallable("extmat", "free_extmat");
  R_PreserveObject (MFC_GAIN_PP_ATTR = mkString("gain.pp"));
  R_PreserveObject (MFC_GAIN_TP_ATTR = mkString("gain.tp"));
  R_PreserveObject (MFC_NEW_STATE_ATTR = mkString("new.state"));
}

void
R_unload_multiframecorr(DllInfo *info)
{
  /* Release resources. */
  R_ReleaseObject (MFC_NEW_STATE_ATTR);
  R_ReleaseObject (MFC_GAIN_TP_ATTR);
  R_ReleaseObject (MFC_GAIN_PP_ATTR);
}
