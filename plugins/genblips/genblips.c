/*  svn $Id: genblips.c 701 2011-01-06 17:08:36Z john $

    radR : an R-based platform for acquisition and analysis of radar data
    Copyright (C) 2006-2010 John Brzustowski        

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


    Given that a target with simple accelerated motion was seen by a
    radar at given coordinates, how much time elapses before the
    target is next seen?

    We ignore "degenerate" situations where the target is moving fast
    enough to pass the beam; it is sufficient to assume that target
    velocities are lower than the beam's tangential sweep velocity at
    the same range.  For even a slow radar (say 1 radian per second ~=
    9.5 RPM) at close range (100 m), this only weakly restricts target
    velocity (must be <= 100 m/s).

*/
  
#include "radRmodule.h"
  
#define CONVERGENCE_PRECISION 1e-8  // we treat solution as converged when change in time is <= 1 microsecond
#define MAX_ITERATIONS 100 // if this is exceeded, no convergence was obtained
#define SQUARED_DISTANCE_TOLERANCE 10 // if square of distance to target is <= this, target is treated as "seen"

void
next_encounter (int *n, int *ns, double *x, double *y, double *z, double *vx, double *vy, double *vz, double *ax, double *ay, double *az, double *t, double *omega) {

  // n: vector: number of targets;
  // ns: vector: number of sweeps forward (or backward) in time to look for next encounter
  // x, y, z: inout: target positions
  // vx, vy, vz: inout: target velocites
  // ax, ay, az: in: target accelerations
  // t: inout: time target was seen
  // omega: in: radar angular velocity, in rotations per second (positive is clockwise)

  // Target [i] was seen at coordinates (x[i], y[i], z[i]) at time t[i], at which time its velocity was (vx[i], vy[i], vz[i])
  // Its constant acceleration is (ax[i], ay[i], az[i])
  // How long before the the radar next sees the target, given the radar completes omega
  // rotations (clockwise) per second (i.e. what is time to next encounter)?

  // Outputs new position in *x, *y, *z; new velocity in *vx, *vy, *vz; time of next encounter in *t
  
  // This assumes an infinitely narrow and infinitely tall beam with vertical rotation axis at the origin.
  // i.e. by "seen", we mean "seen at the beam's horizontal center".

  // The problem is solve this system for dt (and hence x1, y1):
  //
  //    x1 = x0 + vx * dt + ax * 1/2 * dt^2
  //    y1 = y0 + vy * dt + ay * 1/2 * dt^2
  //    atan2(y1, x1) = atan2(y0, x0) + dt*2*pi*omega  (letting omega > 0 for a clockwise-turning radar)

  // We obtained a solution by iteration: start with a duration
  // corresponding to one rotation, then compare
  // the azimuths of target and radar, and adjust the duration accordingly:
  // if the target has moved counterclockwise, reduce the duration;
  // otherwise, increase it.

  //
  // For tracking a single target over k sweeps, pass n=1 and ns as a vector of length k, and make
  // sure that there is sufficient space in x, y, z, t, vx, vy, vz to store k outputs.
  
  int i, j;

  double rt = 1.0 / *omega; // time for one rotation
  double dt0, dt, ddt;
  double x2, y2;

  double ang0, ang1, dang;

  for (i = 0; i < *n; ++i) {
    dt = dt0 = *ns * rt ; // initial guess for time to next encounter: ns whole rotations
    ang0 = atan2(y[i], x[i]);
    for (j = 0; j < MAX_ITERATIONS; ++j) {
      x2 = x[i] + dt * (vx[i] + dt / 2.0 * ax[i]);
      y2 = y[i] + dt * (vy[i] + dt / 2.0 * ay[i]);

      if (x2*x2 + y2*y2 < SQUARED_DISTANCE_TOLERANCE) // target is "at" radar, so seen
	break;

      ang1 = atan2(y2, x2);
      dang = ang1 - (ang0 - 2 * M_PI * (dt - dt0) / rt);
      if (dang >= M_PI)
	dang -= 2 * M_PI;
      else if (dang <= - M_PI)
	dang += 2 * M_PI;
      ddt = dang / (2 * M_PI) * rt;
      dt -= ddt;
      if (fabs(ddt) < CONVERGENCE_PRECISION)
	break;
    }
    if (j == MAX_ITERATIONS)
      dt = NA_REAL; // FIXME: kludge; we should return NA here and not change other values and let caller deal with it
    x[i] = x2;
    y[i] = y2;
    z[i] = z[i] + dt * (vz[i] + dt / 2.0 * az[i]);
    vx[i] = vx[i] + ax[i] * dt;
    vy[i] = vy[i] + ay[i] * dt;
    vz[i] = vz[i] + az[i] * dt;
    t[i] += dt;
  }
}

// same thing, except each target is projected multiple sweeps into the future

void
multiple_next_encounters (int *n, int *k, int *ns, double *xin, double *yin, double *zin, double *tin, double *vx, double *vy, double *vz, double *ax, double *ay, double *az, double *omega, double *xout, double *yout, double *zout, double *tout) {

  // n: input scalar: number of targets;
  // k: input scalar: how many sweeps to examine
  // ns: input vector of length k: number of sweeps forward (or backward) in time to look for next encounters
  // xin, yin, zin, tin: input vectors of length n each: initial target positions and times
  // omega: input: radar angular velocity, in rotations per second (positive is clockwise)
  // vx, vy, vz: input: target velocites
  // ax, ay, az: input: target accelerations
  // xout, yout, zout, tout: target positions and times in the specified future sweeps; each must have size n * k;
  // target positions and times are returned sorted by time within target.

  // Target [i] was seen at coordinates (x[i], y[i], z[i]) at time t[i], at which time its velocity was (vx[i], vy[i], vz[i])
  // Its constant acceleration is (ax[i], ay[i], az[i])
  // Compute the encounter position and times for each of several sweeps in the future for each of several targets.

  // Outputs encounter positions and times in *xout, *yout, *zout, *tout
  
  // This assumes an infinitely narrow and infinitely tall beam with vertical rotation axis at the origin.
  // i.e. by "seen", we mean "seen at the beam's horizontal center".
  
  int it; // iterate over targets
  int is; // iterate over sweeps
  int iout; // iterate over coordinate outputs
  int j; // iterate solver

  double rt = 1.0 / *omega; // time for one rotation
  double dt0, dt, ddt;
  double x2, y2;

  double ang0, ang1, dang;

  iout = 0;
  for (it = 0; it < *n; ++it) {
    for (is = 0; is < *k; ++is, ++iout) {
      dt = dt0 = ns[is] * rt ; // initial guess for time to next encounter: ns whole rotations
      ang0 = atan2(yin[it], xin[it]);
      for (j = 0; j < MAX_ITERATIONS; ++j) {
	x2 = xin[it] + dt * (vx[it] + dt / 2.0 * ax[it]);
	y2 = yin[it] + dt * (vy[it] + dt / 2.0 * ay[it]);
	
	if (x2*x2 + y2*y2 < SQUARED_DISTANCE_TOLERANCE) // target is "at" radar, so seen
	  break;
	
	ang1 = atan2(y2, x2);
	dang = ang1 - (ang0 - 2 * M_PI * (dt - dt0) / rt);
	if (dang >= M_PI)
	  dang -= 2 * M_PI;
	else if (dang <= - M_PI)
	  dang += 2 * M_PI;
	ddt = dang / (2 * M_PI) * rt;
	dt -= ddt;
	if (fabs(ddt) < CONVERGENCE_PRECISION)
	  break;
      }
      if (j == MAX_ITERATIONS) {
	xout[iout] = NA_REAL;
	yout[iout] = NA_REAL;
	zout[iout] = NA_REAL;
	tout[iout] = NA_REAL;
      } else {
	xout[iout] = x2;
	yout[iout] = y2;
	zout[iout] = zin[it] + dt * (vz[it] + dt / 2.0 * az[it]);
	tout[iout] = tin[it] + dt;
      }
    }
  }
}

static unsigned int next_encounter_t[] = {INTSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP};
static unsigned int multiple_next_encounters_t[] = {INTSXP, INTSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP};

static const R_CMethodDef genblips_c_methods[]  = {
  {"next_encounter", (DL_FUNC) &next_encounter, 13, next_encounter_t},
  {"multiple_next_encounters", (DL_FUNC) &multiple_next_encounters, 18, multiple_next_encounters_t},
  {NULL, NULL, 0}
};

void
R_init_genblips(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, genblips_c_methods, NULL, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

void
R_unload_genblips(DllInfo *info)
{
  /* Release resources. */
}
