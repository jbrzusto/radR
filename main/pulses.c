/*  svn $Id: pulses.c 574 2010-05-11 02:07:15Z john $

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

    generate pulse meta-data

*/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#ifdef Win32
#include <malloc.h>
#else
#include <alloca.h>
#endif
extern void estimate_from_approx (const double *ap, unsigned size, unsigned span, double t_start, double t_step, unsigned n_t, double NA, double *est);

/* degree versions of trig functions */

#define cos_d(x) cos((x) * (M_PI / 180))
#define sin_d(x) sin((x) * (M_PI / 180))

/* square an expression */

#define SQUARE(x) ({typeof(x) __x = x; __x*__x;})

/**
 *  \brief get the azimuth, elevation, and rotation of the beam axis for each pulse.
 *
 *  Note: rotation of the beam axis is defined for a waveguide-fed
 *  antenna as the angle between the horizontal plane and the long
 *  axis of the waveguide. The latter is assumed to be perpendicular
 *  to both the beam axis and the antenna rotation axis.
 */


void
get_pulse_metadata (unsigned		 np,		/**< number of pulses */
		    double		 t0,		/**< timestamp of first pulse */
		    double		 dur,		/**< duration of scan, in seconds */
		    double               orientation,   /**< orientation of scan +1 = clockwise, -1 = CCW */
		    double		 ang_offset,	/**< rotation angle of first pulse (degrees CW from N), when ax_elev=90 deg */
		    const double	*rot_ax,	/**< rotation axis azimuth, elevl approximator */
		    unsigned		 na,		/**< number of elements in rotation axis approximator */
		    const double	*tilt,		/**< antenna tilt angle (w.r.t. plane perpendicular to rotation axis 
                                                          (degrees away from plane perpendicular to rotation axis) */
		    unsigned             nt,		/**< number of elements in tilt approximator (1 means constant)  */
		    double		*ts,		/**< [output] timestamp for each pulse */
		    double		*azi,		/**< [output] beam axis azimuth for each pulse (degrees CW from N) */
		    double		*elev,		/**< [output] beam axis elevation for each pulse (degrees above horizontal) */
		    double		*wg_azi,	/**< [output] azimuth of long waveguide axis for each pulse (degrees CW from N) */
		    double		*wg_elev,	/**< [output] elevation of long waveguide axis for each pulse (degrees above horizontal) */
		    double NA	        		/**< NaN value to use for Not Available (on both input and output) */
		       )
{
  double time_step = dur / np;
  double *tmp_rot_ax = (double *) alloca(np * 2 * sizeof(double)) ;
  double *tmp_tilt = wg_azi; 

  unsigned i = 0;
  for (i = 0; i < np; ++i)
    ts[i] = t0 + i * time_step;

  // get values of rotation axis azimuth, rotation axis elevation, and
  // antenna tilt for all pulses (temporarily use output arrays for
  // this)

  estimate_from_approx (rot_ax,  na, 2, t0, time_step, np, NA, tmp_rot_ax  );
  estimate_from_approx (tilt,    nt, 1, t0, time_step, np, NA, tmp_tilt    );

  // for each pulse, compute pulse azimuth, elevation, and rotation

  for (i = 0; i < np; ++i) {

    // unit vector pointing in direction of antenna rotation axis
    double rax = sin_d(tmp_rot_ax[i*2]) * cos_d(tmp_rot_ax[2*i+1]);
    double ray = cos_d(tmp_rot_ax[i*2]) * cos_d(tmp_rot_ax[2*i+1]);
    double raz = sin_d(tmp_rot_ax[2*i+1]);

    // unit vectors in plane perpendicular to rotation axis,
    // corresponding to rotations of 0, 90 degrees about that axis
    // (where zero corresponds to the lower position parallel to the horizon)

    double u1x, u1y, u1z, u2x, u2y, u2z;

    // unit vector in direction of beam axis
    double bax, bay, baz;

    // vector in direction of waveguide long axis
    double wgx, wgy, wgz;

    // angle of rotation from "0" about this axis
    double theta;

    // tilting the axis of rotation rotates the vectors uy and ux
    // (unit vectors in x and y directions) into the vectors u1 and u2 as so:

    u1x = cos_d(tmp_rot_ax[i*2]) * sin_d(tmp_rot_ax[i*2]) * (sin_d(tmp_rot_ax[2*i+1]) - 1.0);
    u1y = SQUARE(sin_d(tmp_rot_ax[i*2])) + sin_d(tmp_rot_ax[2*i+1]) * SQUARE(cos_d(tmp_rot_ax[i*2]));
    u1z = -cos_d(tmp_rot_ax[i*2]) * cos_d(tmp_rot_ax[2*i+1]);

    u2x = 1 + sin_d(tmp_rot_ax[2*i+1]) - u1y;
    u2y = u1x;
    u2z = -sin_d(tmp_rot_ax[i*2]) * cos_d(tmp_rot_ax[2*i+1]);

    // by how much is the antenna rotated around the rotation axis?
    theta = M_PI* (ang_offset / 180.0 + i * orientation * 2.0 / np);

    // combine the rotation axis and the appropriate vector in the perpendicular plane to get the beam axis
    bax = cos_d(tmp_tilt[i]) * (cos(theta) * u1x + sin(theta) * u2x) + sin_d(tmp_tilt[i]) * rax;
    bay = cos_d(tmp_tilt[i]) * (cos(theta) * u1y + sin(theta) * u2y) + sin_d(tmp_tilt[i]) * ray;
    baz = cos_d(tmp_tilt[i]) * (cos(theta) * u1z + sin(theta) * u2z) + sin_d(tmp_tilt[i]) * raz;

    // the cross product of beam axis and rotation axis gives the long waveguide axis
    // this is not necessarily a unit vector

    wgx = bay * raz - ray * baz;
    wgy = -(bax * raz - rax * baz);
    wgz = bax * ray - rax * bay;
    
    // convert beam axis to polar coords

    azi[i]  = fmod(360.0 + 90.0 - atan2 (bay, bax) * 180 / M_PI, 360.0); // azimuth, degrees clockwise from North
    elev[i] = asin(baz) * 180 / M_PI; // elevation; degrees above horizontal

    // convert long waveguide axis to polar coords
    wg_azi[i]  = fmod(360.0 + 90.0 - atan2 (wgy, wgx) * 180 / M_PI, 360.0); // azimuth, degrees clockwise from North
    wg_elev[i] = asin(wgz / sqrt(wgx*wgx + wgy*wgy + wgz*wgz)) * 180 / M_PI; // elevation; degrees above horizontal
    
  }
}


      
  
