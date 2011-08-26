/*  svn $Id: statefun.c 574 2010-05-11 02:07:15Z john $

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

    Functions for state data.

*/


/** 
 * estimate state given a linear sequence of time steps and an approximator
 *
 * An approximator is either:
 * \li a single state (consisting of \c span doubles); the approximator is constant
 * \li a sequence of (state, time) tuples; the approximator can be used to
 * estimate states over time by linear interpolation.
 * 
 * @param ap  pointer to the approximator (an array of doubles)
 * @param size total number of doubles in array ap
 * @param span number of state values per approximation point
 *
 * Note that \c size == \c span or size == \n * (\c span + 1), where \n is the number
 * of approximation points.
 *
 * @param t_start the first time at which to get the state
 * @param t_step how much to change the 
 * @param n_t the total number of times at which to estimate; these times will be
 *    t_start, t_start + t_step, ..., t_start + (n_t - 1) * t_step
 * @param NA the value to use for state components when a time is out of range of the approximator; should be a NaN
 * @param est[out] an array to hold the estimates; must be allocated to a size of at least <tt> n_t * span </tt> doubles
 * If a <tt> x_start +  i * x_step </tt> is outside the range of the approximator, the value stored in
 * <tt> est[i] </tt> is NA
 */

void estimate_from_approx (const double *ap, unsigned size, unsigned span, double t_start, double t_step, unsigned n_t, double NA, double *est) {

  // the frequent simple case: a constant approximator
  if (size == span) {
    unsigned i;
    for (i = 0; i < n_t * span; ++i)
      est[i] = ap[i % span];
    return;
  }
  
  unsigned ia = 0; // index into approximator points
  unsigned i;      // index into specified time sequence
  unsigned j;      // index into state components

  double alpha;    // for linear interpolation

  for (i = 0; i < n_t; ++i) {
    if (t_start + i * t_step < ap[ia + span]) {
      // specified time point is before approximator
      // fill this slot's state with NA
      for (j = 0; j < span; ++j)
	est[i * span + j] = NA;
	  continue;
    }
    
    // find the latest approximation point that is before the
    // current desired time

    while (ia < size && ap[ia + span] <= t_start + i * t_step)
      ia += span + 1;

    if (ia == size) {
      // we have reached the right endpoint of the approximator
      // so unless the desired time points is equal to it, fill
      // the rest of the array with NA
      if (t_start + i * t_step != ap[ia - 1])
	break;
      ia -= span + 1;
    }

    // we now have  ap[ia - 1] <=  t_start + i * t_step < ap[ia + span]
    // so interpolate

    alpha = (t_start + i * t_step - ap[ia -1]) / (ap[ia + span] - ap[ia - 1]);
    
    for (j = 0; j < span; ++j)
      est[i * span + j] = alpha * ap[ia + j] + (1.0 - alpha) * ap[ia + j - (span + 1)];

    // re use the current lower approximation point
    ia -= span + 1;
  }

  // specified time is now after approximator; fill any
  // remaining state slots with NA
  for (i = i * span; i < n_t * span; ++i) 
    est[i] = NA;
}

