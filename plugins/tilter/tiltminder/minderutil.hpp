//  svn: $Id: minderutil.hpp 472 2010-03-11 13:52:49Z john $

//  Copyright John Brzustowski 2010.  Distributed under the Boost
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

/**
 * \file
 * declarations of utility functions
 */

#ifndef MINDERUTIL_H
#define MINDERUTIL_H

#include "mindertypedefs.hpp"

extern double linear_estimate(double x, double x1, double y1, double x2, double y2);

#define NOT_A_DATE_TIME ((double) -1)

extern double duration_to_double(duration dur);
extern duration double_to_duration(double secs);
extern time_stamp double_to_time_stamp(double secs_AE);
extern double time_stamp_to_double(time_stamp ts);


#endif /* MINDERUTIL_H */
