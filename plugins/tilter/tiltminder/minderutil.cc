//  svn: $Id: minderutil.cc 461 2010-02-26 01:54:47Z john $

//  Copyright John Brzustowski 2010.  Distributed under the Boost
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)


/**
 * \file 
 * definitions of estimation and time conversion functions
 */

#include "minderutil.hpp"

/** 
 * linear interpolation 
 * 
 * @param x    x value for which y is to be interpolated
 * @param x1   first x value for which y is known
 * @param y1   y value for first x value
 * @param x2   second x value for which y is known
 * @param y2   y value for second x value
 * 
 * @return linear estimate of y for x.
 *         Tries to deal intelligently with cases where x == x1 and/or x == x2.
 */

double linear_estimate(double x, double x1, double y1, double x2, double y2) {
  if (x == x1)
    if (x == x2)
      return (y1 + y2) / 2.0;
    else
      return y1;
  else if (x == x2)
    return y2;

  // if x1 == x2, the following yields +/-Inf
  return ((x - x1) * y2 + (x2 - x) * y1) / (x2 - x1);
};

/*
 * time_stamp, duration <--> double conversions
 *
 */

/**
 * a double constant representing a bogus duration or timestamp
 */

#define NOT_A_DATE_TIME ((double) -1)


double duration_to_double(duration dur) {
  return dur.total_milliseconds() / 1000.0;
};

duration double_to_duration(double secs) {
  if (secs == NOT_A_DATE_TIME)
    return duration(boost::date_time::not_a_date_time);
  return boost::posix_time::seconds((long) floor(secs)) + boost::posix_time::milliseconds((long)round(1000 * fmod(secs, 1.0)));
};

time_stamp double_to_time_stamp(double secs_AE) {
  if (secs_AE == NOT_A_DATE_TIME)
    return time_stamp(boost::date_time::not_a_date_time);
  return boost::posix_time::from_time_t((time_t)floor(secs_AE)) +
    boost::posix_time::milliseconds((long) round(1000 * fmod(secs_AE, 1.0)));
};

double time_stamp_to_double(time_stamp ts) {
  static time_stamp epoch = double_to_time_stamp(0.0);

  return (ts - epoch).total_milliseconds() / 1000.0;
};

