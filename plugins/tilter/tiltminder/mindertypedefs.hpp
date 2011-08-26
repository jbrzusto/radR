//  svn: $Id: mindertypedefs.hpp 461 2010-02-26 01:54:47Z john $

//  Copyright John Brzustowski 2010.  Distributed under the Boost
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

/**
 * \file
 * define the basic time types for the minder class
 */

#ifndef MINDERTYPEDEFS_HPP
#define MINDERTYPEDEFS_HPP

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/posix_time/posix_time_duration.hpp>

/**
 * \brief the type used to represent points in time
 * 
 * To use a different type, you must define these functions:
 *
 *    time_stamp::operator(boost::posix_time::ptime) // cast a posix time to a time_stamp
 *
 *    boost::posix_time as_posix_time(time_stamp)	    // create a posix time from a time_stamp
 *    Note: change the definition below.
 *
 *    bool time_stamp::is_not_a_date_time()	    // is the time_stamp invalid?
 *
 *    duration operator-(time_stamp);                // subract timestamps to get a duration
 *
 *    time_stamp operator+(duration);                // add a duration to a timestamp to get a timestamp
 *
 *    time_stamp operator-(duration);                // subtract a duration from a timestamp to get a timestamp
 *
 *    bool operator_xX(time_stamp);                   // comparisons: XX is each of <, <=, ==, >=, >, !=  
 *
 *    time_stamp::time_stamp(boost::date_time::special_values sv) // must return a value for sv == not_a_date_time
 *
 *    
 *
 */
typedef boost::posix_time::ptime		time_stamp;
inline const boost::posix_time::ptime &as_posix_time(const time_stamp &x) {return x;};  // redefine if changing time_stamp type

/**
 * \brief the type used to represent lengths of time
 * 
 * To use a different type, you must define these functions:
 *
 *    duration::operator(boost::posix_time::time_duration) // cast a posix time duration to a duration
 *
 *    boost::posix_time::time_duration as_posix_duration(duration) // create a posix time duration from a duration
 *    Note: change the definition below.
 *
 *    bool duration::is_not_a_date_time()                  // is the duration invalid?
 *
 *    bool operator_xX(duration);                           // comparisons: XX is each of <, <=, ==, >=, >, !=  
 *
 *    duration::duration(boost::date_time::special_values) // must return a value for sv == not_a_date_time
 */
typedef boost::posix_time::time_duration	duration;
inline const boost::posix_time::time_duration &as_posix_duration(const duration &x) {return x;}; // redefine if changing duration type

#endif /* MINDERTYPEDEFS_HPP */
