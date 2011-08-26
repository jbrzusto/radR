//  svn: $Id: tiltserv1.1.cc 771 2011-04-06 05:38:39Z john $

//  Copyright John Brzustowski 2011.  Distributed under the Boost
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

/**
 * \file
 * simple server to accept commands for a tilter minder over a socket connection
 *
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <utility>

#include "tilter1.1.h"
#include <ctime>
#include <iostream>
#include <string>
#include <boost/asio.hpp>

using boost::asio::ip::tcp;

/**
 * maximum size of a command, in bytes
 * 
 */

#define MAX_TIMES_IN_QUERY 50
#define __STRINGIFY(X) #X

// horrible kludge to put out messages with CR/LF on windows

#ifdef Win32
#define STD_ENDL '\r' << std::endl
#else
#define STD_ENDL std::endl
#endif

// kludge to output traffic to stdio in debug mode

#define DO_OUTPUT(X) { stream << X; if(debug) std::cout << X;}

int
main (int argc, char *argv[]) {

  // hold command
  std::string buf;

  // echo traffic to cout?
  bool debug = false;

  if (argc < 3) {
    printf("\n\
usage: %s DEV PORT [LOGFILE [-d] [-g]]\n\
where\n\
   DEV:     path to the tilter control serial port (e.g. /dev/ttyUSB0)\n\
   PORT:    port on which to listen for connections\n\
   LOGFILE: where to log commands issued to tilter\n\
   -d:      debug: echo all incoming and outgoing messages to stdout\n\
   -g:      if specified, server listens to all interfaces, not just local host\n\
            which means that any machine on the internet could send a command to\n\
            the tilter. DANGEROUS -  USE A FIREWALL TO PROTECT THE PORT!\n\
            You must also specify LOGFILE to use this option.\n",
	   argv[0]
	 );
  
    exit(1);
  }

  try { // catch server creation errors 

    t11_minder tm(new t11_device(std::string(argv[1])), new t11_model(), new t11_history(10, (argc > 3) ? argv[3] : "/dev/stderr"));
					     
    boost::asio::io_service io_service;

    // open an endpoint for listening to either all interfaces, or just the loopback interface

    tcp::endpoint endpoint;

    if ((argc > 4 && std::string(argv[4]) == std::string("-g"))
	|| (argc > 5 && std::string(argv[5]) == std::string("-g")))
      endpoint = tcp::endpoint(tcp::v4(), atoi(argv[2]));
    else
      endpoint = tcp::endpoint(boost::asio::ip::address_v4::loopback(), atoi(argv[2]));

    debug = (argc > 4 && std::string(argv[4]) == std::string("-d"))
      || (argc > 5 && std::string(argv[5]) == std::string("-d"));

    if (debug)
      std::cout << STD_ENDL << "Tilter Server Debug mode: commands received by server start with '> '" << STD_ENDL;

    tcp::acceptor acceptor(io_service, endpoint);

    for (;;) {
      tcp::iostream stream;
      acceptor.accept(*stream.rdbuf()); // wait for a connection
      DO_OUTPUT("Tilter control server 1.1 - Enter ? for help\n" << std::setprecision(14));  // high precision for timestamps

      while (stream >> buf) {
	if (debug)
	  std::cout << "> " << buf;
	try {   // catch command errors

	  switch(buf[0]) {
	  case 0:
	    // end of line
	    continue;
	    break;

	  case '?':
	    if (debug)
	      std::cout << STD_ENDL;
	    DO_OUTPUT ( "? this help message\r\n\
q  quit\r\n\
c  immediately calibrate to 0 degrees\r\n\
C ANGLE    skip calibration and set known tilter angle to ANGLE\r\n\
a ANGLE    immediately step to given angle\r\n\
t  print the current time\r\n\
p ANGLO ANGHI ANGSTEP DURATION_PER_STEP [SPEED]\r\n\
   set standard pattern; angles in degrees, duration in seconds\r\n\
   Note: duration does not include transition time.\r\n\
P  NUM ANG1 DUR1 ANG2 DUR2 ... ANGN DURN\r\n\
   enter a non-standard pattern with NUM pieces and specified\r\n\
   angles and durations\r\n\
r DELAY DURATION   run (or resume) pattern; start DELAY seconds in the future,\r\n\
   and run for DURATION seconds; -1 means \"start now\" and/or \"run forever\"\r\n\
R (re)start the pattern from the beginning.\r\n\
s stop pattern\r\n\
n immediately move to next angle in pattern\r\n\
e [1-4]   set behaviour at end of pattern; 1=stop, 2=wrap around,\r\n\
   3=bounce back, 4=bounce back but double duration at endpoints\r\n\
d [FB]  set pattern direction forward(F) or backward(B)\r\n\
h  print recent tilt history\r\n\
l TIME N TIME_STEP\r\n\
   print angle(s) at absolute time(s):\r\n\
   TIME, TIME + TIME_STEP, ... TIME + (n-1) TIME_STEP\r\n\
lr TIME_REL N TIME_STEP\r\n\
   print angle(s) at relative time(s):\r\n\
   now - TIME_REL, now - TIME_REL + TIME_STEP, ...\r\n\
L TIME DURATION MAX_ERR\r\n\
   print minimal linear approximator for angle(s) in absolute time window:\r\n\
   [TIME, TIME+DURATION] with error at most MAX_ERR\r\n\
Lr TIME_REL DURATION MAX_ERR\r\n\
   print minimal linear approximator for angle(s) in relative time window:\r\n\
   [now - TIME_REL, now - TIME_REL + DURATION] with error at most MAX_ERR\r\n\
\r\n\
Note: TIMEs are specified numerically (as in column 1 of output from the 'h'\r\n\
command). TIME_STEP is in seconds. TIME_REL is in seconds before now.\r\n\
'[' and ']' show range of args.\r\n");

	    break;

	  case 'q':
	    if (debug)
	      std::cout << STD_ENDL;
	    DO_OUTPUT( "Ok" << STD_ENDL );
	    goto done;
	    break;

	  case 't': 
	    {
	      if (debug)
		std::cout << STD_ENDL;
	      double t = time_stamp_to_double(boost::get_system_time());
	      DO_OUTPUT( t << STD_ENDL);
	    }
	    break;

	  case 's':
	    if (debug)
	      std::cout << STD_ENDL;
	    tm.stop();
	    break;

	  case 'p':
	    {
	      double anglo, anghi, angstep, dur;
	      int speed=1;
	      if (! (stream >> anglo >> anghi >> angstep >> dur))
		throw std::runtime_error("must specify ANGLO ANGHI ANGSTEP DUR");
	      if (debug)
		std::cout << ' ' << anglo << ' ' << anghi << ' ' << angstep << ' ' << dur << STD_ENDL;

	      unsigned n = 1 + floor(fabs((anghi - anglo) / angstep));
	      double total_dur = duration_to_double(tm.set_linear_pattern(state(anglo), state::delta(angstep), n, double_to_duration(dur), trans_mode(speed)));
	      DO_OUTPUT( total_dur << STD_ENDL);
	      break;
	    };
	  case 'P':
	    {
	      double ang, dur;
	      int n, speed = 1;
	      tm.clear_pattern();
	      if (! (stream >> n))
		throw std::runtime_error("must specify NUM_PIECES");
	      if (debug)
		std::cout << n << STD_ENDL;
	      for (int i = 0; i < n; ++i) {
		if (!(stream >> ang >> dur))
		  throw std::runtime_error("must specify ANGLE DURATION");
	      if (debug)
		std::cout <<  ' ' << ang << ' ' << dur << STD_ENDL;
	      tm.add_pattern_piece(state(ang), double_to_duration(dur), trans_mode(speed));
	      }
	      break; 
	    }
	  case 'c':
	    // return the expected wait time
	    if (debug)
	      std::cout << STD_ENDL;
	    DO_OUTPUT( duration_to_double(tm.calibrate()) << STD_ENDL);
	    break;

	  case 'C':
	    {
	      double ang = 0.0;
	      if (! (stream >> ang))
		throw std::runtime_error("must specify ANGLE");
	      if (debug)
		std::cout <<  ' ' << ang << STD_ENDL;
	      tm.set_known_state(state(ang));
	      break;
	    }
	
	  case 'a':
	    {
	      double ang;
	      int speed=1;
	      if (!(stream >> ang))
		throw std::runtime_error("must specify ANGLE");
	      if (debug)
		std::cout <<  ' ' << ang << STD_ENDL;
	      tm.goto_state(state(ang), trans_mode(speed));
	      break;
	    }

	  case 'n':
	    if (debug)
	      std::cout << STD_ENDL;
	    tm.step();
	    break;

	  case 'e':
	    {
	      int which;
	      if (!(stream >> which))
		throw std::runtime_error("must specify MODE");
	      if (debug)
		std::cout <<  ' ' << which << STD_ENDL;
	      if (which < 1 || which > 4)
		throw std::runtime_error("must specify mode 1, 2, 3, or 4");
	      tm.set_at_end_do((t11_pat_ctrl::at_end_do) which);
	      break;
	    }

	  case 'd':
	    {
	      char which;
	      if (!(stream >> which));
		throw std::runtime_error("must specify DIRECTION");
	      if (debug)
		std::cout <<  ' ' << which << STD_ENDL;
	      which = tolower(which);
	      if (which != 'f' && which != 'b')
		throw std::runtime_error("must specify direction F or B");
	      tm.set_step_dir(which == 'f' ? t11_pat_ctrl::FORWARD : t11_pat_ctrl::BACKWARD);
	      break;
	    }

	  case 'h':
	    {
	      if (debug)
		std::cout << STD_ENDL;
	      DO_OUTPUT( "History:\ntime_stamp      Date        Time           Start Dest Speed\n");
	      tm.print_history(stream);
	      if (debug)
		tm.print_history(std::cout);
	      break;
	    }

	  case 'l':
	    {
	      int relative = 0;

	      if (buf[1] == 'r')
		relative = 1;

	      double bt_lo, bt_step;
	      state angs[MAX_TIMES_IN_QUERY];
	      int n_ts;

	      if (!(stream >> bt_lo >> n_ts >> bt_step))
		throw std::runtime_error("must specify TIMELO N_TS TIME_STEP");
	      if (debug)
		std::cout <<  ' ' << bt_lo << ' ' << n_ts << ' ' << bt_step << STD_ENDL;
	      if (n_ts > MAX_TIMES_IN_QUERY)
		throw std::runtime_error("the maximum number of times in an angle query is " __STRINGIFY(MAX_TIMES_IN_QUERY) );

	      if (relative)
		bt_lo = t11_now() - bt_lo;

	      tm.get_state_at_times(double_to_time_stamp(bt_lo), double_to_duration(bt_step), n_ts, angs);

	      DO_OUTPUT( std::setprecision(3));

	      for (int i = 0; i < n_ts; ++i, bt_lo += bt_step)
		DO_OUTPUT( (angs[i].is_na() ? -1 : angs[i].angle) << STD_ENDL);

	      DO_OUTPUT( std::setprecision(14));
	      break;
	    }

	  case 'L':
	    {
	      int relative = 0;

	      if (buf[1] == 'r')
		relative = 1;

	      double t_lo, dur, max_err;

	      if (!(stream >> t_lo >> dur >> max_err))
		throw std::runtime_error("must specify TIME DURATION MAX_ERR");

	      if (dur < 0)
		throw std::runtime_error("DURATION must be positive");

	      if (debug)
		std::cout <<  ' ' << t_lo << ' ' << dur << ' ' << max_err << STD_ENDL;
	      if (relative)
		t_lo = t11_now() - t_lo;

	      std::pair < std::vector <time_stamp> *, std::vector <state> * > approx = tm.get_approx(double_to_time_stamp(t_lo), double_to_duration(dur), max_err);

	      if (approx.first->size() == 0) {
		// no history for given window
		DO_OUTPUT( "-1" << STD_ENDL);
	      } else {
		if (approx.first->size() == 1) {
		  // angle is constant for given window
		  DO_OUTPUT( std::setprecision(3) << approx.second->front().angle << STD_ENDL << std::setprecision(14));
		} else {
		  // output angle time pairs
		  std::vector <time_stamp> :: iterator ts = approx.first->begin();
		  std::vector <state>      :: iterator st = approx.second->begin();

		  while (ts != approx.first->end()) {
		    DO_OUTPUT( std::setprecision(3)  << (st->is_na() ? -1 : st->angle) << STD_ENDL);
		    DO_OUTPUT( std::setprecision(14) << time_stamp_to_double(*ts) << STD_ENDL);
		    ++ts;
		    ++st;
		  }
		}
	      }
	      delete approx.first;
	      delete approx.second;
	      
	      break;
	    }

	  case 'r':
	    {
	      double start, end;
	      time_stamp now = boost::get_system_time();
	      if (!(stream >> start >> end))
		throw std::runtime_error("must specify STARTTIME ENDTIME (-1 for now, -1 for never, respectively)");

	      if (debug)
		std::cout <<  ' ' << start << ' ' << end << STD_ENDL;
	      tm.set_start_time((start > 0) ? now + double_to_duration(start) : boost::date_time::not_a_date_time);
	      tm.set_stop_time((end > 0) ? now + double_to_duration(start + end) : boost::date_time::not_a_date_time);
	      tm.run();
	    }
	    break;

	  case 'R':
	    if (debug)
	      std::cout << STD_ENDL;
	    tm.restart();
	    break;

	  default:
	    if (debug)
	      std::cout << STD_ENDL;
	    throw std::runtime_error("command not known; enter ? for help");
	  }
	}
	catch (std::exception& e) {
	  std::string msg = e.what();
	  stream.clear();
	  if (debug)
	    std::cout << STD_ENDL;
	  DO_OUTPUT( "Error: " << msg << STD_ENDL);
	  stream.ignore(1000, '\n');
	}
	DO_OUTPUT( "Ok" << STD_ENDL);
      }
    }
  
  done:
    tm.stop();

  }
  catch (std::exception& e) {
      std::cerr << e.what() << STD_ENDL;
  };

  return 0;
}
