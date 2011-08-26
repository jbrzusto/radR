//  svn: $Id: runtilt1.1.c 790 2011-05-05 19:18:32Z john $

//  Copyright John Brzustowski 2011.  Distributed under the Boost
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

/**
 * \file
 * test and demo program for accessing the tilter control library from C
 *
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "tilter1.1.h"

/**
 * maximum size of a command, in bytes
 * 
 */

#define BUFSIZE 80
#define MAX_TIMES_IN_QUERY 50
int
main (int argc, char *argv[]) {

  // hold command
  char cmdbuf[1 + BUFSIZE];

  // point to part of command as it is parsed
  char *buf; 

  if (argc < 2) {
    puts("\n\
usage: tilttest DEV [LOGFILE]\n\
where\n\
   DEV:    path to the tilter control serial port (e.g. /dev/ttyUSB0)\n\
"
 );
  
    exit(1);
  }

  t11_minder *tm = t11_get_minder(argv[1], 10, (argc > 2) ? argv[2] : "/dev/stderr" );
					     
  if (!tm)
    exit(2);

  for (;;) {
    printf("Enter command ('?' for help) => ");
    fflush(stdout);

    if (!fgets (cmdbuf, BUFSIZE, stdin))
      break;

    // span white space
    for (buf = cmdbuf; isspace(*buf); ++buf);
    
    switch(*buf++) {
    case 0:
      // end of line
      continue;
      break;

    case '?':
      printf ("%s", 
	      "? this help message\n\
\n\
q  quit\n\
\n\
c  immediately calibrate to 0 degrees\n\
\n\
C [ANGLE]\n\
   skip calibration and set known tilter angle to ANGLE\n\
\n\
a ANGLE\n\
   immediately step to given angle\n\
\n\
t  print the current time\n\
\n\
p ANGLO ANGHI ANGSTEP DURATION_PER_STEP [SPEED]\n\
   set standard pattern; angles in degrees, duration in seconds\n\
   Note: duration does not include transition time.\n\
\n\
P  enter a non-standard pattern: you will be prompted to enter \n\
   ANGLE DURATION [SPEED] pairs or triples\n\
\n\
r [DELAY [DURATION]]\n\
   run (or resume) pattern, optionally starting DELAY\n\
   seconds in the future, and running for DURATION seconds\n\
\n\
R  (re)start the pattern from the beginning.\n\
\n\
s  stop pattern\n\
\n\
n  immediately move to next angle in pattern\n\
\n\
e [1-4]\n\
   set behaviour at end of pattern; 1=stop, 2=wrap around, 3=bounce back,\n\
   4=bounce back; double duration at endpoints to equalize long-term residency\n\
\n\
d [FB]\n\
   set direction forward(F) or backward(B)\n\
\n\
h  print recent tilt history\n\
\n\
l TIME {N {TIME_STEP}}\n\
   print angle(s) at absolute time(s):\n TIME, TIME + TIME_STEP, ... TIME + (n-1) TIME_STEP\n\
\n\
lr TIME_REL {N {TIME_STEP}}\n\
   print angle(s) at relative time(s): now - TIME_REL, now - TIME_REL + TIME_STEP, ...\n\
\n\
   Note: TIMEs are specified numerically (as in column 1 of output from the 'h'\n\
   command). TIME_STEP is in seconds. TIME_REL is in seconds before\n\
   now.  '{' and '}' indicate optional args; '[' and ']' show range of possible\n\
   args.\n"
	      );

      break;

    case 'q':
      goto done;
      break;

    case 't': 
      {
	double t = t11_now();
	printf("Time is %.3f  ", t);
	t11_print_time(t);
	puts(" UTC");
      }
      break;

    case 's':
      t11_stop_pattern(tm);
      break;

    case 'p':
      {
	double anglo, angstep;
	double anghi;
	double dur;
	int speed = 1;
	double total_dur;
	if (sscanf(buf, " %lf %lf %lf %lf %d", &anglo, &anghi, &angstep, &dur, &speed) < 4) {
	  puts("Error: use p ANG_LO ANG_HI ANG_STEP DURATION [speed]\n");
	  break;
	}
	total_dur = t11_set_linear_pattern(tm, anglo, angstep, 1 + (int) floor((anghi - anglo)/angstep), dur, speed);
	printf("One pass through the pattern will take %.3f seconds,\n including transition time but not including wrap-around time\n", total_dur);
	break;
      };
    case 'P':
      {
	double ang;
	double dur;
	int speed = 1;
	t11_clear_pattern(tm);
	puts("Enter pattern one piece at a time as ANGLE DURATION [SPEED]; blank line to finish.");
	for (;;) {
	  if (!fgets (buf, BUFSIZE, stdin))
	    break;
	  if (sscanf(buf, "%lf %lf %d", &ang, &dur, &speed) < 2)
	    break;
	  t11_add_to_pattern(tm, ang, dur, speed);
	}
	break; 
      }
    case 'c':
      printf("Calibration will complete in %.1f seconds\n", t11_imm_calibrate(tm));
      break;

    case 'C':
      {
	double ang = 0.0;
	if (sscanf(buf, " %lf", &ang) > 1) {
	  puts("Error: use 'C'  or 'C ANGLE'\n");
	  break;
	}
	t11_imm_set_known_state(tm, ang);
	break;
      }
	
    case 'a':
      {
	double ang;
	if (1 != sscanf(buf, " %lf", &ang)) {
	  puts("Error: use 'a ANGLE'\n");
	  break;
	}
	t11_imm_tilt_to_angle(tm, ang);
	break;
      }

    case 'n':
      t11_imm_step_pattern(tm);
      break;

    case 'e':
      {
	int which;
	if (1 != sscanf(buf, " %d", &which) || which < 1 || which > 4) {
	  puts("Error: use 'e [1-4]' ; do '?' for details\n");
	  break;
	}
	t11_set_at_end(tm, which);
	break;
      }

    case 'd':
      {
	char which;
	if (1 != sscanf(buf, " %c", &which) || (tolower(which) != 'f' && tolower(which) != 'b')) {
	  puts("Error: use 'd [FB] ; do '?' for details\n");
	  break;
	}
	t11_set_dir(tm, (tolower(which) == 'f') ? 1 : 2);
	break;
      }

    case 'h':
      {
	puts ("History:\n_time_stamp      Date        Time           Start Dest Speed");
	t11_print_history(tm);
	break;
      }

    case 'l':
      {
	int relative = 0;
	int i;

	if (*buf == 'r') {
	  relative = 1;
	  ++buf;
	}

	double bt_lo, bt_step = 1;
	double angs[MAX_TIMES_IN_QUERY];
	int n_ts = 1;
	int n = sscanf(buf, " %lf %d %lf", &bt_lo, &n_ts, &bt_step);

	if (n < 1 || n > 3) {
	  puts("Error: this command needs 1, 2, or 3 numeric timestamp arguments");
	  break;
	}
	if (n_ts > MAX_TIMES_IN_QUERY) {
	  printf("Error: the maximum number of times in an angle query is %d.\n", MAX_TIMES_IN_QUERY);
	  break;
	}

	if (relative)
	  bt_lo = t11_now() - bt_lo;

	puts("Time_stamp\t_angle");
	t11_get_angles_at_times(tm, bt_lo, bt_step, n_ts, angs);

	for (i = 0; i < n_ts; ++i, bt_lo += bt_step) {
	  printf("%10.3f\t%6.3f\n", bt_lo, angs[i] != NA_ANGLE ? angs[i] : -1);
	}	    
	break;
      }

    case 'r':
      {
	double start, end;
	double now = t11_now();
	int n = sscanf(buf, " %lf %lf", &start, &end);

	t11_set_start_time(tm, n > 0 ? now + start : NOT_A_DATE_TIME);
	t11_set_stop_time(tm, n > 1 ? now + start + end : NOT_A_DATE_TIME);
	t11_run_pattern(tm);
      }
      break;

    case 'R':
      t11_imm_restart_pattern(tm);
      break;

    default:
      puts("ERROR: command not known.\n");
    }
  }

 done:
  t11_stop_pattern(tm);
  t11_close_minder(tm);
  return 0;
}
