//  svn: $Id: tilter1.0.cc 474 2010-03-11 14:40:24Z john $

//  Copyright John Brzustowski 2010.  Distributed under the Boost
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

/**
 * \file 
 * definitions for a minder for version 1.0 of Dave Wilson's antenna tilter
 *
 */

#include "tilter1.0.h"



std::string state::to_string() const
{
  if (is_na())
    return std::string("NA");

  char buf[3];
  snprintf(buf, 3, "%02d", (int) angle);
  return std::string(buf);
};

double state::max_ratio(delta d1, delta d2) {
  // there's only one component, so just check that
  // the ratio is defined
  if ((double) d2 == 0.0) {
    if ((double) d1 != 0.0)
      throw new minder_x_delta_division_by_zero();
    return 0.0;
  }
  return fabs(d1 / d2);
};

std::string trans_mode::to_string() const
{
  char buf[2];
  snprintf(buf, 2, "%1d", speed);
  return std::string(buf);
};


duration t10_model::est_transit_time(const state &s1, const state &s2, const trans_mode &xm)
{
  // for now, we ignore xm

  if (s1.angle < 0 || s1.angle > 90 || s2.angle < 0 || s2.angle > 90)
    return duration(boost::date_time::not_a_date_time);

  /**
   * estimate the steps location for each angle, using the table, then
   * subtract the two, and scale by the stepping time.
   * Note: the angles will usually be integers, in which case the linear
   * estimates amount to simple table lookups.
   * The stepping speed in the two directions differs because of the way the
   * stepper firmware bytecode works.
   */
  if (s1.angle < s2.angle)
    return double_to_duration (TC_up_BC * (angle_to_steps(s2.angle) - angle_to_steps(s1.angle)));

  if (s1.angle > s2.angle)
    return double_to_duration (TC_down_BC * (angle_to_steps(s1.angle) - angle_to_steps(s2.angle)));

  return double_to_duration(0.0);
};

duration t10_model::est_total_transit_time(const state &s1, const state &s2, const trans_mode &xm)
{
  // we ignore xm, since all tilting happens at the same nominal speed

  if (s1.angle < 0 || s1.angle > 90 || s2.angle < 0 || s2.angle > 90)
    return duration(boost::date_time::not_a_date_time);

  return est_transit_time(s1, s2, xm) + double_to_duration((s1.angle < s2.angle) ? (TC_up_AB + TC_up_DE) : (TC_down_AB + TC_down_DE));
}

state::delta t10_model::get_max_transit_rate() {
  /**
   * \brief get the maximum rate of change of angle versus time
   * 
   * this comes from the stepping rate and the step->angle mapping
   * at its smallest difference.
   */

  static state::delta max_transit_rate(-1);
  if ((double) max_transit_rate < 0.0) {
    // do a search through the angle_steps table to find
    // the smallest number of steps required to change by one degree
    unsigned min_steps = angle_steps[1] - angle_steps[0];

    for (unsigned i = 2; i < sizeof(angle_steps) / sizeof(angle_steps[1]); ++i)
      if ((unsigned)(angle_steps[i] - angle_steps[i-1]) < min_steps)
	min_steps = angle_steps[i] - angle_steps[i-1];

    // units: degrees / second = degrees / (scaled_steps * time_per_scaled_step)
    max_transit_rate = state::delta(1.0 / (min_steps * step_scale * fmin(TC_up_BC, TC_down_BC)));
  }
  return max_transit_rate;
};

duration t10_model::get_max_transit_time() {
    /**
     * get the maximum time required for a state-to-state transition
     * (Stepping downward is slower than stepping upward.)
     */

  static duration max_transit_time = est_total_transit_time(state(90), state(0), trans_mode(1));
  return max_transit_time;
};

state t10_model::get_calibration_state() {
  return state(0);  // calibration leaves the tilter at 0 degrees
};

trans_mode t10_model::get_calibration_trans_mode() {
  return trans_mode(1);
};

duration t10_model::get_max_calibration_time() {
  /**
   * get the maximum time required for a calibration to 0 degrees.
   * The controller firmware forces a total delay of 2500 msec on top of
   * the time required to reach the calibration point.
   */

  static duration max_cal_time = double_to_duration(step_scale * TC_cal_BC * (angle_steps[90] - angle_steps[0]) + calibration_latency);
  return max_cal_time;
};

state t10_model::est_state_reached(const state &start, const state &dest, const duration &elapsed, const trans_mode &xm)
{
  // Note: the elapsed time is measured from the leading edge of the start bit of the
  // first byte of the command.

  // easy case: the elapsed time is larger than the max transition time, or max calibration
  // time, depending on whether the previous angle was defined

  if (elapsed >= (start.is_na() ? get_max_calibration_time() : get_max_transit_time()))
    return state(dest.angle);

  // case: insufficient data or wonky duration
  if (start.is_na() || dest.is_na() || elapsed < double_to_duration(0))
    return state::NA();

  double set_up_time = (start.angle < dest.angle) ? TC_up_AB : TC_down_AB;
  duration ttm = est_transit_time(start, dest, xm) + double_to_duration(set_up_time);

  // case: sufficient transition time since command issued
  if (elapsed >= ttm)
    return state(dest.angle);

  double double_elapsed = duration_to_double(elapsed);
  // case: insufficient time since command issued to start transitioning
  if (double_elapsed <= set_up_time)
    return state(start.angle);

  // general case: elapsed time is greater than setup time, but less than total time to move;
  // use linear interpolation in steps (which are linear in time)

  return state(steps_to_angle(linear_estimate(double_elapsed,	                        // x for estimate
					   set_up_time, angle_to_steps(start.angle),	        // x1, y1
					   duration_to_double(ttm), angle_to_steps(dest.angle)))	// x2, y2
	       );
};

duration t10_model::get_command_latency() {
  static duration cmd_latency = double_to_duration(command_latency);
  return cmd_latency;
};

duration t10_model::get_command_setup_time() {
  // the typical command setup time
  static duration cmd_setup = double_to_duration((TC_up_AB + TC_down_AB) / 2.0);
  return cmd_setup;
}

/**
 * format the "goto angle" command
 *
 * @param s the target state
 * @param xm the transition mode
 *
 * @return the formatted command, ready for transmission
 */
std::string t10_model::format_goto_state_command(const state &s, const trans_mode &xm) {
  static char buf[CMD_LENGTH + 1];

  snprintf(buf, CMD_LENGTH + 1, "$IIHDT,%02d,%1d,N,E\n", (int) s.angle, xm.speed);
  return std::string(buf);
};

/**
 * format the "calibrate" command
 *
 */
std::string t10_model::format_calibrate_command() {
  return std::string("$IIHDT,00,1,C,E\n");
};

double t10_model::angle_to_steps(double ang) {

    if (ang <= 0)
      return step_scale * angle_steps[0];

    if (ang >= 90)
      return step_scale * angle_steps[90];

    // interpolate from the table and scale
    return step_scale * linear_estimate(ang, floor(ang), angle_steps[(int)floor(ang)], ceil(ang), angle_steps[(int)ceil(ang)]);
  };

  /**
   * \brief: what angle is reached after a given number of steps?
   *
   * because the steps angle_steps mapping is not too far from linear, we
   * start at the linear guess, then search for the bracketing interval
   * and then use linear approximation.
  */

double t10_model::steps_to_angle(double step) {

    step /= step_scale; // unscale steps

    if (step <= angle_steps[0])
      return 0;

    if (step >= angle_steps[90])
      return 90;

    // estimate a reasonable place to start the search for
    // a bracketing pair of entries in the angle_steps table

    int i = (int) (step / angle_steps[90] * 90);

    while (i > 0 && angle_steps[i] > step)
      --i;

    while (i < 90 && angle_steps[i+1] < step)
      ++i;

    // i is now the largest index for which angle_steps[i] <= step
    // so interpolate from the table.
    return linear_estimate(step, angle_steps[i], i, angle_steps[i+1], i+1);
};

const double   t10_model::TC_cal_AB  =  532.2900E-3;    /**< time to initiate a calibration command */
const double   t10_model::TC_cal_BC  =    0.4806E-3;    /**< time per motor step when calibrating */
const double   t10_model::TC_cal_DE  = 2495.7000E-3;    /**< latency time for a calibration command */

const double   t10_model::TC_up_AB   =   33.2192E-3;    /**< time to initiate a stepping up command */
const double   t10_model::TC_up_BC   =    0.4269E-3;    /**< time per motor step when stepping up */
const double   t10_model::TC_up_DE   =  500.3320E-3;    /**< latency time for a stepping up command */

const double   t10_model::TC_down_AB =   33.3390E-3;    /**< time to initiate a stepping down command */
const double   t10_model::TC_down_BC =    0.4291E-3;    /**< time per motor step when stepping down */
const double   t10_model::TC_down_DE =  500.2745E-3;    /**< latency time for a stepping down command */

const double t10_model::calibration_latency = TC_cal_AB + TC_cal_DE;
const double t10_model::command_latency     = TC_up_AB + TC_up_DE;	/* this is longer than TC_down_AB + TC_down_DE */
const double t10_model::speed_cPS           =  480;			/* 4800 bps / (8 data + 1 start + 1 stop bits) */

const unsigned t10_model::step_scale = 23; // this multiplies angle_steps[] values to get actual motor steps

const unsigned t10_model::angle_steps[] = {
      0,   7,  14,  21,  28,  35,  42,  49,  56,  64, //  0 -  9
     71,  78,  86,  93, 101, 109, 116, 125, 134, 141, // 10 - 19
    150, 156, 165, 174, 183, 192, 201, 210, 218, 229, // 20 - 29
    239, 248, 254, 267, 277, 287, 296, 306, 316, 326, // 30 - 39
    336, 344, 356, 367, 377, 388, 398, 409, 417, 431, // 40 - 49
    440, 453, 463, 474, 487, 498, 509, 519, 531, 541, // 50 - 59
    554, 565, 576, 588, 599, 612, 622, 635, 647, 658, // 60 - 69
    670, 681, 694, 706, 719, 730, 743, 754, 766, 779, // 70 - 79
    791, 801, 812, 825, 838, 850, 860, 873, 884, 896, // 80 - 89
    907						      // 90
};


/*
  ------------------------------- C API ---------------------------------

  The following functions provide an interface to the library for C code.
  They also demonstrate how to use the library from C++ itself.

*/

double t10_now() {
  return time_stamp_to_double(boost::get_system_time());
}

t10_minder *t10_get_minder(const char *devname, unsigned hist_size, const char *logname) {
  t10_minder *tm;
  try {
    tm = new t10_minder(new t10_device(std::string(devname)), new t10_model(), new t10_history(hist_size, std::string(logname)));
  } catch (std::runtime_error e) {
    fprintf(stderr, "Error: %s\n", e.what());
    return NULL;
  }
  return tm;
}

void t10_close_minder (t10_minder *tm) {
  delete tm;
}

double t10_set_linear_pattern (t10_minder *tm, double start, double step, unsigned n, double dur, int xm) {
  return duration_to_double(tm->set_linear_pattern(state(start), state::delta(step), n, double_to_duration(dur), trans_mode(xm)));
}

void t10_clear_pattern (t10_minder *tm) {
  tm->clear_pattern();
};

void t10_add_to_pattern (t10_minder *tm, double ang, double dur, int xm) {
  tm->add_pattern_piece(state(ang), double_to_duration(dur), trans_mode(xm));
}

void t10_set_start_time (t10_minder *tm, double t) {
  tm->set_start_time(double_to_time_stamp(t));
}

void t10_set_stop_time (t10_minder *tm, double t) {
  tm->set_stop_time(double_to_time_stamp(t));
}

int t10_run_pattern (t10_minder *tm) {
  try {
    tm->run();
    return 0;
  } catch (std::runtime_error e) {
    fprintf(stderr, "Error: %s\n", e.what());
    return 1;
  }
}

int t10_is_pattern_running (t10_minder *tm) {
  return tm->is_pattern_running();
}

int t10_stop_pattern (t10_minder *tm) {
  try {
    tm->stop();
    return 0;
  } catch (std::runtime_error e) {
    fprintf(stderr, "Error: %s\n", e.what());
    return 1;
  }
}

void t10_get_angles_at_times (t10_minder *tm, double t, double d, int n, double *a) {
  state *s = new state[n];

  tm->get_state_at_times(double_to_time_stamp(t), double_to_duration(d), n, s);

  for (int i=0; i < n; ++i)
    a[i] = s[i].angle;

  delete [] s;
}

double t10_imm_calibrate (t10_minder *tm) {
  try {
    return duration_to_double(tm->calibrate());
  } catch (std::runtime_error e) {
    fprintf(stderr, "Error: %s\n", e.what());
    return -1;
  }
}

int t10_imm_tilt_to_angle (t10_minder *tm, double ang) {
  try {
    tm->goto_state(state(ang), trans_mode(1));
    return 0;
  } catch (std::runtime_error e) {
    fprintf(stderr, "Error: %s\n", e.what());
    return 1;
  }
}

int t10_imm_step_pattern (t10_minder *tm) {
  try {
    tm->step();
    return 0;
  } catch (std::runtime_error e) {
    fprintf(stderr, "Error: %s\n", e.what());
    return 1;
  }
}

int t10_imm_restart_pattern (t10_minder *tm) {
  try {
    tm->restart();
    return 0;
  } catch (std::runtime_error e) {
    fprintf(stderr, "Error: %s\n", e.what());
    return 1;
  }
}

int t10_is_state_known (t10_minder *tm) {
  return (int) tm->is_state_known();
}

void t10_imm_set_known_state (t10_minder *tm, double ang) {
  tm->set_known_state(state(ang));
}

void t10_set_at_end (t10_minder *tm, int at_end) {
  tm->set_at_end_do((t10_pat_ctrl::at_end_do) at_end);
}

void t10_set_dir (t10_minder *tm, int dir) {
  tm->set_step_dir((t10_pat_ctrl::step_dir) dir);
}

void t10_print_history (t10_minder *tm) {
  tm->print_history(std::cout);
}

void t10_print_time (double t) {
  std::cout << boost::posix_time::to_simple_string(double_to_time_stamp(t));
}
