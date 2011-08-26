//  svn: $Id: tilter1.1.cc 789 2011-05-05 19:15:45Z john $

//  Copyright John Brzustowski 2011.  Distributed under the Boost
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

/**
 * \file 
 * definitions for a minder for version 1.1 of Dave Wilson's antenna tilter
 *
 */

#include "tilter1.1.h"



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


duration t11_model::est_transit_time(const state &s1, const state &s2, const trans_mode &xm)
{
  // for now, we ignore xm

  if (s1.angle < 0 || s1.angle > 90 || s2.angle < 0 || s2.angle > 90)
    return duration(boost::date_time::not_a_date_time);

  if (s1.angle == s2.angle)
    return double_to_duration(0.0);

  /**
   * estimate the steps location for each angle, using the table, then
   * subtract the two, and convert to stepping time.
   * Note: the angles will usually be integers, in which case the linear
   * estimates amount to simple table lookups.
   * The stepping speed in the two directions is identical for rev 1.1 firmware.
   */
  return double_to_duration (steps_to_time(abs(angle_to_steps(s2.angle) - angle_to_steps(s1.angle))));
};

duration t11_model::est_total_transit_time(const state &s1, const state &s2, const trans_mode &xm)
{
  // we ignore xm, since all tilting happens at the same nominal speed

  if (s1.angle < 0 || s1.angle > 90 || s2.angle < 0 || s2.angle > 90)
    return duration(boost::date_time::not_a_date_time);

  if (s1.angle == s2.angle)
    return double_to_duration(0.0);
  return est_transit_time(s1, s2, xm) + double_to_duration((s1.angle < s2.angle) ? (TC_up_AB + TC_up_DE) : (TC_down_AB + TC_down_DE));
}

state::delta t11_model::get_max_transit_rate() {
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
    max_transit_rate = state::delta(1.0 / (min_steps * step_scale * TC_step_BC[NUM_STEPPING_RATES - 1]));
  }
  return max_transit_rate;
};

duration t11_model::get_max_transit_time() {
    /**
     * get the maximum time required for a state-to-state transition
     * (Stepping downward is slower than stepping upward.)
     */

  static duration max_transit_time = est_total_transit_time(state(90), state(0), trans_mode(1));
  return max_transit_time;
};

state t11_model::get_calibration_state() {
  return state(0);  // calibration leaves the tilter at 0 degrees
};

trans_mode t11_model::get_calibration_trans_mode() {
  return trans_mode(1);
};

duration t11_model::get_max_calibration_time() {
  /**
   * get the maximum time required for a calibration to 0 degrees.
   * The controller firmware forces a total delay of 2500 msec on top of
   * the time required to reach the calibration point.
   */

  static duration max_cal_time = double_to_duration(step_scale * TC_step_BC[NUM_STEPPING_RATES - 1] * (angle_steps[90] - angle_steps[0]) + calibration_latency);
  return max_cal_time;
};

state t11_model::est_state_reached(const state &start, const state &dest, const duration &elapsed, const trans_mode &xm)
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

  double direction = dest.angle > start.angle ? 1.0 : -1.0;
  // general case: elapsed time is greater than setup time, but less than total time to move;
  // get time difference and convert to steps gone; requires knowing total steps for move

  return state(steps_to_angle(angle_to_steps(start.angle) + direction * time_to_steps(double_elapsed - set_up_time,
					    abs(angle_to_steps(dest.angle) - angle_to_steps(start.angle)))));
};

double t11_model::steps_to_time(int n)
{
    double t = 0.0;
    int i;
    for (i = 0; i < NUM_STEPPING_RATES; ++i) {
      if (n < 2 * STEPS_AT_RATE[i])
	break; // we always leave the loop via this step, because STEPS_AT_RATE[NUM_STEPPING_RATES] is large
      t += 2 * STEPS_AT_RATE[i] * TC_step_BC[i];
      n -= 2 * STEPS_AT_RATE[i];
    }
    t += n * TC_step_BC[i];
    return t;
}

double t11_model::time_to_steps(double t, int total_steps)
{
  int n = total_steps;
  int i;
  // determine maximum stepping rate used
  for (i = 0; i < NUM_STEPPING_RATES; ++i) {
    if (n < 2 * STEPS_AT_RATE[i])
      break; // we always leave the loop via this step, because STEPS_AT_RATE[NUM_STEPPING_RATES] is large
    n -= 2 * STEPS_AT_RATE[i];
  }
  int imax = i; // index of max stepping rate
  int nmax = n; // number of steps done at max rate
  int ns; // number of steps taken at current rate
  n = 0;
  i = 0;
  int k = 1; // stepping direction for i through rate table
  for (;;) {
    // loop through rate table, up to max and then back down, until we exhaust time
    if (i == imax) {
      ns = nmax;
      k = -1; // reverse direction of motion through table
    } else {
      ns = STEPS_AT_RATE[i];
    }
    if (t <= ns * TC_step_BC[i] )
      return n + t / TC_step_BC[i]; // remainder of time is at this rate; finish up
    t -= ns * TC_step_BC[i];
    n += ns;
    i += k;
  } 
}

duration t11_model::get_command_latency() {
  static duration cmd_latency = double_to_duration(command_latency);
  return cmd_latency;
};

duration t11_model::get_command_setup_time() {
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
std::string t11_model::format_goto_state_command(const state &s, const trans_mode &xm) {
  static char buf[CMD_LENGTH + 1];

  snprintf(buf, CMD_LENGTH + 1, "$IIHDT,%02d,%1d,N,E\n", (int) s.angle, xm.speed);
  return std::string(buf);
};

/**
 * format the "calibrate" command
 *
 */
std::string t11_model::format_calibrate_command() {
  return std::string("$IIHDT,00,1,F,E\n"); // need to use the "forced" calibrate command to get to known state of 0 degrees
};

double t11_model::angle_to_steps(double ang) {

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

double t11_model::steps_to_angle(double step) {

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

const int      t11_model::STEPS_AT_RATE[t11_model::NUM_STEPPING_RATES] = {400, 300, 200, 200, 200, 200, 200, 0x10000000}; // last value represents "remaining"

const double   t11_model::TC_cal_AB = 0.133607;
const double   t11_model::TC_cal_BC = 0.000416;
const double   t11_model::TC_cal_DE = 3.598884;
const double   t11_model::TC_down_AB = 0.049746;
const double   t11_model::TC_down_DE = 0.000327;
const double   t11_model::TC_up_AB = 0.049747;
const double   t11_model::TC_up_DE = 0.000339;

const double   t11_model::TC_step_BC[t11_model::NUM_STEPPING_RATES] = {0.001666, 0.001348, 0.001029, 0.000709, 0.000629, 0.000549, 0.000509, 0.000462};

const double t11_model::calibration_latency = TC_cal_AB + TC_cal_DE;
const double t11_model::command_latency     = TC_down_AB + TC_up_DE;	/* longest possible command latency */

const unsigned t11_model::step_scale = 23; // this multiplies angle_steps[] values to get actual motor steps

const unsigned t11_model::angle_steps[] = {
  // _0   _1   _2   _3   _4   _5   _6   _7   _8   _9
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

double t11_now() {
  return time_stamp_to_double(boost::get_system_time());
}

t11_minder *t11_get_minder(const char *devname, unsigned hist_size, const char *logname) {
  t11_minder *tm;
  try {
    tm = new t11_minder(new t11_device(std::string(devname)), new t11_model(), new t11_history(hist_size, std::string(logname)));
  } catch (std::runtime_error e) {
    fprintf(stderr, "Error: %s\n", e.what());
    return NULL;
  }
  return tm;
}

void t11_close_minder (t11_minder *tm) {
  delete tm;
}

double t11_set_linear_pattern (t11_minder *tm, double start, double step, unsigned n, double dur, int xm) {
  return duration_to_double(tm->set_linear_pattern(state(start), state::delta(step), n, double_to_duration(dur), trans_mode(xm)));
}

void t11_clear_pattern (t11_minder *tm) {
  tm->clear_pattern();
};

void t11_add_to_pattern (t11_minder *tm, double ang, double dur, int xm) {
  tm->add_pattern_piece(state(ang), double_to_duration(dur), trans_mode(xm));
}

void t11_set_start_time (t11_minder *tm, double t) {
  tm->set_start_time(double_to_time_stamp(t));
}

void t11_set_stop_time (t11_minder *tm, double t) {
  tm->set_stop_time(double_to_time_stamp(t));
}

int t11_run_pattern (t11_minder *tm) {
  try {
    tm->run();
    return 0;
  } catch (std::runtime_error e) {
    fprintf(stderr, "Error: %s\n", e.what());
    return 1;
  }
}

int t11_is_pattern_running (t11_minder *tm) {
  return tm->is_pattern_running();
}

int t11_stop_pattern (t11_minder *tm) {
  try {
    tm->stop();
    return 0;
  } catch (std::runtime_error e) {
    fprintf(stderr, "Error: %s\n", e.what());
    return 1;
  }
}

void t11_get_angles_at_times (t11_minder *tm, double t, double d, int n, double *a) {
  state *s = new state[n];

  tm->get_state_at_times(double_to_time_stamp(t), double_to_duration(d), n, s);

  for (int i=0; i < n; ++i)
    a[i] = s[i].angle;

  delete [] s;
}

double t11_imm_calibrate (t11_minder *tm) {
  try {
    return duration_to_double(tm->calibrate());
  } catch (std::runtime_error e) {
    fprintf(stderr, "Error: %s\n", e.what());
    return -1;
  }
}

int t11_imm_tilt_to_angle (t11_minder *tm, double ang) {
  try {
    tm->goto_state(state(ang), trans_mode(1));
    return 0;
  } catch (std::runtime_error e) {
    fprintf(stderr, "Error: %s\n", e.what());
    return 1;
  }
}

int t11_imm_step_pattern (t11_minder *tm) {
  try {
    tm->step();
    return 0;
  } catch (std::runtime_error e) {
    fprintf(stderr, "Error: %s\n", e.what());
    return 1;
  }
}

int t11_imm_restart_pattern (t11_minder *tm) {
  try {
    tm->restart();
    return 0;
  } catch (std::runtime_error e) {
    fprintf(stderr, "Error: %s\n", e.what());
    return 1;
  }
}

int t11_is_state_known (t11_minder *tm) {
  return (int) tm->is_state_known();
}

void t11_imm_set_known_state (t11_minder *tm, double ang) {
  tm->set_known_state(state(ang));
}

void t11_set_at_end (t11_minder *tm, int at_end) {
  tm->set_at_end_do((t11_pat_ctrl::at_end_do) at_end);
}

void t11_set_dir (t11_minder *tm, int dir) {
  tm->set_step_dir((t11_pat_ctrl::step_dir) dir);
}

void t11_print_history (t11_minder *tm) {
  tm->print_history(std::cout);
}

void t11_print_time (double t) {
  std::cout << boost::posix_time::to_simple_string(double_to_time_stamp(t));
}
