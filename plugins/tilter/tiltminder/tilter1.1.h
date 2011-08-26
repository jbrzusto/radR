//  svn: $Id: tilter1.1.h 790 2011-05-05 19:18:32Z john $

//  Copyright John Brzustowski 2011.  Distributed under the Boost
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

/**
 * \file 
 * declarations for a minder for version 1.1 of Dave Wilson's antenna tilter
 * This version allows microstepping, and deals reasonably with triggering
 * of limit switches.  Timing has changed too.
 *
 */

#ifndef TILTER_1_1_H
#define TILTER_1_1_H

#ifdef __cplusplus

#include "minder.cc"
#include <fstream>

class	state;			// forward declaration
class	trans_mode;		// forward declaration


typedef double	tilt_angle;	/**< the type representing an angle */

/**
 * \brief the class representing the state of the version 1.1 tilter
 *
 * This is really just a single angle in the range 0..90.
 * The methods are just to allow type-checking of operations like
 * adding deltas to tilt_angles, and to allow for an "tilt_angle Unknown"
 * value.
 *
 */

class state {
public:

  tilt_angle angle;		/**< the angle of elevation above horizontal, in degrees */

  /**
   * the type representing the difference between two angles; it's just a double, but
   * we wrap it in a class so that we can use it generically from the minder template class
   */

  struct delta {
    double d;			/**< the angle difference */

    /**
     * construct an angle difference of 0 degrees
     */
    delta() : d(0.0) {};

    /**
     * construct an angle difference of dd degrees
     */
    delta(double dd) : d(dd) {};

    /**
     * convert an angle difference to a plain double
     */
    operator double() {return this->d;};

    /**
     * the larger of two angle differences
     */
    static inline delta max(const delta & d1, const delta & d2) {return delta(fmax(d1.d, d2.d));};

    /**
     * the smaller of two angle differences
     */
    static inline delta min(const delta & d1, const delta & d2) {return delta(fmin(d1.d, d2.d));};

    /**
     * the absolute value of an angle difference
     */
    static inline delta abs(const delta & d) {return delta(fabs(d.d));};

    /**
     * multiply an angle difference by a factor
     */
    inline delta operator*(double f) {return delta(this->d * f);};

    /**
     * divide an angle difference by a factor
     */
    inline delta operator/(double f) {return delta(this->d / f);};

    /**
     * the largest magnitude of a ratio of the component-wise division
     * of d1 / d2 (with 0 / 0 treated as 0).  If a component of d2 is
     * zero and the corresponding component of d1 is not, throws an
     * exception.
     * 
     */
    static double max_ratio(delta d1, delta d2);

    /**
     * is one angle difference less than or equal to another?
     */
    inline bool operator<=(const delta & d) {return this->d <= d.d;};
  };


  /**
   * constructors
   *
   */
  state() {};
  state(tilt_angle ang) {angle = ang;};

  /**
   * \brief angle value representing "Unknown tilt_angle"
   *
   * we use a value unlikely to ever be needed in future designs
   */
  static const tilt_angle NA_ANGLE = -360;

  /**
   * return a state representing Unknown state
   *
   */

  static inline state NA() { return state(NA_ANGLE);};

  /**
   * is this state the NA state?
   *
   */
  inline bool is_na() const {return angle == NA_ANGLE;};

  /**
   * how to add a delta to a state to get another state
   *
   */

  state operator+(const state::delta &d) {return state(angle + d.d);};

  /**
   * how to increment a state by a delta
   *
   */

  state & operator+=(const state::delta &d) {angle += d.d; return *this;};

  /**
   * how to subtract a delta from a state to get another state
   *
   */

  state operator-(const state::delta &d) {return state(angle - d.d);};

  /**
   * how to subtract one state from another to get a delta
   *
   */
  delta operator-(const state &s) {return state::delta(angle - s.angle);};

  /**
   * how to tell if two states are the same
   *
   */
  bool operator	== (const state &s) {return this->angle == s.angle;};

  /**
   * print the state as a string
   *
   */
  std::string to_string() const;

  /**
   * the largest magnitude of a ratio of the component-wise division
   * of d1 / d2 (with 0 / 0 treated as 0).  If a component of d2 is
   * zero and the corresponding component of d1 is not, throws an
   * exception.
   * 
   */
  static double max_ratio(delta d1, delta d2);


};

/**
 * \brief the class representing the transition mode for version 1.1 of the tilter
 *
 * We don't currently use this, although in theory, we could use it to represent
 * the unused "speed" component in the tilter NMEA command.
 */

class trans_mode {
 public:
  int speed;

  /**
   * constructors
   *
   */

  trans_mode() {};

  trans_mode(int speed) {this->speed = speed;};

  /**
   * represent transition mode as a string
   *
   */
  std::string to_string() const;
};

/**
 * Typedefs for the template classes, using the tilter 1.1 state and transition mode types.
 * These serve as base classes for our derived classes, which will have the same name, but
 * prefixed with Tilter_1_0_
 *
 */

typedef class minder_pattern_piece <state, trans_mode>	t11_pattern_piece;
typedef class minder_pattern	 <state, trans_mode>	t11_pattern;
typedef class minder_pat_ctrl	 <state, trans_mode>	t11_pat_ctrl;
typedef class minder_hist_entry	 <state, trans_mode>	t11_hist_entry;
typedef class minder_history	 <state, trans_mode>	t11_history;
typedef class minder_device	 <state, trans_mode>	t11_device;
typedef class minder_model	 <state, trans_mode>	t11_model_base;
typedef class minder		 <state, trans_mode>	t11_minder;

/**
 * Class representing a physical model of version 1.1 of Dave Wilson's radar antenna tilter
 *
 */

class t11_model : public t11_model_base {
/**
 * \details Time constants for various timelines
 *
 *
 *   Calibration command
 * \verbatim
 +----------+----------+----- ... -----+----------+
 A          B          C               D          E
 \endverbatim
 *
 *        \li CAL_A:  leading edge of start bit for first byte in command
 *        \li CAL_B:  just before first calibration step
 *        \li CAL_C:  just before 2nd calibration step
 *        \li CAL_D:  just after nth calibration step
 *        \li CAL_E:  ready to receive another command
 *
 *
 *
 *   Tilt upwards command
 * \verbatim
 +----------+----------+----- ... -----+----------+
 A          B          C               D          E
 \endverbatim
 *
 *        \li TILTUP_A:  leading edge of start bit for first byte in command
 *        \li TILTUP_B:  just before first upward step
 *        \li TILTUP_C:  just before 2nd upward step
 *        \li TILTUP_D:  just after nth upward step
 *        \li TILTUP_E:  ready to receive another command
 *
 *
 *   Tilt downwards command
 * \verbatim
 +----------+----------+----- ... -----+----------+
 A          B          C               D          E
 \endverbatim
 *
 *        \li TILTDOWN_A:  leading edge of stop bit for first byte in command
 *        \li TILTDOWN_B:  just before first downward step
 *        \li TILTDOWN_C:  just before 2nd downward step
 *        \li TILTDOWN_D:  just after nth downward step
 *        \li TILTDOWN_E:  ready to receive another command
 *
 *  The class constants TC_*_* were obtained by compiling tilter firmware code in file
 *
 *  BIRD RADAR LINEARIZED STEP RX - 16_f876_a REV1.1.BAS
 *
 *  and then running the .HEX file through Microchip's MPLAB SIM,
 *  using breakpionts and the stopwatch feature.
 *
 *  Exact addresses at which timing was done are flagged in the listing
 *  file keep1_1.lst by the comments containing "TILTER_TIMING_POINT"
 *
 *  Due to the nature of the nested loops in the code, the times
 *  for motor steps when moving up and down were derived
 *  from linear models fitted to steps between (0, 10), (10, 30), and
 *  (30, 70) degrees.  The fit is perfect, as you'd expect.  The constant
 *  from the linear model is folded into the latency time, and in both
 *  cases is less than 1 msec.
 *
 *  Of course, the precision here is excessive, given the error in our knowledge
 *  of exactly when a command was issued.  This error can be kept small by running
 *  the Tilt_minder process at high (realtime?) priority.
 */

public:

  virtual duration est_transit_time(const state &s1, const state &s2, const trans_mode &xm);

  virtual duration est_total_transit_time(const state &s1, const state &s2, const trans_mode &xm);

  virtual state::delta get_max_transit_rate();

  virtual duration get_max_transit_time();

  virtual state get_calibration_state();

  virtual trans_mode get_calibration_trans_mode();

  virtual duration get_max_calibration_time();

  virtual state est_state_reached(const state &start, const state &dest, const duration &elapsed, const trans_mode &xm);

  virtual duration get_command_setup_time();

  virtual duration get_command_latency();

  /**
   * version 1.1 of the tilter firmware is not interruptible - a command must
   * complete before any subsequent command is accepted
   */
  virtual bool can_interrupt() {return false;};

  /**
   * is a given state physically valid?
   * This is where we set the "safe operating range", within which no limit
   * switch is tripped.  The setting here does not prevent going to 0 degrees
   * when calibrating.
   */
  virtual inline bool is_state_valid(const state &s) {return s.angle >= 1.0 && s.angle <= 89.0;};


  /**
   * is a given transition mode valid?
   *
   * Here, the transition mode is just the "speed" parameter, which although
   * unused, should be between 1 and 5.
   *
   */
  virtual inline bool is_trans_mode_valid(const trans_mode &xm) {return xm.speed >= 1 && xm.speed <= 5;};

  static const int CMD_LENGTH = 17;		/**< the length of a command, including the trailing end-of-line */

  /**
   * format the "goto angle" command
   *
   * @param s the target state
   * @param xm the transition mode
   *
   * @return the formatted command, ready for transmission
   */
  virtual std::string format_goto_state_command(const state &s, const trans_mode &xm);

  /**
   * format the "calibrate" command
   *
   */
  virtual std::string format_calibrate_command();

protected:
  /**
   * \brief stepping time for up and down directions
   *
   * The firmware operating the Pic Microchip 16f876a controller is in
   * interpreted bytecode, so hard to predict timing for.  From the
   * Fourier spectrum of audio recordings of the tilter moving up and
   * down by 90 degrees, the actual stepping frequencies are 2364 Hz
   * going up, and 2339 Hz going down.  Inverting these gives the
   * time taken per single motor step, in seconds. The downward
   * stepping rate was found to be the same for both calibration and
   * normal commands.  (That the desired frequencies were near 2300 Hz
   * came from dividing the known steps for a full range by the approximate
   * time taken to perform it.)
   */

  static const double time_per_step[2];

  /**
   * amount by which table step values are scaled to convert to
   * motor steps
   *
   */

  static const unsigned step_scale;

  /**
   * \brief steps per angle
   *
   * for each integer angle from 0 to 90, how many steps are required
   * to reach it, starting at 0 degrees?
   * This table copied from Dave Wilson's microcontroller program:
   *   "BIRD RADAR LINEARIZED STEP RX - 16_f876_a REV1.1.BAS"
   * Each step takes a fixed time.
   * NOTE: each value in this table must be multiplied by step_scale
   * to get the actual number of steps required.
   */

  static const unsigned angle_steps[91];

  /**
   * how many steps are required to move to an angle?
   *
   * @param ang angle above horizontal, in degrees
   *
   * @return number of steps required.
   */
  double angle_to_steps(double ang);

  /**
   * \brief: what angle is reached after a given number of steps?
   *
   * because the steps angle_steps mapping is not too far from linear, we
   * start at the linear guess, then search for the bracketing interval
   * and then use linear approximation.
  */

  double steps_to_angle(double step);

  static const double	TC_cal_AB;    /**< time to initiate a calibration command */
  static const double	TC_cal_BC;    /**< time per motor step when calibrating */
  static const double	TC_cal_DE;    /**< latency time for a calibration command */

  static const double	TC_up_AB;       /**< time to initiate a stepping up command */
  static const double	TC_up_DE;       /**< latency time for a stepping up command */

  static const double	TC_down_AB;       /**< time to initiate a stepping down command */
  static const double	TC_down_DE;       /**< latency time for a stepping down command */

  static const int      NUM_STEPPING_RATES = 8; /**< number of different stepping rates */
  static const int      STEPS_AT_RATE[NUM_STEPPING_RATES]; /**< number of steps at each rates for both starting and ending of stepping */
  static const double	TC_step_BC[NUM_STEPPING_RATES];    /**< time per motor step; table because of varying rates */  

  static const double	calibration_latency;	/**< how long a wait is forced after a calibration (seconds) */
  static const double	command_latency;		/**< how long a wait is forced after a command completes (seconds) */

  double steps_to_time(int n); /**< how much time does it take to go a total of n steps (as a single command) */
  double time_to_steps(double t, int n); /**< how many steps have we gone if we're t seconds into a move of n steps */
};

#else /* __cplusplus */
/**
 * \cond
 *
 */

typedef struct t11_minder t11_minder;

#define NA_ANGLE -360 // must match value used in state class
#define NOT_A_DATE_TIME ((double) -1) // must match value used in

/**
 * \endcond
 *
 */
#endif /* __cplusplus */

/***********************  C interface ***********************/

/*
 * These functions provide an interface to the library from C.
 */

#ifdef __cplusplus
extern "C" {
#endif

#include <math.h>

/**
 * Return the current time as a Time_stamp
 *
 * @return the current time in seconds since the epoch
 */
double t11_now();

/**
 * Create a minder for a tilter attached via a named serial port device
 *
 * @param devname filename of the serial port device to which the tilter is attached
 *
 * @param max_hist the maximum number of command history entries
 *
 * @param logname filename to which sent commands will be logged; can
 * be 0 for no logging; always appended to.
 *
 * Note: the speed and other settings (parity, stop bits, etc.) for the device must be
 * set elsewhere by the user.  The speed parameter passed here is used only in estimating the
 * time it takes to transmit a command to the tilter, and does not change the serial port settings.
 *
 * @return pointer to the t11_minder object, or NULL on error
 */
  t11_minder *t11_get_minder(const char *devname, unsigned max_hist, const char *logname);

/**
 * close a t11_minder device, and stop its controlling thread.
 *
 * @param tm pointer to the t11_minder
 */
void t11_close_minder (t11_minder *tm);

/**
 * set a t11_minder on a linear pattern, with a uniform range of angles each visited for a fixed time
 *
 * @param tm      pointer to a t11_minder
 * @param start  start angle of the pattern, in degrees
 * @param step   angle step size, in degrees
 * @param n      number of angles in pattern
 * @param dur    duration to spend at each angle, in seconds (not including transition time)
 * @param xm     the desired transition mode
 *
 * @return the total time taken for a single pass through the pattern, including transition times
 */
  double t11_set_linear_pattern (t11_minder *tm, double start, double step, unsigned n, double dur, int xm);

/**
 * clear the pattern for a t11_minder.  This will stop any current pattern.
 *
 * @param tm      pointer to a t11_minder
 */
void t11_clear_pattern (t11_minder *tm);

/**
 * add a pattern piece to the end of a pattern.  The piece is a
 * target angle and duration.
 *
 * @param tm    pointer to a t11_minder
 * @param ang  angle of pattern piece, in degrees
 * @param dur  duration to spend at this angle, in seconds
 * @param xm   the desired transition mode
 */
  void t11_add_to_pattern (t11_minder *tm, double ang, double dur, int xm);

/**
 * set the starting time for a pattern.  A time in the past, including
 * 0, means start immediately.
 *
 *
 * @param tm    pointer to a t11_minder
 * @param t    timestamp at which to begin, in seconds past the epoch
 */
void t11_set_start_time (t11_minder *tm, double t);

/**
 * set the ending time for a pattern.  A time of 0 means the pattern
 * is repeated forever.  If a pattern is running and the new end time
 * is in the past, then the pattern is stopped.
 *
 * @param tm    pointer to a t11_minder
 * @param t    timestamp at which to end the pattern, in seconds past the epoch
 */
void t11_set_stop_time (t11_minder *tm, double t);

/**
 * begin running a pattern by creating a thread to run the tilter.  The new thread will
 * sleep until the pattern's start time.  If the pattern was previously run and stopped,
 * this will resume the pattern where it left off; use t11_restart_pattern to re- run
 * the pattern from the start.
 *
 * @param tm     pointer to a t11_minder
 *
 * @return 0 if successful; otherwise, an error code
 */
int t11_run_pattern (t11_minder *tm);

/**
 * is a pattern running?  i.e. is there a (possibly sleeping) control thread?
 *
 * @param tm     pointer to a t11_minder
 *
 * @return   zero if a control thread has been started
 */
int t11_is_pattern_running (t11_minder *tm);

/**
 * stop the pattern's control thread, if any
 *
 * @param  tm     pointer to a t11_minder
 *
 * @return   0 on success; otherwise, an error code
 */
int t11_stop_pattern (t11_minder *tm);

/**
 * estimate the angle at several evenly spaced times in the recent past.
 *
 * @param[in] tm pointer to a t11_minder
 * @param[in] t time_stamp for which to get state
 * @param[in] d duration between time_stamps for which to get state
 * @param[in] n number of time_stamps for which to get state
 * @param[out] a pointer to an array for holding estimated angles
 *             If the angle at a particular time cannot be estimated,
 *             NA_ANGLE is put in the corresponding slot of a[].
 *             The angles are for times t, t+d, t+2d, ..., t+(n-1)d
 */
void t11_get_angles_at_times (t11_minder *tm, double t, double d, int n, double *a);

/**
 * immediately send the tilter to the 0 degree position for calibration.
 * Any running pattern is stopped.
 *
 * @param tm   pointer to a t11_minder
 *
 * @return    expected time until calibration completes, in seconds; -1 for failure
 */
double t11_imm_calibrate (t11_minder *tm);

/**
 * immediately send the tilter to the specified angle
 * Any running pattern is stopped.
 *
 * @param tm   pointer to a t11_minder
 * @param ang destination angle, in degrees
 *
 * @return    0 on success; otherwise, an error code
 */
int t11_imm_tilt_to_angle (t11_minder *tm, double ang);

/**
 * immediately send the tilter to the next piece of the current pattern.
 *
 * @param tm   pointer to a t11_minder
 *
 * @return    0 on success; otherwise, an error code
 *            1 means the previous command had not yet completed and can't be interrupted,
 *              or the device has not been calibrated.
 *            2 means the angle is out of bounds
 */
int t11_imm_step_pattern (t11_minder *tm);

/**
 * immediately restart the current tilter pattern.
 *
 * @param tm   pointer to a t11_minder
 *
 * @return    0 on success; otherwise, an error code
 */
int t11_imm_restart_pattern (t11_minder *tm);

/**
 * is the tilter in a known state (i.e. angle)
 *
 * @param tm pointer to a t11_minder
 *
 * @return 0 if the state is unknown; non-zero if it is known
 */
int t11_is_state_known (t11_minder *tm);

/**
 * let user tell us the tilter is in a known state now
 *
 * @param tm pointer to a t11_minder
 * @param ang angle tilter is at now
 */
void t11_imm_set_known_state (t11_minder *tm, double ang);

/**
 * set the "at end of pattern" behaviour
 *
 * @param tm pointer to a t11_minder
 * @param at_end one of the codes 0:3 from t11_minder::at_end_do
 */
void t11_set_at_end (t11_minder *tm, int at_end);

/**
 * set the current pattern traversal direction
 *
 * @param tm pointer to a t11_minder
 * @param dir directon: one of the codes 0:1 from t11_minder::step_dir
 */
void t11_set_dir (t11_minder *tm, int dir);

/**
 * print the saved command history
 *
 * @param tm pointer to a t11_minder
 */
void t11_print_history (t11_minder *tm);

/**
 * print a time in human readable form
 *
 * @param t time, in seconds past the epoch
 */
void t11_print_time (double t);

#ifdef __cplusplus
}
#endif  /* __cplusplus */

#endif /* TILTER_1_1_H */
