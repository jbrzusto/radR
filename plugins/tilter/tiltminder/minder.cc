//  svn: $Id: minder.cc 785 2011-05-05 17:26:03Z john $

//  Copyright John Brzustowski 2010.  Distributed under the Boost
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <cstdio>
#include <vector>
#include <string>
#include <stdexcept>
#include <utility>

#include <boost/circular_buffer.hpp>
#include <boost/thread/thread.hpp>
#include <boost/thread/condition_variable.hpp>
#include <boost/bind.hpp>

#include "mindertypedefs.hpp"
#include "minderutil.hpp"

/**
 * \file
 * operate a stateful device in a pattern, managing transitions and state history
 */

/**
 * \mainpage The Minder Class and Application to Controlling Dave Wilson's Tiltable Antenna

\section The Generic Model

(template classes in minder.cc; type definitions in mindertypedefs.hpp)

A minder operates a device in a predetermined pattern, keeping track of where it has been
recently.

This uses the following concepts:

  \b PHYSICAL \b DEVICE: can be in an unknown state, or one of a set of known
  physical states.  Moving between states takes time, and the movement
  can be modelled so that intermediate states can be estimated.  Transitions
  between states might have properties (e.g. speed).

  \b DEVICE \b CONTROLLER: is the part of the physical device to which we will
  issue commands for changing state.

  \b INITIAL \b STATE: is unknown, and no state transitions can be
  requested.

  \b CALIBRATE: the controller can be asked to put the device into a special
  \b KNOWN state, and can give an upper bound for how long it will take
  to get there.

  \b SPECIFY \b CURRENT \b STATE: the user can tell us that the device is
  currently in a specified state (e.g. to skip calibration when not
  needed)

  \b TRANSITIONS \b BETWEEN \b STATES: the minder initiates the
  state transition, can estimate how long it will take to
  complete, and how far it has got at any time along the way.  The
  device's must already be in a known state.

  \b HISTORY: the minder maintains a list of recently sent commands to allow it
  to estimate device state over some time window up to and including the present.

  \b LOGGING: each time a state transition command is issued, the
  minder logs the time, command, and estimated current state; this
  allows for monitoring device usage.

  \b PATTERN \b PIECE: consists of a device state, transition mode,
  and desired duration of time to be spent in that state, not including
  initial transit time.

  \b PATTERN: a sequence of pattern pieces for traversing device
  states.

  \b PATTERN \b CONTROL: how should the minder deal with reaching the end
  of the pattern sequence (stop?  wrap around? bounce back?).  Also, the
  direction in which the pattern is being traversed (forwards or
  backwards).

  TIMER: when should the minder begin the pattern, how long should
  it be run for?

\section A Specific Model for the Tilter

(concrete classes in tilter1.0.h / tilter1.0.cc)

  This code reflects the design of the Dave Wilson's antenna tilter
  hardware and firmware, version 1.0.  Device \b state means "angle of
  elevation above the horizontal", in degrees".  Timing constants,
  command latencies, and the non-interruptibility of commands are all
  modelled, with two main goals:

   - be sure that a command sent to the tilter is actually executed
   - be able to estimate the angles of radar pulse during transitions

(concrete classes in tilter1.1.h / tilter1.1.cc)

  This code reflects my version 1.1 of the tilter firmware, which
  has variable stepping speed (slower at the start and end of
  a stepping command, faster in the middle), and which treats limit
  switch hits as recalibration events that don't stop further operation.

\section Tilter Server: Operating the Tilter via a Socket

For safety, the tilter minder is run in its own process, at realtime
priority so that timing of commands is fairly precise.  Any other
process can communicate with the server by sending text commands to
the server's socket.  For example, in a windows Command Prompt window:

\code

   tiltserv1.0 com9 12345 log.txt
   telnet localhost 12345

\endcode
The first command starts the server.  The tilter device is attached via the
COM9 serial port.  The server listens on port 12345 and appends a record
of activity to the file log.txt  This assumes COM9 has been properly
configured.  The file \b tiltserv.bat takes care of this in Windows, and
\b tiltserv.sh does it for linux.

The second line connects to the server on port 12345.  The user can issue commands
by typing in the window.  The server only accepts one connection at a time.

 */

/**
 * \brief Exception: the piece index is out of range
 *
 */
class minder_x_piece_out_of_range : public std::runtime_error {
public:
  minder_x_piece_out_of_range() : std::runtime_error("invalid pattern piece index") {};
};

/**
 * \brief Exception: the log file cannot be opened
 *
 */
class minder_x_logfile_unopenable : public std::runtime_error {
public:
  minder_x_logfile_unopenable() : std::runtime_error("can't open log file for appending") {};
};

/**
 * \brief Exception: the controller device cannot be opened
 *
 */
class minder_x_control_device_unopenable : public std::runtime_error {
public:
  minder_x_control_device_unopenable() : std::runtime_error("can't open control device for writing") {};
};

/**
 * \brief Exception: the requested state is not valid
 *
 */
class minder_x_invalid_state : public std::runtime_error {
public:
  minder_x_invalid_state() : std::runtime_error("invalid state") {};
};

/**
 * \brief Exception: the device is not in a known state; calibration is required
 *
 */
class minder_x_not_in_known_state : public std::runtime_error {
public:
  minder_x_not_in_known_state() : std::runtime_error("not in known state; calibration required or not yet completed") {};
};

/**
 * \brief Exception: the requested transition mode is not valid
 *
 */
class minder_x_invalid_transition_mode : public std::runtime_error {
public:
  minder_x_invalid_transition_mode() : std::runtime_error("invalid transition mode") {};
};

/**
 * \brief Exception: the current command is still executing and cannot be interrupted
 *
 */
class minder_x_not_ready_for_cmd : public std::runtime_error {
public:
  minder_x_not_ready_for_cmd() : std::runtime_error("can't accept a command until current one is finished running") {};
};

/**
 * \brief Exception: the requested linear pattern is not feasible; one or more transitions can't be completed in the allotted time
 *
 */
class minder_x_infeasible_pattern : public std::runtime_error {
public:
  minder_x_infeasible_pattern() : std::runtime_error("the requested linear pattern is not feasible; one or more transitions can't be completed in the alloted time") {};
};

/**
 * \brief Exception: d2 has zeroes in components where d1 does not
 *
 */
class minder_x_delta_division_by_zero : public std::runtime_error {
public:
  minder_x_delta_division_by_zero() : std::runtime_error("attempted to divide deltas where d2 is zero in component(s) where d1 is not") {};
};

/**
 * a piece of a pattern: what state, how do we get there, how long to spend
 *
 */

template < typename STATE, typename TRANSMODE >
struct minder_pattern_piece
{
/**
 * \tparam STATE -- see description in \link minder::minder \endlink
 *
 *
 * \tparam TRANSMODE -- see description in \link minder::minder \endlink
 */
public:
  minder_pattern_piece(STATE s, duration dur, TRANSMODE xm) {
    this->s = s;
    this->dur = dur;
    this->xm = xm;
  };
  STATE		s;		/**< target state for this piece */
  duration	dur;		/**< how long is to be spent in target state */
  TRANSMODE	xm;		/**< mode in which to transition to target state */
};

/**
 * \brief defines a pattern for operating a device
 *
 */

template < typename STATE, typename TRANSMODE >
class minder_pattern : boost::noncopyable
{
/**
 * \tparam STATE -- see description in \link minder::minder \endlink
 *
 *
 * \tparam TRANSMODE -- see description in \link minder::minder \endlink
 */
public:
  /**
   * delete the pattern entries
   *
   */
  virtual inline void clear() {pieces.clear();};

  /**
   * add a piece to the pattern
   *
   * @param s target state
   * @param dur how long to spend in target state
   * @param xm mode in which to transition to target state
   */
  virtual inline void add(const STATE &s, const duration &dur, const TRANSMODE &xm)
  {
    pieces.push_back(minder_pattern_piece <STATE, TRANSMODE> (s, dur, xm));
  };


  /**
   * get a piece from the pattern
   *
   * @param i index of the piece 0 <= i <= size()
   *
   * @return the pattern piece
   */
  virtual const minder_pattern_piece<STATE, TRANSMODE> &get_piece(unsigned i)
  {
    if (i < pieces.size())
      return pieces[i];
    throw minder_x_piece_out_of_range();
  };

  /**
   * get the size of the pattern
   *
   */
  virtual inline unsigned size() {return pieces.size();};

  /**
   * \brief reserve space for a pattern
   *
   * Calling this method is not required, but can improve space/time efficiency.
   *
   * @param n number of pattern pieces
   */
  virtual inline void reserve (unsigned n) {pieces.reserve(n);};


protected:
  std::vector<minder_pattern_piece <STATE, TRANSMODE> > pieces;

};

/**
 * \brief control the use of a pattern.
 *
 * keeps track of where we are in the pattern, what direction we are
 * moving in, when we start and stop, and what to do when the end of the
 * pattern is reached
 */

template < typename STATE, typename TRANSMODE >
struct minder_pat_ctrl
{
/**
 * \tparam STATE -- see description in \link minder::minder \endlink
 *
 *
 * \tparam TRANSMODE -- see description in \link minder::minder \endlink
 */
public:
  enum step_dir {FORWARD = 1, BACKWARD = 2};

  /**
   * what to do when we reach either end of the pattern
   */
  enum at_end_do {
    /**
     * stop the pattern
     */
    STOP = 1,

    /**
     * go to the piece at the opposite end of the pattern
     */
    WRAP = 2,

    /**
     * reverse directions, going back to the previous piece
     */
    BOUNCE = 3,

    /**
     * repeat the end piece, then reverse direction; this gives equal coverage to
     * end pieces which otherwise are only used once per cycle.
     */
    SLOW_BOUNCE = 4
  };

  time_stamp	start_time;	/**< when does/did the pattern start */
  time_stamp	stop_time;	/**< when does/did the pattern stop */
  int		i_piece;	/**< index of the most recently begun piece of the pattern; negative for none */
  step_dir	dir;		/**< direction in which pattern is to be traversed */
  at_end_do	at_end;		/**< what to do when the end of pattern is reached */

};

/**
 * keeps track of an issued command
 *
 * This is much like minder_pattern_piece, except that
 * we also record the estimated state when the command was issued.
 * If the physical device commands are non-interruptible, then
 * the init entry of a record will equal the dest entry of the
 * previous history record.
 * Bit if the physical device allows commands to be interrupted by
 * subsequent commands, then recording this information will simplify reconstruction
 * of state history.
 */
template < typename STATE, typename TRANSMODE >
struct minder_hist_entry
{
/**
 * \tparam STATE -- see description in \link minder::minder \endlink
 *
 *
 * \tparam TRANSMODE -- see description in \link minder::minder \endlink
 */
public:
  minder_hist_entry(const time_stamp &ts, const STATE &init, const STATE &dest, const TRANSMODE &xm) {
    this->ts = ts;
    this->init = init;
    this->dest = dest;
    this->xm = xm;
  };

  time_stamp	ts;		/**< time at which command was received by physical device */
  STATE		init;		/**< estimated state of physical device at time command was received
				 * this value is STATE::NA if and only if this command was a calibration
				 * or a forced setting to a known state
				 */
  STATE		dest;		/**< state to which device was commanded to transition */
  TRANSMODE	xm;		/**< mode in which transition occurred */

  virtual std::string to_string(bool numeric_date = true, bool text_date = false) const
  {
    std::ostringstream s;

    if (numeric_date)
      s << std::fixed << std::setprecision (3) << time_stamp_to_double(ts) << ",";
    if (text_date)
      s << boost::posix_time::to_simple_string(ts) << ",";
    s << init.to_string() << ","
      << dest.to_string() << ","
      << xm.to_string();
    return std::string(s.str());
  };
};

/**
 * keeps track of recently issued commands
 *
 */

template < typename STATE, typename TRANSMODE >
class minder_history : boost::noncopyable
{
/**
 * \tparam STATE -- see description in \link minder::minder \endlink
 *
 *
 * \tparam TRANSMODE -- see description in \link minder::minder \endlink
 */
  typedef boost::circular_buffer< minder_hist_entry <STATE, TRANSMODE> > Hist_buf;

protected:
  std::ofstream log_stream;	/**< the logging stream */
  Hist_buf hb;			/**< the circular buffer of recent commands */

public:
  minder_history(unsigned max_hist, std::string log_file) : hb(max_hist)
  {
    if (log_file.length() > 0) {
      log_stream.open(log_file.c_str(), std::ios_base::app | std::ios_base::out);
      if (! log_stream)
	throw minder_x_logfile_unopenable();
      log_stream << "Start " << boost::posix_time::to_simple_string(boost::get_system_time()) << '\n';
      log_stream.flush();
    }
  };

  ~minder_history()
  {
    if (log_stream) {
      log_stream << "End " << boost::posix_time::to_simple_string(boost::get_system_time()) << '\n';
      log_stream.close();
    }
  };

  /**
   * return the size of the history buffer
   *
   */
  inline unsigned size() {return hb.size();};

  /**
   * return the most recent entry in the history buffer
   *
   */
  inline const minder_hist_entry<STATE, TRANSMODE> &back() const {return hb.back();};

  /**
   * return an entry in the history buffer
   *
   */
  inline const minder_hist_entry<STATE, TRANSMODE> & operator[] (unsigned i) const {return hb[i];};

  /**
   * record a command to the history
   *
   * @param h a history entry
   */
  void note (const minder_hist_entry<STATE, TRANSMODE> &h) {
    hb.push_back(h);

    if (log_stream)
      log_stream << h.to_string() << '\n';
  };

  /**
   * print the history to a stream
   *
   * @param out the output stream
   *
   * We use both a numeric and a text representation of the date.
   */
  virtual void print(std::ostream &out) {
    for (unsigned i = 0; i < hb.size(); ++i)
      out << hb[i].to_string(true, true) << '\n';
  }
};

/**
 * represents the computer device for sending controls to the
 * physical device.
 */

template < typename STATE, typename TRANSMODE >
class minder_device : boost::noncopyable
{
/**
 * \tparam STATE -- see description in \link minder::minder \endlink
 *
 *
 * \tparam TRANSMODE -- see description in \link minder::minder \endlink
 */
public:
  /**
   * constructor
   *
   * @param dev_name filename of device for communicating with physical device
   *
   */
  minder_device(const std::string &dev_name)
  {
    out.open(dev_name.c_str());
    if (! out)
      throw minder_x_control_device_unopenable();
  };

  virtual ~minder_device()
  {
    out.close();
  };

  /**
   * send a command to the physical device
   *
   * @param cmd: the command string
   *
   * @returns the time at which the physical device will have received the command
   *
   */
  virtual time_stamp send_command(const std::string &cmd) {
    out << cmd;
    out.flush();
    return (time_stamp) boost::get_system_time();
  };

protected:
  std::ofstream out;		/**< the stream to which commands are written */
};

/**
 * represents a model of the physical device being controlled,
 * including the controller's command syntax.
 */

template < typename STATE, typename TRANSMODE >
class minder_model
{
/**
 * \tparam STATE -- see description in \link minder::minder \endlink
 *
 *
 * \tparam TRANSMODE -- see description in \link minder::minder \endlink
 */
public:
  minder_model(){};

  virtual ~minder_model() {};

  /**
   * how long it should take to transition from one state to another
   * in a given mode?  This time is measured from the instant the
   * device actually begins transitioning, to the instant at which it
   * stops, and does not include set-up time or post-command latency.
   *
   * @param s1 initial state
   * @param s2 target state
   * @param xm transition mode
   *
   * @return transit time
   */
  virtual duration est_transit_time(const STATE &s1, const STATE &s2, const TRANSMODE &xm) = 0;


  /**
   * how long it should take to transition from one state to another in a given mode,
   * including set-up and post-command latency time?  i.e. the total time taken between
   * receiving a command, finishing the command, and being ready to receive the next one.
   *
   * @param s1 initial state
   * @param s2 target state
   * @param xm transition mode
   *
   * @return total transit time
   */
  virtual duration est_total_transit_time(const STATE &s1, const STATE &s2, const TRANSMODE &xm) = 0;

  /**
   * to what state has the device transitioned a given amount of time after the command was issued?
   * This takes into account command set-up time.
   *
   * @param start the initial state
   * @param dest  the target state
   * @param elapsed how much time has elapsed since the command was issued
   * @param xm the transition mode for the command
   *
   * @return estimated state reached
   */
  virtual STATE est_state_reached(const STATE &start, const STATE &dest, const duration &elapsed, const TRANSMODE &xm) = 0;

  /**
   * the maximum rate of state change per second, for each state component
   *
   */
  virtual typename STATE::delta get_max_transit_rate() = 0;

  /**
   * the maximum time required for a state-to-state transition
   *
   */
  virtual duration get_max_transit_time() = 0;

  /**
   * get the state in which calibration leaves the device
   *
   */
  virtual STATE get_calibration_state() = 0;

  /**
   * get the transition mode used in calibration
   *
   */
  virtual TRANSMODE get_calibration_trans_mode() = 0;

 /**
   * the maximum time required for a calibration
   *
   */
  virtual duration get_max_calibration_time() = 0;

  /**
   * the typical time between issuing of a command and start of transition
   *
   */
  virtual duration get_command_setup_time() = 0;

  /**
   * the maximum time between completion of one (non-calibration) command and readiness to accept the next command
   *
   */
  virtual duration get_command_latency() = 0;

  /**
   * can a command be interrupted before it has reached its target state?
   */
  virtual bool can_interrupt() = 0;

  /**
   * is a given state physically valid?
   */
  virtual bool is_state_valid(const STATE &s) = 0;


  /**
   * is a given transition mode valid?
   *
   */
  virtual bool is_trans_mode_valid(const TRANSMODE &s) = 0;

  /**
   * format the "goto a state" command
   *
   * @param s the target state
   * @param xm the transition mode
   *
   * @return the formatted command, ready for transmission
   */
  virtual std::string format_goto_state_command(const STATE &s, const TRANSMODE &xm) = 0;

  /**
   * format the "calibrate" command
   *
   */
  virtual std::string format_calibrate_command() = 0;

};


/** 
 * \brief shrink a linear approximation to a state versus time function, with bounded error
 *
 * the algorithm is described in shrink_approx.R 
 * 
 * @param t  array of times (as doubles) of size n
 * @param s  array of STATEs of size n
 * @param n  number of slots in the original linear approximation
 * @param e  maximum pointwise error allowed in the shrunken approximation
 * @param M  maximum rate of change of STATE vector per second of time
 * 
 * @return  a vector of indexes into the original approximation corresponding to those elements
 *          retained in the shrunken approximation.  If the resulting shrunken approximation is
 *          constant, only a single index is returned.
 *
 * Note: the true map, f, of time to state is assumed to satisfy the following conditions:
 *       - f(t[i]) = s[i]; i.e. the initial approximation is exact at the specified points
 *       - |f(t1) - f(t2)| <= M * |t2 - t1|; i.e. the rate of change of the true state is bounded by M
 *       where '<=' applies to all components simultaenously.
 * emax and M are both of type STATE::delta, which means the maximum error and transition rates are
 * specified independently for each component of STATE.
 *
 */

template < typename STATE >
std::vector <unsigned> shrink_approx (double *t, STATE *s, unsigned n, typename STATE::delta e, typename STATE::delta M)
{
  std::vector <unsigned> new_i;

  if (n == 0)
    return new_i;

  new_i.push_back(0);		// add the first old approximation point to the new set

  if (n == 1)			// special case: old approximation has only 1 point
    return new_i;

  if (n == 2) {			// special case: old approximation has only 2 points
    new_i.push_back(1);
    return new_i;
  }

  unsigned i = 0;		// index into old approximation points
  unsigned j = 0;		// index into new approximation points

  typename STATE::delta m_min(-HUGE_VAL); // for constraining slopes
  typename STATE::delta m_max( HUGE_VAL);

  while (i < n - 1) {
    typename STATE::delta m1;
    typename STATE::delta m2;

    if (i > new_i[j]) {
      m1 = (s[i] - e - s[new_i[j]]) / (t[i] - t[new_i[j]]); 
      m2 = (s[i] + e - s[new_i[j]]) / (t[i] - t[new_i[j]]); 
      m_min = STATE::delta::max(m_min, m1);
      m_max = STATE::delta::min(m_max, m2);
    }

    m1 = (s[i+1] - e - s[new_i[j]]) / (t[i+1] - t[new_i[j]]);
    m2 = (s[i+1] + e - s[new_i[j]]) / (t[i+1] - t[new_i[j]]);
    m_min = STATE::delta::max(m_min, m1);
    m_max = STATE::delta::min(m_max, m2);
    
    double tt = t[i] + abs(s[i+1] - s[i]) / M;
    m1 = (s[i+1] - e - s[new_i[j]]) / (tt - t[new_i[j]]);
    m2 = (s[i+1] + e - s[new_i[j]]) / (tt - t[new_i[j]]);
    m_min = STATE::delta::max(m_min, m1);
    m_max = STATE::delta::min(m_max, m2);

    tt = t[i+1] - abs(s[i+1] - s[i]) / M;
    m1 = (s[i] - e - s[new_i[j]]) / (tt - t[new_i[j]]);
    m2 = (s[i] + e - s[new_i[j]]) / (tt - t[new_i[j]]);
    m_min = STATE::delta::max(m_min, m1);
    m_max = STATE::delta::min(m_max, m2);

    if (i > new_i[j]) {
      typename STATE::delta mm = (s[i+1] - s[new_i[j]]) / (t[i+1] - t[new_i[j]]);
      if (! (m_min <= mm) || ! (mm <= m_max)) {
	new_i.push_back(i);
	++j;
	m_min = typename STATE::delta(-HUGE_VAL);
	m_max = typename STATE::delta( HUGE_VAL);
      } else {
	++i;
      }
    } else {
      ++i;
    }
  }
  if ((new_i.size() != 1) || ! (s[new_i[0]] == s[n-1]))
    new_i.push_back(n - 1);
  return new_i;
};

/**
 * \brief operate a physical device in a pattern, using a separate thread
 *
 * The model for the physical device allows estimation of state at
 * recent times in the past.  Commands are sent to the device over the
 * control device, and are logged to a stream.
 *
 */

template < typename STATE, typename TRANSMODE >
class minder : boost::noncopyable
{
/**
 * \tparam STATE the class representing the physical state of the device
 *
 * this class must include the following public members:
 *
 * \li delta: a class representing the difference between two states
 * \li static delta delta_max(): the maximum possible delta value
 * \li static delta delta_min(): the minimum possible delta value (i.e. most negative)
 * \li static delta max(delta d1, delta d2): the component-wise maximum of two deltas
 * \li static delta min(delta d1, delta d2): the component-wise minimum of two deltas
 * \li static delta abs(delta d): the component-wise magnitude of a delta
 * \li static bool operator<=(delta d1, delta d2): are all components of d1 less than or equal to
 *     corresponding components of d2?
 * \li static double max_ratio(delta d1, delta d2): the smallest non-negative M such that
 *     d1 <= d2 * M is true; basically, the largest of the ratios d1 / d2, except that
 *     0 / 0 is treated as 0.  Should throw a minder_x_delta_division_by_zero if d2 is 0 in a component
 *     where d1 is non-zero.
 * \li operator*: (for delta) overloaded as STATE::delta operator*(double factor)
 *     multiply each component of delta by the given factor
 * \li operator/: (for delta) overloaded as STATE::delta operator/(double factor)
 *     divide each component of delta by the given factor
 * \li NA(): a static function returning a STATE treated as unknown
 * \li is_na(): a boolean function that says whether or not a state is unknown
 * \li operator+: overloaded as STATE operator+(STATE::delta d); add a delta to a state to get another state
 * \li operator-: overloaded as STATE::delta operator-(STATE s); get the difference between two states
 * \li operator==: overloaded as bool operator==(STATE s); are two states equal?
 * \li to_string(): return a representation of state as a std::string
 * 
 * \tparam TRANSMODE the class representing a mode for transitioning between states (e.g. a speed)
 * must have the following methods:
 *
 * \li to_string(): return a representation of transition mode as a std::string
 */

public:
  minder(minder_device<STATE, TRANSMODE> *dev, minder_model<STATE, TRANSMODE> *model, minder_history<STATE, TRANSMODE> *hist)
    : state_known(false),
      running(false)
  {
    this->dev = dev;
    this->model = model;
    this->hist = hist;
  };

  virtual ~minder() { delete hist; delete model; delete dev;};


  /**
   * issue a command to the physical device to cause it to transition
   * to a particular known state.  This will usually be the first
   * method called once the minder has been created.
   *
   * \returns the amount of time before calibration will be completed.
   */

  virtual duration calibrate() {
    boost::unique_lock<boost::mutex> dev_lock(dev_mutex);
    dev->send_command(model->format_calibrate_command());

    boost::unique_lock<boost::mutex> lock_hist(hist_mutex);

    minder_hist_entry<STATE, TRANSMODE> new_he((time_stamp) boost::get_system_time(), STATE::NA(), model->get_calibration_state(), model->get_calibration_trans_mode());

    hist->note(new_he);

    return model->get_max_calibration_time();
    // don't mark state_known = true: this will happen after the appropriate delay
  };

  virtual bool is_state_valid(const STATE &s) { return model->is_state_valid(s);};

  virtual bool is_trans_mode_valid(const TRANSMODE &xm) { return model->is_trans_mode_valid(xm);};

  /**
   * \brief tell the minder that the physical device is already in a specific known state.
   *
   * Warning: if the physical device is not in the specified known state, then
   * subsequent commands might not work properly, and estimates of the device state will
   * generally be wrong.  USE WITH CAUTION.
   */

  virtual void set_known_state(const STATE &s) {
    boost::unique_lock<boost::mutex> lock_hist(hist_mutex);

    // use s as both the initial and target state, so that history estimation works
    minder_hist_entry<STATE, TRANSMODE> new_he((time_stamp) boost::get_system_time(), s, s, model->get_calibration_trans_mode());

    hist->note(new_he);

    // mark physical device state as known
    state_known = true;
  };


  /**
   * \brief is the physical device in a known state?
   *
   * If not already in a known state, check whether a possible
   * calibration command has completed, thus leaving the device in a
   * known state.
   *
   * @return true if the device is in a known state
   */
  virtual bool is_state_known()
  {
    if (state_known)
      return true;

    boost::unique_lock<boost::mutex> lock(hist_mutex);

    if (hist->size() == 0)
      return false;

    minder_hist_entry< STATE, TRANSMODE > he = hist->back();

    duration needed = model->get_max_calibration_time();

    if ((time_stamp) boost::get_system_time() - he.ts < needed)
      return false;

    state_known = true;
    return true;
  };

  /**
   * ask for the device to transition to a new state using a given mode
   *
   * @param s the new state desired
   * @param xm the transition mode to use
   *
   * Notes:
   * - we don't allow non-calibration commands to be issued if the
   * physical device is not in a known physical state.
   *
   * - the physical device can be put in a known state in either of two ways:
   *    - a call to calibrate(); this sends a calibration command to the device
   *    - a call to set_known_state(); this tells us the device is already in a particular
   *       known state, and sends no command.  USE WITH CAUTION.
   *
   * - we don't allow interruption of a calibration command, even
   * if the physical device has an interruptible controller.
   *
   */
  virtual void goto_state(const STATE &s, const TRANSMODE &xm)
  {
    if (! is_state_known())
      throw minder_x_not_in_known_state();

    if (! is_state_valid(s))
      throw minder_x_invalid_state();

    if (! is_trans_mode_valid(xm))
      throw minder_x_invalid_transition_mode();

    // lock the history (we need to lookup whether the previous command completed, and
    // what its target state was
    boost::unique_lock<boost::mutex> lock(hist_mutex);

    // what was the previous command issued?

    minder_hist_entry< STATE, TRANSMODE > he = hist->back();

    // what time is it now

    time_stamp now = (time_stamp) boost::get_system_time();

    // if the device is not interruptible, and the previous
    // command has not completed, then bail out

    if (! model->can_interrupt() &&
	now - he.ts < model->est_total_transit_time(he.init, he.dest, he.xm))
      {
	throw minder_x_not_ready_for_cmd();
      }

    STATE where_now = model->est_state_reached(he.init, he.dest, now - he.ts, he.xm);

    // lock the device
    boost::unique_lock<boost::mutex> dev_lock(dev_mutex);

    time_stamp when_sent = dev->send_command(model->format_goto_state_command(s, xm));

    // create the history entry for this command (we do this in a separate
    // statement to avoid messing up emacs' c++-mode indenting)

    minder_hist_entry<STATE, TRANSMODE> new_he(when_sent, where_now, s, xm);

    hist->note(new_he);
  };


  /**
   * what is the estimated current state of the device?
   */
  virtual const STATE get_state_now()
  {
    if (! is_state_known())
      return STATE::NA();

    time_stamp ts = (time_stamp) boost::get_system_time();
    STATE s;

    get_state_at_times(ts, duration(boost::date_time::not_a_date_time), 1, &s);
    return s;
  };

  /**
   * determine the state of the device at a set of evenly spaced recent times
   * (without locking the history, on the assumption the caller has)
   *
   * @param t first time for which to get state; should be in the past
   * @param d amount by times are spaced (should be positive)
   * @param n number of times for which go get state
   * @param s [out] array of estimated states at times t, t+d, t+2d, ..., t + (n-1)d
   */
  virtual void get_state_at_times_no_lock(time_stamp t, duration d, unsigned n, STATE *s)
  {
    unsigned size = hist->size();

    for (unsigned ih = 0; n > 0; --n, t += d) {
      // find the latest history entry at or before t

      while (ih < size && (*hist)[ih].ts <= t)
	++ih;

      if (ih == 0) {
	*s++ = STATE::NA(); // pre-historic time or no history: no estimate possible
      } else {
	--ih;	            // previous index value was latest at or before t
	*s++ = model->est_state_reached((*hist)[ih].init, (*hist)[ih].dest, t - (*hist)[ih].ts, (*hist)[ih].xm);
      }
    }
  };

  /**
   * determine the state of the device at a set of evenly spaced recent times
   *
   * @param t first time for which to get state; should be in the past
   * @param d amount by times are spaced (should be positive)
   * @param n number of times for which go get state
   * @param s [out] array of estimated states at times t, t+d, t+2d, ..., t + (n-1)d
   */
  virtual void get_state_at_times(time_stamp t, duration d, unsigned n, STATE *s)
  {
    boost::unique_lock<boost::mutex> lock(hist_mutex);
    get_state_at_times_no_lock(t, d, n, s);
  };

  /**
   * return an entry in the history buffer
   *
   */
  inline const minder_hist_entry<STATE, TRANSMODE> & get_hist_entry (unsigned i) const {return (*hist)[i];};


  /**
   * get a piecewise-linear approximation for the state of the device
   * during a time window, subject to a maximum pointwise error, assuming the
   * transitions between states happens monotonically in each component.
   * The algorithm is not guaranteed to find the minimum (in number of points)
   * such approximation, but it should be reasonably short, and the error bound
   * *is* guaranteed.
   *
   * @param t start of time window
   * @param d duration of time window (must be non-negative)
   * @param emax maxmimum amount by which the piecewise linear approximation is
   * allowed to differ from the true state at any time in the given window; this
   * has a component for each component of state.
   *
   * @returns a pair consisting of a time_stamp vector and a STATE vector, which are
   * the known points from which the piecewise-linear approximation is obtained by
   * interpolation.
   *
   * Special cases:
   * - if the history is empty or the entire time window is
   *   prehistoric, the lengths of the returned vectors is zero.
   * - if the time window begins in prehistory, but ends within
   *   recorded history, the first pair of elements will be
   *   the state and time of the earliest recorded history item.  The user must 
   *   take this possibility into account when using the approximator.
   * - if the state of the device was constant (to within emax) for the entire time
   *   window, the lengths of the returned vectors is one.  This is the "typical" case.
   * - if necessary, history is extrapolated into the future by
   *   assuming no further state changes are to be made after the last
   *   command completes.
   */
  virtual std::pair < std::vector <time_stamp> *, std::vector <STATE> * > get_approx(time_stamp t, duration d, typename STATE::delta emax)
  {
    std::vector <time_stamp> *times = new std::vector < time_stamp >();
    std::vector <STATE>     *states = new std::vector < STATE >     ();

    time_stamp t_end = t + d;

    boost::unique_lock<boost::mutex> lock(hist_mutex);

    unsigned nh = hist->size();

    if (nh == 0 || t_end <= get_hist_entry(0).ts)
      // no known state in history or entire time window is prehistoric  so return empty vectors
      return std::pair < std::vector <time_stamp> *, std::vector <STATE> * > (times, states);
    
    // find the relevant range of entries in the history
    unsigned i, hlo, hhi;
    
    for (i = 0; i < nh && t >= get_hist_entry(i).ts; ++i) /**/;

    hlo = (i > 0) ? i-1 : 0;  // the earliest relevant history entry

    for (/**/; i < nh && t_end > get_hist_entry(i).ts; ++i) /**/ ;

    hhi = i - 1;              // the latest relevant history entry
   
    // Estimate the number of points required to make the initial linear approximation.
    // We have two requirements for this initial approximation:
    //  (1) the state changes monotonically between points
    //  (2) the maximum point-wise error is at most emax
    // Condition (1) means intervals in the initial approximation must
    // nest inside the intervals between history entries (otherwise, we might
    // catch both the end of an "up" transition and the beginning of a "down" transition
    // within the same window, thereby violating monotonicity).

    // the maximum rate at which the device can transition away from a state

    typename STATE::delta trate = model->get_max_transit_rate();

    // the maximum time step we can go without exceeding the allowed error
    // in any component of the state

    double time_step = STATE::max_ratio(emax, trate);
    duration time_step_duration = double_to_duration(time_step);

    // estimate of the number of steps needed to span the window
    // we include 2 extra steps for each history entry because of windowing effects
    unsigned steps_needed = 1 + (unsigned) ceil(duration_to_double(d) / time_step) + 2 * (hhi - hlo + 1);

    // get temporary storage for holding times and states estimated at fixed time steps
    STATE  est_states[steps_needed];
    double est_times[steps_needed];

    unsigned num_states = 0;

    // the earliest time to use for a history period
    time_stamp t_lo = (t >= get_hist_entry(hlo).ts) ? t : get_hist_entry(hlo).ts; 

    // for each relevant history entry, get the times and states for its region of overlap
    // with the target time window

    for (i = hlo; i <= hhi; ++i) {
      if (i > hlo) 
	t_lo = get_hist_entry(i).ts;
      time_stamp t_hi = (i < hhi) ? get_hist_entry(i+1).ts : t_end;
      // invariant:   max(t, get_hist_entry(i).ts) <= t_lo <= t_hi <= min(t_end, get_hist_entry(i+1).ts)
      unsigned n = ceil(duration_to_double(t_hi - t_lo) / time_step);
      // get the states at the stepped times
      get_state_at_times_no_lock (t_lo, time_step_duration, n, est_states + num_states);
      double hist_start = time_stamp_to_double(t_lo);
      assert (num_states + n <= steps_needed);
      for (unsigned j = 0; j < n; ++j)
	est_times[num_states + j] = hist_start + j * time_step;
      num_states += n;
    }

    // get the state and time for the last point
    get_state_at_times_no_lock (t_end, time_step_duration /* don't care */, 1, est_states + num_states);
    est_times[num_states++] = time_stamp_to_double(t_end);

    // unlock the history
    lock.unlock();

    // now shrink the approximation to a "reasonable" number of points, preserving
    // the error bound.

    std::vector < unsigned > keep = shrink_approx (est_times, est_states, num_states, emax, trate);

    for (unsigned j = 0, n = keep.size(); j < n; ++j) {
      times->push_back(double_to_time_stamp(est_times[keep[j]]));
      states->push_back(est_states[keep[j]]);
    }

    return std::pair < std::vector <time_stamp> *, std::vector <STATE> *> (times, states);
  };


  /**
   * clear the pattern; stops the control thread it it is running
   *
   */
  virtual void clear_pattern() {
    stop();
    boost::unique_lock<boost::mutex> lock(pat_mutex);
    pat.clear();
  };

  /**
   * add a piece to a pattern
   *
   * @param s the state in which to spend time
   * @param dur the length of time to spend (does not include transit time)
   * @param xm the transition mode to use to get to the state
   */
  virtual void add_pattern_piece(const STATE &s, const duration &dur, const TRANSMODE &xm)
  {
    boost::unique_lock<boost::mutex> lock(pat_mutex);
    pat.add(s, dur, xm);
  };

  /**
   * set a linear pattern
   *
   * @param begin the starting state
   * @param step the delta between states
   * @param n the number of states in the pattern; these will be begin, begin+step, begin+step+step, ...
   * @param dur the (constant) length of time to spend in each state; this doesn't count transit time
   * @param xm the (constant) transition mode between states
   *
   * @return the time taken for one pass through the pattern
   */
  virtual duration set_linear_pattern(const STATE &begin, const typename STATE::delta &step, unsigned n, const duration &dur, const TRANSMODE &xm)
  {
    duration t = (duration) boost::posix_time::seconds(0);
    {
      boost::unique_lock<boost::mutex> lock(pat_mutex);

      pat.clear();
      pat.reserve(n);

      STATE s(begin);
      STATE s_prev = model->get_calibration_state();

      for (unsigned i = 0; i < n; ++i, s += step) {
	pat.add(s, dur, xm);
	t += model->est_total_transit_time(s_prev, s, xm) + dur;
	s_prev = s;
      }
    }

    {
      boost::unique_lock<boost::mutex> lock(pat_ctrl_mutex);

      pat_ctrl.dir = minder_pat_ctrl < STATE, TRANSMODE >::FORWARD;
      pat_ctrl.at_end = minder_pat_ctrl < STATE, TRANSMODE >::SLOW_BOUNCE;
      pat_ctrl.i_piece = -1;
    }
    return t;
  };

  virtual inline const minder_pattern_piece < STATE, TRANSMODE > &get_current_pattern_piece()
  {
    return pat.get_piece(pat_ctrl.i_piece);
  };

  virtual const time_stamp &get_start_time()
  {
    boost::unique_lock<boost::mutex> lock(pat_ctrl_mutex);
    return pat_ctrl.start_time;
  }

  virtual void set_start_time(const time_stamp &t)
  {
    boost::unique_lock<boost::mutex> lock(pat_ctrl_mutex);
    pat_ctrl.start_time = t;
  }

  virtual const time_stamp &get_stop_time()
  {
    boost::unique_lock<boost::mutex> lock(pat_ctrl_mutex);
    return pat_ctrl.stop_time;
  }

  virtual void set_stop_time(const time_stamp &t)
  {
    boost::unique_lock<boost::mutex> lock(pat_ctrl_mutex);
    pat_ctrl.stop_time = t;
  }

  virtual void run() {
    if (! is_state_known())
      throw minder_x_not_in_known_state();

    if (running || pat.size() == 0)  // not serious enough to merit exceptions?
      return;

    running = true;
    die_now = false;
    step_now = false;
    ctrl_thread = new boost::thread(boost::bind(ctrl_thread_fun, this));
  };

  virtual bool is_pattern_running() {return running;};

  /**
   * immediately cause the pattern to step to its next piece
   *
   */
  virtual void step()
  {
    boost::unique_lock<boost::mutex> lock(wake_mutex);
    if (! running)
      return;

    step_now = true;		// tell control thread to step now
    wake_c_v.notify_one();	// wake up the control thread
  };

  virtual void stop()
  {
    { // local block for locking
      boost::unique_lock<boost::mutex> lock(wake_mutex);
      if (!running)
	return;

      die_now = true;		// tell control thread to stop
      wake_c_v.notify_one();	// wake up control thread
    }
    // join the thread so we block until it is finished
    // Note that we only join after releasing the lock on wake_mutex
    ctrl_thread->join();
  };

  virtual void restart() {
    stop();

    pat_ctrl.i_piece = -1;
    pat_ctrl.dir = minder_pat_ctrl <STATE, TRANSMODE >::FORWARD;

    // if the previous run was for a fixed length of time,
    // reinitialize the end time so that the pattern runs
    // for the same length of time.

    time_stamp start = pat_ctrl.start_time;
    time_stamp stop = pat_ctrl.stop_time;

    if (! start.is_not_a_date_time() && ! stop.is_not_a_date_time())
      pat_ctrl.stop_time = (time_stamp) boost::get_system_time() + (stop - start);

    // mark an immediate start
    pat_ctrl.start_time = (time_stamp) boost::date_time::not_a_date_time;

    run();
  };

  virtual void print_history(std::ostream &out) {
    boost::unique_lock<boost::mutex> lock(hist_mutex);
    hist->print(out);
  };

  virtual inline void set_step_dir(typename minder_pat_ctrl<STATE, TRANSMODE>::step_dir dir)
  {
    boost::unique_lock<boost::mutex> lock(pat_ctrl_mutex);
    pat_ctrl.dir = dir;
  };

  virtual inline void set_at_end_do(typename minder_pat_ctrl<STATE, TRANSMODE>::at_end_do at_end)
  {
    boost::unique_lock<boost::mutex> lock(pat_ctrl_mutex);
    pat_ctrl.at_end = at_end;
  };

protected:
  minder_device   < STATE, TRANSMODE > *dev;	/**< the computer device that talks to the physical device  */
  minder_model    < STATE, TRANSMODE > *model;	/**< a model of the physical device  */
  minder_pattern  < STATE, TRANSMODE > pat;	/**< the pattern for operation of the physical device */
  minder_pat_ctrl < STATE, TRANSMODE > pat_ctrl;/**< where we are in the pattern and when it starts/stops */
  minder_history  < STATE, TRANSMODE > *hist;	/**< recent states of the physical device */

  bool state_known;		/**< false until we know the state of the physical device */

  /**
   * thread control
   *
   */

  bool				 running;	/**< is there a control thread? */

 /**  Note: the following are protected by wake_mutex */
  bool				 die_now;	/**< is it time for the control thread to die? checked upon waking */
  bool                           step_now;	/**< should the control thread immediately step the pattern? */

  boost::thread			*ctrl_thread;	/**< the control thread */
  boost::mutex			 wake_mutex;	/**< mutex for accessing condition variable for thread wakeup */
  boost::condition_variable      wake_c_v;	/**< condition variable indicating time for thread to awaken */

  /**
   * locking
   *
   */

  boost::mutex                   dev_mutex;	/**< mutex for accessing physical device */
  boost::mutex			 pat_mutex;	/**< mutex for accessing pattern data */
  boost::mutex			 pat_ctrl_mutex;/**< mutex for accessing pattern control */
  boost::mutex                   hist_mutex;	/**< mutex for accessing state history */


  static void ctrl_thread_fun(minder < STATE, TRANSMODE > *m) {

    duration wait_time;
    time_stamp start_time = m->get_start_time();

    if (! start_time.is_not_a_date_time()) {
      wait_time = start_time - (time_stamp) boost::get_system_time();
    } else {
      wait_time = (duration) boost::posix_time::seconds(0);
      // record the actual start time
      m->set_start_time((time_stamp) boost::get_system_time());
    }

    for (;;) {
      bool die;

      // wait the required amount of time, either before starting the pattern
      // or after completing a piece of it

      { // local block for locking wake mutex

	boost::unique_lock<boost::mutex> lock(m->wake_mutex);
	// sleep for the required time

	bool time_up = false;

	boost::system_time target_time = boost::get_system_time() + (boost::posix_time::time_duration) wait_time;
	while (! time_up && ! m->die_now && ! m->step_now) {
	  time_up = ! m->wake_c_v.timed_wait(lock, target_time);
	}
	die = m->die_now;
	// these are protected by wake_mutex:
	m->die_now = false;
	m->step_now = false;
      }

      if (die)
	break;

      // issue command to move to next pattern piece
      // we might reach the end of the pattern here

      if (! m->step_pattern(wait_time))
	break;

    } // continue pattern

    m->set_stop_time((time_stamp) boost::get_system_time());

    // this must be the last step; we don't want is_pattern_running to return false until
    // the thread is truly finished
    m->running = false;
  };

  /**
   * \brief step to the next piece of the pattern
   *
   * This is only called by the control thread function.
   *
   */
  virtual bool step_pattern(duration &wait_time) {
    // lock the pattern and pattern control
    boost::unique_lock<boost::mutex> lock(pat_mutex);
    boost::unique_lock<boost::mutex> lock_ctrl(pat_ctrl_mutex);

    int n = (int) pat.size();

    if (n == 0)
      return false;

    // check whether it's time to finish
    if (! pat_ctrl.stop_time.is_not_a_date_time() && pat_ctrl.stop_time <= (time_stamp) boost::get_system_time())
      return false;

    unsigned i_piece_saved = pat_ctrl.i_piece;
    typename minder_pat_ctrl < STATE, TRANSMODE >::step_dir dir_saved = pat_ctrl.dir;

    // return value
    bool keep_going = true;

    if (pat_ctrl.dir == minder_pat_ctrl < STATE, TRANSMODE >::FORWARD) {
      ++pat_ctrl.i_piece;
      if (pat_ctrl.i_piece >= n) {
	switch(pat_ctrl.at_end) {
	case minder_pat_ctrl < STATE, TRANSMODE >::STOP:
	  pat_ctrl.i_piece = 0;  // so that next call to run() will start afresh
	  keep_going = false;
	  break;

	case minder_pat_ctrl < STATE, TRANSMODE >::WRAP:
	  pat_ctrl.i_piece = 0;
	  break;

	case minder_pat_ctrl < STATE, TRANSMODE >::BOUNCE:
	  --pat_ctrl.i_piece;
	  /* fall through */
	case minder_pat_ctrl < STATE, TRANSMODE >::SLOW_BOUNCE:
	  --pat_ctrl.i_piece;
	  // don't do SLOW_BOUNCE in a 2 piece pattern
	  if (n == 2)
	    --pat_ctrl.i_piece;
	  pat_ctrl.dir = minder_pat_ctrl < STATE, TRANSMODE >::BACKWARD;
	  break;

	}
      }
    } else {
      --pat_ctrl.i_piece;
      if (pat_ctrl.i_piece < 0) {
	switch(pat_ctrl.at_end) {
	case minder_pat_ctrl < STATE, TRANSMODE >::STOP:
	  keep_going = false;
	  break;

	case minder_pat_ctrl < STATE, TRANSMODE >::WRAP:
	  pat_ctrl.i_piece = n - 1;
	  break;

	case minder_pat_ctrl < STATE, TRANSMODE >::BOUNCE:
	  ++pat_ctrl.i_piece;
	  /* fall through */
	case minder_pat_ctrl < STATE, TRANSMODE >::SLOW_BOUNCE:
	  ++pat_ctrl.i_piece;
	  // don't do SLOW_BOUNCE in a 2 piece pattern
	  if (n == 2)
	    ++pat_ctrl.i_piece;
	  pat_ctrl.dir = minder_pat_ctrl < STATE, TRANSMODE >::FORWARD;
	  break;

	}
      }
    }
    if (keep_going) {
      // issue the command
      minder_pattern_piece < STATE, TRANSMODE > p = get_current_pattern_piece();
      try {
	// how long will it take to make the transition (not counting set-up time)
	duration transit_time = model->est_total_transit_time(get_state_now(), p.s, p.xm) - model->get_command_setup_time();
	goto_state(p.s, p.xm);
	// the command was sent, so tell the caller what the wait time should be until the next
	// command is to be issued
	wait_time = p.dur + transit_time;
      } catch (std::runtime_error e) {
	// if the command failed to issue (e.g. the device was not in
	// a known state), restore the original pattern position to
	// cause a retry (the pattern's stop_time might still prevent
	// the controller thread from retrying)
	// we don't modify wait_time, since there's no obvious value to use

	pat_ctrl.i_piece = i_piece_saved;
	pat_ctrl.dir = dir_saved;
      }
    }
    return keep_going;
  };
};
