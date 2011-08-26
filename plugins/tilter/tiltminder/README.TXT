svn: $Id$

A simple driver library for Dave Wilson's tilter antenna controller, but adaptable
to other devices

generic model: (template classes in Minder.cc)

  DEVICE: can be in an unknown state, or one of a set of known physical
  states.  Moving between states takes time, and the movement can be
  modelled; intermediate points are modelled.  Transitions between states
  may themselves be parameterized (e.g. by speed)

  INITIAL STATE: is unknown, and no state transitions can be requested.

  CALIBRATE: controller can be asked to put device into a special
  KNOWN state, and can give an upper bound for how long it will take
  to get there.

  SPECIFY CURRENT STATE: user can tell us that device is
  currently in a specified state. (e.g. to skip calibration when not
  needed)
  
  TRANSITION FROM KNOWN STATE TO OTHER KNOWN STATE: controller initiates the
  state transition, and can estimate how long it will take to complete,
  and how far it has gotten at any time along the way.

  HISTORY: controller maintains enough information to report estimated state
  over some time window; the history maintains up to a maximum number of command entries.

  LOGGING: each time a state transition command is issued, log the time, command,
  and estimated current state; allows for monitoring device usage.

  PATTERN PIECE: [destination state; transition speed; time to spend in state, including initial transit time]

  PATTERN: a sequence of pattern pieces for traversing device states.  Also, how to deal
  with reaching the end of the sequence (stop?  wrap around? bounce back?)

  TIMER: when should the controller begin the pattern, how long should it be run for?  

The overall idea is to define a trajectory through state space defined by a finite set of points
along it, and be able to estimate the trajectory in between those points (e.g. estimate the
state at arbitrary times, and estimate the time taken for arbitrary transitions).

specific model:  derived classes in Tilter1.0.h / Tilter1.0.c

  This code reflects the design of the tilter hardware and firmware, version 1.0
