###########################################################################
###                                                                     ###
### generic.plugin.radR                                                 ###
###                                                                     ###
### This is the generic port-providing plugin, which defines generic    ###
### methods for ports.  A port is an object provided by a plugin,       ###
### and serves as a data source or sink for radR.                       ###
### A plugin providing ports should provide a get.ports() function.     ###
###                                                                     ###
###########################################################################


about = function() {
  return("This plugin defines generic methods for interfaces.\nIt must be loaded to allow the seascan, seascanarch, blipmoviearch, ... plugins to function")
}

## the generic methods are installed from the "globals" item:

globals = list (
  config = function(port, ...) {
    ## send or get configuration options from a port
    ## for example, this is how to set the filename for a port:
    ## config(port, filename="whatever.dat")
    ## if opts == NULL, the current configuration is returned.
    ## if opts != NULL, the configuration is set
    ## and TRUE is returned on success, FALSE on error

    UseMethod("config")
  },

  get.contents = function(port, ...) {
    ## gets the contents of the current (filetype)
    ## source, namely
    ## a list with three elements
    ## num.images: number of images in each run
    ## start.time:  starting timestamp of each run
    ## end.time:    ending timestamp of each run

    UseMethod("get.contents")
  },

  end.of.data = function(port, ...) {
    ## return TRUE if there is no data left to be read
    ## on this port (e.g. if the end of a tape run has been hit)

    UseMethod("end.of.data")
  },

  get.scan.info = function(port, ...) {
    ## gets the header information for the next scan
    ## along with probably caching this information internally,
    ## this returns a list with these elements:
    ##
    ## pulses: number of pulses in this scan
    ##
    ## samples.per.pulse: number of samples per pulse
    ##
    ## bits.per.sample: number of bits per sample
    ##
    ## timestamp: POSIXct-style timestamp of start of scan, including fractional seconds
    ##
    ## duration: length of this scan (milliseconds) (i.e. rotation time
    ## for radar)
    ##
    ## sample.dist: what distance increment each sample represents (metres)
    ##
    ## first.sample.dist: distance at which first sample begins (metres)
    ##
    ## bearing:  how many degrees clockwise from north is the first
    ##                 pulse?
    ##
    ## rotation.direction: +1 = clockwise, -1 = counterclockwise

    UseMethod("get.scan.info")
  },

  get.scan.data = function(port, extmat, exit.code, exit.code.index, ...) {
    ## reads the scan data from the radar module into storage given by extmat
    ## returns extmat (with data filled) on success
    ## returns NULL on error
    ## returns NA if a thread has been created to obtain the data,
    ## in which case, when the thread completes, it will set the first
    ## element of the integer vector bound to exit.code to its (non-NA)
    ## exit code.

    UseMethod("get.scan.data")
  },

  put.scan = function(port, header, extmat, ...) {
    ## puts most recently read scan (including header information) to the current sink
    ## returns NULL on error, TRUE otherwise
    ## header will be what was returned by get.scan.info() for the scan whose
    ## data are in extmat
    UseMethod("put.scan")
  },

  seek.scan = function(port, run, scan, ...) {
    ## seek to a particular run and scan on the current source
    ## scan = integer NAN requests a seek past the last scan in
    ##        the current run (which must be the last run
    ##        for most modules)
    ## run = 0 represents the current run

    UseMethod("seek.scan")
  },

  seek.time = function(port, time, ...) {
    ## seek to the first scan at or after time in the current source
    ## scan = +Inf requests a seek past the last scan in
    ##        the current run (which must be the last run
    ##        for most modules)
    ## run = NULL represents the current run
    
    UseMethod("seek.time")
  },

  start.up = function(port, ...) {
    ## do whatever is required to initialize
    ## a given object.  Return TRUE on sucess,
    ## NULL on error
    UseMethod("start.up")
  },

  shut.down = function(port, ...) {
    ## do whatever is required to minimize
    ## resource consumption by this port
    ## eg. stopping digitization and playback,
    ## closing files, etc.

    UseMethod("shut.down")
  },

  start.run = function(port, start.time, ...) {
    ## add a new run to the current sink
    ## and use it for subsequent put.scan calls

    UseMethod("start.run")
  },

  end.run = function(port, end.time, ...) {
    ## end the current sink run and make any
    ## required changes to its directory

    UseMethod("end.run")
  },

  new.play.state = function(port, new.state, old.state, ...) {
    ## indicate to this port that radR is
    ## changing play state.

    UseMethod("new.play.state")
  }
  )  ## end of globals

