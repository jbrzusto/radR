##  svn $Id: radRmain.R 665 2010-10-08 17:53:49Z john $
##
##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006, 2007, 2008 John Brzustowski
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.
##

##    High-level scan processing and radR initialization

## Note: this file contains only function definitions.

########################################################################################
###
### rss.process.scan
###
### The workhorse.  Process one scan of radar data, using the specified steps.
### Return NULL on failure, TRUE on success
### This function can be called either from the main event loop (see rss.event.loop)
### or by the GUI.  The latter case occurs if the user has paused the event loop
### and makes changes to parameter values using the GUI.
###
########################################################################################

rss.process.scan <- function(filter.noise     = FALSE,
                             calculate.scores = TRUE,
                             classify.samples = TRUE,
                             find.patches     = TRUE,
                             process.patches  = TRUE,
                             update.plot      = TRUE,
                             convert.scan     = FALSE,
                             put.scan         = TRUE,
                             update.stats     = FALSE,
                             is.preview       = FALSE
                             )
{
  on.exit({
    ## mark scan as previewed and/or processed, even if an error
    ## aborts processing
    RSS$have.previewed.scan <- RSS$have.previewed.scan ||   is.preview
    RSS$have.processed.scan <- RSS$have.processed.scan || ! is.preview
  })

  RSS$previewing <- is.preview && ! RSS$have.processed.scan

  ## have the GUI indicate we are busy
  rss.gui(SHOW_BUSY, TRUE)

  ## call the "top of scan" hooks

  rss.call.hooks(RSS$PRE_PROCESS_HOOK)

  ## if we are playing, mark this as the start of the processing
  if (RSS$play.state >= RSS$PS$PLAYING && is.null(RSS$processing.time.start)) {
###.if $DEBUG
    print("Setting RSS$processing.time.start to " %:% RSS$scan.info$timestamp)
    print("   and is.preview=" %:% is.preview )
###.endif
    RSS$processing.time.start <- RSS$scan.info$timestamp
  }

  .Call("radR_filter_noise", RSS$scan.mat, RSS$noise.cutoff)
  
  if (RSS$blip.finding) {
    ## try to find blips in this scan

    if (RSS$have.valid$stats) {
      ## if we are finished the learning phase
      ## and have valid statistics for classifying
      ## the current scan then:

      if (calculate.scores && !RSS$skip$calculate.scores) {
        ## calculate the z-scores of all samples
        rss.calculate.scores()
      }

      if (classify.samples && !RSS$skip$classify.samples) {
        ## classify the samples cold or hot, and get
        ## the number of hot samples
        RSS$num.hot.samples <- sum(rss.classify.samples()[2:3])
      }
    }

    if (RSS$have.valid$classification) {
      if (find.patches && !RSS$skip$find.patches)
        ## look for patches of hot samples

        RSS$num.patches <- rss.find.patches()

      if (process.patches && !RSS$skip$process.patches)
        ## process the patches, including filtering them to get blips
        ## any calling per-blip hooks

        RSS$num.blips <- rss.process.patches()
    }
  }

  ## a hook that is called after each scan is processed (whether in playing state or not)

  rss.call.hooks(RSS$DONE_SCAN_HOOK)

  if (RSS$blip.finding && !is.preview) {
    ## In blipping mode,
    ## we update stats if any of these conditions holds:
    ## - we are explicitly asked to by the caller setting update.stats=TRUE
    ## - we are "playing" (live or archive) and one of these is true:
    ##   - we are in the learning phase (there are scans to learn)
    ##   - we are not in the learning phase, but stats are being updated while blipping

    rss.call.hooks(RSS$PRE_STATS_HOOK)
    if ((update.stats || (RSS$play.state >= RSS$PS$PLAYING && (RSS$num.scans.learned < RSS$scans.to.learn || RSS$update.stats.while.blipping)))
        && !RSS$skip$update.stats){
      ## update the mean and standard deviations for each
      ## cell using the samples in that cell
      rss.update.stats()
    }
    rss.call.hooks(RSS$POST_STATS_HOOK)
  }

  if (put.scan && !RSS$skip$put.scan && !is.preview) {
    ## if this is a new scan, record it if we are in record mode
    ## and have a valid sink (i.e. recording destination)

    if (RSS$play.state >= RSS$PS$PLAYING && RSS$recording && !is.null(RSS$sink))
      put.scan(RSS$sink, RSS$scan.info, RSS$scan.mat)
  }

  if (update.plot && ! RSS$skip$update.plot)
    ## if the plot window is to be updated
    ## tell the gui to do this.
    ## The GUI takes care of calling the
    ## scan conversion routine, if convert.scan is TRUE
    ## We let the GUI do this because we don't know (or care about)
    ## size of the display window; all data processing is
    ## done on raw data, but scan conversion only happens
    ## on the appropriate sub window.

    rss.gui(UPDATE_PLOT_WINDOW, convert.scan)

  ## have the GUI indicate we are no longer busy
  rss.gui(SHOW_BUSY, FALSE)

  ## update the total number of blips
  if (!is.preview)
    RSS$total.num.blips <- RSS$total.num.blips + RSS$num.blips

  ## report success
  return(TRUE)
}

################################################################################
###
###   The event loop, whose job is to process scans
###   and (implicitly) allow the tcltk GUI to operate
###   by callbacks.  run.once is TRUE on unix, where this
###   function is called by a timeout on reading console input.
###   run.once is FALSE on windows, where the loop in this function
###   runs forever.
###
################################################################################

rss.event.loop <- function(run.once = TRUE)
{
  scan.index <- 0
  while(TRUE) {  ## loop forever, unless run.once is TRUE
    
    ## if "playing", try to read a scan; this may spawn a getter
    ## thread which allows UI events to be processed in the main
    ## thread, but the getter thread will terminate before
    ## rss.get.scan returns.

    ##      DETECT SEEK TO A SCAN

    if (!is.null(RSS$source))
      scan.index <- rss.get.and.set("new.scan.index", 0, RSS)

    if (scan.index != 0 && isTRUE(RSS$source$is.seekable)) {
###.if $DEBUG
      print("Event loop got new scan index " %:% scan.index)
###.endif

      seek.scan(RSS$source, RSS$current.run, scan.index)
      ## update the current scan index, subtracting one because rss.get.scan increments it
      RSS$current.scan <- scan.index - 1
    }

    if (RSS$play.state >= RSS$PS$PLAYING || scan.index != 0) {
      rss.call.hooks(RSS$PRE_GET_SCAN_HOOK)
    
      if (isTRUE(end.of.data(RSS$source)) || ! isTRUE(rss.get.scan())) {
        rss.gui(PAUSE_PLAY, user.generated=FALSE)
        rss.do.pause()
        RSS$play.state <- RSS$PS$PAUSED
        RSS$new.play.state <- 0
        if (run.once)
          return()
        else
          next
      }
    }

    ## if play state is PLAY.ONE.SCAN, set pause mode; this will cause
    ## the scan to be processed in preview mode
    if (RSS$play.state == RSS$PS$PLAY.ONE.SCAN) {
      rss.do.pause()
      RSS$play.state <- RSS$PS$PAUSED
    }

    ## Give the GUI a chance to do something; this gives the user
    ## more fine-grained control over pausing, since while playing
    ## it may take some time before a GUI event is noticed.

    rss.process.UI.events()

    ## Perform any deferred assignments, calls, and evals, such as
    ## gui-driven updates to the blipping parameters, and changes
    ## to the gui state that should only occur on scan boundaries.

    ## We do this here because such deferrals will often have triggered
    ## by GUI events during a threaded rss.get.scan()

    rss.do.defers()

    ## Check for event-loop-related events using atomic rss.get.and.set
    ## These would usually have been generated by the GUI, but a plugin
    ## could also be the source.

    ##      DETECT QUIT

    if (rss.get.and.set("time.to.go", FALSE, RSS)) {
      ## confirm and then quit; if successful, doesn't return
      q()
    }

    ##      DETECT LEARNING RESTART

    if (rss.get.and.set("restart.learning", FALSE, RSS))
      rss.restart.learning()

    ##      DETECT NEW PLAY STATE

    ps <- rss.get.and.set("new.play.state", 0, RSS)


    if (ps != 0) {
###.if $DEBUG
      print("Event loop got new play state " %:% ps)
###.endif

      ## notify the ports of an impending change of play state
      if (!is.null(RSS$source))
        new.play.state(RSS$source, ps, RSS$play.state)
      if (!is.null(RSS$sink))
        new.play.state(RSS$sink, ps, RSS$play.state)

      if (ps == RSS$PS$STOPPED)
        rss.do.stop()
      else if(ps == RSS$PS$PAUSED)
        rss.do.pause()
      else if (ps == RSS$PS$PLAYING)
        rss.do.play()
      else if (ps == RSS$PS$PLAY.ONE.SCAN)
        rss.do.play.one.scan()

      RSS$play.state <- ps
    }


    ## if no scan data were obtained, or the scan has already been previewed,
    ## re-run the loop; or bail out

    if (!RSS$have.valid$scan.data || (RSS$play.state < RSS$PS$PLAYING && (RSS$have.previewed.scan || RSS$have.processed.scan))) {
      if (run.once) {
        break
      } else {
        rss.sleep(RSS$event.loop.sleeptime)
        next
      }
    }

    ## we have valid scan data and have not already previewed this scan,
    ## so process it.  This processing is a preview if and only if we
    ## are in play mode.

    previewing <- RSS$play.state < RSS$PS$PLAYING
###.if $DEBUG
    print("Processing scan with is.preview = " %:% previewing)
###.endif

    ## if we are to skip this scan, do so
    
    if (RSS$skip$all.processing) {
      if (! previewing)
        RSS$skip$all.processing <- FALSE
###.if $DEBUG
      print(sprintf("Skipping scan at %s", format(structure(RSS$scan.info$timestamp, class="POSIXct"))))
###.endif
    } else {
      rss.process.scan(filter.noise = TRUE, convert.scan = TRUE, is.preview = previewing, update.plot = RSS$play.state != RSS$PS$PLAY.ONE.SCAN )
    }
    ## tell the gui to advance the scan indicator if we're not previewing

    if (!previewing) {
###.if $DEBUG
      print("Advancing scan")
###.endif
      rss.gui(SCAN_ADVANCE)
    }

    ## if the not playing, give away some processor cycles

    if (RSS$play.state < RSS$PS$PLAYING)
      rss.sleep(RSS$event.loop.sleeptime)

    if(run.once)
      break
  }
}

## a short name for the user to restart the loop

go <- function() rss.event.loop(FALSE)

rss.init.vars <- function()
{
  ## The global level strictenv RSS holds parameters needed during runtime.

  RSS <<- strictenv(

                    ## the port from which radar data are read
                    source = NULL,

                    ## the port to which radar data will be recorded
                    sink = NULL,

                    ## flags reflecting the state of the blipping algorithm

                    scans.to.learn    = 0,   ## the number of scans required to be learned for this session; zero means no learning required
                    num.scans.learned = 0,   ## the number of scans learned
                    end.timestamp     = 0,

                    ## flags reflecting the playback / record state

                    ## states of play
                    ## WARNING: the states representing "playing" in
                    ## some form must follow those representing not playing.
                    ## The canonical test for whether radR is "playing" is
                    ## RSS$play.state >= RSS$PS$PLAYING
                    PS = strictenv(
                      STOPPED       = 1,
                      PAUSED        = 2,
                      PLAYING       = 3,
                      PLAY.ONE.SCAN = 4
                      ),

                    ## initial state
                    play.state = 1,

                    #############################################################################
                    ##
                    ## GUI locks: set by GUI, read via the "locking" function rss.get.and.set()
                    ##
                    ##

                    ## should the learning phase be restarted?

                    restart.learning = FALSE,

                    ## has a player button selected a new play state? non-zero indicates the new state

                    new.play.state   = 0,

                    ## has the slider selected a new scan index? non-zero indicates the new index

                    new.scan.index   = 0,

                    ## has the user asked to quit radR?

                    time.to.go       = FALSE,

                    ##
                    ##
                    ## end of GUI locks
                    ##
                    #############################################################################

                    ## the current source run (a run is a contiguous sequence of scans)
                    current.run = 0,

                    ## the current source scan
                    current.scan = 0,

                    ## flag: recording is on (so playing a scan should output to sink)
                    recording = FALSE,

                    ## flag: a sink run has been started (so stopping should end the run, whether
                    ## or not the user has since turned off the recording button)
                    sink.run.started = FALSE,

                    ## flags for skipping stages in the single-scan processing stack
                    ## these can be set by plugins at any time,
                    ## but are reset at the end of each call to rss.process.scan

                    skip = strictenv(
                      process.noise    = FALSE,
                      calculate.scores = FALSE,
                      classify.samples = FALSE,
                      find.patches     = FALSE,
                      process.patches  = FALSE,
                      update.plot      = FALSE,
                      convert.scan     = FALSE,
                      put.scan         = FALSE,
                      reset.progress   = FALSE,
                      update.stats     = FALSE,
                      all.processing   = FALSE
                      ),

                    ## variables describing the latest scan

                    have.valid = strictenv(
                      bitmap         = FALSE,
                      scan.data      = FALSE,
                      stats          = FALSE,
                      scores         = FALSE,
                      classification = FALSE,
                      patches        = FALSE
                      ),

                    ## variables describing the incoming scan
                    ## (so far, only a subset of have.valid entries are used)

                    new.have.valid = strictenv(
                      scan.data      = FALSE,
                      stats          = FALSE,
                      scores         = FALSE,
                      classification = FALSE,
                      patches        = FALSE
                      ),

                    ## current sample classification

                    timestamp            = 0,
                    num.hot.samples      = 0,
                    num.blips            = 0,
                    total.num.blips      = 0,

                    ## this is the flag that a GUI function can use to
                    ## force this loop to terminate

                    have.gui                = FALSE,

                    ## names we need for binding later

                    all.plugin.names        = c(),    ## names of all plugins, available and loaded
                    available.plugins       = list(), ## descriptors of non-loaded plugins by name
                    blip.k                  = NULL,
                    blip.thresh             = NULL,
                    debug.funs              = c(),    ## list of functions being debugged
                    error.messages          = NULL,
                    errors                  = NULL,
                    error.log.connection    = NULL,   ## the output connection for logging errors
                    full.lib.path           = list(), ## list of full paths to dynamically loaded libraries
                    have.previewed.scan     = FALSE,  ## TRUE if we have previewed the current scan
                    have.processed.scan     = FALSE,  ## TRUE if we have processed the current scan in non-preview mode
                    hooks                   = NULL,
                    last.error              = NULL,
                    lock                    = NULL,   ## list of locks to prevent re-entrancy in multithreaded contexts
                    num.patches             = 0,
                    palette.files           = list(), ## names of files from which palettes were read
                    palettes                = list(), ## a list of palettes for scan conversion
                    play.mode               = NULL,
                    plot.is.tk.image        = FALSE,  ## mirrors flag of same name in GUI; set by GUI
                    plugins                 = list(), ## loaded plugins by name
                    previewing              = FALSE,  ## is current scan being previewed?
                    processing.time.start   = NULL,   ## time at which we most recently started playing current source
                    processing.time.length  = NULL,   ## number of seconds to process current source (in source time)
                    pulses                  = NULL,   ## dataframe of pulse metadata
                    defers                  = list(), ## assignments, calls, and evals' deferred until the start of the next scan
                    save.config             = FALSE,
                    scan.info               = NULL,   ## scan info actually used in processing scan
                    source.scan.info        = NULL,   ## scan info from source, before any overrides from plugins
                    user.comment            = NULL,

                    PARENT = .GlobalEnv
                    )

  ## Create methods for NULL ports, to deal gracefully from race
  ## conditions in the GUI on slow platforms

  as.character.NULL   <<-
  print.NULL          <<-
  config.NULL         <<-
  get.contents.NULL   <<-
  get.scan.info.NULL  <<-
  get.scan.data.NULL  <<-
  put.scan.NULL       <<-
  seek.scan.NULL      <<-
  seek.time.NULL      <<-
  start.up.NULL       <<-
  shut.down.NULL      <<-
  start.run.NULL      <<-
  end.run.NULL        <<-
  new.play.state.NULL <<- function(...) return(NULL)
  end.of.data.NULL    <<- function(...) return(TRUE)

}

rss.antenna.config.changed <- function() {
  ## If we're paused (and presumably previewing) while the antenna
  ## config changes, we recalculate patch parameters and possibly
  ## update the screen.  We don't do this while playing, because the
  ## antenna config changes will simply affect the next scan
  ## processed.

  ## Although we are not re-getting the data, the physical "location"
  ## corresponding to samples can be changed (e.g. by changing
  ## antenna.angle or heading.offset), which may put them in different
  ## zones, so we reclassify samples and refind patches; and for the
  ## future when some antenna parameter might affect score
  ## calculation, we recalculate those too.

  if (RSS$play.state < RSS$PS$PLAYING) {
    rss.build.pulse.table()
    rss.process.scan( put.scan         = FALSE,
                     calculate.scores = TRUE,
                     classify.samples = TRUE,
                     find.patches     = TRUE,
                     process.patches  = TRUE,
                     convert.scan     = TRUE,
                     is.preview       = TRUE)
  }
}

rss.init.extmats <- function()  {
    ## variables representing the large external matrices
    ## and possibly other data structures (e.g. the patch buffer)
    ## the types here must match those given in radR.h

    RSS %$% scan.mat       <- extmat("raw radar data"                , type=RSS$types$sample)
    RSS %$% new.scan.mat   <- extmat("incoming raw radar data"       , type=RSS$types$sample)
    RSS %$% score.mat      <- extmat("sample z-scores"               , type=RSS$types$score)
    RSS %$% palette.mat    <- extmat("colour palettes"               , type=RSS$types$palette, dim=c(2^(RSS$pixel.data.bits), RSS$num.sample.classes))
    RSS %$% class.mat      <- extmat("sample classification"         , type=RSS$types$class)
    RSS %$% pix.mat        <- NULL  ## the GUI will provide one, or we will allocate it later
    RSS %$% prev.class.mat <- extmat("sample classification"         , type=RSS$types$class)
    RSS %$% mean.mat       <- extmat("means of radar data cells"     , type=RSS$types$frame_cell)
    RSS %$% dev.mat        <- extmat("deviances of radar data cells" , type=RSS$types$frame_cell)

    RSS %$% patch.x        <- extmat("patches: x (m east)"            , type="double")
    RSS %$% patch.y        <- extmat("patches: y (m north)"           , type="double")
    RSS %$% patch.z        <- extmat("patches: z (m elev)"            , type="double")
    RSS %$% patch.t        <- extmat("patches: t (timestamp)"         , type="double")
    RSS %$% patch.ns       <- extmat("patches: number of samples"     , type="int")
    RSS %$% patch.area     <- extmat("patches: area (m^2)"            , type="double")
    RSS %$% patch.int      <- extmat("patches: intensity (0 to 1)"    , type="double")
    RSS %$% patch.max      <- extmat("patches: max sample (0 to 1)"   , type="double")
    RSS %$% patch.aspan    <- extmat("patches: angular span (pulses)" , type="short")
    RSS %$% patch.rspan    <- extmat("patches: radial span (samples)" , type="short")
    RSS %$% patch.perim    <- extmat("patches: perimeter (m)"         , type="double")
    RSS %$% patch.range    <- extmat("patches: true range (m)"        , type="double")

    RSS %$% blips          <- NULL ## will hold the indexes among patches of those which are blips

    class(RSS$patch.t)     <- c("extmat", "POSIXct")
    patches                  <- RSS[c("patch.x", "patch.y", "patch.z", "patch.t", "patch.ns", "patch.area", "patch.int", "patch.max", "patch.aspan", "patch.rspan", "patch.perim", "patch.range")]
    class(patches)           <- "data.frame"
    attr(patches, "row.names") <- 0:0
    names(patches)           <- c("x", "y", "z", "t", "ns", "area", "int", "max", "aspan", "rspan", "perim", "range")
    RSS %$% patches         <- patches

    RSS %$% patch.buffer   <- rss.alloc.patch.image()

#    RSS %$% par.names <- c("blip.finding", RSS$blip.finding.parms, "blip.filtering", RSS$blip.filtering.parms)
#    RSS %$% par <- function(...) rss.par(RSS, ...)
  }

rss.get.scan <- function()
{

  ## grab a scan of data and return TRUE on success, or FALSE on EOF

  ## side effects:
  ##   - scan metadata go into RSS$scan.info (after the scan data
  ##     has been read)
  ##   - scan data go into RSS$scan.mat (but are initially read into
  ##     RSS$new.scan.mat, so that user interaction during a threaded
  ##     get.data() works with the previous full scan of data

  ## We use a lock to prevent recursion here, which could happen if a
  ## source's get_scan_data function spawns a getter thread, and sits
  ## in a gui-update loop while waiting for it to complete.  The user
  ## could then trigger another rss.get.scan, which we don't want to
  ## allow, because we only have one extra buffer for scan data.

  if (isTRUE(RSS$lock$rss.get.scan)) {
    rv <- NULL
  } else {
    RSS$lock$rss.get.scan <- TRUE

    if (is.null(RSS$source)) {
      rv <- FALSE
    } else {

      ## Get metadata from possibly multiple sources. We allow for
      ## the methods to spawn threads, because some sources do not get
      ## full metadata until they have acquired a full scan of data.
      ## So each get.scan.info does one of the following:
      
      ##  - sets trv[trv.index] to NA, indicating a threaded read, and
      ##    returns NA. In this case,we wait until trv[trv.index] is
      ##    no longer NA, at which point it is NULL for failure and
      ##    anything else for success
      
      ##  - does not set trv[trv.index] to NA_INTEGER, in which
      ##    case, the return value itself indicates failure (with
      ##    NULL) or success (anything else).  It is unsafe to return
      ##    NA in this situation.
      
      ## Note that this violates R's call-by-value semantics, as the
      ## thread will modify a variable (not a binding!) in the
      ## caller's environment.

      ## For now, we are reading only from a single source

      num.sources <- 1

      ## Create a list with a NULL entry for each source
      rv <- trv <<- vector("list", num.sources)

      ## we use '[1] <- list(' in the following because get.scan.info
      ## might return NULL, and we want to put a NULL in the appropriate slot of rv,
      ## rather than remove that slot from rv.
      
      rv[1] <- list(get.scan.info(RSS$source, RSS$new.scan.mat, trv=trv, trv.index=1L))

      ## how to call get.scan.info for another source
      ## trv[2] <- list(get.scan.info(RSS$other.source, RSS$other.scan.mat, rv=trv, rv.index=2L))
      
      ## Wait until any spawned threads have completed, updating the gui as we go.

      if (any(is.na(rv))) {
        while (any(is.na(trv))) {
          ## let the GUI respond; this is a no-op if there is no GUI installed
          rss.gui("UPDATE_GUI")
          rss.sleep(RSS$get.data.sleeptime)
        }
      } else {
        trv <<- rv
      }
    }

    ## For now, we are using only a single source, so use its scan info from here onwards
    
    si <- trv[[1]]
    
    if (is.null(si)) {
      ## get.scan.info failed, probably because there is no more data
      rv <- FALSE
    } else {

###.if $DEBUG
      print("Got scan info " %:% si$timestamp)
###.endif

      ## allow any plugins to provide additional scan information
      ## (e.g. GPS, weather station, NMEA...), or overwrite existing
      ## items (e.g. blip.samples.minmax might be coming from the blipmovie,
      ## but overwritten by further filtering, so the final effective value
      ## needs to be overwritten).  The plugins might need to know the original
      ## values of the scan info in order to decide whether to modify it
      ## e.g. blip.samples.minmax

      RSS$source.scan.info <- si
      si <- rss.call.hooks.accum(RSS$GET_SCAN_INFO_HOOK, si)

      rss.call.hooks(RSS$SCAN_INFO_HOOK, si)

      ## save current scan info at global level

      RSS$scan.info <- si

      ## create the per-pulse metadata from scan info

      rss.build.pulse.table()
      
      ## set validity flags; this must be done before get.scan.data() is called, since some
      ## sources (e.g. blipmovies) set one or more to TRUE

      RSS$new.have.valid$classification <- FALSE
      RSS$new.have.valid$patches <- FALSE
      RSS$new.have.valid$scores <- FALSE

      ## Get from possibly multiple sources.  The semantics here are
      ## a bit different from get.scan.info, because get.scan.data
      ## is mainly called for the side effect of loading data into
      ## its extmat argument, which it returns on success (it
      ## returns NULL on failure).  So each get.scan.data does one
      ## of the following:
      
      ##  - sets trv[trv.index] to NA, indicating a threaded read;
      ##    in this case, the return value of get.scan.data is
      ##    ignored, and we wait until trv[trv.index] is no longer
      ##    NA, at which point it is NULL for failure and anything
      ##    else for success
      
      ##  - does not set trv[trv.index] to NA_INTEGER, in which
      ##    case, the return value itself indicates failure (with
      ##    NULL) or success (anything else).
      
      ## Note that this violates R's call-by-value semantics, as the thread will modify
      ## a variable in the caller's environment.

      ## Create a list with a NULL entry for each source
      trv <- vector("list", num.sources)
      
      ## Get from the (for now) single source.
      ## we use '[1] <- list(' in the following because get.scan.data
      ## might return NULL, and we want to put a NULL in the appropriate slot of rv,
      ## rather than remove that slot from rv.

      trv[1] <- list(get.scan.data(RSS$source, RSS$new.scan.mat, trv, trv.index=1L))

      RSS$current.scan <- RSS$current.scan + 1
      
      ## Wait until any spawned threads have completed, updating the gui as we go.

      while (any(is.na(trv))) {
        ## let the GUI respond; this is a no-op if there is no GUI installed
        rss.gui("UPDATE_GUI")
        rss.sleep(RSS$get.data.sleeptime)
      }

      ## for now, we're only using one source, so check its return value
      
      if (!is.null(trv[[1]])) {
        ## swap the prev and current scan data (pointers)
        tmp <- RSS$scan.mat
        RSS$scan.mat <- RSS$new.scan.mat
        RSS$new.scan.mat <- tmp

        ## copy some new validity flags
        RSS$have.valid[c("classification", "patches", "scores")] <- RSS$new.have.valid[c("classification", "patches", "scores")]

        ## mark that we have valid scan data (all existing sources provide this,
        ## but in the future they should set this flag themselves)

        RSS$have.valid$scan.data <- TRUE

        ## indicate this is unprocessed scan data
        RSS$have.previewed.scan <- FALSE
        RSS$have.processed.scan <- FALSE

        rss.call.hooks(RSS$FULL_SCAN_HOOK, RSS$scan.mat)
        rv <- TRUE
      } else {
        rv <- FALSE
      }
    }
  }

  RSS$lock$rss.get.scan <- FALSE
  return(rv)
}

rss.classify.samples <- function()
{
  ## classify samples (hot or cold) and return
  ## the number of cold, hot, and blip samples

  ## Before classification of a new scan, we swap the
  ## class and prevclass matrices, so that the most recent
  ## classification appears as the "previous" class,
  ## and the storage from the prior classification is
  ## is re-used to hold the new classification.
  ## If we are re-classifying a scan due to changes in
  ## parameter values, then we don't do this swap.

  if (!RSS$have.previewed.scan) {
    tmp <- RSS$prev.class.mat
    RSS$prev.class.mat <- RSS$class.mat
    RSS$class.mat <- tmp
  }

  ## indicate that the classification has changed
  ## since patches were last found

  RSS$have.valid$patches <- FALSE
  RSS$have.valid$classification <- TRUE

  ## classify the samples and return the numbers found

  .Call("radR_classify_samples",
        RSS$score.mat,
        RSS$class.mat,
        RSS$prev.class.mat,
        RSS$blip.score.threshold)
}

rss.calculate.scores <- function() {
  ## calculate the z-scores for all samples

  RSS$have.valid$scores <- !is.null(.Call("radR_calculate_scores",
                                           RSS$scan.mat,
                                           RSS$mean.mat,
                                           RSS$dev.mat,
                                           RSS$score.mat))
}

rss.find.patches <- function()
{
  ## find patches of hot samples and return the number found
  ##
  ## If we already have valid patches for this scan (e.g. if we
  ## are just adjusting the filtering parameters rather than the
  ## finding parameters) then just unfilter the patches
  ##
  ## Otherwise, if a hook has been defined for this, use it.
  ##
  ## Finally, use the builtin patch finder if neither condition holds
  ##
  ## Side effect: the external storage pointed to by RSS$patch.buffer
  ## will hold the set of patches found, according to the format
  ## described in patchify.h

  if (RSS$have.valid$patches) {
    rss.unfilter.patches()
  } else {
    RSS$have.valid$patches <- TRUE
    ## how many samples should be skipped, if any?
    skip.samples <- as.integer(max(0, -RSS$scan.info$first.sample.dist / RSS$scan.info$sample.dist))
    if (rss.hook.is.active(RSS$FIND_PATCHES_HOOK))
      rss.call.hooks(RSS$FIND_PATCHES_HOOK, RSS$class.mat, RSS$blip.use.diags, RSS$patch.buffer, skip.samples)
    else
      ## otherwise, use the builtin patch finder; don't wrap when data are in rectangular coords
      .Call("radR_find_patches",RSS$class.mat, RSS$blip.use.diags, RSS$patch.buffer, skip.samples, ! isTRUE(RSS$scan.info$is.rectangular), PACKAGE="radR")
  }
}

rss.process.patches <- function()
{
  ## process patches, returning the vector of indexes in RSS$patches
  ## of those patches which are blips

  is.rect <- isTRUE(RSS$scan.info$is.rectangular)
  
  RSS$blips <- .Call("radR_process_patches",
              RSS$blip.filtering,
              RSS$scan.mat,
              if (RSS$have.valid$scores) RSS$score.mat else NULL,
              RSS$class.mat,
              RSS$patch.buffer,
              RSS$patches,
              RSS$blip.samples.minmax,
              RSS$blip.area.minmax,
              RSS$blip.angular.minmax,
              RSS$blip.radial.minmax,
              c(rss.rps(), rss.origin.elev()),
              RSS$blip.area.weighting,
              c(as.numeric(RSS$scan.info$timestamp),
                RSS$scan.info$duration / 1000,
                RSS$scan.info$bearing + RSS$scan.info$bearing.offset,
                RSS$scan.info$orientation,
                RSS$scan.info$first.sample.dist / RSS$scan.info$sample.dist,
                2^RSS$scan.info$bits.per.sample - 1
                ),
              RSS$pulses,
              is.rect,
              PACKAGE="radR")
 
  return(length(RSS$blips))
}

rss.unfilter.patches <- function()
{
  ## restore any filtered patches
  RSS$blips <- .Call("radR_unfilter_patches",
                     RSS$class.mat,
                     RSS$patch.buffer,
                     PACKAGE="radR")
  RSS$num.blips <- length(RSS$blips)
}

## this list of values must be identical to the enum t_stats_mode in
## file radR.h

rss.STATS.MODE <- list(INIT.ACCUM = 1,
                       CONT.ACCUM = 2,
                       LAST.ACCUM = 3,
                       UPDATE = 4)

rss.update.stats <- function()
{
  ## figure out what phase we are in.
  left <- RSS$scans.to.learn - RSS$num.scans.learned
  if (left > 0) {
    if (RSS$num.scans.learned == 0)
      ## first learning scan
      mode <- rss.STATS.MODE$INIT.ACCUM
    else if (left > 1)
      ## subsequent but not last learning scan
      mode <- rss.STATS.MODE$CONT.ACCUM
    else
      ## the last learning scan
      mode <- rss.STATS.MODE$LAST.ACCUM
  } else {
    mode <- rss.STATS.MODE$UPDATE
  }

  .Call("radR_update_stats", RSS$scan.mat,
        if(RSS$blip.exclude.blips.from.stats.update) {
          RSS$class.mat
        } else {
          NULL
        },
        RSS$cell.dims,
        RSS$mean.mat,
        RSS$dev.mat,
        RSS$stats.k,
        mode,
        RSS$num.scans.learned)

  if (left > 0)
    RSS$num.scans.learned <- RSS$num.scans.learned + 1

  ## we have valid stats with which to classify this scan
  ## if there are no learning scans left to do
  ## (Note that "left" represents the count before the most
  ## recent call to radR_update_stats, so left<=1 means we've
  ## finished learning.)
  
  RSS$have.valid$stats <- left <= 1
}

rss.set.port <- function(port, ...)
{
  ## open a port with new configuration information (passed in ...)
  ## if the port is already open, it is first shut down

  which <- ifelse(port$is.source, "source", "sink")
  if (!is.null(RSS[[which]])) {
    rss.call.hooks(if (port$is.source) RSS$END_SOURCE_HOOK else RSS$END_SINK_HOOK)
    shut.down(RSS[[which]])
    RSS[[which]] <- NULL
  }
  config(port, ...)
  ok <- FALSE
  try( {
    rss.restart.learning()
    if (!is.null(start.up(port))) {
      RSS[[which]] <- port
      if (port$is.source) {
        RSS$have.valid$bitmap <-FALSE
        RSS$have.valid$scan.data <- FALSE
      }
      rss.call.hooks(if (port$is.source) RSS$START_SOURCE_HOOK else RSS$START_SINK_HOOK)
      rss.gui(PORT_SET, port)
      ok <- TRUE
    }
  }, silent = TRUE)
  if (!ok) {
    rss.set.no.port(which)
    config <- config(port)
    rss.gui(POPUP_MESSAGEBOX, "Can't start port", "I was unable to start up the " %:% class(port)[1] %:% " with configuration \n" %:% paste(names(config), as.character(config), sep="=", collapse="\n") %:% "\n" %:% geterrmessage() %:% "\nThere may be more details in the console window.")
  }
}

## indicate there is no valid port for a source or sink,
## e.g. when an error occurs on trying to start.up the port,
## or when the user explicitly selects "no source/sink"

rss.set.no.port <- function(which="source") {
  if (!is.null(RSS[[which]])) {
    shut.down(RSS[[which]])
    RSS[[which]] <- NULL
  }

  if (which == "source") {
    rss.invalidate.products()
    zero(RSS$scan.mat)
    zero(RSS$new.scan.mat)
    RSS$have.valid$scan.data <- FALSE
    RSS$have.valid$bitmap <- FALSE
    RSS$scan.info <- rss.call.hooks.accum(RSS$GET_SCAN_INFO_HOOK, list())
    RSS$scan.info$timestamp <- as.POSIXct(Sys.time())
    RSS$num.blips <- RSS$num.patches <- 0
    rss.gui(UPDATE_PLOT_WINDOW)
  }
  rss.gui(SET_NO_PORT, which)
}

## called when the current data products (stats, scores, class) are no
## longer valid, e.g. after hitting stop.

## called when it is time to restart learning

rss.invalidate.products <- function()
{
  RSS$num.hot.samples <- 0
  RSS$num.blips <- 0
  RSS$total.num.blips <- 0
  RSS$current.scan <- 0
  ## Note that each of these strictenvs must be assigned to separately
  RSS$have.valid[c("stats", "scores", "classification", "patches")] <- rep(FALSE, 4)
  RSS$new.have.valid[c("stats", "scores", "classification", "patches")] <- rep(FALSE, 4)
  ## zero out matrices
  zero(RSS$class.mat)
#  zero(RSS$score.mat)
#  zero(RSS$scan.mat)
#  zero(RSS$new.scan.mat)
  RSS$pulses <- NULL
#  rss.gui(UPDATE_PLOT_WINDOW)
}

rss.restart.learning <- function()
{
  RSS$num.scans.learned <- 0
  RSS$scans.to.learn <- RSS$default.scans.to.learn
  rss.invalidate.products()
  rss.gui(UPDATE_PLOT_WINDOW, reconv=TRUE)
  rss.call.hooks(RSS$RESTART_LEARNING_HOOK)
}

## called by the GUI when blipping is turned off

rss.stop.blipping <- function()
{
  ## prevent use of the old classification matrix in
  ## scan conversion.  Otherwise, we see blips in
  ## the position they were during the last blipping scan

  zero(RSS$class.mat)
}

## called by the GUI to pause the blipping loop

rss.do.pause <- function()
{
  ## invoke any user-defined actions
  rss.call.hooks(RSS$ONPAUSE_HOOK)
}

rss.do.play <- function()
{
  ## Called by the GUI to (re)start the blipping loop
  ## by setting the appropriate flags for the event loop.

  ## invoke any user-defined actions before changing
  ## the state, so that the hook functions can
  ## use the current state in deciding what to do.

  rss.call.hooks(RSS$ONPLAY_HOOK)
  ## a hook only called when entering continuous play mode
  rss.call.hooks(RSS$ONPLAY_CTS_HOOK)

  if (RSS$recording && !is.null(RSS$sink) && !RSS$sink.run.started) {
    ## if the user hit play from a stopped state,
    ## and we are recording, start a new run
    start.run(RSS$sink)
    RSS$sink.run.started <- TRUE
  }
}

rss.do.stop <- function()
{

  ## mark the end of run in recording

  if (RSS$sink.run.started) {
    RSS$sink.run.started <- FALSE
    end.run(RSS$sink, RSS$scan.info$timestamp)
  }
    ## invoke any user-defined actions
  rss.call.hooks(RSS$ONSTOP_HOOK)

  ## restart learning, if configured to do so

  if (RSS$restart.learning.after.stop)
    RSS$restart.learning <- TRUE
}

rss.do.play.one.scan <- function()
{
  rss.call.hooks(RSS$ONPLAY_HOOK)
  ## a hook only called when entering single-step play mode
  rss.call.hooks(RSS$ONPLAY_ONE_HOOK)

  if (RSS$recording && !is.null(RSS$sink) && ! RSS$sink.run.started) {
    ## if the user hit play from a stopped state,
    ## and we are recording, but a sink run has not been started,
    ## then do so
    start.run(RSS$sink)
    RSS$sink.run.started <- TRUE
  }
}

rss.init <- function (use.gui=TRUE, event.loop=TRUE) {
  ## use.gui: should a GUI be loaded?

  ## install a custom error handler
  options(error = rss.handle.error)

  ## use millisecond time formatting, by default
  options(digits.secs = 3)

  ## initialize runtime variables
  rss.init.vars()

  ## read the radR library
  rss.dyn.load("main/radR")

  rss.get.shared.constants()
  rss.init.extmats()

  ## add a hook to update patch parameters and possibly update the plot window
  ## when the antenna configuration changes

  rss.add.hook("ANTENNA_CONFIG", "main", list(f=rss.antenna.config.changed, enabled=TRUE, read.only=FALSE))


  ## read in config information for the base system
  rss.load.config("main/radR", RSS)

  ## set the default timezone
  Sys.setenv(tz=RSS$timezone)

  ## load palettes

  rss.load.palettes()

  ## create main hook functions

  rss.install.main.hooks()

  ## determine whether there is a GUI

  RSS$have.gui <- use.gui && exists("GUI.init") && GUI.init() && GUI$enabled
  if (!RSS$have.gui) {
    ## create an empty GUI eventlist; ie. no GUI; this turns rss.gui(...) calls
    GUI <<- new.env(parent = .GlobalEnv)
    GUI$event.list <- list()
    GUI$enabled <- FALSE
    GUI$plot.is.tk.image <- FALSE
  }

  rss.gui(POPUP_MESSAGEBOX, "Loading radR plugins", "Please wait while radR loads plugins; this may take a while...", id="loading")
  rss.gui(CREATE_WINDOWS)

  ## load the list of available plugins

  rss.get.plugin.list()

  ## load any plugins which are designated for load
  ## upon startup, but don't let errors there cause problems

  RSS$plugins <- list()

  old.handler <- options()$error
  options(error=quote({dump.frames();rss.handle.error(extra.msg=sprintf("Error loading plugin '%s'\n", plugin.being.loaded), last.dump=last.dump)}))

  for (p in sort(RSS$plugins.to.load)) {
    plugin.being.loaded <<- p
    rss.load.plugin(p, manually=FALSE)
  }
  rm(plugin.being.loaded, envir=.GlobalEnv)

  options(error=old.handler)

  ## once tearoffs and plugin-menu "Controls" windows
  ## have been created, create main windows and
  ## set all window geometries and states to their
  ## saved values

  rss.gui(RESTORE_WINDOW_SETTINGS)

  ## indicate no source or sink, to start with

  rss.set.no.port("source")
  rss.set.no.port("sink")

  ## if there is a startup file, source it

  if (!is.null(RSS$startup.file) && file.exists(RSS$startup.file))
    try(source(RSS$startup.file), silent=TRUE)

  ## install the event loop
  ## which senses when radR is in a "play" state
  ## and does the appropriate work

  rss.gui(DELETE_MESSAGEBOX, "loading")

  if (event.loop)
      rss.install.event.loop()

  ## start the main loop
  if (RSS$have.gui) {
     switch(.Platform$OS.type,
            windows = {
              cat("\n***Entering the radR event loop. ***\n\nThis loop needs to be running in order for most of radR to work.\nIf you see the R prompt in this window, restart the loop by doing:\n\n   go()\n\n")
              rss.event.loop(FALSE)
            }
            )
   } else {
     ## if any "--source" parameters have been specified, source those files
     args <- commandArgs()
     i <- which(!is.na(match(args, "--source")))
     if (length(i) > 0)
       for (f in args[i+1])
         if (nchar(f) > 0)
           source(f)

     cat("\nradR loaded.\nNo GUI loaded.\n\nEnjoy the friendly command prompt!\n\n")
   }
}

rss.intersect.ranges <- function(r1, r2) {
  ## each of r1 and r2 is a numeric range given by a vector
  ## of the form c(min, max), or a NULL which effectively represents c(-Inf, Inf)
  ## Return the intersection of the two ranges, which might be empty.

  if(length(r1) == 0) r1 <- NULL
  if(length(r2) == 0) r2 <- NULL
  return(c(max(r1[1], r2[1]), min(r1[2], r2[2])))
}

rss.provide.scan.info <- function(si) {
  si$noise.cutoff <- RSS$noise.cutoff
  
  ## provide scan info parameters dealing with blip processing
  if (RSS$blip.finding) {
    si[RSS$blip.finding.parms] <- RSS[RSS$blip.finding.parms]
  } else {
    ## even if we're not blip finding, we need to have slots in the
    ## scan.info list for those parameters (just in case the user turns
    ## off blip finding while recording a movie)
    si[RSS$blip.finding.parms] <- si[RSS$blip.finding.parms]
  }
  if (RSS$blip.filtering) {
    ## we are filtering (possibly already filtered) data; intersect the filter
    ## ranges to reflect the combined filtering
    for (n in RSS$blip.filtering.parms)
      si[[n]] <- rss.intersect.ranges(si[[n]], RSS[[n]])

    ## If we're filtering with an expression, record that expression.
    ## If the data already were filtered, then record the logical conjunction
    ## ("and") of the original filtering expression with the current one.
    
    if (RSS$use.blip.filter.expr) {
      if (is.null(si$blip.filter.expr))
        si$blip.filter.expr <- RSS$blip.filter.expr
      else
        si$blip.filter.expr <- call("&", si$blip.filter.expr, RSS$blip.filter.expr)
    }
  } else {
    ## even if we're not blip filtering, we need to have slots in the
    ## scan.info list for those parameters (just in case the user turns
    ## off blip filtering while recording a movie)
    si[RSS$blip.filtering.parms] <- si[RSS$blip.filtering.parms]
    ## we want a NULL in the "blip.filter.expr" slot
    si["blip.filter.expr"] <- list(NULL)
    si$use.blip.filter.expr <- FALSE
  }

  return(si)
}

rss.install.main.hooks <- function() {
  ## provide basic scan metadata, even in the absence of a data source

  rss.add.hook("GET_SCAN_INFO", "main", rss.provide.scan.info)

  ## zero out class matrices between sources
  rss.add.hook("END_SOURCE", "main",
               function() {
                 RSS$class.mat[] <- RSS$CLASS.VAL$cold
                 RSS$prev.class.mat[] <- RSS$CLASS.VAL$cold
               })

  ## in case the user provides a patch filtering expression
  ## add a hook to evaluate it
  rss.add.hook("PATCH_STATS", "main", rss.filter.patches.by.expr)
}

## a list of expressions to be substituted for variables when
## evaluating a patch filtering expression.  This is just so the user
## doesn't have to use "[]" to get the values from the patch stats
## extmats.

rss.filtering.subs <- list (
                            perim = quote(perim[]),
                            area  = quote(area[]),
                            ns    = quote(ns[]),
                            aspan = quote(aspan[]),
                            rspan = quote(rspan[]),
                            max = quote(max[]),
                            int = quote(int[]),
                            x = quote(x[]),
                            y = quote(y[]),
                            z = quote(z[]),
                            t = quote(t[]),
                            range = quote(range[])
                            )

## evaluate an expression in the context of the patch stats variables:
##
##   ns:    number of samples in the patch
##   area:  patch area in metres^2
##   perim: patch perimeter in metres
##   aspan: angular span of the patch (in pulses)
##   rspan: radial span of the patch (in samples)
##
## The only complication is that because these are currently stored as extmats,
## we must append '[]' to them to extract their values as R vectors.

rss.eval.with.patches <- function(expr) {
  ## The inner eval forces "substitute" to replace "VAR" with "VAR[]"
  ## The outer eval evaluates "expr", looking up each "VAR" in RSS$patches
  eval(eval(substitute(substitute(x__, rss.filtering.subs), list(x__ = expr))), RSS$patches)
}

## a PATCH_STATS hook function which filters patches by the
## expression stored in RSS$scan.info$blip.filter.expr
## "keep": the current status of each patch (TRUE == blip, FALSE=filtered)
##
## This hook function is always enabled, but has no effect unless RSS$use.blip.filter.expr is TRUE
## and RSS$blip.filtering is TRUE.

rss.filter.patches.by.expr <- function(keep, expr) {
  if (!RSS$blip.filtering)
    return(NULL)
  if (missing(expr))
    expr <- if (RSS$use.blip.filter.expr) RSS$blip.filter.expr[[1]] else NULL
  if (is.null(expr))
    return(NULL)
  keep2 <- rss.eval.with.patches(expr)
  if (!is.logical(keep2) || (length(keep2) != length(keep) && length(keep2) != 1))
    stop("The blip filtering expression '", expr, "' did not return a logical vector of the correct length.")
  return(keep & keep2)
}

## a function to get/set processing parameters

## Every important strictenv in radR (i.e. RSS and many plugins)
## will have a function named "par" which gets and sets processing
## parameter values.  Most of these will simply call rss.par with
## their own environment.

rss.par <- function(env, ...) {
  ## names of processing parameters
  l <- as.list(...)
  if (length(l) == 0)
    return (env[par.names])
  for (n in names(l))
    if (! n %in% env$par.names)
      stop("unknown parameter name: ", n)
  invisible(env[n] <- l)
}
