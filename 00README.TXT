05 Dec 2011
- various bug fixes
- new decluttering plugin
- for tilter plugin users: please see a serious FIXME note here:
  http://radr-project.org/03_-_Current_Features_and_Plug-ins/The_tilter_plugin

06 Sep 2011
all branches:
- (IMPORTANT) sample classification has changed:  a sample is hot if its
score is either above the high hot score threshold (as before)
*or* if the score is below the low hot score threshold (NEW!)  
This means unusually dark areas of the sample matrix can be blips too.
For radar data, you probably don't want "cold" blips, so the low hot score
threshold should be set to a big negative integer (e.g. -128).
This is the default, which mimics the earlier functionality.  
For video (camera) data, targets can be darker or lighter than background
in grayscale, so the low hot score threshold might be set to something
less extreme (e.g. -5).

02 Sep 2011
New numbering of radR releases:

  e.g. radR-BRANCH-DATETIME-PLATFORM.zip  (binary)
       radR-BRANCH-DATETIME-src.tar.gz  (source)
  BRANCH is e.g. "video", "stable"
  DATETIME is YYYYMMDDHHMMSS of most recent change to code
  PLATFORM is windows or unix

video branch:
- added rectangular coordinate handling for video plugin,
  so that interaction with plot window and calculation of
  patch parameters is correct

radR 817  10 Aug 2011
Various fixes to stable windows version.  
New ONRESUME_PLAY hook allows resuming play when a live data source
notices the radar is transmitting again.  Only for XIR3000 so far.

radR 777  13 Apr 2011

Fixes to tilter plugin:
- tiltserv now returns correct angle history
- learning over a volume pattern no longer bleeds blips from learned
to unlearned angles


radR 772  06 Apr 2011 

The tilter plugin now requires rev 1.1 of the tilter board firmware
(can be used on both rev 1.0 and rev 2.0 tilter boards).  See notes in
plugins/tilter/tilter.windows.conf.R for what to do if your TTL5USB9M
shows up as a COM port above COM9 (which the plugin doesn't support).


radR 764  10 Mar 2011
More bug fixes:
 - tracker:  clean up behaviour around enable/disable, old 
   filenames that are no longer valid, changing output filenames
   while playing

 - xir3000arch: timestamp fix for REC_TYPE_RLC_4 files; was
   incorrectly treating components of MS_FILETIME as signed,
   leading to timestamp inversions
 

radR 760   8 Mar 2011

Bug fix release, mainly.
main: try not to kill the event loop on errors.

zone and tracker plugins: deal correctly with no-longer-valid names of
files stored in configuration, and clean up some edge-case behaviour
in creating/enabling/disabling the exclusion zone.

rawarch plugin: silently correct archives where the last scan wasn't
fully written to disk (e.g. due to power outage, or disk full).

xir3000arch plugin: don't accept nonsensical dates
in .REC files - defer to dates parsed from filenames in that case.

seascanarch plugin: fix a typo that was preventing the faster main
loop from operating when unpacking 12-bit data into 16-bit integers

genblips: calculate correct time at which target is next seen by radar.


radR 656   6 Oct 2010

The xir3000 plugin is now built against RTI SDK 1.5.0.012, which
is more flexible about the location of XIR3000 firmware.  The minimal
required subset of the RTI SDK is now distributed as part of radR,
and is used by default.

The xenexarch plugin, for reading .REC files recorded by Russell
Technologies software, has been renamed to xir3000arch, and now
supports reading of files recorded as types RLC_3 and RLC_4.

This version also fixes an apparently long-standing display bug
wherein spatial coordinates at the mouse cursor were incorrect if the
plot was rotated from its default orientation.

radR 646  28 Sep 2010

Various fixes to the xir3000 plugin, especially in range cell size calculation.

radR 642   6 Aug 2010

New plugin: xir3000 which operates an RTI XIR3000 USB video processor
under RTI SDK 1.5.0.008.  The xenex plugin is now obsolete.

Tilter: start/stop pattern with play.  Correct a learning-at-different
angles problem.

radR 621   6 July 2010
Fix bug: plot info window wasn't being placed when created, which
triggered continuous error messages in the Rterm console while
moving the cursor through the plot window under certain conditions.

radR 619
2 July 2010
Fix bug introduced in radR 616: don't update estimate of mean
deviation for a stats cell when only 1 non-blip sample is in
the cell; this was causing a divide-by-zero crash.

radR 616
26 June 2010

Fixes to learning: we now estimate cell mean deviations during the
learning phase in an unbiased manner, so that at the end of the
learning phase, the estimates are already at the values to which they
would converge in the original scheme.

Fixes to the tilter plugin: deal correctly with learning at multiple
tilt angles.

radR 560
29 Apr. 2010

Important bug fix: filtering of blips by area was broken by revision
491 (21 March 2010).  Blip areas were calculated correctly, but the
min/max filtering limits were incorrectly applied.  


radR 551
21 Apr. 2010

When upgrading radR, the recommended procedure is now to extract the
new version to a new folder, and then use the GUI menu to source the
script called "copy_params_from_old_install", which will allow you to
selectively copy parameters and antenna, palette and zone files from
an older installation.  This way, the old installation is preserved
for your use in case you discover problems with the new one.

radR 537
6 Apr. 2010
Bug fixes in creation of pulse metadata under edge cases (e.g. no data)

radR 533
31 Mar. 2010

Fixes bugs in the tiltminder and tilter plugin usability issues.
Old / rarely-used plugins are not loaded by default.

radR 521
29 Mar. 2010

Deal gracefully with situations where the GUI gets temporarily out
of sync with the internal state on slow platforms.  Don't attempt to
(re)start a tilter pattern without calibrating.  Deal better with
return code when issuing the tilter calibration command.


radR 512
25 Mar. 2010

There is now a cutoff value for eliminating presumed receiver noise.
Sample values below this parameter are set to zero (and so will never
be classified as part of a blip).  A value of zero (the default) does
nothing.  In the future, the antenna plugin will allow setting this
value in dbm units, based on calibration of a particular
radar+digitizer combination.  For now, you must use your judgement.
If you modify the value of the parameter while paused, the screen is
updated to show the new filtering.  However, if you reduce the
threshold, you must reload the scan (e.g. move forward and backward
one scan) to restore the zeroed data.

Raw archives can now store their data in compressed form, without loss
(and this is the default).  This may save considerable disk space if
the noise cutoff is positive, and/or an exclusion zone is created, and
samples there are zeroed (this behaviour is now the default in an
exclusion zone).  You can turn off compression via a toggle in the
rawarch plugin menu.

radR499
22 Mar. 2010

radR now uses higher precision tilt angle estimates in calculating
target positions.  However, the plot window still uses only the
constant tilt value corresponding to that of the first pulse when
displaying raw data.


radR480
11 Mar. 2010

This release reinstates tilter support, with a new client-server
model that should generalize readily to other devices.
radR does not yet use the higher precision angle estimates which
this new model provides.

radR425
19 Jan. 2010

This includes many small bug fixes from the past 8 months.  New
features include the ability to export full sample data for user-
selected blips. This is intended for development and calibration
work, and is documented here:

  http://www.radr-project.org/07_-_FAQ/13_Save_and_process_full_sample_data_for_multiple_blips

Tilter support is broken in this release, but will be fixed soon.


radR337
29 Apr. 2008

Changes have been made to the tracker plugin, but not in the stable
version of radR.  The changes are intended to make radR's
implementation of the Multiframe Correspondence algorithm conform more
closely to the algorithm described in the paper [1].

Specifically:

     The antenna angle is now used in calculation of the maximum
     possible observable distance between two targets.  This maximum
     value is used in gain function calculations, and the new value,
     which is generally larger than the old value, will reduce gain
     values in most situations.

     The point-to-point gain is now multiplied by the factor (1-alpha)
     to match my revised understanding of the MFC paper.  This will
     reduce the point-to-point gain values relative to the
     track-to-point gain values, and so it should reduce the chances
     of "track interruption" by new points.  "track interruption"
     means an isolated point "steals" the tail from a real track. This
     can occur if an isolated point in scan N is very close to a new
     point in scan N+1, even if the new point is close to the location
     predicted from an existing track.  It is still possible for this
     to happen, (because the MFC algorithm does not explicitly
     consider the possibility, it seems), but it will be less likely
     now.

     The penalty value, eps, which is intendend to help prevent tracks
     from missing blips, is now subtracted from any gain value where
     the two points (or track endpoint and new point) are not from
     consecutive scans.  Up to now, this penalty was not being used in
     my code.  If a track's most recent blip is from scan N, this
     penalty makes the blips from scans N+2, N+3, ... slightly less
     attractive than those from scan N+1.  This will reduce the
     chances that tracks will "miss" blips.

[1] K. Shafique and M. Shah (2005).  "A Noniterative Greedy Algorithm
    for Multiframe Point Correspondence".  IEEE Transactions on Pattern
    Analysis and Machine Intelligence, Vol. 27(1), pp. 51 - 65.

radRstable
21 Apr. 2008
   - The new "stable" branch is recommended for production use.
     No new features will be added to it, but bugs will be fixed.

     The less well tested but newer revisions will continue
     to be posted by revision number.

     Changes to the stable branch will appear in the 00README.TXT

     The first set of bug fixes makes batch processing work again.

radR276
06 Mar. 2008

   - console window improvements:

       - multiline copy/paste now works.  When you hit Enter,
         everything between the previous prompt (or start of window)
         and the end of the window (or the next prompt) is accepted as
         input.  To start a new line in your input without submitting it,
	 use Shift+Enter.

       - "Home" and "End" keys move to the start and end of the
         current input, even if it has multiple lines.

       - at the start of an input line, the "Up" and "Down" keys move
         you through the input history (i.e. retriever lines you
         entered before).

       - if you edit a line from the input history, and wish to undo
         the changes, hit the "Esc" key.

       - if you change a line from the input history, and then retrieve
         another line from the input history with "Up" or "Down",
         the changes to the former line will be saved in the history.

       - console history is now saved if you save parameters when exiting radR 

   - blip finding: blips now have a "range" attribute, which is the
     true centroid range;  this is equal to:
      sqrt(x^2+y^2+(z-RSS$scan.info$elevation)^2)
     i.e. it takes into account the elevation of the antenna
     The variable "range" is available for use in a filtering expression.

   - tracker: output to the .CSV file now includes scan index and
     range for each blip
     
   - tracker multiframecorr model:

      - allow a user-specified R function to calculate the state for a 
        target motion model;  this means the entire tracker model can
        now be specified in R: gain function between pairs of points, gain
        function between a track and potential new points, and state
        update.  The plugin comes with R versions of the internal C 
 	functions, which can be enabled from the radR console using
        this expression:

	with(TRACKER$models$multiframecorr, {
				    set.gain.pp(gain.pp)
				    set.gain.tp(gain.tp)
				    set.new.state(new.state)
				    })

        Wiki documentation will be added soon.

      - preview in the plot-window is now implemented

   - genblips plugin: bug fixes

   - bliptrails plugin: now works correctly with single stepping. Also,
     retains all trail data in original units, and fades only for
     display, so the "fadeout factor" and "scans to retain" both do
     intuitive updates of the plot window when adjusted in preview mode.

   - gifmovie: gifsicle.exe was missing from distribution; getting
     of plot window+frame+titlebar was not working; a last frame
     was being generated and not incorporated into the .GIF movie in
     periodic capture mode

   - blipmovies: recording was not working if no score data were
     available (e.g. recording a new blipmovie from an old blipmovie
     archive)

   - batch processing with zones: allow a zone file to be specified in 
     the batchparm.R file

   - the plugins loaded at radR startup appear in alphabetical order
     in the plugin menu.  Plugins loaded manually appear at the end
     of that menu.     

radR233
24 Feb. 2008

   - allow an R expression for filtering blips.  This expression can
     use the variables x, y, z, t, aspan, rspan, max, int, area, ns
     (which are patch statistics) as well as the usual R functions.
     You can specify an expression for each special zone. 

     Note: this is not yet well tested, and there are some caveats;
     see the wiki.

radR229
19 Feb. 2008

   - fixes to bugs in genblips, tracker, extmat package, some of
     which caused R crashes

   - new zone plugin menu options to enable/disable all, show/hide
     all, and rename zones

   - after doing a "Save zones as...", the default zone filename
     changes to the new name.

   - new antenna parameter: "true range of first sample (in metres)",
     which can be used to shrink or create a hole at the centre of
     radar data to correct for a bad trigger delay value when the data
     where digitized; i.e. this is the way to correct for range
     errors.  

     This value can be:

     * Zero: no change.  Ranges are already correct.

     * Positive: use this to increase the range of targets.  An
     empty hole will appear in the middle of the plot, corresponding
     to missing data.  This can correct for a trigger delay which was
     too large.

     * Negative: use this to decrease the range of targets.  Samples
     will disappear from the middle of the screen.  This can correct
     for a trigger delay which was too short (and will eliminate a
     hole at the middle of the screen.)

     TODO: When you play back a blipmovie and override this parameter
     with a negative value, all blips shift toward the centre of the
     screen.  However, blips which disappear into the middle are still
     processed, even though they should be filtered.
     
     
     

30 Jan. 2008
radR192

   - exclusion zone filtering of patches was leaving blips
     misnumbered, so, e.g., the tracker was building tracks
     regardless of the exclusion zone!
   
   - bliptrails are working again

   - the exclusion zone's palette is now explicitly showin in
     the display options window (the former zone plugin menu
     item for "blanking" the exclusion zone is gone).

   - when editing zones, the scan is reprocessed each time the
     geometry changes (e.g. at the end of dragging or resizing a zone
     segment); so the "finish edit" zone menu item no longer includes
     "and reprocess scan"


29 Jan. 2008
radR183
   - fixes to major bugs in the zone plugin 

26 Jan. 2008
radR176
   - a new zone plugin allows creationg of zones where blips will not
     be found, and other zones where they will be filtered with
     different parameters

   - the gifmovie plugin can now also capture frames at fixed time
     intervals, rather than once per scan; this will be useful for
     creating "howto videos" once I figure out a good way to paint
     the mouse cursor into the captured frames
     
   - various bug fixes (some serious previously unexposed bugs in 
     using negative indexes on extmats)     

17 Dec. 2008
radR123

   - by default, don't try to load hardware-based plugins; the user
     can use the plugin menu to load the appropriate ones the first time

   - if the user cancels loading of a hardware client library when
     asked where it is, the plugin load is cancelled and that plugin
     is marked as not-to-be-loaded next time radR is run (if the user
     chooses "save settings" when quitting radR).


12 Dec. 2008
radR122

   - bug fixes and more precise semantics to processing loop; some GUI
     race conditions have been corrected, and if any remain, we have a
     simple framework (rss.defer.*) for dealing with them
     (e.g. choosing "No Sink" right after hitting "Stop" should no
     longer cause problems)
  
   - can preview data and single-step on live sources; any paused scan
     is processed in preview mode, then processed for real when "play"
     or "play1" is hit; after processing the last scan in an archive,
     it can be "postviewed" by changing parameters. (Scans from a live
     source may of course be lost if one waits too long, but this doesn't
     necessarily cause problems).

   - bug fix to seascanarch table of contents and scan times/durations

   - stats learning phase was undercounting number of accumulation
     scans by 1, which led to initial estimates of cell mean and
     deviation being too high, so that early scans after learning had
     fewer hot samples and blips than they should have had!

   - preliminary xenex plugin; crashes on close of device, but this is a vendor
     bug with a pending fix; still no way to set the reference level
     for A/D converter, so we're really only getting 7 bit data.

   - jump to start/end buttons on player; stop no longer automatically
     seeks to start.

   - restart learning button works more sensibly
 
   - turning blip finding on/off during play works as you'd expect:
     turning it back on uses whatever stats had last been learned, if 
     any, and turning it off now correctly goes back to no hot pixels
     or blips.
     
   - mousewheel zoom now follows Google Maps convention (i.e. reverse
     of what it was)
     
   - to reset plot view to standard, now use Shift-Mouse Button 2,
     because bare Mouse Button 2 was often triggered by vigorous mouse wheeling

   - slightly cleaner and faster smoothed scan conversion

   - simplified hook function calling: e.g. rss.add.hook(DONE_SCAN,
     gui.print.cons(RSS$scan.info$timestamp)) (no quotes for hook names, 
     expressions and calls automatically get wrapped in function(...){}

   - antenna plugin now has a heading correction parameter

13 Nov. 2008
radR76
   - bug fixes to biglist/bigframe;  some of these affect rawarch
   archives.
   *** Upgrading from radR59 is strongly recommended if you use
   raw archives or build tracks.

   - improvements to the build process on windows.

   - further work on the xenex plugin; it basically works with both
     the USB device and the IntegRadar server, but still lacks control
     over antennna-related parameters.
   

31 Oct. 2008
radR36:

   - rawarch plugin: fixed a bug that caused index files to be corrupted
   when more than one rawarch file was written in the same radR
   session.  (the rawarch biglist files themselves were intact and the
   biglist index could be reconstructed by a single pass through the 
   biglist.  Code is available for this from the author.)

   - rawarch plugin: perform a rigorous check for correctness of the
   table of contents upon start.up() of an existing archive.  If 
   the check fails, the user is shown a proposed table of
   contents inferred from the file, and asked whether it should be
   written to disk

   - restore the alpha-blending (transparency) feature which had been
   inadvertently removed for timing tests
   

27 Oct. 2008
radR27:
   - the seascan plugin now correctly reads video gain and threshold
   information from the antenna file

   - batch processing now available on unix and windows

22 Oct. 2008

radR19: I've reset the revision numbering because I lost a big
	chunk of revision history when upgrading my development box.
 	backups, backups, backups...


================================================================================

   - the seascan plugin can now read data using a separate thread,
     maintaining the responsiveness of the GUI.  This is determined
     by the "Use a separate thread..." option on the seascan menu, and
     by the configuration file option "get.scan.is.threaded"

			      


radR657 - in the main directory
  Bug fixes:
   - clicking on an active track draws circles around its points,
     again

   - the "Save Tracks to CSV file?" binary option in the tracker
     plugin menu is now correctly restored from its saved value.
    
   - the number of samples in a patch wasn't being saved to the .CSV
     file for the tracker; this made it look like perimeter was
     missing.

radR650 - back in the main directory, as the tracker plugin is reasonably stable

  Bug fixes:
   - saving radR configuration no longer breaks radR (bug in the
     pointerinfo plugin)
 
   - range ring distances are correct

   - angle for T-bars is no longer forced to zero

  New features:

  - perimeters (in metres) are now available as part of the stats for patches

  - all stats for every patch are available through the data.frame
  RSS$patches.  Each column is an extmat, but you can access this
  data.frame in much the same way as you can a normal data.frame,
  except that you can't (yet) use logical vectors for indexing.
  e.g.

    WON'T WORK:   RSS$patches[RSS$patches$area > 100,]

    INSTEAD DO:   RSS$patches[which(RSS$patches$area > 100),]

  (I'll add logical indexing to the extmat package soon.)

  Those patches which have passed the curret filters for area, number
  of samples, angular and radial span, (aka "blips") are listed in
  RSS$blips, an integer vector.

  i.e. the full stats for all blips is given by:

    RSS$patches[RSS$blips,]

  - these full stats are saved in the .CSV files by the tracker,
    and in the blips biglist

  - screen geometry is now represented internally by "metres per
    pixel" rather than "pixels per sample".  This allowed cleaning
    up of various coordinate issues, and means you can get range rings
    and range info from the pointer info plugin even when you haven't
    loaded any radar data.

  - the gain functions used by the multiframecorr tracker model are
    now available in R, if you don't like the built-in C defaults.
    There are R versions which mirror the calculations done in C;
    these can be enabled, replacing the C versions like so:
                                                                                              
          with(TRACKER$models$multiframecorr, {set.gain.pp(gain.pp); set.gain.tp(gain.tp)})   
                                                                                              
    Use of the internal C versions can be restored by doing                                  
                                                                                              
          with(TRACKER$models$multiframecorr, {set.gain.pp(NULL); set.gain.tp(NULL)})         
    
    The R versions are in plugins/tracker/multiframecorr.model.R,
    called gain.pp and gain.tp.  You can define your own.  
    I'll be adding GUI items for these soon.
    
  - the compass and range-ring controls are now spinboxes, letting you
    enter values directly (a long-standing promise to Carolyn, finally
    fulfilled)

  - time display in the pointerinfo window and the plot window title
    now include milliseconds and deciseconds, respectively.  The time
    in the plot window title corresponds to the time for the first
    pulse in the current scan.  As the cursor moves, time is displayed
    only to the nearest pulse.  Time for blips has higher resolution
    since it is an average of pulse times.
 
  - the timezone should now be consistently used and is
  configured via the timezone variable in radR.conf.R
  By default, it is GMT.    
    
radR594 (in "alpha" directory)

   Bug fixes:
   - the tracker is starting to behave as it should; many low-level
   bugs have been fixed

   New feature: 

   - you can hit the "h" key in the plot window to hide all but
   the currently selected track.  When a track is selected, other
   tracks are not automatically highlighted.  Select / deselect a track
   by clicking on it with the left mouse button.  Select / deselect
   an anchor point on the track (for gain calculation) by clicking on
   it with the left mouse button.

radR585 (in "alpha" directory)

   Bug fixes:
   - several bugs in the tracker have been fixed, especially in 
   the multiframe correspondence model

   Changes:

   - to help diagnose tracker issues, and improve on the gain
   functions for multiframe correspondence, you can now click on
   an active (i.e. not yet complete) track in the track window,
   and see all of its points circled.  By clicking on a circle,
   you select it as the base from which to calculate gain as the
   cursor moves around the screen.  The time difference used to
   calculate the gain is modified to match that of the scan following
   the one containing the selected point.  
   i.e. if you select a point from 5 scans back, then the gain
   is calculated by pretending the cursor is 4 scans back.

   - the underlay plugin is much faster now, and only operates in
   "nice" mode.
 
   Outstanding issue: when using the tracker on the Artificial blips
   source, single-stepping will cause the tracker to miss scans.
   Workaround: record from an artificial blips source to a blipmovie,
   then use the tracker on that movie.

radR564 (in "alpha" directory)

   Bug fixes:

   - the genblips plugin's hooks were setting num.scans.to.learn = 0,
   even when that plugin was not the source of data; this caused
   processing to proceed without a learning phase.

   - end.run() was being called even when no sink existed, leading
   to an error on the console when hitting "STOP" in some cases.

   Changes: 

   - refactored code for rawarch plugin to make pieces more accessible
   to users wanting to write code to import binary data from arbitrary formats

radR558 (in "alpha" directory)

   Bug fixes:

   - the nearestneighbour tracker model was broken by a small recent
     change in the calling sequence for start.new.track(); it's now
     corrected
   
   - the raw archive reader/writer is much faster; I was using the R
     "raw" type instead of a "string" for storing the raw scan data,
     and surprisingly, R is much faster at un/serializing the latter
     than the former.

   New features:

   - the "genblips" plugin provides a new data source consisting of
     artificial blips from dynamically modelled targets and random
     noise For now, this is mainly useful for testing the tracker
     models, and all blips have the same size and shape.  This will
     eventually make sensible use of radar cross sections, and allow
     overlaying of modelled blips and noise on real datasets, for
     more realistic testing of both blip finding/identification and
     track construction.  Data generated by "genblips" can be saved
     as a blipmovie. 

   - for plugin "controls" windows, there is now a preliminary way to
     allow R expressions to be entered as parameters (in the genblips
     plugin, this allows the user to specify the target motion model).
	
radR547 (in "alpha" directory)

   Main new anti-feature:

   - radR now requires R version 2.4.0 or later; I will try to keep
   such upgrade requirements to a minimum, but there have been some
   changes since R 2.3.1 that are difficult to do without

   Main new features:

   - the tracker now uses file-backed biglist/bigframe s to save
   blips, scans and tracks to disk, quite apart from whether you 
   choose to write a .CSV file.

   - new multiframe correspondence tracker model

   - new rawarch plugin for reading/writing full raw data from scans

radR461 (in "alpha" directory)

   Bug fixes:
        - adding a BLIP hook no longer filters out all blips
	  (no plugins were using this hook any more, so this lay
	  undetected for some time - thanks Mike!)

        - splash message is now written to console instead of the
          plot window, avoiding a race that the message sometimes lost

        - when resizing the plot window in tk plotting mode, show (or
	  not) range rings and compass according to the current
	  switch settings

------------------------------------------------------------------------
r457 | (no author) | 2007-04-26 07:41:06 -0400 (Thu, 26 Apr 2007) | 4 lines

M    radR.conf.R
        - set restart.learning.after.stop to TRUE, since not restarting
        learning after a stop is likely only useful for testing



radR456 (in "alpha" directory)
   Usability fixes:

   - when you use the "X" button to close a radR window (except for 
     the plot window), that window is withdrawn so it no longer
     appears in the window manager's (e.g. Windows') window list

   - withdrawn windows can be restored from the "View" submenu
   
   - the pointerinfo window now behaves more cleanly as the cursor
     approaches the window border, and some flashing has been removed

   - when the pointerinfo window is anchored, dragging it or moving
     the cursor through it updates the window contents
   
   - when switching from dish to tbar antenna types, the angle
     for the dish is remembered

   - by default, the tracker plugin is disabled

   Bug fixes:

   - no longer crash when trying to open a Seascan .DAT file that
     Seascan (the program) already has open.  Unfortunately, you
     now just get an obscure message in the console window (error
     handling/reporting in radR needs overhaul)

     The fact that you can't read the archive while Seascan has it
     open is because Seascan locks out other (even read-only)
     processes from its current archive.  To use the archive from
     radR, do ONE of these things:

       - shut down Seascan

       - open a different archive within Seascan

       - from inside radR, connect to the Seascan server on the
       Tape Playback channel

   - missing or renamed antenna files no longer cause radR to crash
     on startup

   - parameter file saving under a user-specified file now works again

   - a bug was interrupting saving of radR's state on exit, so things
     like plugin enable/disable status were not saved

radR442 (in "alpha" directory)
   - pointerinfo window can be locked in place by unchecking
     a new button on its menu; the window can then be manually 
     repositioned by dragging with the left mouse button

   - fixed bug in palette editor when palette has (effectively)
     only one colour

   - udpated documentation (for wiki) on tracker, with diagrams
 
radR433 (in "alpha" directory)
   - extensive bug fixes to tracker infrastructure; previewing of tracks
     is much improved; ghost tracks are gone; single-blip-tracks are
     shown with a (configurable) symbol

   - fix to longstanding (since radR2XX?) bug with live radar; sources where
     is.seekable is FALSE were never resetting RSS$have.previewed.scan
     to FALSE, and hence never getting scans after the first  

radR420 (in "alpha" directory)

   - fixes some bugs in tracker infrastructure

   - first release of the (tiny) antenna plugin for keeping track
     of radar antenna features such as type ("dish" or "tbar"),
     beam angle above horizon, etc.

   - blips and tracks are now treated in 3d spatial coordinates, when
     antenna characteristics allow this (e.g. a dish pointing at an
     angle above the horizon).  Plugins such as saveblips,
     pointerinfo, and underlay should now be 3d-aware.  
 
radR415	(in "alpha" directory)
   - first release of a tracker plugin.  This has only a very dumb
     nearest-neighbour model for assembling tracks, but it includes
     pretty much all of the track supporting infrastructure.
     This is an "alpha" release for feedback on the interface and
     infrastructure functionality.  It is nearly useless for actually
     getting tracks except for very well behaved targets.
     This 

radR368
   - re-implemented the plot display in pure Tcl/Tk; the original platform-
     specific gui is still available because it is much faster for,
     e.g. viewing blip movies
     However, the Tcl/Tk plot display will allow simple addition of
     new items such as tracks, and (for linux) means running radR
     over a network now works (the unix platform gui code assumes the
     display is local, and is optimized for this).

   - last release before work on tracking begins
   

radR340

   - saveblips has been simplified and sped up by processing all blips
     from a scan at once; the DONE_SCAN hook calls rss.get.all.blips
     to get all blip data, then uses tapply to calculate statistics
     by blip, and write.table with append=TRUE to dump these to the
     blips file.  SAVEBLIPS no longer uses a BLIP hook.

   - bliptrails now leaves scan.mat, class.mat, and score.mat alone.
     (scan preview when pausing was causing bliptrail-modified
     matrices to be included in stats updats, which is bad)
     The golden rule: scan conversion does not affect data is followed
     (again).

   - simplified logic: storage-tube and last-N-scans now differ
     only in what you'd expect: whether they store all scans so far
     or just the last n.  Also, storage tube trails do not get faded.  Sample
     data are always retained, whether faded or not.
     Trails are painted in a layer above the background (see underlay,
     below) but under the 3 samples classes.  It "pretends" to have
     class "other", so turning off that display hides the trails (but
     doesn't prevent them from being generated internally; disable
     the bliptrails plugin to do that).
    
   - underlay plugin; can read a gif (with transparency info) and
     scale/rotate/pan to display as background (you need to turn off
     display of e.g. cold pixels to see it) Entirely in R, so not very
     fast, especially in "nice" mode; works fastest if most of the gif
     is transparent (i.e.  has alpha channel < 128) as might be the
     case in an outline map.  Piggybacks on tcltk's gif/PPM file
     reader.
     
     
radR320

    Mainly refinements to the bliptrails plugin

    - I hadn't thought about what to do with the blip trails if 
      you manually moved through an archive. It seems natural to
      just retain each blip no matter where (in time) the next scan
      is.  That way, if you mousewheel back and forth, the trails will too.
      So a blip just behaves like a (possibly fading) crayon.

    - There are new controls to change the number of scans retained
      and the tail fadeout.  (Any other plugin can now take advantage of
      the same mechanism for adding a GUI control for a parameter.)

    - There was a bug: only (N-1), rather than N scans were
      effectively being stored.  Now you should see N "shadows"
      for each blip.  (N is something you can now set; see above)

    - Just to be clear on the sample values you see if you point the
      cursor at blip trails: retained blip data is "faded out" with
      each scan.  If you set the fadeout to 1.0, you preserve the
      blip data until N scans have elapsed, otherwise, the values
      get smaller and smaller.  

      If you want to see the unfaded raw sample values for the last
      N scans, you need to do this:
        - use "last N scans" mode
        - check "raw sample data" under "Old blip data to retain"
        - set fadeout = 1.0  

      Tail data are painted in from oldest to newest, so that if
      former positions of blips from different scans overlap, the
      data are for the more recent blip.  If positions of different
      blips from the same scan overlap, there is no way to know which
      blip the data are for.

   - this was just a first attempt at the bliptrails plugin.  If you
     don't like something about how it works, let me know.

radR314

    - another bugfix which would cause problems when switching between
    archives if the archive switched to had blips in the first scan
    (e.g. a blipmovie recorded from part of another blipmovie)
    
    - bliptrails plugin: this provides two ways of leaving trails
    behind blips.  This is going to be useful for working on tracking,
    and was a nice test of the infrastructure, exposing a couple of
    important bugs in other areas.

    In "last N scans", the locations of old blips
    are painted in class "other", either using actual sample data
    or bogus sample data (the latter is faster, the former lets you
    use the pointer to read sample values from old blips).  You can
    also retain scores for the old blips (the only purpose of which
    is to let you look at the score values with the pointer).
    Retaining sample and/or score data slows down the plugin,
    although even with no blip filtering in the worst of Carolyn's
    rain archive, it still chugs along faster than real time.
    Retaining sample data gives more "realistic" trails, since
    their intensity matches that of the original blip.

    There are two control parameters that I haven't made a GUI for
    yet, and they are in the plugins/bliptrails.conf.R file:

    - num.scans.kept: how many scans are blips retained for 
      (i.e. what is "N" in "last N scans")

    - fadeout: how quickly do old blips fade in brightness.  Old blip
      data is multiplied by this number on each scan, so if it is
      1.0, old blips don't fade (but they still disappear after N
      scans).  In practice, 0.8 seems to be a nice effect, but you can
      change it.  For craziness, you can use a number > 1.
      The gamma for the "Other" palette also affects how well trails
      show up.  There are some automatic attempts at reasonable 
      values by the GUI, but you still have full control.

    In "storage tube" mode, the locations of old blips are painted
    into the background, so you need to turn off "cold blips" plotting
    to see them.  Old blips are saved indefinitely (but using a fixed
    amount of memory regardless of how many blips there are), but
    displayed in only one colour (which you can choose by editing
    the singlecolour palette for the "Other" class).  This is fast,
    but the paint gets pretty thick pretty fast.  You can clear it
    from the plugin menu.  There is no fadeout in this mode.

radR306 ("Bugfixes In Honour Of The Canal Opening For Skating" edition)

   - a problem with saveblips that was messing up the first scan's
     worth of data has been fixed. (It only happened if you enabled
     the saveblips plugin *after* opening an archive).

   - a problem with the blipmovie archive reader has been fixed.  It
     only caused problems if you turned blip filtering on and off
     while playing a blip movie.

   - some problems with re-saving configuration files has been fixed.
   (in particular, mouse and keyboard event bindings were not being
   properly rewritten because of multiple bindings to the same window
   tag.  The gui.bindings.conf.R file format has been slightly
   reworked to get around this issue.
  
   Both these bugs were introduced in the last week or two.  It turns
   out that CS_060914_birdsAfterRain blipmovie is fine.  My apologies
   for the hassles there!
   
   - I've reorganized the plugin menus back into their original
     format.  The infrastructure is still there for adding fancily-
     labelled top-level menus, but for many purposes, those are
     probably just a hassle.

   - there's a rudimentary help option that sends your browser in
     search of radR resources
   
   - Alt-Up, Alt-Down and their combinations with Shift and Control
     now generate mousewheel events regardless of what window has
     focus.  You can use these if your mouse is "legs-only".
     Mousewheel events are always sent to the control under the
     cursor, regardless of window focus.
   
radR287

   - palettes are now 1024 (or higher, if you ask me) colours each
     (instead of 64) and more customizable, which lets you set up colour
     gradients to better reveal detail in blips etc. on the screen
     
   - when viewing data in scores mode, you need to select a sensible
     palette, of which, so far, there is only "scoreredblue".  Scores
     are signed quantities (i.e. can be negative) unlike raw samples,
     and so a "signed" palette is required to view them.  You can
     create more signed palettes by copying the file
     palettes/scoreredblue.palette.R to palettes/WHATEVER.palette.R
     (WHATEVER will be the name you see inside radR), and then editing
     inside radR and saving configuration when you quit.  The only
     difference between signed and unsigned palette files is the
     parameter "signed", which is TRUE for the former.
     
   - scan conversion has been made more sensible.  For each class
     of sample (cold, hot, blip, ...(coming soon)), you can 
     select whether to display those samples (you get background
     colour if not), what palette to use, and what gamma to use.
     This is useful in its own right and will make it easy to 
     add an underlay plugin for showing local geography or whatever.
     You can change the background colour from its default black
     to whatever by changing pix.mat.background in radR.conf.R
     (there will be an onscreen control for this at some point).

   - mousewheel handling bugs have been fixed:  whatever you point the
     mousewheel at, something sensible should happen there.
     (Mock-mousewheel keyboard strokes like Alt+Up still only work in
     the plot window; I'll add them to the palette editor, which is
     the only other place I can think of where there isn't already
     a keyboard way of getting the functionality)
 
   - windows screen update should be smoother and faster.

radR255:

   NEW DOWNLOAD LOCATION:  http://discovery.acadiau.ca/radR/
   (this can be reached from the wiki)
   You can add files to this directory by placing them in the folder
   /home/www/html/radR on discovery.
   I have moved Phil's blipmovies folder there, and added a blipmovie
   of the March 7 2006 roof archive (with scores).

   - blipmovies with scores: new blipmovies will have a fourth file,
     WHATEVER.scr, which holds scores.  If the source from which the
     blipmovie is recorded has scores, they are saved there.  If not,
     the file will be filled with zeroes.  If a blipmovie without a
     .scr file is opened (e.g. an existing blipmovie), then scores are
     not available.  This multi-file format for blipmovies is both
     annoying and space-wasting, but it is easy to read values from
     R (wiki entry on that coming soon).  You can always zip the 3 or 4
     files into an .ZIP archive if you want to email it to someone
     (they'll have to unzip it to use it, of course).

   - excluding blips from stats updates.  Theory: the cell mean and
     deviance are supposed to characterize the "background"
     reflectivity of a region in the scan.  If samples are designated
     part of a "blip", they are not background, and their values
     should not be used in updating stats for the cell (including them
     will bias upward the estimates of cell mean and deviance, thereby
     reducing the probability of the blip sample being identified as
     such on subsequent scans).  Up until now, radR didn't exclude
     blip samples from stats updates, but instead, provided the
     "cold_score_threshold" as a way to work around this bias: samples
     that were in "blips" in the previous scan would be treated as hot
     samples (and so eligible to be parts of blips) if their scores
     exceeded the "cold_score_threshold", which was lower than the
     hot_score_threshold. This is murky and still biases the cell
     mean up so that blips tended to flicker in and out of existence
     if they didn't move too quickly.

     The new (optional, but on by default) approach is to actually do
     the right thing and exclude blip samples from updating stats
     cells (in particular, if all of a cell's samples are blip
     samples, the stats for that cell don't change on that scan).
     
     There is a GUI checkbox for this option, called 
     "exclude blips from stats update", which is on by default.
     
     There is still a coldscore threshold and it is still used.
     To force it not be used (so that only the "exclude blips"
     approach is taken), set the coldscore to a value at least
     as big as the hot score, in which case it has no effect.
     
     I will soon remove the coldscore threshold feature, as it is both
     hard to explain and a less natural and less correct approach to
     the problem.  Comments welcome.
     
   - getting data for all blips in a scan.  (wiki entry coming soon)
     The new function rss.get.all.blips() returns a 3 x n matrix of
     all sample coordinates for all blips in the current scan.
     Column 1 is blip #, column 2 is sample #, column 3 is pulse #.
     To get the raw data for all blips, you can do this:

     ab <- rss.get.all.blips()
     x  <- RSS$scan.mat[ab]   ## a k x 2 matrix: column 1 is blip #,
                              ## column 2 is raw sample value
     sc <- RSS$score.mat[ab]  ## all zeroes if the current source is
                              ## an old blipmovie; otherwise, column 1
                              ## is blip #, column 2 is score

     Note that scores are stored as integers and must be divided by
     1024 to get actual z-score.

     To get the data for just blip 5, say:

     x  <- RSS$scan.mat [ab[ab[,1] == 5], 2:3]
     sc <- RSS$score.mat[ab[ab[,1] == 5], 2:3]

     This first grabs the submatrix of ab that has coordinates of blip
     #5, then uses that to index the scan and score matrices.
     Indexing of scan and score matrices uses the last two columns of
     the matrix you pass to it (e.g. ab), and carries over any other
     columns as labels.  If there are no other columns, the data is

radR248:
   - don't create empty runs in blipmovies; if an existing blipmovie
   is opened and its last run is empty, it is dropped from the table
   of contents; if it is opened as a sink, the empty run is
   overwritten by the next recording session.
   Note: if an existing blipmovie archive has an empty run that is not its last run,
   the empty run still shows up in the table of contents, unfortunately.
  
radR246:
   - preliminary support for ungated seascan archives; no directory is
     available, but they can be played through

   - play/pause/play-one logic cleaned up
  

radR241
   FIXED: - minor bug in unpacking 12-bit data from seascan archives
          (only affected samples 2, 4, and 6 in pulse 1 of each scan,
          but still a bit embarassing)
	  
	  - radR_X_Y_Z.bat icons should now work again for launching
	  radR directly; choose the one corresponding to the version
	  of R you want to use (2.3.1 and 2.4.0 tested and work; 2.3.0
	  may require a rebuild by me - let me know if you want it)

	  - apparently radR236 was not running on some platforms, for
            reasons unknown.  After the meetings this week, I will 
	    put in a more thorough testing system for vetting new
	    versions of radR before upload

	  - reading an archive via SeaScan didn't switch between different scan
            runs

radR236
   FIXED: checkbox handling was broken in places like the plugin enable/disable

radR235
   FIXED: writing blip movies doesn't crash radR on exit
   FIXED: playing to the end of a file doesn't quit the event loop

3. FIXED: The seascan plugin isn't accessible.  That will be fixed very
   shortly (it just doesn't have menu entries).

======================================================================
   Outstanding Issues:

1. The runradRxxx.bat radRxxx.exe are broken (at least on my machine -
   if you can actually get radR to run by double clicking on the radR
   icon, let me know!).  You will have to run Rgui (or Rterm from the
   command line) and then manually change directory and
   source("radR.R").  To save a step, you can create a desktop
   shortcut to Rgui that starts in the radR directory.  (Copy the "R"
   icon, right click and choose properties; select the "Shortcut"
   panel, then change the "Start in:" field to the appropriate
   folder.)

2. You need to have R version 2.3.1 or later.  I can rebuild for R
   2.3.0 if anyone needs it.  I have tested on R 2.4.0 and it also
   works.

4. After hitting "Ok" on the dialog for choosing a file for
   seascanarch or blipmoviearch, a mysterious invisible window with
   title "tk" has focus.  Clicking on any other radR window causes
   the mystery window to be destroyed.  While the mystery window exists
   (which you can see from the menu bar or Alt-TAB window switcher),
   mousewheel events over radR windows are ignored.

5. Using the mousewheel (say over the plot window) when the console
   has focus scrolls the console as well as manipulating the window
   under the mouse.

======================================================================
   Usage Notes:

2. mouse manipulation of compass and range rings: 
   
   - holding Shift while dragging with the left mouse button drags the range rings (i.e. sets
   their spacing); you can just do Shift + left Click to move the closest ring to where
   the mouse is.

   - holding Control while dragging with the left mouse button sets
   the compass ring radius; you can just do Control + left Click to
   set its location to where the mouse is


1. graphics controls can be customized in the file
   gui.bindings.conf.R, although this can be a bit tricky.  

   Some key controls:

      - as long as some radR window has focus, mousewheel events are
      redirected to the underlying control, whether or not it has
      focus.  So if the mouse is over the slider bar in the player,
      the wheel will cause the scan to step forward or backward

      - in the plot window, Ctrl or Shift + mousewheel causes plot rotation
        by a large or small amount.

      - in the plot window only, Alt-keys can be used to get
        mousewheel functions if your mouse doesn't have a wheel:
 
         Alt+Up/Down:  zoom in/out
	 Alt+Shift+Up/Down:  rotate plot by a single azimuth
         Alt+Ctrl+Up/Down:   rotate plot by multiple azimuths

      - middle mouse button (or the key "=" ) in the plot window
        restores the plot to default zoom, pan, and rotation

      - each mousewheel "click" on the Play-1 button advances

