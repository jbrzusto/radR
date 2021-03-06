## svn $Id: gui.conf.R 663 2010-10-08 17:51:54Z john $
##
##  GUI CUSTOMIZATION FILE
##
##  These are parameters for the radR GUI.
##
##  DO NOT EDIT THIS FILE WHILE RUNNING radR, because
##  it may be overwritten with new values by radR.
##
##  Lines beginning with "#" and blank lines are preserved
##  but comments at the ends of lines are not.
##
##  Make sure to include a comma at the end of each item
##  in a list.
##
##  NOTE:  all elements of lists must be given names
##         e.g.   list(x=1, b=2) instead of list(1, 2)
##

## Should the GUI be run on startup of radR?
## Set enabled = FALSE to disable the GUI.

enabled                          = TRUE

## The minimum increment of the mousewheel, as reported by
## the delta parameter of the tcl <MouseWheel> event

mousewheel.step                  = 120

## A small plot rotation rotates the plot by one pulse.
## A big plot rotation rotates the plot by this number of pulses:

plot.big.rotation.increment      = 10

## Currently mapped windows.
## Each item of the list gives the window's name, title, geometry, and state
## where:
##    - name is the Tcl/Tk window name, and begins with a '.'
##    - geometry given by "WIDTHxHEIGHT+LEFT+TOP"
##      e.g. 400x200+10+0 means a window of width
##      400, height 200 positioned so its upper left corner is 10 pixels to
##      the right of the upper left corner of the screen
##    - state is "normal" for open and visible, "iconic" for shrunken
##    - z is the window stacking order, with 1 being the highest; only the
##      ordering matters
##
## This list will be added to if windows are open when radR quits and the
## user has chosen to save configuration.

windows = list (
  .play = list (
    title = "player",
    geometry = "787x80+231+652",
    state = "normal",
    z = 1
    ),
  .blip = list (
    title = "blip processing",
    geometry = "252x536+0+292",
    state = "normal",
    z = 2
    ),
  .pctl = list (
    title = "display options",
    geometry = "528x295+232+420",
    state = "normal",
    z = 3
    ),
  .cons = list (
    title = "console",
    geometry = "500x500+492+227",
    state = "normal",
    z = 4
    ),
  .plot = list (
    title = "plot",
    geometry = "1014x535+0+-4",
    state = "normal",
    z = 5
    )
  )

## the following is the window which is created when
## the command menu is torn off.  The command menu pops up after
## a right click in the plot window.

win.cmd = list (
  title                          = "radR menu"
  )

## How the date appears in the playlist.
playlist.date.format             = "%d %b %Y  %H:%M:%S"

## How the data appears in the plot window title
plot.title.date.format           = "%Y %b %d %H:%M:%OS1"

## The initial screen resolution, in metres per pixel
## along the horizontal axis.

mpp                         = 5

## The default zoom, in metres per pixels

default.mpp                 = 5

## The factor by which a single zoom step changes mpp
## (divied for zooming in, multiplied for zooming out)

zoom.factor                = 1.25

## where the centre of the plot goes, in window coordinates

plot.origin                      = c(507, 267.5)

## what direction (in degrees) is north on the plot?  0 means up
## 90 means right, and so on.

north.angle                      = 0
default.north.angle              = 0

## Rotations are done by integral numbers of pulses, but in the
## absence of scan data, we need to specify a rotation step, in degrees

default.rotation.step            = 0.5

## should scans be plotted at all?

plot.enabled                     = TRUE

## number of range rings
num.range.rings                  = 15

## spacing of range rings, in metres

range.ring.spacing               = 500

## should range rings be plotted?

range.rings.enabled              = TRUE

## colour of range rings:

range.ring.colour                = "#00FF00"

## range ring scale (in metres)

range.ring.scale.max             = 100000

## range ring scale increment (in metres)

range.ring.scale.increment       = 1

## should a compass be plotted?

compass.enabled                  = TRUE

## the length of ticks for the compass (in pixels)

compass.tick.length              = 5

## the colour of the compass ticks and text

compass.colour                   = "#ffffff"

## the radius of the compass ring (in pixels)

compass.radius                   = 400

## the default compass radius

compass.default.radius           = 400

## max compass radius (in pixels)

compass.scale.max                = 32767

## compass scale increment (in pixels)

compass.scale.increment          = 1

## the possible angular spacings of compass labels, in degrees
## As the compass is zoomed, angular spacing of labels are reduced
## from right to left in this vector.

compass.angular.spacing = c(1, 2, 5, 10, 20, 30, 45, 90)

## how quickly the spinner box for selecting noise level changes:
noise.cutoff.spinner.increment   = 10

## how quickly the spinner boxes for selecting learning scans changes

learning.scans.spinner.increment = 1

## how quickly the spinner boxes for selecting these k values

stats.k.spinner.increment        = 0.01

## The hotscore and coldscore values can be changed by graphical
## spinners with these increments:

hotscore.spinner.increment       = 0.02
coldscore.spinner.increment      = 0.02

## The minblip and maxblip values can be changed by graphical
## spinners with these increments:

minblip.spinner.increment        = 100
maxblip.spinner.increment        = 100

## The minsamples and maxsamples values can be changed by graphical
## spinners with these increments:

minsamples.spinner.increment     = 1
maxsamples.spinner.increment     = 10

## The minangles and maxangles values can be changed by graphical
## spinners with these increments:

minangles.spinner.increment      = 1
maxangles.spinner.increment      = 10

## The minranges and maxsranges values can be changed by graphical
## spinners with these increments:

minranges.spinner.increment      = 1
maxranges.spinner.increment      = 10

## the gamma controls for the class palettes can have different
## default increments; the smaller the value, the more fine the mouse
## control over palette gamma (although the user can specify any
## value by typing it in, of course).

class.palette.gamma.increment = list (
  cold  = 0.05,
  hot   = 0.05,
  blip  = 0.02,
  other = 0.1,
  excluded = 0.1
  )

## The common background colour of text widgets and spinboxes.

entry.field.background.colour = "white"

## When editing values of controls such as hot_score by hand
## we want the user to see whether the changed value has been registered.
## When the first change is made, the background of the control text is set
## to the following colour, until the user registers the change by hitting
## Enter, or leaving the entry widget.

entry.field.changed.colour  = "yellow2"

## To avoid pop-up boxes for errors, set the following flag to true.

errors.logged.to.console    = TRUE

## Text styles for console input, output, and errors

console.style = list (
  prompt       = "-foreground black -background #c0ffff",
  input        = "-foreground black",
  output       = "-foreground blue",
  error        = "-foreground red",
  traceback    = "-foreground darkgreen",
  splash       = "-foreground #309020"
  )

## The console prompt string
console.prompt.string       = "> "

## The console line width
console.line.width          = 60

## The maximum number of lines to maintain in the GUI console history

console.history.max.length  = 500

## The GUI console history itself

console.history = c()

## The colour for marking the record button as active

record.button.active.colour = "red"

## Ensure that recording mode is turned on after a sink
## (e.g. blipmoviearchive) has been chosen?

press.record.after.choosing.sink = TRUE

## Ensure that recording mode is turned off after
## "stop" has been pressed?

release.record.after.stopping = TRUE

## The background colour for menus

menu.background = "#e0e0e0"

## The background colour for headings in menus

menu.heading.background = "#ffffff"

## The foreground colour for headings in menus

menu.heading.foreground = "#000000"

## Default (cold sample) palettes for each plot data source

default.cold.palette.for.source = list (
  score.mat = "scoreredblue"
  )

## with what should elements of the clipboard contents
## be separated?  "\t" is a TAB, which will make the contents fit in one
## row

clipboard.element.separator = "\t"

## what should the clipboard contents end with?  "\n" means a new-line

clipboard.element.terminator = "\n"

## is the plot window a pure tk canvas + image?

plot.is.tk.image = FALSE

## is the popup info window fixed and draggable, or does
## it automatically follow the mouse?

info.window.follows.mouse = FALSE

## coordinates of the top left corner of the popup info window,
## for when info.window.follows.mouse = FALSE

info.window.coords = c(0, 0)

## the name of a temporary file to use for creating .PNG plots

png.plot.filename = "gui/tmp_graph.png"

## the colour of the line to draw when the user is locating a line on the image

locate.line.colour = "#ffffff"

## should a plugin's menu be displayed when it is manually loaded?

show.plugin.menu.on.manual.load = TRUE

## the folder in which to look for R scripts is initially this:

default.scripts.folder = "./scripts"
