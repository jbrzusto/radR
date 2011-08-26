######################################################################
###
### underlay.plugin.radR
###
### The underlay radR plugin, for displaying an image in the plot
### background.  The image can optionally be panned, zoomed, and
### rotated to follow these changes to the plot.
###
######################################################################

about = function() {
  rv <- "Display a background image anchored to the plot\n\n" %:% "Image file: " %:% image.file
  if (!is.null(image.mat)) {
    rv <- rv %:% "\nDimensions: " %:% paste(dim(image.mat), collapse=" x ")
    rv <- rv %:% "\nRadar Location : (" %:% paste(image.geom$centre, collapse = ", ") %:%
      ") pixels (right, down) from top left corner"
    rv <- rv %:% "\nScale: " %:% image.geom$pixel.size %:% " metres per (horizontal) pixel"
    rv <- rv %:% "\nBearing: 'Up' on image points " %:% image.geom$azimuth %:% " degrees clockwise from North.\n"
  } else {
    rv <- rv %:% "\nNo image data loaded.\n"
  }
  return(rv)
}

get.menus = function() {
  list(
       plugin = list(
         "Open an image file..." =
         list ("file",
               type = "open.one",
               title = "Choose a radR background image", 
               file.types = list(".png" = "PNG image", ".gif" = "GIF image", ".ppm" = "Portable Pixmap"),
               on.set = function(f) {load.image.file(f); update()},
               init.file = function() image.file
               ),
         "Reload current image file" = function() {load.image.file(image.file); update()},
         
         "Save geometry for this image" = save.geometry,
         list (xloc = "gauge",
               label = "radar X location (pixels from left of image)",
               range = c(-5000, 5000),
               increment = 10,
               value = image.geom$centre[1],
               on.set = function(x) { image.geom$centre[1] <<- x; update() },
               set.or.get = "set.centre.x"
               ),
         list (yloc = "gauge",
               label = "radar Y location (pixels from top of image)",
               range = c(-5000, 5000),
               increment = 10,
               value = image.geom$centre[2],
               on.set = function(x) { image.geom$centre[2] <<- x; update() },
               set.or.get = "set.centre.y"
               ),
         list (azi = "gauge",
               label = "bearing of image Y axis (degrees clockwise from North)",
               range = c(-360, 360),
               increment = 1,
               value = image.geom$azimuth,
               on.set = function(x) { image.geom$azimuth <<- x; update() },
               set.or.get = "set.azimuth"
               ),
         list (pixs = "gauge",
               label = "image pixel width (metres)",
               range = c(0, 1000),
               increment = 0.1,
               format = "%3.3f",
               value = image.geom$pixel.size,
               on.set = function(x) { image.geom$pixel.size <<- x; update() },
               set.or.get = "set.pixel.size"
               ),
         "---",
         "Set radar location by clicking on image..." = set.radar.location,
         "Set image scale by choosing a line of known length..." = set.image.scale,
         "---",
         "Image tracks changes in plot:",
         list(keep="choose.any",
              on.set = function(n, v) {
                image.geom$track[[n]] <<- v
              },
              set.or.get = "set.track",
              "zoom"     = image.geom$track$mpp,
              "pan"      = image.geom$track$plot.origin,
              "rotation" = image.geom$track$north.angle
              )
         )
       )
}

enable = function(enab) {
  if (enab) {
    remap <- FALSE
    if (is.null(image.mat)) {
      remap <- TRUE
      load.image.file(image.file)
    }
    rss.enable.hook("PAINT_BACKGROUND", name)
    rss.enable.hook("TK_PLOT_MODE", name)
    update(remap)
  } else {
    rss.disable.hook("TK_PLOT_MODE", name)
    rss.disable.hook("PAINT_BACKGROUND", name)
    update(remap=FALSE)
  }
}

load = function() {
  ## initialize state variables here
  rss.dyn.load("underlay", in.dir.of=plugin.file)
}

unload = function(save.config) {
  rss.dyn.unload("underlay")
}

update = function(remap = TRUE) {
  if (remap) image.mapper <<- NULL
  if (!silent.updates)
    rss.gui(UPDATE_PLOT_WINDOW, (reconv=TRUE))
}

synch.controls = function() {
  ## update the values in the GUI controls for
  ## image parameters.  Called when these are
  ## read from a new file.
  ## In case the plugin is enabled upon loading,
  ## we need to check whether the set.XXX functions
  ## have been created by the gui (they are created
  ## at GUI menu creation time).
  if (exists("set.centre.x")) {
    set.centre.x (image.geom$centre[1])
    set.centre.y (image.geom$centre[2])
    set.azimuth (image.geom$azimuth)
    set.pixel.size (image.geom$pixel.size)
    for (i in 1:3)
      set.track(i, image.geom$track[[i]])
  }
}

set.radar.location = function() {
  ## Let the user set the image's radar location by clicking on it
  
  if (is.null(rv <- rss.gui(LOCATE_POINT, title="Locate the radar",
                         "Click on the plot window.\n  Move the cursor to the radar location and hit Space, or hit Esc to cancel.",
                timeout=120)) || rv$key == "Escape")
    return()
  if (is.null(rv$coords) && !any(is.na(rv$coords))) {
    rss.gui(POPUP_MESSAGE, "Timed out", "The radar location was not set because you didn't choose a point.", time.to.live=20)
    return()
  }
  coords <- image.geom$centre + (rv$coords - GUI$plot.origin) * (GUI$mpp / image.geom$pixel.size)
  silent.updates <<- TRUE
  set.centre.x (coords[1])
  set.centre.y (coords[2])
  silent.updates <<- FALSE
  rss.gui(CENTRE_PLOT_AT_RADAR)
}

set.image.scale = function() {
  ## Let the user set the image's scale by providing a line of known length
  
  if (is.null(coords <- rss.gui(LOCATE_LINE, title="Set image scale",
                                msg="Choose a line of known length",
                                timeout=120)))
    return()
  pix.dist <- sqrt(sum((coords[1:2] - coords[3:4])^2))
  if (pix.dist == 0)
    return()
  rss.gui(CONSOLE_PRINT, coords)
  rv <- rss.gui(POPUP_DIALOG, title="What length?", msg="Enter the length of the line in km", entry=TRUE, buttons=c("Ok",
                                "Cancel"))
  rss.gui(CONSOLE_PRINT, rv)
  if (rv[[1]] == 1) {
    set.pixel.size (image.geom$pixel.size * (as.numeric(rv[[2]]) * 1000) / (pix.dist * GUI$mpp))
    rss.gui(CENTRE_PLOT_AT_RADAR)
  }
}
      

  
load.image.file = function(f) {
  image.file <<- f
  tryCatch(
           {
             rss.gui(POPUP_MESSAGEBOX, "Loading Underlay Image", "This may take a minute, as Tk unpacks the image into memory...",
  "underlay_loading")
             rss.gui(DELETE_MESSAGEBOX, "underlay_loading")
             tcl("image", "create", "photo", image.name, file=image.file)
             image.mat <<- extmat(type="int", dim=c(0, 0), name="underlay image")
             .Call("radR_attach_image_to_extmat", image.name, .Call("get_tcl_interp", tclint("winfo", "id", ".plot")), image.mat)
             if (!GUI$plot.is.tk.image) {
               rb.bytes.swapped <<- TRUE
               .Call("radR_image_swap_RB", image.mat)
             } else {
               rb.bytes.swapped <<- FALSE
             }
             opaque <<- c(image.mat[] < 0)
             geom.file <- f %:% geom.file.suffix
             ## if a geom file exists for this image,
             ## load the values from it and force a repaint
             if (file.exists(geom.file)) {
               image.geom <<- source(geom.file)$value
               if ("pps" %in% names(image.geom$track)) {
                 ## correct old nomenclature
                 names(image.geom$track) <<- c("mpp", "plot.origin", "north.angle")
               }
               synch.controls()
             }
             image.mapper <<- NULL
           },
           error = function(e) {
             image.mat <<- NULL
             image.mapper <<- NULL
             rss.gui(POPUP_MESSAGEBOX, "Underlay not found", "Unable to load image from file " %:% image.file, "no_underlay",
                     20)
           }
           )
}

save.geometry = function() {
  dput(image.geom, file=image.file %:% geom.file.suffix, control=NULL)
}

hooks = list(

  TK_PLOT_MODE = list( enabled = FALSE, read.only = FALSE,
    f = function(is.tk.mode, ...) {
      ## swap Red and Blue bytes in pixels when going between Tk and normal plotting mode
      if (is.tk.mode == rb.bytes.swapped) {
        .Call("radR_image_swap_RB", image.mat)
        rb.bytes.swapped <<- ! rb.bytes.swapped
      }
    }),
  
  PAINT_BACKGROUND = list( enabled = FALSE, read.only = TRUE,
    f = function(...) {
      if (is.null(image.mat))
        return(FALSE)
      get.new.mapper <- is.null(image.mapper)
      if (! all(dim(RSS$pix.mat) == pix.dims)) {
        pix.dims <<- dim(RSS$pix.mat)
        get.new.mapper <- TRUE
      }

      ## see if any relevant GUI parameters have changed since last time
      ## If so, we must regenerate the image.mapper
      
      for (n in names(last)) {
        if (is.null(last[[n]]) || (image.geom$track[[n]] && !identical(last[[n]], GUI[[n]]))) {
          last[[n]] <<- GUI[[n]]
          get.new.mapper <- TRUE
###.if $DEBUG
          print("New mapper because of " %:% n)
###.endif          
        }
      }
      
      if (get.new.mapper)
        image.mapper <<- calc.mapper(dim(image.mat),
                                     image.geom$centre,
                                     image.geom$azimuth,
                                     image.geom$pixel.size,
                                     pix.dims,
                                     last$plot.origin,
                                     last$north.angle,
                                     last$mpp)
      if (!is.null(image.mapper)) {
        
        ## if the image doesn't cover the whole plot window,
        ## paint in the background colour
        
        if (length(image.mapper$lhs) < prod(pix.dims))
          RSS$pix.mat[NOTRIGGER=TRUE] <- rss.tclcolour.to.rgbint(RSS$pix.mat.background, RSS$plot.is.tk.image)
        
        ## copy image pixels
        
        RSS$pix.mat[image.mapper$lhs, NOTRIGGER=TRUE] <- image.mat[image.mapper$rhs]
        
        return (TRUE)
      } else {
        return (FALSE)
      }
    })
  )
## end of hooks

rint = function(x) as.integer(1+floor(x))
rrint = function(x) as.integer(round(x))

calc.mapper = function(src.dim, src.origin, src.rotation, src.size, dst.dim, dst.origin, dst.rotation, dst.size)
  ## for each pixel location in a rectangle of dimension src.dim
  ## find the appropriate index into a matrix of dimension dst.dims
  ## (or vice versa, if current mode is "nice")
  ## given the origins, dimensions, rotations and block sizes of the two
  ## rectangles
  {
    rv <- .Call("calc_mapper_nice", src.dim, src.origin, src.rotation, src.size, dst.dim, dst.origin, dst.rotation, dst.size, opaque, PACKAGE="underlay")
    names(rv) <- c("lhs", "rhs")
  
    return(rv)
  }

## state variables

last = list (
  mpp         = NULL,
  plot.origin = NULL,
  north.angle = NULL
  )

last.planar.rps = 0

image.mat = NULL  ## NULL if no image data are loaded
pix.dims = c(0, 0)

image.mapper = NULL

## the internal tcl name to use for the loaded image
image.name = "::img::underlay"

## suffix for the image geometry file
geom.file.suffix = ".geom.R"

## vector of flags for each linear image index: is this pixel opaque?
opaque = NULL

## should control updates happen without updating the plot window?
silent.updates = FALSE

## has the current image had red and blue bytes swapped?
## This will generally be true, except for tcltk mode.
rb.bytes.swapped = FALSE
