######################################################################
###
### saveblips.plugin.radR
###
### a radR plugin for collecting, saving, and plotting blips
###
### a short machine-readable description of this plugin is available
### in the file saveblips.desc.R
###
######################################################################

about = function() {
    if (num.blips > 0) {
        first <- first.timestamp
        class(first) <- "POSIXct"
        first <- paste("\nEarliest blip from scan: ",
                       format(first, format=GUI$plot.title.date.format, tz=RSS$timezone, usetz=TRUE))
        last <- last.timestamp
        class(last) <- "POSIXct"
        last <- paste( "\nLatest blip from scan:   ",
                      format(last, format=GUI$plot.title.date.format, tz=RSS$timezone, usetz=TRUE))
    } else {
        first <- last <- ""
    }
    return(plugin.label %:% "\nVersion " %:% version %:% "\n" %:%
           ifelse(new.file, "\nWriting new file.", "\nAppending to existing file.") %:%
           "\nCurrent output file is " %:% ifelse(length(blip.filename) > 0, blip.filename, "not defined") %:%
           "\nNumber of blips written:  " %:% num.blips %:%
           first %:% last %:%
           ifelse(have.file.data, "", "\n**File data has not been read; timestamps and count apply to the current run.") %:%
           "\n\nCurrently " %:% ifelse(enabled, "", "not ") %:% "saving blips." %:%
           "\n")

}

get.menus = function() {
    list(
        plugin = list(
            list(type="choose.any",
                 on.set = function(which, enable) {
                     switch(which,
                     { ## 1:
                         plot.on.pause <<- enable
                     },
                     { ## 2:
                         plot.range.rings <<- enable
                         if (have.file.data)
                             do.plot(force=TRUE)
                     }
                     )
                 },
                 "Plot a graph of blips when paused" = plot.on.pause,
                 "Show range rings on plot" = plot.range.rings
                 ),

            "Choose output file..." = list(
                "file",
                type="save",
                title = "Choose a file for saving blips",
                init.file = function() blip.filename,
                init.dir = function() blip.filename,
                file.types = list(".sqlite" = "SQLite files", ".*" = "All files"),
                on.set = function(f) {
                    open.blip.file(f)
                }),

            "Plot now" = function () {
                do.plot(force=TRUE)
            },

            "Hide blips from scans before this one" = function() {
                plot.min.timestamp <<- RSS$scan.info$timestamp
                do.plot()
            },

            "Hide blips from scans after this one" = function() {
                plot.max.timestamp <<- RSS$scan.info$timestamp
                do.plot()
            },

            "Show all blips" = function() {
                plot.min.timestamp <<- NULL
                plot.max.timestamp <<- NULL
                do.plot()
            },

            "---",

            "Delete all blips from current file" = function() {
                graphics.off()
                gui.popup.dialog("Confirm deletion",
                                 "Delete all blips from file " %:% blip.filename %:% " ?",
                                 function(button) {
                                     if (button == 1) {
                                         delete.blips()
                                         do.plot()
                                     }
                                 },
                                 buttons = c("Yes", "No"),
                                 default = 2)
            }
        )
    )
}

## open the blip file when plugin gets enabled,
## close it when plugin gets disabled

enable = function(enab) {
    enabled <<- enab
    if (!enabled) {
        close.blip.file()
        ## disable all hooks
        for (hook in c("ONPLAY", "ONPAUSE", "ONSTOP", "DONE_SCAN"))
            rss.disable.hook(hook, name)
    } else {
        ## enable the hooks that control the DONE_SCAN hook
        for (hook in c("ONPLAY", "ONPAUSE", "ONSTOP"))
            rss.enable.hook(hook, name)

        ## in case the user enables the plugin while we are already playing
        ## call the ONPLAY hook
        if (RSS$play.state >= RSS$PS$PLAYING)
            hooks$ONPLAY$f()
    }
}

## try to open blip file for output

open.blip.file = function(f) {
    close.blip.file()

    blip.filename <<- f

    ## if the file doesn't exist, we'll need to create the table
    new.file <<- !file.exists(f)
    con <<- dbConnect(RSQLite::SQLite(), f)

    if (!is.null(con)) {
        for (td in table.defs) {
            dbExecute(con, td)
        }
        if (new.file) {
            ## new file, so reset the blip count
            RSS$total.num.blips <- 0
            have.file.data <<- TRUE ## effectively, since there are none to read
        } else {
            have.file.data <<- FALSE
        }
        if (!enabled)
            rss.enable.plugin("saveblips")
    } else {
        gui.error(paste("Unable to open file", f))
    }
    return()
}

## close the current output file

close.blip.file = function () {
    if (!is.null(con))
        dbDisconnect(con)
    con <<- NULL
    reset.file.parms()
}

reset.file.parms = function () {
    num.blips <<- 0
    first.timestamp <<- Inf
    last.timestamp <<- -Inf
    have.file.data <<- FALSE
}

delete.blips = function() {
    close.blip.file()
    try(file.remove(blip.filename), silent=TRUE)
    plot.min.timestamp <<- NULL
    plot.max.timestamp <<- NULL
    have.file.data <<- TRUE ## effectively, since there are none to read
}

## read blips from the current blip file,
## respecting plot.min/max.timestamp where not NULL
## Set file stats and returning the blip data on success,
## NULL on failure

read.blips = function() {
    x <- NULL
    if (is.null(con))
        open.blip.file(blip.filename)
    query = "select * from blips"
    where = FALSE
    if (! is.null(plot.min.timestamp)) {
        query = paste0(query, sprintf(" where t >= %.3f", as.numeric(plot.min.timestamp)))
        where = TRUE
    }
    if (! is.null(plot.max.timestamp)) {
        query = paste0(query, ifelse(where, " and ", " where "), sprintf(" where t <= %.3f", as.numeric(plot.max.timestamp)))
        where = TRUE
    }
    try(x <- dbGetQuery(con, query), silent=TRUE)
    if (is.null(x))
        return(NULL)
    class(x$t) <- "POSIXct"
    if ((num.blips <<- dim(x)[1]) > 0) {
        first.timestamp <<- min(x$t)
        last.timestamp <<- max(x$t)
    }
    have.file.data <<- TRUE
    return(x)
}


load = function() {
    ## load the graphics library, which we use for plotting
    library("graphics")

    library("RSQLite")
    ## initialize state variables here
    reset.file.parms()
}

unload = function(save.config) {
    close.blip.file()
}

do.plot = function(force=FALSE) {
    ## fixme: allow more of the plot to be seen
    ## perhaps mirroring the plot window geometry

    if (is.null(blip.filename))
        return ()

    close.blip.file()

    if (!plot.on.pause && !force)
        return()

    ## read the file, and get the number of blips and first/last timestamps

    if (is.null(x <- read.blips())) {
        if (dev.cur() != 1)
            dev.off()
        return()
    }

    if (dev.cur() == 1)
        ## enable the platform-appropriate graphics driver
        switch(.Platform$OS.type,
               windows=windows(),
               unix=x11()
               )

    ## plot the points
    plot(x$x, x$y, xlim=c(-2000, 2000), ylim=c(-2000, 2000),
         cex=x$int * 10, pch=".", xlab="east", ylab="north")
    if (plot.range.rings)
        symbols(x=rep(0,10), y=rep(0,10), circles=GUI$range.ring.spacing*(1:10),
                inches=FALSE, fg="green", add=TRUE)

}

hooks = list (

    DONE_SCAN = list (enabled = FALSE, read.only = TRUE,
                      f = function(...) {

                          ## save info for all blips from this scan
                          ## get a summary of all blips

                          if (length(RSS$blips) == 0)
                              return()

                          dbWriteTable(con, "blips", RSS$patches[RSS$blips, c("t", "x", "y", "z", "ns", "area", "int", "max", "aspan", "rspan", "perim")], append=TRUE)

                          ## keep track of what's been recorded

                          ts <- as.numeric(RSS$scan.info$timestamp)
                          first.timestamp <<- min(first.timestamp, ts)
                          last.timestamp <<- max(last.timestamp, ts)
                          num.blips <<- num.blips + length(RSS$blips)

                      }),

    ONPLAY = list (enabled = FALSE, read.only = TRUE,
                   f = function() {
                       ## if the plugin is disabled, don't do anything
                       if (!enabled)
                           return()

                       ## if there is no file open and there is a blip filename
                       ## then open it for appending

                       if (is.null(con) && !is.null(blip.filename))
                           open.blip.file(blip.filename)

                       ## enable the BLIP hook to avoid having to test for play mode
                       ## and the existence of an output file
                       ## inside the BLIP hook function itself

                       if (!is.null(con))
                           rss.enable.hook("DONE_SCAN", name)
                   }),

    ONPAUSE = list (enabled = FALSE, read.only = TRUE,
                    f = function() {
                        ## if the plugin is disabled, don't do anything
                        if (!enabled)
                            return()

                        ## We only want to save blips in PLAY mode,
                        ## not when they are processed in the PAUSED state.
                        ## (e.g. when a processing a preview scan)
                        rss.disable.hook("DONE_SCAN", name)
                        do.plot()
                    }),

    ONSTOP = list (enabled = FALSE, read.only = TRUE,
                   f = function() {
                       ## if the plugin is disabled, don't do anything
                       if (!enabled)
                           return()
                       ## We only want to save blips in PLAY mode,
                       ## not when they are processed in the STOPPED state.
                       ## (e.g. when a processing a preview scan)

                       rss.disable.hook("DONE_SCAN", name)
                       close.blip.file()
                   })

)  ## END OF HOOKS

## aditional state variables for this plugin:

con                = NULL
first.timestamp    = NULL
have.file.data     = NULL
last.timestamp     = NULL
new.file           = NULL
num.blips          = NULL
plot.max.timestamp = NULL
plot.min.timestamp = NULL
