######################################################################
###
### audio.plugin.R
###
### the audio radR plugin template.
###
### This plugin will be installed as AUDIO
###
######################################################################

about = function() {
  ## a function returning a description and the current state of the plugin.
  return("A simple plugin for selectively recording audio corresponding to each radar scan")
}


enable = function(enab) {
  enabled <<- enab
  for (n in names(hooks))
    rss.enable.hook(n, name, enab)
}

load = function() {
  rss.dyn.load("audio", in.dir.of=plugin.file)
  mci("close all")
}

unload = function(save.config) {
  stop.recording()
  rss.dyn.unload("audio")
}

get.menus = function() {
  list(
       plugin = list(
         "Choose output folder..." = list (
           "file",
           type = "open.dir",
           title = "Choose a folder for saving audio .WAV files",
           init.file = function() AUDIO$wav.folder,
           on.set = function(x) wav.folder <<- x
           ),
         "---",
         "Sampling rate (Hz):",
         c(list ("choose.one",
                 on.set = function(x) samples.per.second <<- samples.per.second.opts[x]),
           structure(samples.per.second.opts == samples.per.second, names=as.character(samples.per.second.opts))
           ),
         "Bits per sample:",
         c(list ("choose.one",
                 on.set = function(x) bps <<- bps.opts[x]),
           structure(bps.opts == bps, names=as.character(bps.opts))
           ),
         "Channels:",
         c(list ("choose.one",
                 on.set = function(x) num.channels <<- num.channels.opts[x]),
           structure(num.channels == num.channels.opts, names=as.character(num.channels.opts))
           ),
         "---",
         list ("string",
               label = "expression returning TRUE to save audio for current scan, FALSE to discard",
               width = 40,
               height = 3,
               value = as.character(save.filter),
               val.check = function(x) tryCatch({parse(text=x); TRUE}, error=function(e)FALSE),
               on.set = function(x) { save.filter <<- parse(text=x)}
               ),
         list ("string",
               label = "strftime-style formatting expression for .WAV file names",
               width = 40,
               height = 1,
               value = as.character(filename.format),
               val.check = function(x) tryCatch({format (Sys.time, x, tzone=RSS$timezone); TRUE}, error=function(e)FALSE),
               on.set = function(x) { filename.format <<- x}
               )
         )
       )
}


### This plugin uses Windows' mciSendString function to allow recording.
### These commands are sent using the R function mci(...), which accepts
### any parameters, and applies paste() to them to create space delimited strings
### which are sent to mciSendMessage.  The return value is the vector of strings returned
### by the call.  Errors are triggered as R level errors.
###
### mci command summary for waveaudio devices:
###

## "WNB" means "wait" "notify" or "both" are accepted

## capability
##         can eject
##         can play
##         can record
##         can save
##         compound device
##         device type
##         has audio
##         has video
##         inputs
##         outputs
##         uses files

## close   WNB

## delete
##         from position
##         to position

## info
##         file
##         input
##         output
##         product

## open
##         alias <device_alias>
##         buffer <buffer_size>
##         shareable
##         type <device_type>

## pause   WNB

## play
##         from position
##         to position

## record
##         from <position>
##         insert
##         overwrite
##         to <position>

## resume  WNB

## save
##         filename

## seek
##         to end
##         to position
##         to start

## set
##         alignment <integer>
##         any input
##         any output
##         audio all off
##         audio all on
##         audio left off
##         audio left on
##         audio right off
##         audio right on
##         bitspersample <bit_count>
##         bytespersec <byte_rate>
##         channels <channel_count>
##         door closed
##         door open
##         format tag pcm
##         format tag <tag>
##         input <integer>
##         output <integer>
##         samplespersec <integer>
##         time format bytes
##         time format milliseconds
##         time format samples

## status
##         alignment
##         bitspersample
##         bytespersec
##         channels
##         current track
##         format tag
##         input
##         length
##         length track <number>
##         level
##         media present
##         mode
##         number of tracks
##         output
##         position
##         position track <number>
##         ready
##         samplespersec
##         start position
##         time format

## stop    WNB

## sysinfo
##         installname 	
##         quantity 	
##         quantity open 	
##         name <index>
##         name <index> open

globals = list (
  ## call mciSendString with each string in a vector, after pasting components
  mci = function (...)
    tryCatch (.Call("mci_send_strings", paste(...)), error = function(e) warning(conditionMessage(e))),
    
  ## call mciSendString to set parameters from a list in the form (param1=value1, param2=value2, ...)
  mciset = function (handle, list) {
    for (i in seq(along=list))
      tryCatch (.Call("mci_send_strings", paste("set", handle, names(list)[i], list[[i]])),
                error = function(e) warning(conditionMessage(e)))
  }
  )

stop.recording = function() {
  ## stop any existing recording
  mci("close all")
  is.open <<- c(FALSE, FALSE)
}
  
restart.recording = function() {
  ## stop any existing recording and
  ## start recording on the first waveaudio instance
  
  stop.recording()
  for (i in 1:2) {
    mci("open new type waveaudio alias", alias[i], "buffer", rec.buff.size)
    mciset(alias[i],
           list(
                samplespersec = samples.per.second,
                bitspersample = bps,
                channels      = num.channels,
                "format tag"  = "pcm",
                "time format" = "milliseconds",
                "alignment"   = bps * num.channels / 8,
                "bytespersec" = bps * samples.per.second * num.channels / 8
                ))
    is.open[i] <<- TRUE
  }
  mci("record", alias[1], "from 0 to", max.audio.per.file)
  cur.rec <<- 1
}  

swap.and.save = function() {
  ## Save the current recording to disk while starting recording on the
  ## other waveaudio instance.
  ## We build the commands first and send them as a set
  ## to the C wrapper to minimize latency and loss of audio signal.

  file <- file.path(wav.folder, format(RSS$scan.info$timestamp, filename.format, timezone=RSS$timezone))
  rv <- mci(c(paste("stop",   alias[cur.rec]),
              paste("record", alias[3 - cur.rec]), ##, "from 0 to", max.audio.per.file),
              paste("status", alias[cur.rec], "length track 1"),
              paste("save",   alias[cur.rec], '"' %:% file %:% '"'),  ## no spaces inside the quotes!
              paste("seek",   alias[cur.rec], "to start"),
              paste("delete", alias[cur.rec])
        ))
  
  cur.rec <<- 3 - cur.rec
  print("Saved audio clip of length " %:% rv[3] %:% " to " %:% file)
}

drop.current = function() {
  ## Drop the current recording and restart at the beginning of the segment
  ## We don't swap waveaudio instances since there is no lag time due to saving.
  
  mci(c(paste("stop",   alias[cur.rec]),
        paste("seek",   alias[cur.rec], "to start"),
        paste("delete", alias[cur.rec]),
        paste("record", alias[cur.rec], "from 0 to", max.audio.per.file)
        ))
}  
  
hooks = list (
  ONPLAY = list (enabled=TRUE, read.only=TRUE,
    f = function(...) {
      restart.recording()
    }
    ),
  
  ONSTOP = list (enabled=TRUE, read.only=TRUE,
    f = function(...) {
      stop.recording()
    }
    ),

  DONE_SCAN = list (enabled=TRUE, read.only=TRUE,
    f = function(...) {
      if (RSS$play.state >= RSS$PS$PLAYING && !is.na(cur.rec)) {
        if (isTRUE(eval(save.filter)))
          swap.and.save()
        else
          drop.current()
      }
    }
    )
  )
         
### state variables

## aliases of the two wave device instances we'll use for recording.
## These are not windows names, but simply handles of our own choosing.
##
## We use two so that we minimize the time lost due to saving the recorded
## audio to a file.  
##
## The source data for recording by both instances is selected via Windows as so:
##
##   Control Panel | Sounds and Audio Devices | Audio | Sound recording Default device
##

alias = c("aud1", "aud2")

## flag whether each instance is open
 
is.open = c(FALSE, FALSE)

## which instance is currently recording; 1 or 2.  NA means neither

cur.rec = NA
