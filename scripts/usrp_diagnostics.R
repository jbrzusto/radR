## usrp_diagnostics.R:  generate plots of raw signal coming in on USRP lines
##
## Requires the "test_usrp_pulse_buffer" program in the plugins/usrp folder.
##
prog <- "plugins/usrp/test_usrp_bbprx"
temp <- "tmp.dat"

ok <- rss.gui("POPUP_DIALOG", "USRP Diagnostics",
              paste(
                    "This script will analyze raw data from the USRP.",
                    "Please ensure that:",
                    "   - you have loaded the usrp plugin",
                    "   - the usrp is NOT the current source",
                    "   - your radar is transmitting",
                    "\nDo you wish to proceed?",
                    sep="\n"),
              buttons = c("   Yes   ", "    No   "))

if (ok == 1) {

channel.map <- list(vid=1, trig=2, hdg=3, hd=3, arp=3, bp=4, azi=4, acp=4)

get.data <- function(channel.name, time, samples, extra="", datfile="temp_usrp.dat") {
  ## get data from a raw radar channel
  ##   - channel: one of the elements of names(channel.map) e.g. "azi"
  ##   - time: amount of time to sample for, in seconds
  ##   - samples: number of samples desired
  ##   - extra: other parameters to the test program
  ##   - datfile: place to store data before reading it in
  ##
  ## Returns a list with these items:
  ## x: the time offsets of the data samples, in seconds (double)
  ## y: the data samples (integer)

  decim <- round(64e6 / (samples / time)) - 1  ## required decimation rate
  channel <- channel.map[[channel.name]]
  if (is.null(channel))
    stop("bad channel name:")

  msg.id <- rss.gui("POPUP_MESSAGEBOX", paste("Getting", channel.name), paste("I'm acquiring", round(time, 4), "seconds of data from the", channel.name, "channel on the USRP\n extra options: ", extra))

  np <- ceiling(samples / 256) ## number of USB packets needed
  
  res <- system(paste(prog,
                      "-m ", channel, # which raw mode
                      "-d ", decim,   # decimation rate
                      "-P ", np,      # number of "pulses" (really, USB packets)
                      "-n ", 256,     # number of samples in a USB packet
                      "-F ", 512,     # size of fast USB block
                      "-N ", 1,       # number of fast USB blocks
                      extra,          # extra options from caller
                      datfile,        # name of data file
                      sep = " "),
                intern = TRUE)

  rss.gui("DELETE_MESSAGEBOX", msg.id)
                     
  return (list(x = ((1:samples) - 1) * (time / samples), y = readBin(datfile, integer(), size=2, n=samples)))
}

## get 3 msec of video data at 3 gain levels
vid <- list()
for (g in c(0, 10, 20)) {
  which <- paste("g", g, sep="")
  vid[[which]] <- get.data("vid", 0.003, 3072, paste("-g", g)) ## -g flag sets video gain
}

## get 3 msec of trigger data at 3 gain levels
trig <- list()
for (g in c(0, 10, 20)) {
  which <- paste("g", g, sep="")
  trig[[which]] <- get.data("trig", 0.003, 3072, paste("-G", g)) ## -G flag sets trigger gain
}

## get 50 msec of azimuth data
azi <- get.data("azi", 0.050, 2048)

## get 3 sec of heading data
hdg <- get.data("hdg", 3, 3072)


par(mfcol=c(2,2))

plot(vid$g0,
     xlab="time (s)",
     ylab="video (integer units)",
     ylim=c(0, 4095),
     main=c("Video signal", "gains of 0 (black), 10 (red), 20dB (blue)"),
     type="l",
     col="black"
     )
points(vid$g10, col="red", pch="+")
points(vid$g20, col="blue", pch="+", cex=0.5)
lines(vid$g0, col="black", lty=2)

plot(trig$g0,
     xlab="time (s)",
     ylab="trigger (integer units)",
     ylim=c(0, 2048),
     main=c("Trigger signal", "gains of 0 (black), 10 (red), 20dB (blue)"),
     type="b",
     col="black"
     )
points(trig$g10, col="red", pch="+")
points(trig$g20, col="blue", pch="+", cex=0.5)
lines(trig$g0, col="black", lty=2)  ## redo lines at low gain

plot(azi,
     xlab="time (s)",
     ylab="azimuth / bearing pulse / ACP (integer units)",
     ylim=c(0, 2048),
     main=c("Azimuth / bearing pulse / ACP"),
     type="l",
     col="black"
     )

plot(hdg,
     xlab="time (s)",
     ylab="Heading / SHM / ARP (integer units)",
     ylim=c(0, 2048),
     main=c("Heading / SHM / ARP"),
     type="l",
     col="black"
     )

usrp.diag <<- list(vid=vid, trig=trig, azi=azi, hdg=hdg)
save(list="usrp.diag", file="usrp_raw_data.RData")

bringToTop()

rss.gui("POPUP_DIALOG", "Finished",
        paste(
              "The data for this plot are stored in the global-level variable",
              "      usrp.diag",
              "which you can access from the radR console.",
              "Raw data has also been saved in the workspace:",
              "      usrp_raw_data.RData",
              "in the radR folder.", sep="\n"),
        buttons = c("   OK    "))

par(mfcol=c(1,1))
}
