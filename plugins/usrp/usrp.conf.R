## svn $Id$
##                                                         
##  Configuration for USRP plugin.
##  Read data from Ettus Research's USRP-1 USB board.
##                                                         
##  DO NOT EDIT THIS FILE WHILE RUNNING radR, because      
##  it is overwritten with new values when you exit        
##  radR.                                                  

## is this plugin currently enabled when loaded?

enabled = TRUE 

## The following flag says that this plugin is enabled whenever
## it is loaded.  TRUE here makes the "enabled" parameter redundant.

always.enabled = TRUE

## The path to the antenna database.  This is a directory of
## R-readable files, with filename extension ".antenna.R", each of
## which defines an antenna and its signal parameters.

antenna.db.path = "plugins/usrp/antenna_db"

## The name of the antenna to use with this plugin.
## The antenna file determines what signal parameters to use
## for obtaining video, trigger, ARP, ACP.

antenna = "Furuno_1954"

###
## Application-dependent digitization parameters.  These values override
## those in the antenna database.
###

## pulses to capture per sweep

n_pulses = 4096

## samples to capture per pulse; determines total range in conjunction
## with decim, below

n_samples = 1024

## sample decimation rate (determines range cell size)
## the number of samples to drop before each sample we keep;
## 0 means use every sample, 1 means use every 2nd sample, and so on
## This determines the range cell size:
##   range_cell_size = 2.344 m * (1 + decim)

decim = 1

## video gain (power amplification in db)

vid_gain = 0

## number of sweep buffers to maintain; larger numbers use more memory,
## but will be less likely to drop scans under temporarily heavy processing loads
## Realistically, 3 is the smallest number that should be used here.

n_bufs = 3

## filenames for the FPGA binary file and the firmware
## The paths for these are set by path.to.usrp.hw.files, defined in usrp.PLATFORM.conf.R,
## where PLATFORM is unix or windows

hw.filenames = c("usrp_bbprx.rbf", "std.ihx")

## front-end-board DAC values for A, B, C, D:
frontend.dac = c(0, 0, 0, 0)

## DANGER: if the following is FALSE, omit the start-up check for
## an LFRX daughterboard in the USRP

check_for_daughterboard = FALSE
