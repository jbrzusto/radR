## svn $Id$
##
## Part of the antenna database for the USRP-1 board.
##
## This file can be copied and customized for a different radar.
##

desc               = "Furuno FR1954/FR1964 C-BB attached through version 2.0 of LFRX-based interface"

## signal parameters; names must be members of usrp.param.names defined in usrp.plugin.R

vid_negate         = 0

trig_gain          = 0
trig_thresh_excite = 20
trig_thresh_relax  = 40
trig_latency       = 0
trig_delay         = 0

arp_gain           = 0
arp_thresh_excite  = 20
arp_thresh_relax   = 40
## latency: wait at least 1.0 msec before allowing another ARP pulse (debouncing)
arp_latency        = 64000

acp_gain           = 0
acp_thresh_excite  = 20
acp_thresh_relax   = 40
## latency: wait at least 0.1 msec before allowing another ARP pulse (debouncing)
acp_latency        = 6400

use_acp_for_sweeps = 0

## radar parameters for verifying empirical measurements:

acps_per_sweep     = 450
rpm                = 24

## pulse modes: each corresponding slot in the following vectors is a mode

modes              = c("SP", "MP", "LP")
prf                = c(2100, 1200, 600)   ## Hz
plen               = c(  80,  300, 800)   ## ns
