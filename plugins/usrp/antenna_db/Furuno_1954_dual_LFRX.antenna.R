## Part of the antenna database for the USRP-1 board.
##
## This file can be copied and customized for a different radar.
##

desc               = "Furuno FR1954/FR1964 C-BB attached through the (new) dual LFRX usrp interface"

## signal parameters; names must be members of usrp.param.names defined in usrp.plugin.R

vid_negate         = 0

trig_gain          = 0
trig_thresh_excite = 40
trig_thresh_relax  = 20
trig_latency       = 0
trig_delay         = 0

arp_gain           = 10
arp_thresh_excite  = 40
arp_thresh_relax   = 20
## latency: wait at least 1.0 msec before allowing another ARP pulse (debouncing)
arp_latency        = 64000

acp_gain           = 0
acp_thresh_excite  = 0
acp_thresh_relax   = 100
## latency: wait at least 0.1 msec before allowing another ARP pulse (debouncing)
acp_latency        = 6400

## radar parameters for verifying empirical measurements:

acps_per_sweep     = 450
rpm                = 24

## for the single LFRX USRP model, video and trigger are analog inputs to the LFRX A side
## while ARP and ACP are analog inputs to the LFRX B side

signal_sources = 66051 ## vid = LFRX_A_A, trig = LFRX_B_A, arp = LFRX_A_B, acp = LFRX_B_B

## pulse modes: each corresponding slot in the following vectors is a mode

modes              = c("SP", "MP", "LP")
prf                = c(2100, 1200, 600)   ## Hz
plen               = c(  80,  300, 800)   ## ns
