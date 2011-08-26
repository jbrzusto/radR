##
##
## generate timing constants for rev 1.1 firmware:
##
##

## stimulus times, in seconds since reset: (encoded in tiltercommandStimulus1_1.sbs)

t.cal.cmd              <-  1.0 ## start sending cal command
t.lower.limit.hit      <-  3.033333 ## hit lower limit switch during low calibration
t.lower.limit.released <-  6.533333 ## release lower limit switch during calibration back-off; we allow for 1 second of this
                               ## (there are 2.5 seconds of stabilization waiting time)
t.up.cmd               <-  8.533333 ## start sending tilt up to 85 degrees command
t.down.cmd             <- 20.566666 ## start sending tilt down to 5 degrees command

## time intervals based on stimulus times and stopwatch timings as measured using MPLAB's MPSIM simulator

TC_cal_AB <- 1.133607 - t.cal.cmd ## set-up time for calibration command
TC_cal_BC <- (1.134856 - 1.133607) / 3  ## average time interval per step, from 1st three down steps of calibration
TC_cal_DE <- 6.632217 - t.lower.limit.hit  ## post-calibrate latency; possibly add 1.0 here for an extra second's insurance

TC_up_AB <-  8.583080 - t.up.cmd ## set-up time for tilt up command
TC_up_DE <- 19.557924 - 19.557585 ## post tilt-up latency; possibly add 0.1 seconds here for extra insurance

up_times <- c(8.583117,9.249585,9.653852,9.859655,10.001458,10.127261,10.237065,10.338868,17.801835,17.903638,18.013441,18.139245,18.281048,18.486851,18.891118,19.557585)  ## times at which top of FOR loop in up-tilting code is hit
dt.up <- diff(up_times)

up.steps <- (850 - 0) * 23 - 0 ## total steps going from 0 to 85 degrees
TC_up_BC <- c(
              (dt.up[1] + dt.up[15]) / (2 * 400),
              (dt.up[2] + dt.up[14]) / (2 * 300),
              (dt.up[3] + dt.up[13]) / (2 * 200),
              (dt.up[4] + dt.up[12]) / (2 * 200),
              (dt.up[5] + dt.up[11]) / (2 * 200),
              (dt.up[6] + dt.up[10]) / (2 * 200),
              (dt.up[7] + dt.up[ 9]) / (2 * 200),
              dt.up[8] / (up.steps - 2 * (5 * 200 + 300 + 400))
              )


TC_down_AB <- 20.616412 - t.down.cmd ## set-up time for tilt down command; this includes the change in direction which
                                     ## TC_up_AB does not.
TC_down_DE <- 31.319260 - 31.318933  ## post tilt-down latency; possibly add 0.1 seconds here for extra insurance

down_times <- c(20.716376,21.382843,21.787110,21.992914,22.134717,22.260520,22.370323,22.472126,29.563184,29.664987,29.774790,29.900593,30.042396,30.248200,30.652467,31.318933) ## times at which top of FOR loop in down-tilting code is hit
dt.down <- diff(down_times)
down.steps <- (850 - 35) * 23 ## total steps going from 85 to 5 degrees
TC_down_BC <- c(
                (dt.down[1] + dt.down[15]) / (2 * 400),
                (dt.down[2] + dt.down[14]) / (2 * 300),
                (dt.down[3] + dt.down[13]) / (2 * 200),
                (dt.down[4] + dt.down[12]) / (2 * 200),
                (dt.down[5] + dt.down[11]) / (2 * 200),
                (dt.down[6] + dt.down[10]) / (2 * 200),
                (dt.down[7] + dt.down[ 9]) / (2 * 200),
                dt.down[8] / (down.steps - 2 * (5 * 200 + 300 + 400))
                )

for (sym in ls(pattern="^TC_.*$"))
  if (length(get(sym)) == 1) {
    cat(sprintf("const double   t11_model::%s = %.6f;\n", sym, get(sym)))
  } else {
    cat(sprintf("const double   t11_model::%s[t11_model::NUM_STEPPING_RATES] = {%s};\n", sym, paste(sprintf("%.6f", get(sym)), collapse=", ")))
  }



