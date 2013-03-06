## trackstats.R:  calculate mean heading and speed for each track from a tracks.csv file
##
## Copyright (C) 2013 John Brzustowski
## License: GPL V2 or later.
##
## usage: info = trackstats(x)
##   where x is either the path to a tracks.csv file,
##   or a dataframe from such a file
##
## Returns:
##   a data.frame with these columns:
##
##     track.no: track ID number from original data
##     timestamp: mean timestamp of points in track
##     n: number of points in track
##     vx: least-squares velocity along x-axis (m/s)
##     vy: least-squares velocity along y-axis (m/s)
##     z: mean height
##     hdg: heading (degrees clockwise from north)
##     speed: least-squares speed (m/s)

deg = function(x) x * (180/pi)

trackstats = function(x) {
  if (is.character(x))
    x = read.csv(x, as.is=TRUE)

  ## calculate velocities for each track
  rv = tapply(1:nrow(x), x$track.no,
    function(i) {
      ## time offsets from first blip
      dt = x$timestamp[i] - min(x$timestamp[i])
      vdt = var(dt)
      
      ## least-squares x velocity
      vx = cov(x$x[i], dt) / vdt

      ## least-squares y velocity
      vy = cov(x$y[i], dt) / vdt

      ## mean altitude
      z = mean(x$z[i])
      
      return (c(x$track.no[i[1]], mean(x$timestamp[i]), length(i), vx, vy, z))
      })

  rv = as.data.frame(matrix(unlist(rv), byrow=TRUE, ncol=6))
  names(rv) = c("track.no", "timestamp", "n", "vx", "vy", "z")
  rv$hdg = (90 - deg(atan2(rv$vy, rv$vx))) %% 360
  rv$speed = sqrt(rv$vx^2 + rv$vy^2)
  return(rv)
}
