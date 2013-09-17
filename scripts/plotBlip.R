## plotBlip.R - plot the max-intensity power vs. azimuth cross
## section for a blip.
##
## Requires a dataset created from a blipmovie using the export_blips.R script
##
##
## Example usage:
##
##  x = readRDS("blips.rds")  ## read the dataset created by export_blips.R
##
##  x is a tricky dataframe: some table slots have a vector squeezed into them
##  (those ones have names starting with "samp.")  So each row in x is a single
##  blip, but data for every pixel in x is squeezed into some of the slots
##  in this row.  e.g. x$samp.theta[[i]] is the vector of azimuth values of
##  all pixels in blip i.  They are not necessarily in any particular order,
##  but the order is the same for all pixel-wise variables:
##  "samp.r"      "samp.theta"  "samp.phi"   
##  "samp.dbm"    "samp.iangle" "samp.irange
##
##  i.e. x$samp.theta[[i]][k] and x$samp.range[[i]][k] are the azimuth
##  and range (respectively) of the k'th pixel in the i'th blip of x.
##
##  A radial cross-section of pixels in blip i (those in a line pointing out from
##  the centre of the screen) have the same values of x$samp.theta[[i]]
##
##  An angular cross-section of pixels in blip i (those in an arc at constant
##  range) have the same values of x$samp.range[[i]]
##
## say we want to plot the cross-section of blip # 10:
##
##   plotBlip(x, 10)

plotBlip = function(x, i) {
  if (i > nrow(x))
    stop("index out of range")

  ## find the brightest pixel for this blip
  ## note; x$samp.dbm[[i]] is the vector of reflectivity values for all pixels in blip i
  
  best.pixel = which.max(x$samp.dbm[[i]])

  ## which range slot is this?

  rangeSlot = x$samp.irange[[i]][best.pixel]

  ## get indices of all pixels in this rangeSlice (x$samp.irange[[i]] is the set of
  ## range-slot-indexes for all pixels in blip i)

  rangeSlice = which(x$samp.irange[[i]] == rangeSlot)

  ## now plot power (dbm) vs azimuth for this range slice

  plot(x$samp.theta[[i]][rangeSlice], x$samp.dbm[[i]][rangeSlice], type="b")
}
