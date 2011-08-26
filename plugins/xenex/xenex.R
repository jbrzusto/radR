`%:%` <- function (a,b) paste(a, b, sep="")

## load the CANRib dll from its location on either my laptop or the XIR3000c box

tryCatch (dyn.load("C:\\Program Files\\R.T.I\\Horizon NT\\CANRib.dll"),
          error = function(e) dyn.load("c:/Documents and Settings/john/Desktop/xenex/xenexSDK/Xenex SDK 1.4.0.004/Release/CANRib.dll"))
dyn.load("plugins/xenex/xenexutil.dll")
print("radar board: " %:% .Call("have_radar_board"))
print("winsock: " %:% .Call("have_winsock"))
print("xir3000: " %:% .Call("have_xir3000"))
## h <- .Call("open_socket",c("xir3000c", "", ""))

## openxir3000 <- function(aname="Furuno FR1954C-BB", master=TRUE, timeout=1000)
##   {
##     .Call("open_xir3000", as.character(aname),
##           as.logical(master), as.integer(timeout))
##   }

## h<-openxir3000()
## .Call("set_sample_depth", h, 7)
## .Call("set_range", h, 2)
## .Call("set_standby_mode", h, FALSE)
## .Call("set_video_neg", h, TRUE)
## .Call("set_cable_length", h, 51L)
