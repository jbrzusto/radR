gdb <- function() {
  .Call("gdb")
}

oldq <- q
q <- function() {
  dyn.unload("seascan")
  oldq()
}

`%:%` <- function(x, y) {
  ## Quick paste syntax:
  ## A%:%B will do exactly what you want it to
  paste(x, y, sep="")
}

`%$%` <- function(x, y) {
  ## Quick attribute syntax:
  ## A %$% B is attribute B of A
  attr(x, as.character(substitute(y)))
}

`%$%<-` <- function(x, y, value) {
  ## Quick attribute assignment
  ## A %$% B <- C
  attr(x, as.character(substitute(y))) <- value
  x
}

mod <- source("seascan.R")$value
`[.extmat` <- function(x, i=NULL) {
  .Call("radR_extmat_index", x, i)
}

scan.buff <- .Call("make_scan_buff")
  
print(u <- get.units(mod))
r <- u[[1]]
print(p<-get.ports(r))
port <- p[[3]]
print(tc <- get.contents(port))
goto <- function(run=NULL, scan=NULL) {
  if (!is.null(scan)) {
    ## seek to run, scan
    print(paste("Seeking to run:", run, " and scan:", scan))
    print(paste("Seek.scan returned ", seek.scan(port, run, scan)))
    print(paste("Next.scan returned ", next.scan(port)))
  } else {
    time <- run
    class(time) <- "POSIXct"
    print(paste("Seeking to time:", format(time)))
    print(paste("Seek.time returned", seek.time(port, as.integer(time))))
    print(paste("Next.scan returned", next.scan(port)))
  }
  inf<-get.scan.info(port)
  print(inf$timestamp)
  sb<<-get.scan.data(port, scan.buff)
  print(sb[][1:100, 1])
}

goto(1, 1)
goto(-1, 200)
#debug(seek.scan.seascan)
goto(-1, 100)
goto(1, 300)
goto(2, 10)
goto((tc$start.time[3] + (tc$end.time[3] - tc$start.time[3]) / 2))

