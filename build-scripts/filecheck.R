##
## filecheck.R - stub to parse an R file
##               a zero exit status indicates success, non-zero is failure with an error
##               message printed to stderr
##
## invoke as: R --vanilla --slave --args FILE_NAME [SOURCE_FILE_NAME] < filecheck.R
##
## If SOURCE_FILE_NAME is specified, error messages are reported as if that were
## the file processed, and the file FILE_NAME.lineinfo is sought to read line number
## origin information for the file FILENAME.
##
## (c) 2006 by John Brzustowski  john AT brz DOT ca
## 

fullname <- function(file) {
  if (!identical(substr(file, 1, 1), "/"))
    file <- file.path(getwd(), file)
  return(file)
}
  
err <- function(e) {
  msg.lines <- strsplit(geterrmessage(), "\n")[[1]]
  ## get the line number of the error
  line <- scan(textConnection(msg.lines[2]), integer(0), nmax=1, comment.char=":", quiet=TRUE) + 1

  if (!is.na(source.file)) {
    ## a SOURCE_FILE_NAME was specified
    ## read file's lineinfo
    li <- read.table(paste(file, ".lineinfo", sep=""), sep=",", col.names=c("line", "file"), fill=TRUE, as.is=TRUE)
    
    ## look for actual filename and sourceline for this line
    if (NCOL(li) > 1 && nchar(li[line, 2])) {
      file <- fullname(li[line, 2])
      line <- li[line, 1]
    } else {
      file <- source.file
    }
  }
  cat(paste(file, ":", line, ":", paste(msg.lines, collapse="\n"), "\n", sep=""), file=stderr())
  quit(save="no", status=1)
}

options(error=err, warn=2)


## drop non file arguments
args <- commandArgs()
args <- args[-(1:which(args == "--args"))]

## add the current path to relative filenames
for (i in seq(along=args))
  args[i] <- fullname(args[i])

file <- args[1]
source.file <- args[2]
ignore <- tryCatch(parse(file), error=err)
quit(save="no", status=0)
