"biglist" <-
function(filename, overwrite=FALSE, names=NULL, header=NULL, read.only=FALSE) {
  ## create a biglist object, or open an existing one.
  ##
  ## filename:  a filename in which to store the biglist
  ##            if the file exists, the existing object is opened for reading
  ##            and writing
  ##
  ## overwrite: if TRUE, any existing file is deleted
  ##
  ## names:     default names() attribute for items in the biglist.
  ##            These names are used, by default, for any
  ##            item in the biglist which has none; this can be used to
  ##            avoid repeated saving of names which are common to most or
  ##            all items in the biglist.  If an item has its own names()
  ##            attribute that differs from that in the header, then that
  ##            set of names is saved with the item.
  ##
  ## header:    an arbitrary object stored in the header of the
  ##            associated index file; if this object contains an item
  ##            called "names", it is overridden by a non-null value of
  ##            the names parameter.
  ##           
  ## Return value: an EXTPTR object of class "biglist" with these attributes
  ##   ndx: a bigframe object (holds the index) opened with the name filename.ndx
  ##   filename: filename for this biglist
  ##   file: an open binary R read/write file connection for the list
  ##
  
  if (!is.null(names)) {
    if (is.null(header))
      header <- list()
    header$names <- names
  }
  biglist.header.tag = "R biglist"
  biglist.version = "v0.2"
  biglist.header.string = sprintf("%-31s", paste(biglist.header.tag, biglist.version))
  if (overwrite || !file.exists(filename)) {
    ## create a new biglist file, write a plaintext header, and create the index file
    file <- file(filename, "w+b")
    writeBin(biglist.header.string, file)
    ## open the index file, which holds the user header
    ndx <- bigframe(paste(filename, biglist.indexfile.suffix, sep=""), data.frame(offs=double(0), size=double(0)), header=header)
  } else {
    ## open an existing file and verify that it is a biglist
    file <- file(filename, if(read.only) "rb" else "r+b")
    hdr <- rawToChar(readBin(file, "raw", size=1, n=1 + nchar(biglist.header.string)))
    if (substr(hdr, 1, nchar(biglist.header.tag)) != biglist.header.tag)
      stop(sprintf("%s is not a valid biglist", filename))
    biglist.version = substr(hdr, 11, 14)
    ndx <- bigframe(filename=paste(filename, biglist.indexfile.suffix, sep=""), read.only=read.only)
    names <- attr(ndx, "header")$names
  }

  ## an integer representing the version
  numver = as.integer(round(as.numeric(substring(biglist.version, 2))*10))
  
  ## 
  bl <- structure(list(numver=numver, filename=filename, file=file, ndx=ndx, names=names, length=dim(ndx)[1], read.only=read.only), class="biglist")

  return(bl)
}

