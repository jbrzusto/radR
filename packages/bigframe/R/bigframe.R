"bigframe" <-
function(filename, frame=NULL, header=NULL, cache.size=bigframe.default.cache.size, read.only=FALSE) {
  ## create a bigframe object from a data frame
  ## or open an existing bigframe file, if frame is NULL
  ##
  ## frame: a data frame with no character columns
  ## filename: a filename in which to store the bigframe
  ## header: an arbitrary list to be stored with the bigframe
  ##         - it should not contain items with these names, which we
  ##           add:
  ##           $signature - a one-row dataframe (easy way to save the column names and types)
  ##                        character columns will have, as their value, a string of the maximum
  ##                        length permitted for that column
  ##           $row.bytes - bytes in a bigframe row
  ##
  ## Return value: an EXTPTR object of class "bigframe" with this attribute:
  ## header: the header, as defined above

  if (is.null(frame)) {
    ## attach a bigframe object from a file
    file <- file(filename, if(read.only) "rb" else "r+b")
    hdr <- readBin(file, "raw", size=1, n=1 + nchar(bigframe.header.string))
    if (rawToChar(hdr) != bigframe.header.string)
      stop(sprintf("%s is not a valid bigframe", filename))
    ## try to read the user header
    ## BUGGY: load(file=file)
    readBin(file, "raw", 5)
    .Internal(loadFromConn(file, environment()))

    ## make sure the header is valid; ie. has at least the fields we require: row.bytes, signature
    if (length(header[c("row.bytes", "signature")]) < 2)
      stop("invalid user header in bigframe")

    data.offset = seek(file, rw="read")
    close(file)
    num.rows <- (file.info(filename)$size - data.offset) %/% header$row.bytes
    
    ## create the bigframe object, but with bogus values for num.rows and chunk.bytes
    ptr <- .Call("bigframe_create",
                 as.character(filename),
                 as.double(data.offset),
                 as.integer(header$row.bytes),
                 as.integer(num.rows),
                 as.integer(cache.size),
                 as.logical(read.only),
                 header$signature)
    fr <- structure(ptr, header=header, class="bigframe")
    return(fr)
  } else {
    ## create a new bigframe object from a dataframe
    if (!is.data.frame(frame))
      stop("frame argument must be a dataframe, possibly with zero rows")
    ## make sure the data frame has only logical or numeric rows
    col.sizes <- bigframe.type.sizes[sapply(frame, storage.mode)]
    if (any(is.na(col.sizes)))
      stop("unsupported column type; must be logical, factor, integer, real, complex, or character")

    ## use a one-row subframe version of frame as the signature
    header$signature <- frame[1, ]

    ## for any character columns, put a string of maximum width (rounded up
    ## to the nearest multiple of 4, including a spot for the null byte)
    ## and correct the corresponding column size
    for (i in length(header$signature))
      if (is.character(header$signature[,i])) {
        maxlen <- floor((4+max(sapply(frame[,i], nchar)))/4)*4
        header$signature[,i] <- format(" ", width=maxlen-1)
        col.sizes[i] <- col.sizes[i] + maxlen - 4
      }
    
    ## set up the user header, including fields we add
    header$row.bytes = sum(col.sizes)

    ## open the file and write a plaintext then the user header
    file <- file(filename, "wb")
    writeBin(bigframe.header.string, file)
    save(list="header", file=file, compress=FALSE)
    data.offset <- seek(file, rw="write")
    close(file)
    ## initialize the bigframe from the given data
    ptr <- .Call("bigframe_create",
                 as.character(filename),
                 as.double(data.offset),
                 as.integer(header$row.bytes),
                 as.integer(0),
                 as.integer(cache.size),
                 FALSE,
                 header$signature)
    fr <- structure(ptr, header=header, class=c("bigframe"))
    fr[1,] <- frame
    return(fr)
  }
}

