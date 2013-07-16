## grab_file_slice.R

## Read chunk of arbitrary size (limited by system memory etc.) at arbitrary offset of a file on
## user's computer, and save to new file.

## This file must be run from within a radR session, typically from
## the radR menu's "Source an R script..." entry

suffix.vals = list(k=1024, K=1024, m=1024^2, M=1024^2, g=1024^3, G=1024^3)

get.num.with.units = function (x) {
  ## from a string like "123.22M", return a number, with units k,m,g mapped
  ## to 1024^c(1, 2, 3)

  if (is.numeric(x))
    return(x)
  suffix = substring(x, nchar(x))
  if (suffix %in% names(suffix.vals)) {
    mult = suffix.vals[[suffix]]
    x = substring(x, 1, nchar(x)-1)
  } else {
    mult = 1
  }
  return (mult * as.numeric(x))
}

grab_file_slice = function() {
  f = rss.gui("FILE_DIALOG",
    mode = "open.file",
    title = "Choose the file to slice",
    init.file = file_to_slice
    )

  if (length(f) == 0)
    return (NULL)

  file_to_slice <<- f

  if (!file.exists(f))
    return (NULL)

  res = rss.gui("POPUP_DIALOG", "Enter slice start and length",
    sprintf( "I'm going to save a chunk from file \n%s\n.  Please enter the start position (0 = first) and length of the slice\nYou can use abbreviations k, M, G for kilo, mega, and gig.\nUse a space between start and length, but no space between numbers and k, M, G.", basename(f)), buttons="Ok", entry=TRUE, default.entry="0 200k")

  fields = read.table(textConnection(res[[2]]), as.is=TRUE)

  start = get.num.with.units(fields[1,1])
  len = get.num.with.units(fields[1,2])

  fcon = file(f, "rb")
  if (len > 0)
    seek(fcon, start)
  else
    seek(fcon, len, "end")
  data = readBin(fcon, raw(), n=abs(len))
  close(fcon)
  outf = sprintf("%s_slice@%.0f_%.0f", f, start, len)
  writeBin(data, outf)

  id = rss.gui("POPUP_DIALOG", "Wrote slice...", sprintf("I wrote the file:\n %s\n to the same folder.", basename(outf)))
  return (TRUE)
}

if (! exists("file_to_slice"))
  file_to_slice <<- "."

repeat {
  f = grab_file_slice()
  if (is.null(f))
    break
}
