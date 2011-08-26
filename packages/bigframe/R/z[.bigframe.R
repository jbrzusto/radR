"[.bigframe" <-
function(x, i=1:dim(x)[1], j=1:length(attr(x, "header")$signature)) {

  ## - returns a dataframe with rows given by the indexes "i" and columns
  ##   given by j

  ## As for [.data.frame, a single index with no comma specifies columns
  if (nargs() == 2 && !missing(i)) {
    j <- i
    i <- 1:dim(x)[1]
  }
  as.data.frame(structure(.Call("bigframe_get_data", x, i, j), names=colnames(attr(x, "header")$signature)[j]), row.names=as.character(i))
}

