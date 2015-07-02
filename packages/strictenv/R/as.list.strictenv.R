"as.list.strictenv" <-
function(x, ...) as.list(structure(x, class=NULL), all.names=TRUE)
