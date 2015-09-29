## test the maximum matching plugin by generating a complete bipartite
## graph whose vertices are two sets of random points in the unit
## square.  Edges joining points at distances greater than MD are
## removed by making their weight zero.  Other edges get a weight
## equal to 2-d, so that a maximum matching has "smallest" (in a
## sense) total sum of distances.  There is some ambiguity here in
## that the choice of "2" over other possible numbers (i.e. any
## number larger than the maximum pairwise distance) determines a tradeoff
## between maximizing the match cardinality and minimizing its sum of
## distances.
## There is no ambiguity if all points in c1 have distinct nearest
## neighbours in c2, or if |c1|==|c2| and the graph is complete.

dyn.load("plugins/tracker/maxmatch.so")
SIZE1 <- 150
SIZE2 <- 250
c1 <- complex(r=runif(SIZE1), i=runif(SIZE1))
c2 <- complex(r=runif(SIZE2), i=runif(SIZE2))

doit <- function(MD = 0.1, SCALE = 100000) {
  for (MAX.DIST in MD) {
    d <- Mod(outer(c1, c2, `-`))
    ## the gain matrix converts distances into their inverses, making smaller
    ## distances more favourable for a matching
    g <- matrix(as.integer(SCALE * (2 - d)), SIZE1, SIZE2)
    ## to disallow matches beyond a certain distance, set the gain
    ## to zero for those distances
    g[d > MAX.DIST] <- 0
    print("Calling")
    which <- .Call("bipartite_matching", g)
    print(paste("Num matched: ", length(unique(sort(which)))))
    print("Plotting")
    plot(Re(c(c1, c2)), Im(c(c1, c2)), col=rep(c(1,2), times=c(SIZE1, SIZE2)))
    for (i in seq(along=which)) {
      if (!is.na(which[i]))
        lines(Re(c(c1[i], c2[which[i]])), Im(c(c1[i], c2[which[i]])))
    }
    p<-c1[is.na(which)]
    if (length(p) > 0)
      symbols(Re(p), Im(p), circles=rep(MAX.DIST, length(p)), inches=FALSE, add=TRUE, fg="blue")
    par(new=TRUE)
  }
  par(new=FALSE)
}
doit((1:100)/1000)
