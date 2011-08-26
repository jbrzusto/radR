## svn: $Id: shrinkapprox.R 471 2010-03-10 10:57:40Z john $
##
## an algorithm for subsetting a linear approximation
## while preserving an error bound
##
##
##  Given: - a function f from R -> R, evaluated at points t_1, t_2, ..., t_n
##         such that f(t_1) = s_1, f(t_2)=s_2, ..., f(t_n) = s_n
##         and such that f is monotonic on each interval [t_i, t_(i+1)],
##         and the slope of secants of f is bounded in magnitude by M on the interval
##         t in [t_1, t_n].
##
##         - a maximum error e > 0
##
##  Find, if possible, a minimal subset i_1, i_2, ..., i_m, m <= n such that the linear
##  approximation to f using only the values at the points t_i_1, t_i_2, ..., t_i_m
##  has error at most e on the interval [t_1, t_n]
##
##  This is a greedy algorithm which keeps the first point, and drops
##  subsequent points until it reaches one where the linear
##  approximation induced by that point and the first point is no
##  longer within the specified error bounds over the interval between
##  the two points.  It then appends the previously tested point to
##  the approximation, and continues with that as the new "first"
##  point.
##
##  The constraints on f mean that on the interval [t_i, t_(i+1)], the graph of
##  f will lie within the parallelogram whose vertices are:
##
##  A = (t_i,     s_i)
##  B = (t_(i+1), s_(i+1))
##  C = (t_i     + |s_(i+1) - s_i| / M, s_(i+1))
##  D = (t_(i+1) - |s_(i+1) - s_i| / M, s_i)
##
##
##	   |	   
##	   |
##    s	   +	    C---------B
##     i+1 |	   /      _-"/
##         |      /    _-"  /
## 	   |     /  _-"    / <- slope = M
## 	   |    /_-"      /
##    s	   +   A---------D
##     i   |       
##         |            
## 	   |      
##         |       
##         +---+--------------+-----------
##             t              t
##              i              i+1
##
## So the graph of f on the interval [t_1, t_n] lies within a "chain"
## of such parallelograms, joined at the corners.  The largest
## vertical distance of a line from the parallelogram over the
## interval [t_i, t_(i+1)] will occur at one of the four vertices, the
## parallelogram, because it is a convex set and vertical distance
## from the line is a linear function of the (t, s) coordinates.  So
## we can guarantee a bound on the approximation error by checking it
## at each vertex.  Conversely, if the error at each vertex is to be
## within a certain bound, this implies bounds on the allowable slope
## of a linear approximation anchored at a given point.  These bounds
## on slope are narrowed (or at best, maintained) each time a new
## parallelogram is considered. As soon as the line from the starting
## point to a point in the set no longer has a slope satisfying the
## constraints up to that point, the preceding point must be appended
## to the approximation and made the new starting point.

max.err <- function(t, s, M, new.i) {
  ## find the maximum error at any of the piecewise envelope corners
  ## (we don't check the last corner since the approximation is exact there)
  n <- length(t)
  f <- approxfun(t[new.i], y=s[new.i])
  me <- -Inf
  for (i in 1:(n-1)) {
    if (me < abs(f(t[i]) - s[i])) {
      which <- c(i, 1)
      me <- abs(f(t[i]) - s[i])
    }
    if (me < abs(f(t[i] + abs((s[i+1] - s[i]) / M)) - s[i+1])) {
      which <- c(i, 2)
      me <- abs(f(t[i] + abs((s[i+1] - s[i]) / M)) - s[i+1])
    }
    if (me < abs(f(t[i+1] - abs((s[i+1] - s[i]) / M)) - s[i])) {
      which <- c(i, 3)
      me <- abs(f(t[i+1] - abs((s[i+1] - s[i]) / M)) - s[i])
    }
  }
  return (c(me, which))
}

## Algorithm: constrain the slope, starting at the first point

shrinkapprox <- function(t, s, e, M) {
  
  n <- length(t)
  if (n <= 2)
    return (list(x=t, y=s))
  new.i <- 1
  j <- 1  ## index in new.i of last approximation segment starting point
  m.min <- -Inf  ## minimum slope allowed
  m.max <- Inf   ## maximum slope allowed
  i <- 1 ## keep track of point in t being examined
  while (i < n) {
    ## current segment of approximation starts at point whose index is new.i[j]
    ## and requires slopes in the interval [m.min, m.max] to satisfy the error bound

    ## Find the constraining slopes for approximating in the interval from t_(i-1) to t_i by examining each
    ## corner of the parallelogram with corners at (t_(i-1), s_(i-1)), (t_i, s_i), edges at s=s_(i-1) and s=s_i,
    ## and whose other two edges have slope +M or -M according to whether s_i >= s_(i-1) or s_i < s_(i-1)

    ## allowed range of slopes to be within e of (t[i], s[i])
    if (i > new.i[j]) {
      m11 <- (s[i] - e - s[new.i[j]]) / (t[i] - t[new.i[j]])
      m12 <- (s[i] + e - s[new.i[j]]) / (t[i] - t[new.i[j]])
      m.min <- max(m.min, m11)
      m.max <- min(m.max, m12)
    }

    ## allowed range of slopes to be within e of (t[i], s[i])
    m21 <- (s[i+1] - e - s[new.i[j]]) / (t[i+1] - t[new.i[j]])
    m22 <- (s[i+1] + e - s[new.i[j]]) / (t[i+1] - t[new.i[j]])
    m.min <- max(m.min, m21)
    m.max <- min(m.max, m22)

    ## allowed range of slopes to be within e of first induced corner
    tt <- t[i] + abs(s[i+1] - s[i]) / M
    m31 <- (s[i+1] - e - s[new.i[j]]) / (tt - t[new.i[j]])
    m32 <- (s[i+1] + e - s[new.i[j]]) / (tt - t[new.i[j]])
    m.min <- max(m.min, m31)
    m.max <- min(m.max, m32)

    ## allowed range of slopes to be within e of second induced corner
    tt <- t[i+1] - abs(s[i+1] - s[i]) / M
    m41 <- (s[i] - e - s[new.i[j]]) / (tt - t[new.i[j]])
    m42 <- (s[i] + e - s[new.i[j]]) / (tt - t[new.i[j]])
    m.min <- max(m.min, m41)
    m.max <- min(m.max, m42)

    if (i > new.i[j]) {
      ## get the slope from the previous approximation point to point i+1
      mm <- (s[i+1] - s[new.i[j]]) / (t[i+1] - t[new.i[j]])
      
      ## if the slope to point i+1 is not feasible, add point i to the approximation
      ## (includes the case of m.min > m.max; i.e. no feasible slopes)
      
      if (mm < m.min || mm > m.max) {
        new.i <- c(new.i, i)
        j <- j+1
        m.min <- -Inf
        m.max <- Inf
      } else {
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }

  ## add point n
  new.i <- c(new.i, n)

  me <- max.err(t, s, M, new.i)

  cat(sprintf("Max error is %f at index %d type %d\n", me[1], me[2], me[3]));
  plot(t, s, type="b", ylim=c(min(s) - e, max(s) + e))
  for (i in 1:(n-1)) {
    lines(c(t[i], t[i] + abs(s[i+1] - s[i]) / M), c(s[i], s[i+1]), type="l", lty=2, col="blue")
    lines(c(t[i+1], t[i+1] - abs(s[i+1] - s[i]) / M), c(s[i+1], s[i]), type="l", lty=2, col="blue")
    lines(c(t[i] + abs(s[i+1] - s[i]) / M, t[i+1]), c(s[i+1], s[i+1]), type="l", lty=2, col="blue")
    lines(c(t[i], t[i+1] - abs(s[i+1] - s[i]) / M), c(s[i], s[i]), type="l", lty=2, col="blue")
  }
  lines(t[new.i], s[new.i], col="red")
  lines(t[new.i], s[new.i]+e, lty=2, col="red")
  lines(t[new.i], s[new.i]-e, lty=2, col="red")
  
  return(list(x=t[new.i], y=s[new.i]))
}

demo <- function(np = 10) {
t <- cumsum(10*runif(np))              ## random times (non-decreasing)
s <- 10*runif(np)                      ## random states
M <- max(abs(diff(s) / diff(t))) * 1.1 ## hypothetical maximum slope, larger than any observed
emax <- 5                              ## half the possible range of states; arbitrary and will sometimes be unattainable

shrinkapprox(t, s, emax, M)
}

demo()
