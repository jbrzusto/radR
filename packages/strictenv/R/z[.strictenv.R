"[.strictenv" <-
function(se, index) {
  rv <- list()
  switch(mode(index),
         character = {
           for (s in index) {
             if (!exists(s, se))
               stop("Unknown symbol '", s, "' in strict environment.")
             rv[[s]] <- get(s, se)
           }
         },
         numeric = {
           nm <- names(se)
           for (s in index) {
             rv[[nm[s]]] <- get(nm[s], se)
           }
         },
         stop("Mode of index for [.strictenv must be 'character' or 'numeric'")
         )
  return (rv)
}

