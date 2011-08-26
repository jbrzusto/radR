"names<-.strictenv" <-
function(x, value) {
  n <- names(x)
  ## remove symbols not in "value" from x
  rm(list=n[!(n %in% value)], pos=x)
  ## add symbols in "value" but not in x
  for (v in value[!(value %in% n)])
    assign(v, NULL, x)
  return(x)
}
    
  

