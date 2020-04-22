invers <- function (v){
  x <- vector()
  
  for ( i in length(v):1){
    x <- c(x, v[i])
  }
  
  return (x)
}

v <- c(5,6,9,8)

y <- invers(v)