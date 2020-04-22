n <- 4
x <- 3
  
serie <- function( n, x){
  suma <- x  # el primer terme, o sigui quan n=0, sempre serà el valor de x
  fact <- 1
  for (i in 1: n){
    terme <- (x + 2*i) / fact
    suma <- suma + terme
    fact <- fact * (i+1)
  }
  return(suma)
}