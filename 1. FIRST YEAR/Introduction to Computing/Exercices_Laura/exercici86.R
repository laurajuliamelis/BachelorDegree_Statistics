v <- c(1,2,3,4,5,6,7,8,9)

parells <- function(v){
  x <- vector()
  i <- 2
  while ( i <= length(v)){
   
    x <- c(x, v[i])
    
    i <- i + 2
  }
  
  return(x)
}