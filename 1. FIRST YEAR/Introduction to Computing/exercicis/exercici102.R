v <- c(1,3,2,5,6,4,7,8)

mediana <- function(v){
  v <- sort(v)
  N <- length(v)
  
  if (N%%2 ==0){
    return(mean(c(v[N/2], v[N/2 + 1])))
  } else {
    return(v[N/2 + 1])
    
  }

}