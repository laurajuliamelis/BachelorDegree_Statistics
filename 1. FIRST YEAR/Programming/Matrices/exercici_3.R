v <- c(1,2,3,4,5)

GeneraDiagonal <- function(v){
  aux <- rep(0, length(v) * length(v))
  m <- matrix(aux, nrow=length(v))

  for ( i in 1:nrow(m)){
    for (j in 1: ncol(m)){
      if(i==j){
        m[i,j] <- v[i]
      }
    }
  } 
  
  return(m)
}