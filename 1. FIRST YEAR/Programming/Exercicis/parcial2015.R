m <- matrix ( c(1,0,4,1,2,1,2,1,3), nrow=3, ncol=3, byrow=TRUE)

DiagonalDominant <- function(m){
  v <- vector()
  for ( i in 1: nrow(m)){
    for ( j in 1: ncol(m)){
      if(i ==j){
        v <- c(v, m[i,j])
      }
    }
  }
  
  i <- 1
  trobat <- TRUE
  
  while (i <= nrow(m) && trobat == TRUE){
    j <- 1
    
    while (j <= ncol(m) && trobat == TRUE){
      
      if ( v[i] < m[i,j] ){
        trobat <- FALSE
      }
      
      j<- j + 1
    }
    
    i <- i + 1
  }
  
  return(trobat)
}