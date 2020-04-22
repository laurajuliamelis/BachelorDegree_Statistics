m <- matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,ncol=3, byrow=TRUE)

ElementsDiagonal <- function(m){
  v <- vector()
  
  for ( i in 1:nrow(m)){
    for (j in 1:ncol(m)){
      if(i == j){
        v <- c(v,m[i,j])
      }
    }
  }
  
  return(v)
}