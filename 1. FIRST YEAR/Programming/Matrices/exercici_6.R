m <- matrix(c(1,2,3,2,1,3,1,2,3,1,3,2), nrow=3,ncol=4, byrow=TRUE)
  
Combinacio31H <- function(m){
  quants <- 0
  
  for (i in 1: nrow(m)){
    for(j in 1: (ncol(m)-1))
      if(m[i,j]==3 && m[i,j+1] == 1){
        quants <- quants + 1
      }
  }
  return(quants)
}