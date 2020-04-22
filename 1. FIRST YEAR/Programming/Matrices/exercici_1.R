e <- 2
m <- matrix(c(2,5,4,12,2,6), nrow=2, ncol=3, byrow=TRUE)

QuantsCops <- function (m,e){
  
  quants <- 0 
  
  for (i in 1:nrow(m)){
    for(j in 1:ncol(m)){
      
      if (m[i,j]==2){
        quants <- quants + 1
      }
    }
  }
  
  return(quants)
}