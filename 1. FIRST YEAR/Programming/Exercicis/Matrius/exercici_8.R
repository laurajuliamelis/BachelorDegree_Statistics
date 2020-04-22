m <- matrix(c(0,0,0,1,4,5,0,4,0,0,2,0), nrow=4, ncol=3)

MatriuDispersa <- function(m){
  compt<- 0 
  elements <- 0.7 * dim(m)
  for(i in 1:nrow(m)){
    for (j in 1:ncol(m)){
      if(m[i,j] == 0){
        compt <- compt + 1
      }
    }
  }
  
  if(compt > elements || compt == elements){
    return(TRUE)
  }else{
    return(FALSE)
  }
}