inserit <- function(v,e,p){
  x <- vector()

  for (i in 1:p-1){
    x <- c(x, v[i])
  }
  
  x <- c(x, e)

  for (i in p:length(v)){
    x<- c(x,v[i])
  }
  
  return(x)
}

v <- c(3,1,2,0,7,3,3,3)
e <- 6
p <- 5