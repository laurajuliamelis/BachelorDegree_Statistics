v <- c(4, 2,33, 90, 92, 1, -1)

interessant <- function(v){
  e1 <- v[1]
  bool <- TRUE
  i <- 2
  
  while (bool==TRUE && v[i] != -1){
    e2 <- v[i]
    
    if ((e1%%2) == 0){
      if ((e2%%2) ==0){
        if (e2 >= e1){
          bool<- FALSE 
        }
      }
    }
    
    if ((e1%%5)==0 && bool==TRUE){
      if(e2 <=100){
        bool <- FALSE
      }
    }
    
    if (e1 == 33){
      if((e2%%3)!= 0){
        bool <- FALSE
      }
    }
    
    i <- i+1
    e1 <- e2
  }
  return (bool)
}