v <-c(1,2,1,2,3,1,2,3,2,3,0)

codificacio_correcta <- function (v){
  
  bool <- TRUE
  i <- 2
  e1 <- v[1]
  
  while( bool == TRUE && v[i]!= 0){
    e2 <- v[i]
    
    if(e1==1){
      if(e2 != 2){
        bool <- FALSE
      }
    } else if ( e1 == 2){
      if (e2 == 2){
        bool <- FALSE
      }
    } else {
      if (e2 == 3){
        bool <- FALSE
      }
    }
    
    
   i  <- i +1 
   e1 <- e2
  }
  
  return(bool)
  
}

cat("Aquesta seqüència és", codificacio_correcta(v),"\n") 