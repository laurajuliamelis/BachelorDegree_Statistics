v <- c(3,3,5,4,4,7,5)

funcio <- function(v){
  N <- v[1]
  x1 <- v[2]
  y1 <- v[3]
  boolx <- TRUE
  booly <- TRUE
  
  for (i in 4:2*N){
    x2 <- v[i]
    if (x1 > x2){
      boolx <- FALSE
      
    }
    
    i<- i +1
    y2 <- v[i]
    if (y1 < y2){
      booly <- FALSE
    }
    
    x1 <- x2
    y1 <- y2
    
    }

  
  return(boolx && booly)
}