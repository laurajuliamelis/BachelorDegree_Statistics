x <- c(6,9,1,2,4,8)
y <- c(8,11,1,2,4,8)

dist_mes_curta <- function(x,y){
 
  min <- sqrt(  ((x[1]-x[2])^2)  + ((y[1]-y[2])^2)  )
  N <- length(x)
  for ( i in 1: (N-1)){
    p1 <- c(x[i],y[i])
    for(j in (i+1):N){
      
      p2 <- c(x[j], y[j])
      
      distancia <- sqrt(  ((p1[1]-p2[1])^2)  + ((p1[2]-p2[2])^2)  )
      if(distancia < min){
        min <- distancia
    }
    
  }
}
  
  return(min)
}