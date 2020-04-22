x<- c(1,2,3,4,5)
y<- c(1,2,3,4,5)

mitjana <- function(v){
  s<- 0
  for (i in 1:length(v)){
    s <- s + v[i]
  }
  return (s/length(v))
}

Pearson <- function (x,y){
  num <- 0
  for ( i in 1: length(x)){
   n <- x[i] - mitjana(x)
   m <- y[i] - mitjana(y)
   
   num <- num + (n*m)
  } 
  
  t<-0
  u <- 0
  for( i in 1:length(x)){
    d<- (x[i] - mitjana(x))^2
    t <- t + d
    e <- (y[i] - mitjana(y))^2
    u<- u + e
  }
  den <- sqrt(t*u)
  
  return(num/den)
}