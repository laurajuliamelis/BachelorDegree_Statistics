x <- c(1,2,3,4)
y <- c(2,5,8,1)

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


cor_menys_un <- function(x,y){
  corr <- 0
  max <- 0
  
  for ( i in 1:length (x)){
    x_nou <- x[-i]
    y_nou <- y[-i]
    
    p <- Pearson(x_nou, y_nou)
    if (p > corr){
      corr <- p 
      max <- i 
    }
  }
  
  
  return(max)
}