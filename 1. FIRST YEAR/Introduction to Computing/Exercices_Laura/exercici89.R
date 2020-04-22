x <- c(1,2,3,4,5)
y <- c(1,2,3,4,5)

escalar<- function (x,y){
  s <- 0
  
  for (i in 1:length(x)){
    
    s <- s + (x[i]* y[i])
  }
  
  return(s)
}