v <- c(3,4,5,4,5,1,2,3,1,-1)

pics <- function(v){
  n_pics <- 0
  i<-3
  e1 <- v[1]
  e2 <- v[2]
  e3 <- v[3]
  while(e3 != -1){
    if(e2 > e1 && e2 > e3){
      n_pics <- n_pics + 1
    }
    
  i<- i+1 
  e1 <- e2
  e2 <- e3
  e3 <- v[i]
  }
  return(n_pics)
}