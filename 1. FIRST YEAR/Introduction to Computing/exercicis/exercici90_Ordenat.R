v <- c( 2,3,3,3,4,7,9,9)
e <- 3

repetit <-function(v,e){
  s <- 0
  i <- 1
  
  while (v[i] <= e ){
    
    if( e == v [i]){
      s <- s + 1
    }
    
    i<- i + 1
  }
  
  return(s)
}