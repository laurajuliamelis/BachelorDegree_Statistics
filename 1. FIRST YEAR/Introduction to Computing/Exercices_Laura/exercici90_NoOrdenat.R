v <- c( 2,3,7,8,3,3,5,9)
e <- 3

repetit <-function(v,e){
  s <- 0
  
  for( i in 1: length(v)){
    
    if(v[i] == e){
      s <- s + 1
    }
  }
  
  return(s)
}