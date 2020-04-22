quants <- function(V){
  s <- 0
  
  for (i in 1: length(v)){
    s<- s + v[i]
  }
  
  mitjana <- s / length(v)
  
  mesgrans <-0
  mespetits <-0
  iguals <- 0
  
  for( i in 1:length(v)){
    
    if (v[i] > mitjana){
      mesgrans <- mesgrans + 1 
    } else if (v[i] < mitjana){
      mespetits <- mespetits + 1
    }else{
      iguals <- iguals + 1
    }
  }
  return(c(mesgrans,mespetits,iguals))
}


v<- c(2,2,3,2,1)
