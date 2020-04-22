v <- c(5,7,6,5,4,5,6,7,6,5)

segona_moda <- function(v){
  vuniq <- uniq(v)
  freq <- vector()
  
  for ( i in 1:length(vuniq)){
    compt <- 0
    for(j in 1:length(v)){
     if(vuniq[i] == v[j]){
       compt <- compt + 1
     }
    }
    freq[i] <- compt
  }
  moda2 <- sort(freq,decreasing = TRUE)[2]
  i <- 1
  trobat <- FALSE
  while ( i <= length(freq) && trobat == FALSE){
    if(freq[i] == moda2){
      trobat <- TRUE
    }
    i<- i + 1
  }
  return( vuniq[i-1])
}
 
