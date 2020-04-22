N <- 5

Es_Primer<- function(n){
  ho_es <- TRUE
  for (i in 1:n){
    for (j in 1:i){
      if(j %/% i != 0)
        return (FALSE)
    }
  }
  return(TRUE)
}

Eratostenes <- function(N){
  result <- vector()
  
  for ( i in 1: N){
    if(Es_Primer(i) == TRUE){
      result<- c(result, i)
    }
  }
  
  return(result)
}