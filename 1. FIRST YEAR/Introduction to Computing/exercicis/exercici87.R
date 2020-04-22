iguals <- function(v){
  e1 <- v[1]
  
  for (i in 2: length(v)){
    if (e1 != v[i]){
      return(FALSE)
    }
  }
  return(TRUE)
}

v <- c(1,1,2,1,1,1,1)
