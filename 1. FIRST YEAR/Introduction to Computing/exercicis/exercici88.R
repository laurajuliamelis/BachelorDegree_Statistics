v <- c(2,2,2,2,2,2)
e <- 4

iguals <- function(v,e){
  
  for (i in 1: length(v)){
    if (e != v[i]){
      return(FALSE)
    }
  }
  
  return(TRUE)
  
}