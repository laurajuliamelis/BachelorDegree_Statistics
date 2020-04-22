v <- c(1,1,2,5,3)

parell <- function(v){
  i <- 1
  trobat <- FALSE
  
  while (trobat == FALSE && i <= length(v) ){
    
    if( (v[i]%%2) == 0){
      return (TRUE)
    }else{}
    
    i <- i + 1
  }
  
  return(FALSE)
}