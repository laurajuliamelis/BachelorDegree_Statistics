e<- 3
v <- c(1,13,4,5,67)

divisor <- function(v,e){
  i<- 1
  trobat <- FALSE
  
  while (i <= length(v) && trobat ==FALSE){
    if (v[i]%%e == 0){
      return(TRUE)
    }
    i<- i + 1
  }
  return(FALSE)
}