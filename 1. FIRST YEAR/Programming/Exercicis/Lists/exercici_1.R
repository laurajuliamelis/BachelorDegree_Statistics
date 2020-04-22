readlist <- function(){
  text <- scan(what=numeric())
  l <- list()
  for ( i in 1: length(l)){
    paraula <- text[i]
    l[[paraula]] <- c(l[[paraula]], i)
  }
  
  return(l)
} 