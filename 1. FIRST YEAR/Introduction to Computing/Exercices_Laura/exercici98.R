v <- c(2,4,6,8,8,6,10)

parells <- function(v){
  i<- 1
  
  while(i <= length(v)){
    if(v[i]%%2 != 0){
      return(FALSE)
    }
    
    i<-i + 1
  }
  return(TRUE)
}