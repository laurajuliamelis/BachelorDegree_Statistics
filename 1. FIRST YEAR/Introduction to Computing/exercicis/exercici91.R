a <- c(1,2,3,10)
b <- c(1,2,6,8)

unio <- function(a,b){
  c <- a
  for(i in 1:length(b)){
    j<- 1
    trobat <- FALSE
    
    while(trobat == FALSE && j <= length(a)){
        if (b[i] == a[j]){
          trobat <- TRUE
        }
      
      j <- j + 1
    }
    
    if( trobat == FALSE ){
      c <- c(c, b[i])
    }
  }
  
  return(c)
}