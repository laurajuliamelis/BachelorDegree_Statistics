a <- c(1,2,3,10)
b <- c(1,2,6,8)

interseccio <- function(a,b){
  c <- vector()
  
  for ( i in 1: length(a)){
    
    for ( j in 1:length(b)){
      
      if ( a[i]==b[j]){
        c <- c(c, a[i])
      
        }
    
      
    }
    
  }

  return(c)
}