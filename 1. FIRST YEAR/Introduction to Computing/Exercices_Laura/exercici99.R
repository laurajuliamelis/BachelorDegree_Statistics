n <- 5
if (n==0){
  cat ("La successió és buida\n")
  
} else if ( n==1 || n==2){
  e1 <- 1
  e2 <- 2
  cat (e1, "\n",e2, "\n")

} else {
  e1 <- 1
  e2 <- 2
  cat (e1, "\n",e2, "\n")
  
  for( i in 3:n){
    x <- e1 + e2
    cat(x , "\n")
    
    e1<- e2
    e2<- x
  }
}




