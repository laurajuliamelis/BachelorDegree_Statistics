vector <- function(v){
 x<- scan(n=1, quiet=TRUE, what=numeric())
 
 while (x != 0){
   v <- c(v,x)
   x <- scan(n=1, quiet=TRUE, what=numeric())
 }
 
 return(v)
}

v<- vector()
