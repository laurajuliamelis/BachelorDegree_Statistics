v<- c(3,6,2,3,1,3,5,4,1,8)
x <- 20

alfamitjana <- function(x,v){
 vord<-  sort(v)
 vector <- vector()
 retallada <- (x/100) * length(v)
 
 for ( i in (retallada + 1) : (length(vord)-retallada)){
   vector <- c(vector, vord[i])
 }
 
 s <- 0
 for (i in 1: length(vector)){
   s <- s + vector[i]
 }

 return( s/length(vector))
  
}