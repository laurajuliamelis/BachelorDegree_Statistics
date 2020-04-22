v<- c(3,6,2,3,1,3,5,4,1,8)
x <- 20

alfamitjana <- function(x,v){
 vord<-  sort(v)
 retallada <- (x/100) * length(v)
 
 vector<-vord[(retallada + 1) : (length(vord)-retallada)]

 
 s <- 0
 for (i in 1: length(vector)){
   s <- s + vector[i]
 }

 return( s/length(vector))
  
}