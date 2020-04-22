v <- (c(15,2,3,4,5,6,9))
 maxim<- function(v){
   max <- v[1]
   for (i in 2:length(v)){
     if(v[i] > max){
       max <- v[i]
     }
   }
   return(max)
 }