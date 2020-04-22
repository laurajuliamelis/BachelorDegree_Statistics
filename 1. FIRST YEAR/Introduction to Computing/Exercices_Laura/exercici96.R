mida <- 0

zeros_i_uns <- function (mida){
  x <- vector()
  if( mida == 0){
    return(cat("Aquest vector és buit\n"))
  }else{
    for (i in 1:mida){
      if (i %%2 != 0) {
        x <- c(x,0)
      } else{
        x <- c(x,1)
      }
    }
    return(x) 
  }
}