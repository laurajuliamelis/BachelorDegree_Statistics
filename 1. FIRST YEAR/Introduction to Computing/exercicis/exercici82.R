SumaVectors <- function (v1,v2){
  s1 <- 0
  s2 <- 0
  for (i in 1:length(v1)){
    s1 <- s1 + v1[i]
  }
  for (i in 1:length(v2)){
    s2 <- s2 + v2[i]
  }
  
  return(s1+s2)
}

v1 <- c(1,1,1,1)
v2 <- c(1,1,1,1,5,6)
