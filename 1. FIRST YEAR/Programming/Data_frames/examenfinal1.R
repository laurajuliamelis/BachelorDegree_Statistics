A <- matrix(c(1,2,2,2,1,2), nrow=3)
B <- matrix(c(3,1,3,1,3,1), nrow=2)



surprise1 <- function(vA, vB){
  s <- 0
  for( i in 1:length(vA)){
    s <- s + vA[i] * vB[i]
  }
  return(s)
}

surprise2 <- function (A,B){
  C <- matrix(rep(0, nrow(A)*ncol(B)), nrow=nrow(A))
  for ( i in 1:nrow(A)){
    for( j in 1: ncol(B)){
      C[i,j] <- surprise1(A[i,],B[,j])
    }
  }
  return(C)
}
