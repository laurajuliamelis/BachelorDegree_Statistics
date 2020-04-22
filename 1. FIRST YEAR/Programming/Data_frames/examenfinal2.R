L1 <- list(Joan=1000, Pep=1300, Maria=500, Isabel=800, Miquel=1200, Nuria=1500, Marc=800, Axel=480, Jana=600)
L2 <- list(Maria=20,Axel=0, Pep=10, Miquel=5, Jana=0, Joan=10, Marc=5, Isabel=0, Nuria=10)

Exercici2 <- function(L1,L2){
  L3<- vector()
  
  for(i in 1:length(L1)){
    interessos <-0
    for(j in 1: length(L2)){
      if(names(L1)[i] == names(L2)[j]){
        if(L2[[j]] != 0){
          interessos <- (L2[[j]]/100) * L1[[i]]
          L3 <- c(L3, names(L2)[j],"=", interessos)
        }
      }
    }
  }
  return(L3)
}