m <- matrix(c(6,7,5,9,1,5,2,6,8,5,6,8),nrow=3, ncol=4)

MitjanaAssignatura <- function(m){
  notes <- vector()
  
  for (i in 1: ncol(m)){
    suma <- 0
    for (j in 1:nrow(m)){
      suma <- suma + m[j,i]
    }
    notes <- c(notes, suma/j)
  }
  return(notes)
}
 