m <- matrix(c(6,7,5,9,1,5,2,6,8,5,6,8),nrow=3, ncol=4)

MitjanaEstudiant <- function(m){
  notes <- vector()
  
  for (i in 1: nrow(m)){
    suma <- 0
    for (j in 1:ncol(m)){
      suma <- suma + m[i,j]
    }
    notes <- c(notes, suma/j)
  }
  return(notes)
}

notes <- MitjanaEstudiant(m)

MitjanaNotes <- function(notes){
  mitj <- 0
  for( i in 1:length(notes)){
    mitj <- mitj + notes[i]
  }
  
  return(mitj/length(notes))
}