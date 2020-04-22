v <- c(2,3,4,5)
GeneraDiagonal <- function(v){
	m <- matrix(rep(0, length(v)*length(v)))
	for ( i in 1: nrow(m)){
		for( j in 1:ncol(m)){
			if( i==j ){
				m[i,l] <- v[i]
			}
		}
	}
return(m)
}

x <- GeneraDiagonal(v)
cat("La matriu és:", x, "\n"