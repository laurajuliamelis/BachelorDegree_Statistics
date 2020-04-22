eleccions <- read.table("eleccions.txt", header=TRUE , sep="", stringsAsFactor= FALSE)

ex2c <- function(eleccions){
	max <- eleccions[4 ,2]
	partit <- names(eleccions)[2]

	for ( j in 3: ncol(eleccions) ) {
		if( eleccions [4, j] > max){
			max <- eleccions[4, j]
			partit <- names(eleccions)[j]
		}
	}
result <-  list(Partit= partit, Escons= max) 
return( c(max, partit))
}

resultat <- ex2c(eleccions)

cat(resultat, "\n")