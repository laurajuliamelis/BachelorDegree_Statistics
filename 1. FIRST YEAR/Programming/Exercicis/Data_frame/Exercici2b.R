eleccions <- read.table("eleccions.txt", header=TRUE , sep=" ", stringsAsFactor= FALSE)

ex2b <- function(eleccions){
	max <- abs(eleccions$CiU[1] - eleccions$PSC[1])
	any <- eleccions$Anys[1]

	for ( i in 2: nrow(eleccions)){
		
		if( abs(eleccions$CiU[i] - eleccions$PSC[i]) > max){
			max <- abs(eleccions$CiU[i] - eleccions$PSC[i])
			any <- eleccions$Anys[i]
		}
	}

	return(c(any, max))
}

resultat <- ex2b(eleccions)

cat(resultat, "\n")