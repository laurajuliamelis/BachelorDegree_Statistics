eleccions <- read.table("eleccions.txt", header=TRUE , sep="", stringsAsFactor= FALSE)

ex2a <- function(eleccions){
	pitjor_res <- eleccions$PP[1]
	any <- eleccions$Any[1]

	for ( i in 2:nrow(eleccions)){
		if( eleccions$PP[i] < pitjor_res ) {
			pitjor_res <- eleccions$PP[i]
			any <- eleccions$Any[i]
		}
	}

	return(c(any, pitjor_res))
}

resultat <- ex2a(eleccions)

cat(resultat, "\n")

