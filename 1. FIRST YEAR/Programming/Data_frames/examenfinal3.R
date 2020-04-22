D1 <- read.table("D1.txt", header= TRUE, sep ="", stringsAsFactor=FALSE)

Any_estrena <- function (D1){
	any_estrena <- vector()
	for( i in 1: nrow(D1)){
		any_estrena <- c(any_estrena, D1$any_estrena[i])
	}
	anyord <- sort(any_estrena)
	
	compt <- 0
	quants <- 0
	any1 <- anyord[1]
	result <- any1

	for ( i in 2: length(anyord)){
		if(any1 == anyord[i]){
			compt <- compt + 1
			if(compt > quants){
				result <- anyord[i]
				quants <- compt
			}
		}else{
			compt <- 0
		}
		any1 <- anyord[i]
	}
	
	return(result)
}

x <- Any_estrena(D1)
cat(x, "\n")