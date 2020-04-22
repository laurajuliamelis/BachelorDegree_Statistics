dat <- read.table("eleccions.txt", header= TRUE, sep= "", stringsAsFactors=FALSE)
Nom <- scan(n=1, quiet =TRUE, what=character())

ex3 <- function(Nom, dat){
	suma <- 0
	for( i in 1: nrow(dat)){
		suma <- suma + dat[i, Nom]
	}
	return(suma/nrow(dat))
}

x<- ex3(Nom,dat)

cat( "La mitjana és:", x, "\n")