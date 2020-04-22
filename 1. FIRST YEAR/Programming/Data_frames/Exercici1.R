cat("Escriu la nota mitjana històrica de programació:\n")
Nota_hist <- scan(n=1, quiet=TRUE, what=numeric())
dat <- read.table("ex1.txt" , header=TRUE, sep="", stringsAsFactor= FALSE)

ex1 <- function (Nota_hist , dat){
	result <- vector()
	for ( i in 1: nrow(dat) ){
		if( (dat$Nota_prog[i] > dat$Nota_mitjana[i]) &&  (dat$Nota_prog[i] > Nota_hist)){
			result <- c(result, dat$Nom[i])
		}
	}
	
	return(result)
}

r<- ex1(Nota_hist, dat)

cat("Els estudiants que ho compleixen són:", r , "\n")