cat("Escriu el primer nombre de la seqüència: \n")
x <- scan ( n=1, quiet=TRUE, what=numeric())

cat("Escriu la mida de la seqüència: \n")
N <- scan ( n=1, quiet=TRUE, what=numeric())
s <- x

for ( i in 1:(N-1) ) {

	cat("Escriu el següent nombre de la seqüència: \n")
	x <- scan ( n=1, quiet=TRUE, what=numeric())
	
	s <- s + x
}

cat ( "La mitjana és:" , s/N , "\n")