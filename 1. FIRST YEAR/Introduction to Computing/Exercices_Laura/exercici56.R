cat("Escriu el primer nombre de la seq��ncia: \n")
x <- scan ( n=1, quiet=TRUE, what=numeric())

cat("Escriu la mida de la seq��ncia: \n")
N <- scan ( n=1, quiet=TRUE, what=numeric())
s <- x

for ( i in 1:(N-1) ) {

	cat("Escriu el seg�ent nombre de la seq��ncia: \n")
	x <- scan ( n=1, quiet=TRUE, what=numeric())
	
	s <- s + x
}

cat ( "La mitjana �s:" , s/N , "\n")