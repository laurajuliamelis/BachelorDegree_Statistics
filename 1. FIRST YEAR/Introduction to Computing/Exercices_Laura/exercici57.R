cat("Escriu el primer nombre de la seq��ncia: \n")
x <- scan ( n=1, quiet=TRUE, what = numeric() )

cat("Escriu la mida de la seq��ncia: \n")
N <- scan ( n=1, quiet=TRUE, what = numeric() )

max <- x
min <- x

for ( i in 1:N ) {

	y <- scan ( n=1, quiet=TRUE, what = numeric() )
	
	if ( y > x ){
		max <- y
	
	} else if ( y < x ) {
		min <- y
		
	} 

}

cat ( "El m�xim �s" , max , "i el m�nim" , min , "\n")

