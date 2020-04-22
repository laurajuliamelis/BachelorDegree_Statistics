n <- scan(n=1, quiet=TRUE)
compt <- 0

while ( n>0 ){

	digit <- n %% 10
	n <- n %/% 10
	if ( digit == 7 ){
		compt <- compt + 1
	}
	
}
cat(compt, "\n")