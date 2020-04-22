n <- scan(n=1, what=numeric(), quiet=TRUE)

for (i in 2:n){
	if ( i%%2 == 0){
		cat(i, "\n")
	}
}