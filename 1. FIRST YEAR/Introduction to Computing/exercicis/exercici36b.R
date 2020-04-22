n <- scan(n=1, what=numeric(), quiet=TRUE)
i <- 2

while ( i < n ) {
	if ( i %% 2 == 0){
		cat(i, "\n")
	}
	i <- i + 1
}


