n <- scan(n=1, quiet=TRUE)
i <- 1

while (i <= n){

	r <- n%%i
	if ( r==0 ){
		cat(i,"\n")
	}
	i <- i + 1
}
