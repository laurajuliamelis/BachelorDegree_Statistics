n <- scan (n=1, quiet=TRUE)
i <- 1

while ( 3*i < n){
	i <- i + 1
}
cat ( i*3 == n, "\n")