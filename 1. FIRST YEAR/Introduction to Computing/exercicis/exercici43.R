n <- scan(n=1, quiet=TRUE)
s <- 0

while (n > 0){

	n <- n%/%10
	s <- s+1
	
}
cat(s,"\n")