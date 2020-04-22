n <- scan(n=1, quiet=TRUE)
s <- 0

while (n > 0){

	xifra <- n%%10
	n <- n%/%10
	s <- s + xifra
	
}
cat(s,"\n")