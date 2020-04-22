n <- scan (n=1, quiet=TRUE)
i <- 1
m <- 0
s <- 0

while ( i<=n ){
	
	m <- m + 3
	s <- s + m 
	i <- i + 1
}
cat( s, "\n")