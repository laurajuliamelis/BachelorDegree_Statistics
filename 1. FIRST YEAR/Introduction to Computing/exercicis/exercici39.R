n <- scan (n=1, what=numeric(), quiet=TRUE)
x <- scan (n=1, what=numeric(), quiet=TRUE)

s <- 1 #primer terme
i <- 1
terme <- 1

while (i<=n){
	terme <- (x/2)*terme
	s <- s+terme #terme=(x/2)^i
	i <- i+1
}

cat(s, "\n")