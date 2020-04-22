a <- scan (n=1, quiet=TRUE)
b <- scan (n=1, quiet=TRUE)

if (b > a){
	aux <- b
	b <- a
	a <- aux
} 

while (a != b){
	a <- a - b
}

cat(a, "\n")