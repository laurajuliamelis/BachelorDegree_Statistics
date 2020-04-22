a <- scan (n=1, quiet=TRUE)
b <- scan (n=1, quiet=TRUE)

while (a!=b){
	if(a>b){
		a <- a-b
	}else{
		b <- b-a
	}
}

cat(a, "\n")