a <- scan(n=1, what=character(), quiet=TRUE)
b <- scan(n=1, what=character(), quiet=TRUE)
c <- scan(n=1, what=character(), quiet=TRUE)
compt <- 0

if (a==b){
	compt <- compt+1
}
if (b==c){
	compt <- compt+1
}
if (a==c){
	compt <- compt+1
}

cat("La paraula més freqüent apareix", compt, "vegades\n")