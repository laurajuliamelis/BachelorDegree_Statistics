a <- scan(n=1, what=numeric(), quiet=TRUE)
b <- scan(n=1, what=numeric(), quiet=TRUE)
c <- scan(n=1, what=numeric(), quiet=TRUE)
d <- scan(n=1, what=numeric(), quiet=TRUE)

mitjana <- (a+b+c+d)/4
compt <- 0

if(a>mitjana){
	compt <- compt + 1
}
if(b>mitjana){
	compt <- compt + 1
}
if(c>mitjana){
	compt <- compt + 1
}
if(d>mitjana){
	compt <- compt + 1
}

cat (compt, "valors són més grans que la mitjana\n")