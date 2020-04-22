cat("Introduïu tres valors:\n")

x  <- scan(n=1, what=numeric(), quiet=TRUE)
y  <- scan(n=1, what=numeric(), quiet=TRUE)
z  <- scan(n=1, what=numeric(), quiet=TRUE)

if (x>y) {
	if(x>z){
		cat(x,"\n")
	}else{
		cat(z,"\n")
	}
} else {
	if(y>z){
		cat(y,"\n")
	}else{
		cat(z,"\n")	
	}
}