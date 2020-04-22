cat("Introduïu tres valors:\n")

x  <- scan(n=1, what=numeric(), quiet=TRUE)
y  <- scan(n=1, what=numeric(), quiet=TRUE)
z  <- scan(n=1, what=numeric(), quiet=TRUE)

if (x>y) {
	if(y>z){
		cat(x,y,z,"\n")
	}else if (x>z){
		cat(x,z,y, "\n")
	}else{
		cat(z,x,y,"\n")
	}
} else {
	if(x>z){
		cat(y,x,z,"\n")
	}else if(y>z){
		cat(y,z,x,"\n")
	}else{
		cat(z,y,x,"\n")
	}
}
