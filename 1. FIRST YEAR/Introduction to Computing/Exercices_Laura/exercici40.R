cat("Introdu�u un enter positiu:\n")
original <- scan (n=1, what=numeric(), quiet=TRUE)
invertit <- 0

if(original < 0){

	cat("El nombre introdu�t no �s un enter positiu\n")

}else{

	while (original > 0) {
		d�git <- original %% 10
		invertit <- invertit*10 + d�git
		original <- original %/% 10
	}
	cat(invertit, "\n")

}