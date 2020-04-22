cat("Introduïu un enter positiu:\n")
original <- scan (n=1, what=numeric(), quiet=TRUE)
invertit <- 0

if(original < 0){

	cat("El nombre introduït no és un enter positiu\n")

}else{

	while (original > 0) {
		dígit <- original %% 10
		invertit <- invertit*10 + dígit
		original <- original %/% 10
	}
	cat(invertit, "\n")

}