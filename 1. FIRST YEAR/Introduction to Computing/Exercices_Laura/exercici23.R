cat("Escriu TRUE o FALSE a cada alarma segons si est� activada o no:\n")
b1 <- scan(n=1, what=logical(), quiet=TRUE)
b2 <- scan(n=1, what=logical(), quiet=TRUE)
b3 <- scan(n=1, what=logical(), quiet=TRUE)

if (b1 || b2 || b3) {
	cat("La alarma �s perillosa!\n")
} else {
	cat("Uff! La alarma �s segura\n")
}