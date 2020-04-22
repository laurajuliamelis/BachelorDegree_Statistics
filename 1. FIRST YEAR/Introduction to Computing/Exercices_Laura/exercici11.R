cat("Escriu tres nombres enters:","\n")
x <- scan(n=1, quiet=TRUE)
y <- scan(n=1, quiet=TRUE)
z <- scan(n=1, quiet=TRUE)

cat(x>y && y>z || x<y && y<z, "\n")