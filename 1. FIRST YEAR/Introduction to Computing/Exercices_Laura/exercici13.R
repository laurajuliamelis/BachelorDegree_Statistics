cat("Escriu els coeficients d'una equaci� de segon grau:","\n")
a <- scan(n=1, quiet=TRUE)
b <- scan(n=1, quiet=TRUE)
c <- scan(n=1, quiet=TRUE)

x <- ((-b)+ sqrt((b^2)-4*a*c))/(2*a)
y <- ((-b)- sqrt((b^2)-4*a*c))/(2*a)

cat("Les arrels s�n:",x ,y,"\n")