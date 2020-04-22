cat("Introdueix dos nombres enters:\n")
x <- scan(n=1, what=numeric(), quiet=TRUE)
y <- scan(n=1, what=numeric(), quiet=TRUE)

z <- (x%%y)==0 || (y%%x)==0

cat("Això és:", z,"\n")