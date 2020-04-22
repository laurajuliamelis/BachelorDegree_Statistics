x <- scan(n=1, quiet=TRUE)

z <- (x%%3)==0 && x>30

cat("Això és:",z,"\n")