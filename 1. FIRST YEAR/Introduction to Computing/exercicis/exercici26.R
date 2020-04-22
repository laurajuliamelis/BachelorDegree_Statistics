x <- scan(n=1, what=numeric(), quiet=TRUE)

r <- x>=0 && x<=5

cat(r, "\n")