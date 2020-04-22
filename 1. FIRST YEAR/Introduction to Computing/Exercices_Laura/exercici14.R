x <- scan(n=1, what=numeric(),quiet=TRUE)
d <- x%%7==0 || x%%5==0 || x%%3==0

cat(d,"\n")