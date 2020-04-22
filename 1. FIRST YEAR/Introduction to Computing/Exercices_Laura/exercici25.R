#variables

#d1 <- dígit 1
#d3 <- dñigit 3
#r  <- resultat

#programa

cat("Escriu un enter entre 100 i 1000 per saber si és cap-i-cua:\n")
x <- scan(n=1, what=numeric(), quiet=TRUE)

d1 <- x%/%100
d3 <- (x%%100)%%10
r  <- d1==d3

cat("El resultat és:",r,"\n")