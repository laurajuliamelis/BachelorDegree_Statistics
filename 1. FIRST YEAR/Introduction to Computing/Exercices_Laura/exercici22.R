cat("Escriun un enter entre 100 i 1000:\n")
x <- scan(n=1, what=numeric(), quiet=TRUE)

if( x<100 || x>1000){
  stop("El nombre ha d'estar entre 100 i 1000\n")
}

d1 <- x%/%100
d2 <- (x%%100)%/%10
d3 <- (x%%100)%%10

s <- d1+d2+d3
r <- s%%5==0
cat("Això és:",r,"\n")