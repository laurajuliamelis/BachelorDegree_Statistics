cat("Escriu un enter major que 10\n")

x <- scan(n=1, what=numeric(), quiet=TRUE)

if(x<=10){
  stop("El nombre ha de ser major que 10\n")
}

z <- ((x%/%10)%%10)==7

cat("Això és:", z,"\n")