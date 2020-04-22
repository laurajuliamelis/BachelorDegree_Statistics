cat("Escriu un nombre d'hores, minuts i segons, respectivament\n")
h <- scan(n=1,quiet=TRUE)
m <- scan(n=1,quiet=TRUE)
s <- scan(n=1,quiet=TRUE)

segons <- (h*3600)+(m*60)+s

cat("Aquestes dades corresponen a",segons,"segons\n")