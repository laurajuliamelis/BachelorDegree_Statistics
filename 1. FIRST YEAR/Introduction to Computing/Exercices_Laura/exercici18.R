cat("Escriu un nombre d'euros\n")
e <- scan(n=1,quiet=TRUE)

cent      <- e%/%100
cincuanta <- (e%%100)%/%50
vint      <- ((e%%100)%%50)%/%20
deu       <- (((e%%100)%%50)%%20)%/%10
cinc      <- ((((e%%100)%%50)%%20)%%10)%/%5
un        <- (((((e%%100)%%50)%%20)%%10)%%5)/1

cat("Això són",cent,"bitllets de 100,",cincuanta,"de 50,",vint,"de 20,",deu,"de 10,",cinc,"de 5 i",un,"monedes d'euro.\n")