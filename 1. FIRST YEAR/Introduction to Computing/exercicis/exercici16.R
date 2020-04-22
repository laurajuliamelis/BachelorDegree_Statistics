cat("Introduiu un nombre de segons\n")
x <- scan(n=1, quiet=TRUE)
hores <- x%/%3600
minuts <- (x%%3600)%/%60
segons <- (x%%3600)%%60
cat(hores,"hores,",minuts, "minuts i",segons,"segons\n")