a  <- scan(n=1, what=numeric(), quiet=TRUE)
b  <- scan(n=1, what=numeric(), quiet=TRUE)
c  <- scan(n=1, what=numeric(), quiet=TRUE)

if ((b^2)-(4*a*c)==0){
    r <- (-b)/(2*a)
  cat("El resultat és:", r, "\n")
}else if ((b^2)-(4*a*c)<0){
  cat("El resultat és un nombre complex\n")
}else{
    r <- ((-b)+ sqrt((b^2)-(4*a*c)))/(2*a)
    s <- ((-b)- sqrt((b^2)-(4*a*c)))/(2*a)
  cat("Els resultats són:", r, "i", s, "\n")
}
  