#variables

#o  <- operació
#e1 <- primer enter 
#e2 <- segon enter 

#variables

o  <- scan(n=1, what=character(), quiet=TRUE)
e1 <- scan(n=1, what=numeric(), quiet=TRUE)
e2 <- scan(n=1, what=numeric(), quiet=TRUE)

if (o== "+"){
  r <- e1+e2
} else if (o=="-"){
  r <- e1-e2
} else if (o== "*"){
  r <- e1*e2
} else if (o=="/"){
  r <- e1/e2
} else
  r <- "operació no acceptada"
cat(r,"\n")