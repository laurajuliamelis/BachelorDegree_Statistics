e1 <- scan(n=1, quiet=TRUE, what=numeric())
e2 <- scan(n=1, quiet=TRUE, what=numeric())
compt <- 1
  
while (e2 != 0){
	if( e1 == e2){
	compt <- compt + 1 
	} else{
		cat ("(",e1,":", compt,")\n")
		compt <- 1
	}
	e1 <- e2
	e2 <- scan(n=1, quiet=TRUE, what=numeric())
}
cat ("(",e1,":", compt,")\n")


