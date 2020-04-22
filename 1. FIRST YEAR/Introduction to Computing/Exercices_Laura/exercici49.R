n <- scan (n=1, quiet=TRUE)
F1 <- 1
F2 <- 1
cat (F1,"",F2," ")

for (i in 3:n){
	F <- F1 + F2
	cat (F, " ")
	F2 <- F1
	F1 <- F

}

cat("\n")
