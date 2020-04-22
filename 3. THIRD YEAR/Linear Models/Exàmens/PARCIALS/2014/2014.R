
# creem el vector
x <- c(rep(c(1,-1,1),3),rep(c(1,2,1),3),rep(c(2,1,2),3),rep(c(0,3,0),3))
x
x <- matrix(x,ncol=3, byrow=T)
y <- c(2.95, 3.01, 2.98, 6.03, 6.01, 5.99, 8.88, 8.97, 9, 3.02, 3.10, 2.98)

library(MASS)
xtx <- t(x) %*% x
xtx.g <- ginv(xtx)
betas <- xtx.g %*% t(x) %*% y
resultat <- sum(betas)
resultat

# (c) Calculeu l’estimaci´o de la covari`ancia entre els estimadors lineals `optims 
# de α − β + γ i 3β i lavari`ancia de l’estimador lineal `optim de α + β + γ.

n <- length(y)
r <- qr(x)$rank

SCR <- t(y - x %*% betas) %*% (y-x %*% betas)
SCR
MSE <- SCR/(n-r)
MSE
cov <- as.numeric(MSE) * xtx.g
cov

#Covariancia entre dos files de matriu x
a1<-c(1,-1,1)
a2<-c(0,3,0)

cov1<-MSE* t(a1) %*% xtx.g %*% a2
cov1

# la variancia de α + β + γ, sera les 3 variancies sumades mes el doble 
#de les 3 covariancies corresponents

svar <- cov[1,1]+ cov[2,2]+cov[3,3]+ 2*cov[1,2]+2*cov[1,3]+2*cov[2,3]
sum(cov)

