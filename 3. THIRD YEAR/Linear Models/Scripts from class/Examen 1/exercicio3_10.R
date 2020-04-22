#exercici 3.10
y<-c(533, 583, 1111, 1069)

x<- c(2, 1, 0,
      0, 2, 1,
      2, 3, 1,
      4, 2, 0)
x<- matrix( x, ncol=3, byrow=T)
x
qr(x)$rank

#posibles solucions
library(MASS)
betas<- ginv (t(x)%*% x) %*% t(x) %*% y 
betas
  

#afegim una altra fila   
  
xa<- c(2, 1, 0,
          0, 2, 1,
          2, 3, 1,
          4, 2, 0,
          1, 1, 1)

xa<- matrix(xa, ncol=3, byrow=TRUE)
qr(xa)$rank

betas2<- solve(t(xa)%*%xa)%*% t(xa)%*%c(y,0)
betas2

#anem a veure el nou trajecte

a<- c(1,3/2,1/2)
sum(a * betas) #opcio 1 

t(a) %*% betas #opcio 2
t(a) %*% betas2

#suma de quadrats mitjos i estandar error 

n <- 4
r <- 2
residus <- y - x %*% betas   # si posso betas2 no donara diferent pq els residus son unics 
SSE <- sum(residus^2)
MSE <- SSE / (n-r)
MSE

xtx<- t(x) %*% x
se_a<- sqrt(MSE * t(a) %*% ginv(xtx) %*% a)
se_a

eigen(xtx)

