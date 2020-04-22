## Ex 2.4 del tema 2

y <- c(9.2,8.3,5.4,-1.6,8.7,3.5) #vector resposta

x <- c(1,1,1,1,
       1,-1,1,1,
       1,0,0,1,
       1,0,0,-1,
       1,0,1,1,
       1,1,-1,1)#matriu de disseny

x <- matrix(x, ncol=4, byrow=T)

xtx <- t(x) %*% x
xtx 
solve(xtx)

estimacio <- solve(xtx) %*% t(x) %*% y

#en aquest problema no tenim model lineal, aix? que no podem utilitzar la funci? residuals
#hem de calcular els residus a m?
#residus:diferencia entre la y observada i la prediccio
residus <- y - x %*% estimacio

RSS <- sum(residus^2) 
n <- dim(x)[1]
m <- dim(x)[2]

sigma2 <- RSS/(n-m)
sigma2 #variabilitat del error
#IMPORTANT: sigma2 est? en kg^2

#C?lcul del standar error
s <- sigma2 * solve(xtx)
se <- sqrt(s[1,1])

#Interval de confian?a al 95% de beta1
estimacio[1] + c(-1,1) * qt(p = 0.975, df = n-m) * se

#Model Lineal
x1 <- x[ ,1]
x2 <- x[ ,2]
x3 <- x[ ,3]
x4 <- x[ ,4]

g <- lm(y ~ 0+x1+x2+x3+x4) #lm: lineal model  
# 0+ ? -1: No t? terme de intersecci?
summary(g)

#t value ?s el quocient
#Pr(>|t|) per decidir si l'estad?st ?s 0 o no, en aquest cas l'?nic que podria ser 0 ?s el 2n pes
#Residual standard error: 0.2893

0.2893^2 #es sigma^2 o MSE