## Prova parcial 29 octubre 2007

##################
## Primer problema
##################

y <- c(1.95,6.03,7.88,4.10)
alpha <- c(1,1,2,0)
bet <- c(-1,2,1,3)
gamm <- c(0,1,1,1)
X <- matrix( c(alpha,bet,gamm), ncol=3)

# Apartat (a)
# Aquest apartat s'ha de fer de forma algebraica
# Una funci贸 param猫trica  a1 * alpha + a2 * bet + a3 * gamm  茅s fpe ssi
#
# a1 + a2 - 3*a3 = 0

# Apartat (b)

# el rang de X 茅s 2 i fem servir g-inverses
library(MASS)
param <- ginv(t(X)%*%X)%*%t(X)%*%y

# soluci贸 equivalent amb la restricci贸 gamma=0 (sense fer servir g-inverses)
X1 <- matrix( c(alpha,bet), ncol=2)
param2 <- solve(t(X1)%*%X1)%*%t(X1)%*%y

# (i) per a una fpe les solucions s贸n equivalents
a <- c(3,3,2)
as.vector(a %*% param)  # 3*param[1]+3*param[2]+2*param[3]
as.vector(a %*% c(param2,0)) # 3*param2[1]+3*param2[2]

# (ii) no 茅s fpe

# Apartat (c)

# Suma de quadrats dels residus i estimaci贸 de la vari脿ncia del model
pred <- X%*%param
e <- y-pred
SQR <- sum(e^2)
(sigma_q <- SQR/(4-2))

# covari脿ncia entre dos estimadors de dues fpe
a1 <- c(1,-1,0)
a2 <- c(0,3,1)
cov.a1.a2 <- sigma_q * t(a1) %*% ginv(t(X)%*%X) %*% a2
as.vector(cov.a1.a2)
# vari脿ncia de l'estimador d'una fpe
var.a <- sigma_q * t(a) %*% ginv(t(X)%*%X) %*% a
as.vector(var.a)

# Apartat (d)
# contrast de la hip貌tesi 3*alpha + gamm = 0
a <- c(3,0,1)
numerador <- as.vector(a %*% param)
denominador <-  sqrt(sigma_q * t(a) %*% ginv(t(X)%*%X) %*% a)
t <- numerador / denominador 
( p_valor <- pt(t,df=4-2,lower.tail=FALSE) * 2 )     # es rebutja

#################
## Segon problema
#################

x<-c(1,3,5,10,11)
y<-c(1.95,5.78,11.21,19.50,22.03)
plot(x,y)

# Apartat (a)
r <- lm(y~x) #Montamos el linear model
sr <- summary(r) #Con el summary hallaremos los coeficientes
sr
sr$coef[,1] #Intercept es el t閞mino independiente o corte con el eje y. "x" es el t閞mino que acompa馻 a la x o pendiente
#y= 1.972x + 0.264
sr$sigma^2

# Apartat (b)
# Interval per a beta_0
sr$coef[1,1] + qt(c(0.025,0.975),df=5-2) * sr$coef[1,2]
# Interval per a beta_1
sr$coef[2,1] + qt(c(0.025,0.975),df=5-2) * sr$coef[2,2]

confint(r, level = 0.95)

# Apartat (c)
# H_0: beta_0=0
t <- sr$coef[1,3]
(p_valor <- sr$coef[1,4])      # s'accepta
# H_0: beta_1=2
t <- (sr$coef[2,1] - 2)/sr$coef[2,2]
(p_valor <- pt(t,df=5-2) * 2)  # s'accepta

# Apartat (d)
r0 <- lm(y ~ 0 + x)            # model sense intercepci贸
r  <- lm(y ~ 0 + offset(2*x))  # model sense intercepci贸 i amb beta_2 = 2 (fixe)
anova(r,r0)                    # s'accepta
