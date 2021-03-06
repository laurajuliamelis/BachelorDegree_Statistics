x <- c(1,1,
       2,1,
       -1,1,
       2,-1)
x <- matrix(x, ncol=2, byrow=T)

y <- c(6.6, 7.8, 2.1, 0.4)

qr(x)$rank

# H_0: beta_2 = 2*beta_1

a <- c(2,-1) # 2*beta_1 - beta_2 = 0

xtx <- t(x) %*% x
xtx <- crossprod(x)
betas <- solve(xtx) %*% t(x) %*% y
betas

numt <- t(a) %*% betas # els dos numt s�n iguals
numt <- sum(a*betas)

residus <- y - x %*% betas # El residu �s la diferencia entre la y observada i el model
mse <- sum(residus^2)/(4-2) # Mean Squared Error => estimador de sigma^2


error_estandar_fpe <- sqrt(mse * t(a) %*% solve(xtx) %*% a)

est_t <- numt / error_estandar_fpe
est_t # estad�stic t

# La pregunta �s: est_t est� a prop o lluny del 0
# Per contestar podem calcular l'estad�stic de la taula o el p-valor

qt(0.975, df=4-2,) # quantil positiu
# Acceptem la hip�tesi nul�la

p.valor <- 2*pt(est_t, df = 4-2, lower.tail = F) # per calcular el p-valor necessito la cua esquerra, no dreta
p.valor
# p.valor > 0.05       

## ARA FEM EL MATEIX PER� CALCULANT LA F DE FISHER
x1 <- x[,1]
x2 <- x[,2]

g <- lm(y ~ 0 + x1 + x2) 
summary(g) #comprovaci�
summary(g)$sigma^2 # = mse

#Com ser� el model sota H_0?
g0 <- lm(y ~ 0 + I(x1 + 2 * x2) )
summary(g0)

anova(g0,g)
# LA f-fisher per aquest contrast �s 0.1519
# La t-student al quadrat �s la f-fisher
# I el p.valor �s el mateix
est_t^2
#�s molt m�s f�cil utilitzar la f-fisher, comparaci� de contrastos

#LLIBRE: Linear Models with R, Julian Faraway (est� en pdf)
#
faraway linear models -> Linear Models with R