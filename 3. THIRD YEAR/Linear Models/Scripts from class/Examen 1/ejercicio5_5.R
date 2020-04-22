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

numt <- t(a) %*% betas # els dos numt són iguals
numt <- sum(a*betas)

residus <- y - x %*% betas # El residu és la diferencia entre la y observada i el model
mse <- sum(residus^2)/(4-2) # Mean Squared Error => estimador de sigma^2


error_estandar_fpe <- sqrt(mse * t(a) %*% solve(xtx) %*% a)

est_t <- numt / error_estandar_fpe
est_t # estadístic t

# La pregunta és: est_t està a prop o lluny del 0
# Per contestar podem calcular l'estadístic de la taula o el p-valor

qt(0.975, df=4-2,) # quantil positiu
# Acceptem la hipòtesi nul·la

p.valor <- 2*pt(est_t, df = 4-2, lower.tail = F) # per calcular el p-valor necessito la cua esquerra, no dreta
p.valor
# p.valor > 0.05       

## ARA FEM EL MATEIX PERÒ CALCULANT LA F DE FISHER
x1 <- x[,1]
x2 <- x[,2]

g <- lm(y ~ 0 + x1 + x2) 
summary(g) #comprovació
summary(g)$sigma^2 # = mse

#Com serà el model sota H_0?
g0 <- lm(y ~ 0 + I(x1 + 2 * x2) )
summary(g0)

anova(g0,g)
# LA f-fisher per aquest contrast és 0.1519
# La t-student al quadrat és la f-fisher
# I el p.valor és el mateix
est_t^2
#És molt més fàcil utilitzar la f-fisher, comparació de contrastos

#LLIBRE: Linear Models with R, Julian Faraway (està en pdf)
#
faraway linear models -> Linear Models with R