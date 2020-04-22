install.packages("MASS")
library(MASS)

#condicion -> parametros(4) - rango(3) = 1condicion

#matriz de diseño
x <- c(rep(c(1, 0, -1, 0), 4), rep(c(0, 1, 0, -1), 4), rep(c(1, 1, 0, 0), 4), rep(c(2, 2, -1, -1), 4))#por filas
x <- matrix(x, ncol = 4, byrow = T) 
y <- c(2.1, 1.9, 2.0, 2.2, 1.2, 0.8, 1.1, 0.9, 5.1, 5.2, 4.9, 4.9, 7.9, 8.0, 8.2, 7.9)
xtx <- t(x)%*%x

#calculo de rango
install.packages("matrix")
library(matrix)
rank(x)
#Aunque tambien se podria calcular:
R <- qr(x)
R$rank

#ginv() para rango no max, solve() para rango max
beta <- ginv(xtx)%*%t(x)%*%y 

#Estimaciones MQ:
ii <- sum(beta[1:2])#alpha + beta
iii <- sum(beta)#alpha + beta + gamma + delta

#Calculo de la Var = sigma^2(a'(x'x)^- * a)  y la Cov = sigma^2(a'(x'x)^- *b)
e <- y-x%*%beta # Residus 
RSS <- sum(e^2) # Residual sum square(suma de cuadrats residuals)
var <- RSS/(16-3) #var global (16 filas - rango3)
a <- c(1, 1, 0, 0)#alpha + beta
vara <- var%*%t(a)%*%ginv(xtx)%*%a
b <- c(2, 1, -1, 0)#2alpha + beta - gamma
varb <- var%*%t(b)%*%ginv(xtx)%*%b
cov <- var%*%t(a)%*%ginv(xtx)%*%b

# Se podria calcular la predicion(*)
x%*%beta

#Hipotesis simple -> alpha + beta - 5 (tstudent)
t.est <- (ii - 5)/sqrt(vara) #Estimacion-5/sqrt(RSS/(var%*%t(a)%*%ginv(xtx)%*%a))
pt(t.est, 16-3, lower.tail = F)*2#pueba bilateral

#Hipotesis compuesta -> alpha + beta - 5, 2alpha + beta -gamma - 7 (F fisher = ((RSSH0 -RSS)/2)/(RSS/G.LL.)
y<-c(2.1,1.9,2.0,2.2,1.2,0.8,1.1,0.9,5.1,5.2,4.9,4.9,7.9,8.0,8.2,7.9)
alpha<-c(1,1,1,1,0,0,0,0,1,1,1,1,2,2,2,2)
length(alpha)
beta<-c(0,0,0,0,1,1,1,1,1,1,1,1,2,2,2,2)
length(beta)
gamma<-c(-1,-1,-1,-1,0,0,0,0,0,0,0,0,-1,-1,-1,-1)
length(gamma)
delta<-c(0,0,0,0,-1,-1,-1,-1,0,0,0,0,-1,-1,-1,-1)
length(delta)
x1<-data.frame(y,alpha, beta, gamma, delta)
x1
g<-lm(y~0+alpha+beta+gamma, data=x1)
library("car")
linearHypothesis(g, "alpha+beta=5")#Hipotesis simple
linearHypothesis(g, c("alpha+beta=5", "2*alpha+beta-gamma=7"))#Hipotesis compuesta

#Contrastes hipotesis por recta de regression
#H0: beta0 = 0
# opcio 1. t-Student (ja que només tenim una fpe)
t_exp1 = coefficients(recta)[[1]]/sqrt(vcov(recta)[1,1])
p_value <- 2*pt(t_exp1, 3, lower.tail = TRUE)  # df = n(tabla)-r = 5 - 2 = 3
# opcio 2. Contrast de models
recta0 <- lm(y ~ 0 + x)
summary(recta0)
anova(recta0, recta)
# opcio 3. Al summary del model
summary(recta)
p_value <- summary(recta)$coeff[1, 4]
#H0: beta1 = 1
# opcio 1. t-Student (ja que només tenim una fpe)
t_exp2 = (coefficients(recta)[[2]] - 1)/sqrt(vcov(recta)[2,2])
p_value <- 2*pt(t_exp2, 3, lower.tail = TRUE)  # df = n-r = 5 - 2 = 3
# opcio 2. Contrast de models
recta0 <- lm(y ~ offset(x))
summary(recta0)
anova(recta0, recta)
#H0: beta1 = 2
# opcio 1. t-Student (ja que només tenim una fpe)
t_exp2 = (coefficients(recta)[[2]] - 2)/sqrt(vcov(recta)[2,2])
# t_exp2 = (summary(recta)$coef[2,1] - 2)/summary(recta)$coef[2,2]
p_value <- 2*pt(t_exp2, 3, lower.tail = TRUE)  # df = n-r = 5 - 2 = 3
# p_value = 0.762 --> ACCEPTEM H0
# opcio 2. Contrast de models
recta0 <- lm(y ~ offset(2*x))
summary(recta0)
anova(recta0, recta)
# Si la recta passa per l'origen
recta0 <- lm(y ~ 0 + x)
summary(recta0)

#Estimar pendiente B1 de la recta(passa por origen coordenades);contrastar H0:beta1 = 2
# Amb aquesta nova recta, contrastar H0:beta1 = 2
# opcio 1. t-Student (ja que només tenim una fpe)
t_exp = (coefficients(recta0)[[1]] - 2)/sqrt(vcov(recta0)[1,1])
p_value <- 2*pt(t_exp, 4, lower.tail = FALSE)  # df = n-r = 5 - 1 = 4
# opcio 2. Contrast de models
recta00 <- lm(y ~ 0 + offset(2*x))
summary(recta00)
anova(recta00, recta0)

#Codi per seleccionar dades
install.packages("HistData")
library(HistData)

#Gràfica de dispersió
with(GaltonFills1, plot(mother, childHeight))

#Primera regressió lineal
g <- lm(childHeight ~ mother, data = GaltonFills1)
with(GaltonFills1, plot(mother, childHeight))
abline(g)

#Recta de regressió
peso <- c(28.80, 29.12, 29.76,30.56 ,NA ,28.64, 30.40, 29.44, 29.28, 29.12, 29.12, 28.96, 29.12, NA, 28.32, 29.60)
comp <- factor(rep(1:4, each=4))
levels(comp) <- c("T1", "T2", "T3", "T4")
conc <- factor(rep(1:4,4))
levels(conc) <- c("30", "40", "50", "65")
datos <- data.frame (Peso=peso, Composicion=comp, Concentracion=conc) 
g <- with(datos, lm(peso ~ comp + conc));
#Otra manera
plot(x, y)# Tenim algun valor outlier. Mirem si fem logaritmes millora:
plot(log(x), log(y))# Com que sí que millora, es podrien usar els logaritmes de les variables d'aquí cap endavant.
x <- log(x)
y <- log(y)
recta <- lm(y ~ x)#Ajustem la recta de regressió
plot(x, y)# Fem un gràfic i afegim la recta
abline(recta)

#anàlisi de residus
plot(g)
plot(g, which =4)  

#Estimacion de parametros (Intercept)B0, (mother)B1, sigma^2
Es <- summary(g)#Forma colectiva
coef(g)#Forma individual
Es$sigma^2#Càlcul de sigma^2
Es$r.squared#Coeficient de de determinacio
r <- sqrt(R_2)# Coeficient de correlacio 
RSS <- (summary(recta)$sigma)^2*(13-2)# Variància residual (var no explicada = SCR) + R^2 + r  (13 datos - rango?) 
RSS_ <- sum(residuals(recta)^2)#suma de residuos var residual

#Calculo de la Normalidad
plot(g, which = 2)
shapiro.test(residuals(g))

#Ic del 90% de B0, B1 y sigma^2
confint(g, level=0.9)#IC para parametros
Es$df[2]#grados de libertat
RSS <- sum(Es$residuals^2)#Residual sum square
c(RSS/qchisq(0.95, Es$df[2]), RSS/qchisq(0.05, Es$df[2]))#IC para sigma^2

#Coeficientes
#Estatura = 91.5796+0.495*Estatura Padre; Estatura = 104.023+0.450*Estatura Madre
#GaltonFills(.homes o GaltonFills2) <- GaltonFills[ind, ] para cojer base datos hombres
#H0: B1=0.495 pare/fill(home)
gh <- lm(childHeight ~ father, data = GaltonFills.homes)
g0 <- lm(childHeight ~ offset(I(0.495 * father)), data = GaltonFills.homes)
anova(g0, gh)
#H0: B1=0.450 mare/fill(home)
gm <- lm(childHeight ~ mother, data = GaltonFills.homes)
g0 <- lm(childHeight ~ offset(I(0.45 * mother)), data = GaltonFills.homes)
anova(g0, gm)
#H0: H0:B0=104.023, B1=0.450 mare/fill(home)
g0 <- lm(childHeight ~ 0 + offset(I(104.023/2.54 + 0.45 * mother)), data = GaltonFills.homes)#(B0/centim. +B1)
anova(g0, gm)

#IC para prediccion del 99%, (70,altura pare);(63,altura mare); para un 13% -> x=13
predict(g, newdata = data.frame(mother = 63), interval = "confidence", level = 0.99)#media de altura de todas las madres
#interval = prediction: media de altura para tu hija
#para regression lineal x0 = 8:predict(recta, newdata = data.frame(X = 8),interval = "prediction", level = 0.95)

# Interval de confiança per a beta1 - beta0
# fp. a'*beta = (-1 1)beta. és estimable?
xmat <- matrix(c(1, 1, 1, 1, 1, -1, 1, 2, 5, 10, 20, 1), ncol = 2)
svd(t(xmat)%*%xmat)   # rang = 2 = rangX --> sí es estimable
# IC: a'*beta +- t*sqrt(MSE*a'*(X'X)^-1*a)
a <- c(-1, 1)
x_recta <- model.matrix(recta)
xtxi <- solve(t(x_recta)%*%x_recta)
ee <- summary(recta)$sigma*sqrt(t(a) %*% xtxi %*% a)  # error estandard de a'beta
ic <- t(a)%*%coefficients(recta) + c(-1, 1)*qt(0.975, 3)*ee
# IC: [beta1-beta0  +-  t * sqrt(var)] donde var = var(beta0) + var(beta1) - 2*covar(beta0, beta1)
ee <- sqrt(vcov(recta)[1,1]+vcov(recta)[2,2]-2*vcov(recta)[1,2])
ic <- (coefficients(recta)[[2]]-coefficients(recta)[[1]])  + c(-1, 1)*qt(0.975, 3)*ee