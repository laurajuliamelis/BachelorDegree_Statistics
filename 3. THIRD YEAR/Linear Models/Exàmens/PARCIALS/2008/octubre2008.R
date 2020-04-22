# EXAMEN DE MODELS LINEALS OCTUBRE 2008

# PROBLEMA 1

# Dades

y <- c(3.98, -3.95, 4.03, 7.94)

x = c(3, -1, 2,
      1, -1, 0,
      1, 0, 1,
      0, 1, 1)
x <- matrix(x, ncol = 3, byrow = TRUE)

svd(x)  # rang 2
  # Descomposició en valors singulars
  # d = valors singulars
  # rang = nº de valors singulars no nuls.
  # en aquest cas d = 2
svd(t(x)%*%x)
  # Ho fem de X'X millor que de X perquè  X'X és sempre simètrica

# ----------------------------

# Podem saber si una fp es fpe si l'afegim com a fila a la matriu de disseny
# i el rang es manté en 2, ja que voldrà dir que es combinacio lineal.

xpri = c(3, -1, 2,
      1, -1, 0,
      1, 0, 1,
      0, 1, 1,
      5, -2, 3)
xpri <- matrix(xpri, ncol = 3, byrow = TRUE)
svd(t(xpri)%*%xpri)

# ----------------------------

# Opció 1. Resolem les equacions normals fent gamma = 0

x1 = c(3, -1,
      1, -1,
      1, 0,
      0, 1)
x1 <- matrix(x1, ncol = 2, byrow = TRUE)

svd(t(x1)%*%x1)  # Ara el rang = 2 = màxim

betas1 <- solve((t(x1)%*%x1), t(x1)%*%y)

# L'estimació MQ de la funcio es
est1 <- 5*betas1[1] - 2*betas1[2]

# ----------------------------

# Resolem les equacions normals trobant una g-inversa
library(MASS)

# Resolem les equacions normals
betas2 <- ginv(t(x)%*%x) %*% t(x)%*%y

# L'estimació MQ de la funcio es
est2 <- 5*betas2[1] - 2*betas2[2] + 3*betas2[3]
  # sum(c(5, -2, 3)*betas2)


# ----------------------------

# Busquem la cov(alpha-beta, beta+gamma)
  # Són dos fpe que son comb lineal de Y. La formula es:
  # cov(a'betas, b'betas) = sigma^2 * a'(X'X)^-b

a <- c(1, -1, 0)
b <- c(0, 1, 1)

# 1. a'(X'X)^-b
 t(a) %*% ginv(t(x) %*% x) %*% b

# 2. Estimació de sigma^2 --> EQM = RSS/(n-r)

  # Calculem les prediccions
pred <- x %*% betas2
  # Calculem els residus
resid <- y - pred
  # Residual Sum of Squares
RSS <- sum(resid^2)
  # ECM
MSE = RSS/(4-2)

# 3. cov(a'betas, b'betas)
cov <- MSE * t(a) %*% ginv(t(x) %*% x) %*% b

# ----------------------------

# Busquem la var(2*alpha-beta+gamma)

a <- c(2, -1, 1)

# var(a'betas) = sigma^2 a'(X'X)^-a
var <- MSE * t(a) %*% ginv(t(x) %*% x) %*% a

# la var dona el mateix que la covar. Casualitats de la vida

# ----------------------------

# Volem contrastar una fpe

# Ho: 2alpha - beta + gamma = 0
    # c = 0
a <- c(2, -1, 1)

    # test F si hi ha varies fpe
    # test t si hi ha nomes una. és el nostre cas
      # graus de llibertat = n -r = 2

  # Fórmula: t = (a'beta_somb - c) / ee(a'beta_somb)

num <- t(a) %*% betas2 # (- 0)
  # ja hem fet: var <- MSE * t(a) %*% ginv(t(x) %*% x) %*% a
den <- sqrt(var)
t_exp <- num/den   # t mostral, la observada

# Ara ens queda decidir
p_value <- pt(t_exp, df = 2, lower.tail = FALSE)*2  
  # Busquem la cua de la dreta
  # Multipliquem per 2 -> dues cues -> es una t (si fessim una F no)

# Acceptem que amb les dades que tenim la fpe es 0

#_____________________________________________________

# PROBLEMA 2. REGRESSIÓ

x <- c(1, 2, 5, 10, 20)
y <- c(0.69, 1.43, 3.48, 6.95, 14.09)

# AED
plot(x, y)
  # Tenim algun valor outlier. Mirem si fem logaritmes millora:
plot(log(x), log(y))
  # Com que sí que millora, es podrien usar els logaritmes de les variables d'aquí cap endavant.

x <- log(x)
y <- log(y)

# Ajustem la recta de regressió
recta <- lm(y ~ x)

# Fem un gràfic i afegim la recta
plot(x, y)
abline(recta)

# Estimacio dels parametres de regressio i càlcul de R^2
summary(recta)
  # beta0 = -0.022
  # beta1 = 0.704
  # R^2(adjusted) = 0.9999
summary(recta)$sigma  # RSE
(summary(recta)$sigma)^2  # Em demanen la variància del model
  # sigma^2 = 0.00026

# Contrast d'hipòtesis

# 1. H0: beta0 = 0

  # opcio 1. t-Student (ja que només tenim una fpe)
  t_exp1 = coefficients(recta)[[1]]/sqrt(vcov(recta)[1,1])
  p_value <- 2*pt(t_exp1, 3, lower.tail = TRUE)  # df = n-r = 5 - 2 = 3
    #p_value = 0.57-> Acceptem H0

  # opcio 2. Contrast de models
  recta0 <- lm(y ~ 0 + x)
  summary(recta0)
  anova(recta0, recta)
    #p_value = 0.57-> Acceptem H0

# 2. H0: beta1 = 1

  # opcio 1. t-Student (ja que només tenim una fpe)
  t_exp2 = (coefficients(recta)[[2]] - 1)/sqrt(vcov(recta)[2,2])
  p_value <- 2*pt(t_exp2, 3, lower.tail = TRUE)  # df = n-r = 5 - 2 = 3
  # p_value ~=0 -> Rebutgem H0

# opcio 2. Contrast de models
  recta0 <- lm(y ~ offset(x))
  summary(recta0)
  anova(recta0, recta)
    # p_value ~=0 -> Rebutgem H0

# Intervals de confiança per a les prediccions
predict(recta, newdata = data.frame(x = 8), interval = 'confidence')
predict(recta, newdata = data.frame(x = 8), interval = 'prediction')


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