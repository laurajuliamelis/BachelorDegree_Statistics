#################################
## EXAMEN FINAL MODELS LINEALS ##
## 01.02.2013                  ##
## Oleguer Vives Gil           ##
#################################


#### PROBLEMA 1 ####

x <- c(1, 1, 1,
       1, 0, 1,
       0, 1, 0,
       2, -3, 2)
X <- matrix(x, ncol=3, byrow=TRUE)


# Apartat c)
y <- c(3.03, 1.98, 1.02, 0.97) 
  
esq <- t(X) %*% X
dret <- t(X) %*% y

X2 <- X[,1:2] 

betas <- solve(t(X2) %*% X2) %*% t(X2) %*% y


# Apartat d)
# Estimacions MQ
MQ.1 <- betas[1]-2*betas[2]
MQ.2 <- betas[1]-betas[2]

# COVAR
library(MASS)
# Estimació de la variància del model
pred <- X2 %*% betas
e <- y-pred
SQRH <- sum(e^2)
sigma_q <- (SQRH/(4-2))

a1 <- c(1, -2) 
a2 <- c(1, -1)

covar.a1.a2 <- as.vector(sigma_q * t(a1) %*% ginv(t(X2) %*% X2) %*% a2)


# Apartat e)
# Contrast H0: BETA1 + BETA3 = 2
beta1 <- c(1, 1, 0, 2)
beta2 <- c(1, 0, 1, -5)
beta3 <- c(1, 1, 0, 2)
g <- lm(y ~ beta1 + beta2 + beta3)
ge <- lm(y ~ I(beta1 - offset(2*beta3)) + beta2)
anova(ge,g)


#########################################################


#### PROBLEMA 2 ####

# Càrrega de dades
install.packages("faraway")
library(faraway)
data(teengamb)


# Apartat 1)
g <- lm(gamble ~ ., data=teengamb)
sg <- summary(g)
sg$sigma^2       # Estimació de la variància de l'error
sg$adj.r.squared # Coeficient de determinació ajustat


# Apartat 2)
pf(sg$fstatistic[1], 4, 42, lower.tail=F)


# Apartat 3)
# i) Var constant dels errors
plot(fitted(g),abs(residuals(g)),xlab="Fitted",ylab="Residuals")
abline(lm(abs(residuals(g))~ fitted(g)))

# ii) H normalitat
plot(g, which=2)
shapiro.test(residuals(g))

# iii) Punts amb influència potencial
leverage <- influence(g)$hat
sum(leverage)    
frontera <- 2*5/47		# 5-> sum(leverage); 47: n obs
which(leverage>frontera)

# iv) Outliers
g.outlier <- rstudent(g)
which(g.outlier > abs(3.5))

# v) Punts influents
infl.g <- cooks.distance(g)
par(mfrow=c(2,1))
plot(infl.g, type="h")
halfnorm(infl.g)
par(mfrow=c(1,1))

# vi) Multicolinealitat
round(cor(teengamb),3)
x <- model.matrix(g)[,-1]
vif(x)


# Apartat 4)
# Treiem el punt 24
teengamb2 <- teengamb[-24,]
g2 <- lm(gamble ~ ., data=teengamb2)
sg2 <- summary(g2)
sg$adj.r.squared
sg2$adj.r.squared


# Apartat 5)
predict(g2, newdata=data.frame(sex <- 0, status <- 60, 
        income <- 10, verbal <- 11), interval="prediction")


# Apartat 6)
teengamb6 <- teengamb2
teengamb6$sex <- as.factor(teengamb6$sex)
summary(teengamb6)

g6 <- lm(gamble ~ sex + income + sex*income, data=teengamb6)
sg6 <- summary(g6)
sg6

# Construcció de les dues rectes
r0 <- lm(gamble ~ income, data=teengamb6, subset=sex==0) # nois
r1 <- lm(gamble ~ income, data=teengamb6, subset=sex==1) # noies

# Gràfic de teengamb6 i les dues rectes
idx <- teengamb6$sex==0
plot(teengamb6$income, teengamb6$gamble, pch=ifelse(idx, 1, 20), xlab="income",
     ylab="gamble")
abline(r0)
abline(r1, lty=2)

with(teengamb6, plot(sex, income))
with(teengamb6, plot(sex, gamble))


#########################################################


#### PROBLEMA 3 ####

# Apartat 1)
# MÈTODE AIC
  # a)
  step(g)

  # b)
  g3 <- lm(gamble ~ sex + income + verbal, data=teengamb)
  sg3 <- summary(g3)
  sg3$adj.r.squared

  # c)
  confint(g, level=0.95)["income",]
  confint(g3, level=0.95)["income",]

# MÈTODE BACKWARD
  # a)
  gf <- formula(g)
  g4 <- step(g, scope=gf, direction="backward", test="F")

  # b)
  g4 <- lm(gamble ~ sex + income + verbal, data=teengamb)
  sg4 <- summary(g4)
  sg4$adj.r.squared

  # c)
  confint(g, level=0.95)["income",]
  confint(g4, level=0.95)["income",]


# Apartat 2)
# OLS)
library(nlme)
g.OLS <- gls(gamble ~ sex + status + income + verbal, data=teengamb2)
sg.OLS <- summary(g.OLS)
coef(g.OLS)

# Huber)
g.huber <- rlm (gamble ~ sex + status + income + verbal, data=teengamb)
sg.huber <- summary(g.huber)
coef(g.huber)

# Least Trimed Squares)
g.lts <- ltsreg(gamble ~ sex + status + income + verbal, data=teengamb)
sg.lts <- summary(g.lts)
coef(g.lts)
