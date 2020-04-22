library(faraway)
library(car)
library(MASS)
library(leaps)

library(MASS)
# Coeficient de determinació
R_2 <- summary(recta)$adj.r.squared # = 0.583
# Coeficient de correlacio
r <- sqrt(R_2)   # r = 0.764
#varianza residual 
# SCR
RSS <- (summary(recta)$sigma)^2*(13-2)   #recta es el modelo (13-2) numero de observacions-1-num paremetros a estimar (CREO )
RSS_ <- sum(residuals(recta)^2)

#calcular las betas
betas <-ginv(t(x)%*%x) %*% t(x) %*% y
#calcular ecuaciones normales
#introducir matriz

y <- c(3.03,1.98,1.02,0.97)
x <- c(1,1,1,
       1,0,1,
       0,1,0,
       2,-3,2)
x <- matrix(x, ncol=3, byrow=T)
#ecuaciones normales
#Podemos hacerlo mediante la g inversa
library(MASS)
xtx <- t(x)%*%x
xtxi <- ginv(xtx)
betas <- xtxi %*% t(x) %*% y

betas[1] #beta1
betas[2] #beta2

#o bien quitando beta tres e igualandola a 0 beta3=0
x2 <- c(1,1,
        1,0,
        0,1,
        2,-3)
x2 <- matrix(x2, ncol=2, byrow=T)
svd(t(x2)%*%x2)

betas2 <- solve(t(x2)%*%x2,t(x2)%*%y)

betas2[1] #beta1
betas2[2] #beta2

#calcular estimaciones minimo cuadraticas: según el metodo que se haya
#utilizado antes
#Amb el mètode G-inversa
betas[1]-2*betas[2]+betas[3] #Estimació MQ de B1 - 2*B2 + B3
betas[1]-betas[2]+betas[3] #Estimació MQ de B1 - B2 + B3

#Amb el mètode beta_3=0
betas2[1]-2*betas2[2]+0 #Estimació MQ de B1 - 2*B2 + B3
betas2[1]-betas2[2]+0 #Estimació MQ de B1 - B2 + B3

#calculo de covarianza  . cov(a'betas, b'betas)
#ponemos los vestores por separado
a <- c(1,-2,1) #B1 - 2B2 + B3
b <- c(1,-1,1) #B1 - B2 + B3

pred <- x%*%betas
resid <- y-pred
RSS <- sum(resid^2) 
MSE <- RSS/(4-1) # numero de observaciones(numero filas)-rango

cov <- MSE * t(a) %*% ginv(t(x) %*% x) %*% b

#calculo de varianza

a <- c(3, 3, 2)

# var(a'betas) = sigma^2 a'(X'X)^-a
var <- as.vector(MSE * t(a) %*% ginv(t(x) %*% x) %*% a)

#contrastes de hipotesis
a <- c(1,0,1)#vector del contraste

var_a <- MSE * t(a) %*% ginv(t(x) %*% x) %*% a #Variància d'A, varianza del vestor del contraste

Numerador  <- (t(a)%*% betas)-2 #el menos dos viene dado pro el otro lado de a igualdad, 
#se pone lo de alante siempre y luego lo del otro lado de la igualdad
Numerador2 <- (1*betas[1]-0*betas[2]+1*betas[3])-2#la ecuacion del contraste
Denominador <- sqrt(var_a)#varianza del vector va en el denominador
t_exp <- Numerador / Denominador

r <- length(which(svd(t(x)%*%x)$d>0.0001))#rango
n <- dim(x)[[1]]
n-r #Graus llibertat

(p.valor <- pt(t_exp,df=n-r,lower.tail=F)*2) 






#regresiones lineales
g <- lm(gamble~., teengamb)  #el punto inidca que ponemos el resto d elas variables
summary(g)
#Estimación de la  varianza del error
summary(g)$sigma^2

#Coeficiente de determinación ajustado
summary(g)$adj.r.squared 
#para saber si es o no significativa miramos el summary, nosc entramos
#en la F y en el pvalor


#mirar cosas del modelo 
#Variància constante de los errores
plot(g,which=1)

#hipotesis de normalidad
plot(g,which=2)

#leverage (potencialmente influyentes)
leverage <- hatvalues(g)
k <- 4   #Variables regressores
n <- length(teengamb$gamble)
which(leverage > 2*(k+1)/n) #Criteri elecció


#Outliers
install.packages('MASS')
library(car)
outlierTest(g)

#puntos influyentes (influencia real)
#Distància de cook
plot(g, which=4)

#FIVS
vif(g)
################################################
#lo mismo pero de LAura
###########################################
# a. Homoscedasticitat

# - Residuals vs. Fitted
# - Residuals (en valor absolut) vs. Fitted
# - "Test" de Faraway
# - John Fox: test i gràfic

# - Residuals vs. Fitted (marcant el 0) i Residuals (en valor absolut) vs. Fitted
par(mfrow = c(1, 2))
plot(g, which = 1)
plot(fitted(g),abs(residuals(g)),xlab="Fitted",ylab="|Residuals|")
par(mfrow = c(1, 1))

# Hi ha alguns punts crítics
# Línia vermella una mica quadràtica
# punts amb patró estrany... agrupats

# - "Test" per comprovar si la variància és constant
# Regressió entre el valor absolut dels residus i els valors ajustats
summary(lm(abs(residuals(g)) ~ fitted(g)))
# La regressió és significativa, el que implica que seria una recta amb pendent != 0
# Els errors no són constants
# Dibuixem la recta
plot(fitted(g),abs(residuals(g)),xlab="Fitted",ylab="|Residuals|")
abline(summary(lm(abs(residuals(g)) ~ fitted(g))))

# - John Fox.
# non-constant error variance test
par(mfrow=c(2,2))
plot(g)
ncvTest(g)
# p = 8.284638e-07  -> Rebutgem homoscedasticitat

# plot studentized residuals vs. fitted values
spreadLevelPlot(g)
# Dibuixa una recta de regressió que indica el nivell de constància
# No és horitzontal sinó que té pendent -> els residus van creixent

# Com que es rebutja la homoscedasticitat i es violen les suposicions
# de Gauss-Markov, es podria fer alguna transformació de variables
# per intentar solucionar aquest problema.

# b. Normalitat

# Gràfic: QQ-plot
plot(g, which = 2)
# Hi ha 3 o 4 punts molt allunyats

# Test formal
shapiro.test(residuals(g))
# p-value = 8.16e-05 -> Rebutgem normalitat

# c. Leverage
leverage <- hatvalues(g)

# criteri: h_ii > 2(k+1)/n on k = 4 ja que es tenen 4 vars
which(leverage > 2*(4 +1)/47)
# Aquests punts són POTENCIALMENT INFLUENTS: 31 33 35 42

# d. Outliers

# A. Criteri: |t_i| > 2

t <-rstudent(g)
which(abs(t)>2)
# 24 36 39

plot(1:47, t, type = 'h')
abline(h = c(2, 0, -2))

# Gràficament
Boxplot(t)
# 24 36 39

# B. Ajust del criteri amb Bonferroni
outlierTest(g)
# 24

# c. qqPlot d'ajust als quantils teòrics d'una t-Student
qqPlot(g, main="QQ Plot", id.n = 3)
# 21  3  4  esto son los puntos 
#1 20 21 posicion/coordenada del punto 

# e. Punts influents

# Influència real:
# - sobre els coeficients:
# - DISTÀNCIA DE COOK
# - DFBETAS
# - sobre les prediccions:
# - DISTÀNCIA DE COOK
# - DFFITS

# - DISTÀNCIA DE COOK  Una observació es considera realment influent si té un valor de la distància de Cook superior al valor
#crític d'una F de Snedecor amb graus de llibertat k i N - k.
C <- cooks.distance(g)
which(C > 0.1)#no se pk es a 0.01

# criteri: com que no és clar, el millor és dibuixar-ho.
plot(g, which = 4)
# 5 24 39

# - DFBETAS
dfbetas(g1)

# limit 2/sqrt(n)
l <- 2/sqrt(50)

which(abs((dfbetas(g))[,1])>l)  # 24 35
which(abs((dfbetas(g))[,2])>l)  # 5 24
which(abs((dfbetas(g))[,3])>l)  # 5 24
which(abs((dfbetas(g))[,4])>l)  # 24 39
which(abs((dfbetas(g))[,5])>l)  # 24 27 35
#creo qe es el numero de regresoras
# - DFFITS

dffits(g)

# limit 2*sqrt((k + 1)/n)
l <- 2*sqrt((4 + 1)50)
which(abs((dffits(g)))>l) #si queremos reducir el numero que da y coger solo el mas raro aumentamos l, lo podemos poner a 1 p ejemplo
# 24 39

# f. Problemes de multicolinealitat

# 1. Detecció mirant la correlació entre les vars regressores
round(cor(teengamb[,1:4]),3)
# Hi ha un 0.53 entre status i verbal, que és el més alt

# 2. Detecció pels FIVS
vif(g)
# Són petits, molt menors que 10
############################################3 FIN DE LO DE LAURA

#eliminar los puntos raros
teengamb1 <- teengamb[-24,]
g1 <- lm(gamble~., teengamb1)
summary(g1)
plot(g1,which=1)
#parte de laura comentada cuando se eliminan los puntos raros

# 4) Model sense el punt 24#IMPORANTE SABER 
QUIATR UN PUNOR DEL MODELO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Punt 24: es outlier i es infuent
g2 <- lm(gamble ~ ., data = teengamb[-24,])
summary(g2)
# summary(g2)$sigma^2 = 280.1236 -> Disminueix molt
# L'ajust augmenta una mica

# Residus
plot(g2, which = 1)

# Mirem la normalitat
shapiro.test(residuals(g2))
# p-value = 0.4603 -> ACCEPTEM NORMALITAT

# Mirem homoscedasticitat
# non-constant error variance test
ncvTest(g2)
# Seguim rebutjant, pero ara el p-valor p = 0.0004163668 és mes gran


#Hacer predicciones
# sabemos que se tienen los siguientes valores (0; 60; 10; 11)
x0 <- data.frame(sex=0,status=60,income=10,verbal=11)
#IC
predict(g2,x0,interval="prediction")
#si no funcion la anterior probar asi 
predict(recta, newdata = data.frame(x = 13), interval = 'prediction')


#si nos piden una variable concomitante se pone de la siguiente manera
#var concomitante : a la que comcomita xd
#la variable sex com un factor i la variable income com variable concomitant,
income:sex # y se estima el modelo cn esta nueva variable



#cp de mallows
library(leaps)
mallows <-  regsubsets(gamble~.,data=teengamb)
(rs <- summary (mallows))

plot (2:5, rs$cp, xlab="No. of Parameters", ylab="Cp Statistic")
plot (2:5, rs$adjr2, xlab="No. of Parameters",ylab="Adjusted R-square")

coef(mallows, 1:4)[[3]] #el 3 es l num de vars cn el q nos  qedamos, 1:$ numero de variables



#AIC
gc <- lm(gamble~., teengamb)
gc.formula <- formula(gc)
g0 <-lm(gamble ~ 1,data=teengamb)

step(g0, scope=gc.formula, direction="forward", test="F")
step(gc, scope=gc.formula, direction="backward", test="F")
step(gc, scope=gc.formula, direction="both", test="F")  #me quedo coon lo qe pone call

#IC calcular el IC de la variable income de cda uno de los mdoelos
confint(m0, level=0.95,parm="income") 
confint(m1, level=0.95,parm="income")  
confint(m2, level=0.95,parm="income") 



#seleccion de variables: aic CP de mallows
#seleccion de variables

# 1) Model de regressio OLS
g2 <- lm(gamble ~ ., data = teengamb)
summary(g2)


# 2) Model seleccionat per AIC
g_AIC <- step(g2, direction = "both")
# gamble ~ sex + income + verbal
# Hem tret status
summary(g_AIC)


# 3) Model seleccionat per Cp de Mallows
# install.packages('leaps')
# library(leaps)
X <- as.matrix(teengamb[,-5])#quitamos la variabel gamble,
#creo q pk es la dependiente
y <- teengamb[,5]  #guardamos la variable teengamb, 
#estamos creando matrices parecidas al el dipo de ejercicidio de todos os ejercicios 1

g_Cp <- leaps(X, y, method = "Cp", names = c("sex", "status", "income", "verbal"))#nombres 
#de las explicativas


#para quedarnos con 1 (mas intercept)
g_Cp$which[g_Cp$Cp==min(g_Cp$Cp[g_Cp$size==2])]
#nos quedamos con income

#Para quedarnos solo con 2
# Si P = 3 (2 regressores més intercept)
g_Cp$which[g_Cp$Cp==min(g_Cp$Cp[g_Cp$size==3])]
# sex + income

#PAra quedarnos con 3
# Si P = 4 (3 regressores més intercept)
g_Cp$which[g_Cp$Cp==min(g_Cp$Cp[g_Cp$size==4])]
# sex + income + verbal


# Gràficament
plot(g_Cp$size,g_Cp$Cp, ylim = c(0,6),xlab="P",ylab="C_P")
abline(0, 1)

# El model que més s'apropa a Cp=P és el millor model
# amb P = 3 (2 regressores més l'intercept)
# Per tant, escolliriem sex + income

g_Mallows <- lm(gamble ~ sex + income, data = teengamb)
############################################################
#PAJAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
# Model seleccionat per FORWARD STEPWISE
# Fórmula del model complet
gc.formula <- formula(g2)
# gamble ~ sex + status + income + verbal

# Model simple
g0 <- lm(gamble ~ 1, data = teengamb)
summary(g0)
# Forward stepwise
model <- g0 #cuando pone el ~1 solo va a sacar el intercept
add1(model, scope = gc.formula, test="F")
# Haig d'afegir aquella var amb la F més gran --> income
model <- update(g0, ~ . + income)
summary(model)#income añadida
# Al model g0 li sumo income
add1(model, scope = gc.formula, test="F")
# Haig d'afegir aquella var amb la F més gran --> sex
model <- update(model, ~ . + sex)
summary(model)#sex añadida
# Al model "model" li sumo sex
add1(model, scope = gc.formula, test="F")
# Ja no tinc que afegir-ne cap
# gamble ~ income + sex


#least absolute desviation 
# b) LAD REGRESSION
library(quantreg)
g_lad <- rq(stack.loss ~ ., data = stackloss)

summary(g_lad)
# Canvia bastant el valor dels coeficients per a:
# - Water.Temp -> passa de 1.2953 a 0.57391 (es redueix a la meitat)
# - Acid.Conc -> passa de --0.1521 a -0.06087 (es redueix a la més de la meitat)
# Ara tots són significatius mentre que amb OLS Acid.Conc no ho era.

#componentes principales: toston del mil 






#ajustar los modelos por....
#MQO
g0 <- lm(gamble~., teengamb1)
#b) Mètode de Hubert
library(MASS)
g1 <- rlm(gamble~.,teengamb1)
# c) Least Trimmed Squares (LTS)

library(MASS)
g2 <- lqs(gamble~.,teengamb1, nsamp="exact")
#pedir coeficientes con coef mirar justo debajo de este cacho de script


#ajustar modelos robustos ###########
######PARTELAURA####
# 1) Model de regressio multiple OLS sense el punt 24
#para quitar mas de uno 
stackloss2 <- stackloss[-c(4, 21),]#cambio de base de datos
g_ols <- lm(gamble ~ ., data = teengamb[-24,])
summary(g_ols)

# 2) HUBER
g_huber <- rlm(gamble ~ ., data = teengamb)
summary(g_huber)
# Modificació d'alguns valors dels coeficients
# Intercept
# Verbal
# Mateixes conclusions respecte a la significació de les vars

plot(g_huber)

# 3) LTS
g_lts <- ltsreg(gamble ~ ., data = teengamb, nsamp = "exact")#no olvidar el exact
coef(g_lts)
#  (Intercept)          sex       status       income       verbal 
#    1.942662441  0.089223455  0.009302694  0.603803487 -0.340491284

# Posem 'nsamp = "exact"' ja que així es demana una cerca més
# exhaustiva, tot i que per a grans conjunts de dades el temps
# de computació és alt.

# Amb LTS es troben diferències MOLT substancials en els coeficients.

plot(fitted(g_lts),residuals(g_lts),xlab="Fitted",ylab="Residuals")
#########################################################
#########################REZAR PARA QUE NO SALGA!!!!!
#######################################
#esttimacions de multi 
# (c) Regressio per components principals

# Sempre convé centrar les variables, i a més en el nostre cas es tenen variables
# amb unitats diferents -> optaríem per escalar les vars

# 1. Ho fem amb les vars originals

#library(pls)
#pca <- pcr(siri ~ ., data = train, validation="CV", ncomp=14)#el 14 es el numero de variables
#summary(pca)

#validationplot(pca, legendpos = "topright")
#plot(RMSEP(pca), legendpos = "topright")

#min(RMSEP(pca, "CV")$val[1:15])  # El mínim RMSE
# 4.598189
#which.min(RMSEP(pca, "CV")$val[1:15]) # On es produeix aquest mínim
# Amb 11 components tenim el mínim
# Amb 7 ja s'obte un valor notable.

# Model PCA
#model3 <- lm(train$siri ~ pca$scores[,1:7])

#summary(model3)
# Regressio significativa
# Ajust del 0.72
# Casi cap coefs 0 -> totes les vars menys una significatives

# Ajust del model3
# Per a la mostra de training
#rmse(model3$fit, train$siri)  # 4.399395
# Per a la mostra de test
#rmse(predict(pca, test, ncomp=7), test$siri) # 4.346266

# 2. Ho fem per a les variables centrades#

#trainx <- scale(train[-1], center = T, scale = F)
#pca_centrat <- pcr(train$siri ~ trainx, validation="CV", ncomp=14)
#summary(pca_centrat)

#validationplot(pca_centrat, legendpos = "topright")
#plot(RMSEP(pca_centrat), legendpos = "topright")

#min(RMSEP(pca_centrat, "CV")$val[1:15])  # El mínim RMSE
# 4.563464
#which.min(RMSEP(pca_centrat, "CV")$val[1:15]) # On es produeix aquest mínim
# Amb 11 components tenim el mínim
# Amb 7 ja s'obte un valor notable.

# Ajust del model pca centrat
# Per a la mostra de training
#rmse(predict(pca_centrat, train, ncomp=7), train$siri)  # 4.399395
# Per a la mostra de test
#mm <- apply(train[,-1],2,mean)
#tx <- as.matrix(sweep(test[,-1],2,mm))
#nx <- tx %*%  pca_centrat$loadings[,1:7]
#pv <- cbind(1,nx) %*% coefficients(lm(siri ~ pca_centrat$scores[,1:7], train))
#rmse(pv, test$siri)  # 4.346266

# Em dóna el mateix que si no centrem les variables

# Intent d'interpretacio (3 primeres PC)
#matplot(1:14,pca_centrat$loadings[,1:3],type="l",xlab="Frequency",ylab="")





##PArtial least squares
# No centrem les vars

#pls <- plsr(siri ~ ., data=train, ncomp=14, validation="CV")
#summary(pls)
#validationplot(pls, legendpos = "topright")
#plot(RMSEP(pls), legendpos = "topright")

#min(RMSEP(pls, "CV")$val[1:15])  # El mínim RMSE
# 4.59284
#which.min(RMSEP(pls, "CV")$val[1:15]) # On es produeix aquest mínim
#model4 <- lm(siri ~ pls$scores[,1:4], train)
#summary(model4)






# (e) Ridge regression.
#library(MASS)

# Primer centrem la variable resposta i les regressores per al GRUP TRAINING
#trainx <- as.data.frame(scale(train, scale = FALSE)) # Centra les variables

# Fem la Ridge regression per un rang de valors de lambd#a
#gridge <- lm.ridge(siri ~ .,lambda = seq(0,5e-4,1e-9), data =trainx) #

#matplot(gridge$lambda,t(gridge$coef),type="l",lty=1,xlab=expression(lambda),ylab=expression(hat(beta)))#

#select(gridge)
#abline(v=0.000270109, lwd = 3)

# Ajust per al grup training
#which.min(gridge$GCV)
# Coefs de la columna 270110

## Per fer les prediccions cal escalar les regressores (ja estan centrades) i afegir la mitjana
## (La Ridge Regression es fa amb totes les variables escalades i centrades)
#ypred <- scale(trainx[,-1],center=FALSE,scale=gridge$scales)%*% gridge$coef[,270110] + mean(train$siri)
## I mirem l'ajust
#rmse(ypred, train$siri)
#  4.178651

# Ajust per al grup test
#testx <-  as.matrix(sweep(test[,-1],2,mm)) # centro les observacions del grup test amb les mitjanes del grup training
#ytpred <- scale(testx,center=F,scale=gridge$scales) %*% gridge$coef[,270110] + mean(train$siri)
#rmse(ytpred, test$siri)
# 4.395527

#ESPERO QUE NO SALGA; LO JODIOD DEL EXAMEN DEL 2012
###############################################

#contraste reset: forma lineal correcta o no 
# Model OLS
g <- lm(gamble ~ sex + income + verbal, data = teengamb)
summary(g)
# Prediccions amb el model OLS
yp <- fitted(g)

# Contrast de models

# Model de la nul·la = model OLS
g1 <- g

# Model de la alternativa

# k = 2
g2 <- lm(gamble  ~ sex + income + verbal + I(yp^2), data = teengamb)

# k = 3
g3 <- lm(gamble  ~ sex + income + verbal + I(yp^2) + I(yp^3), data = teengamb)

# k = 4
g4 <- lm(gamble  ~ sex + income + verbal + I(yp^2) + I(yp^3) + I(yp^4), data = teengamb)

# Contrastos

# Contrast 1. g1 vs. g2
anova(g1, g2)
# Rebutgem la nul·la
# Ens quedem amb el model 2

# Contrast 2. g1 vs. g3
anova(g1, g3)
# Rebutgem la nul·la
# Ens quedem amb el model 3

# Contrast 3. g1 vs. g4
anova(g1, g4)
# Rebutgem la nul·la
# Ens quedem amb el model 4

# Per tant, es rebutja la linealitat del model




#ajustar rectas de regresion con datos 
plot(x, y)

# Ajustem la recta de regressió
recta <- lm(y ~ x)

# Fem un gràfic i afegim la recta
plot(x, y)
abline(recta)

# Estimacio dels parametres de regressio i càlcul de R^2
summary(recta)
coefficients(recta)
# beta0 = 0.264
# beta1 = 1.972
summary(recta)$adj.r.squared #coeficiiente de determinacion
# R^2(adjusted) = 0.9925
summary(recta)$sigma  # RSE
(summary(recta)$sigma)^2  # Em demanen la variància del model
# sigma^2 = 0.554
#ICs
confint(g, level=0.95,parm="(Intercept)")
confint(g, level=0.95,parm="x")

#contrastes de hipotesisi dentro del modelo lineal
# si el contraste es q la var es =0 lo podemos mirar cn el summary; o bien cn una anova de dos modelos, uno qe beta sea 0 y otro que no 
recta0 <- lm(y ~ 0 + x)
summary(recta0)
anova(recta0, recta) # miramos el p valor de abajo de Pr(>F)
#hay otro metodo que es raro pero es muy util 
#para beta0=0
t_exp1 = coefficients(recta)[[1]]/sqrt(vcov(recta)[1,1])
p_value <- 2*pt(t_exp1, 3, lower.tail = FALSE)
#para beta1=2
t_exp2 = (coefficients(recta)[[2]] - 2)/sqrt(vcov(recta)[2,2])
# t_exp2 = (summary(recta)$coef[2,1] - 2)/summary(recta)$coef[2,2]
p_value <- 2*pt(t_exp2, 3, lower.tail = TRUE)  # df = n-r = 5 - 2 = 3
# si el contraste no es que beta sea =0 si no a otro numero :
recta0 <- lm(y ~ offset(2*x))
summary(recta0)
anova(recta0, recta)
# miramos el p valor de abajo de Pr(>F)

#ademas tambien se pude hacer con los de arriba
#IR con cuidado con los lower tail!

# Intervals de confiança per a les prediccions
predict(recta, newdata = data.frame(x = 8), interval = 'confidence')
predict(recta, newdata = data.frame(x = 8), interval = 'prediction')


#IC para b1-bo (o sea IC para dos betas)

#dos maneras diferentes de hacerlo
#primera
# IC: a'*beta +- t*sqrt(MSE*a'*(X'X)^-1*a)
a <- c(-1, 1)#lo mismo que antes, vector del contarste)
x_g <- model.matrix(g)
xtxi <- solve(t(x_g)%*%x_g)
ee <- summary(g)$sigma*sqrt(t(a) %*% xtxi %*% a)  # error estandard de a'beta
ic <- t(a)%*%coefficients(g) + c(-1, 1)*qt(0.975, 3)*ee#3 supongo que son los grados lebiertad
#segunda
# IC: [beta1-beta0  +-  t * sqrt(var)] donde var = var(beta0) + var(beta1) - 2*covar(beta0, beta1)
ee <- sqrt(vcov(g)[1,1]+vcov(g)[2,2]-2*vcov(g)[1,2])
ic <- (coefficients(g)[[2]]-coefficients(g)[[1]])  + c(-1, 1)*qt(0.975, 3)*ee




###################################################
#PARANOYA: si sabemos que la recta para por el origen e coordenadas 
#calcular el valor de beta1 y contrastarlo con la hipotesisi de b1=2

#sabemos que la recta pasa por el origen de corrdenadas: esto quiere decir que beta0 es 0
#por lo tanto ajustamos el modelo :
g1<-lm(y~0+x)
summary(g1)
#da que x es 2.00262
#contrastamos ahora con beta1=2
t_exp2 = (coefficients(g1)[[1]]-2)/sqrt(vcov(g1)[1,1])  #lo de g1 pasa a ser 1 pk no hay intercept
#pk sabemos que beta0=0 pk pasa por el origen de coordenadas
p_value <- 2*pt(t_exp2, 3, lower.tail = FALSE) #el 3 viene de 5 que es el nuero 
#segunda opcion :con la anova
# opcio 2. Contrast de models
g2<- lm(y ~ 0 + offset(2*x))
summary(g2)
anova(g2, g1)
anova(g1, g2)
#FIN DE PARANOYA
###############################################


# creo que es asi 
#correlacion parcial 
install.packages("ggm")
library(ggm)
pcor(c(1, 5), cov(base))
# 1y 5 numero de variable en la base de datos dentro de cov
#va el nombre de la bse



