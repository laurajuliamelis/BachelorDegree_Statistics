# EXAMEN FINAL GENER 2012

# EXERCICI 4

library(faraway)

# Dades
data(fat)

# Escollim la mostra training i la test

index <- seq(10, 250, by = 10)
# Extraiem les dades i eliminem les vars que no són regressores
train <- fat[-index, -c(1,3,8)]
test <- fat[index, -c(1,3,8)]

# Funció per comprovar l'ajust dels models
rmse <- function(x,y) sqrt(mean((x-y)^2))


# (a) Regressio lineal amb totes les predictores
model1 <- lm(siri ~. , data = train)
summary(model1)
  # Regressio significativa
  # Ajust del 0.74
  # Molts coefs 0 -> moltes vars no significatives

vif(model1)
  # Hi han alguns majors de 10 -> normal amb 14 regressores

# Ajust del model1
  # Per a la mostra de training
rmse(model1$fit, train$siri)  # 4.179
  # Per a la mostra de test
rmse(predict(model1, test), test$siri) # 4.395559
# Són del mateix ordre
# l'error real és aproximadament igual al que el model es suggereix.


# (b) Regressio lineal amb les variables seleccionades per AIC
model2 <- step(model1, direction = "both")
  # siri ~ age + weight + neck + abdom + thigh + forearm + wrist, data = train)
# Ens quedem amb 7 regressores de les 14 que teníem

summary(model2)
  # Regressio significativa
  # Ajust del 0.74
  # Cap coefs 0 -> totes les vars significatives

# Ajust del model2
# Per a la mostra de training
rmse(model2$fit, train$siri)  # 4.217687
# Per a la mostra de test
rmse(predict(model2, test), test$siri) # 4.342456
  # Del mateix ordre
  # Casi igual al model1 i amb 7 variables menys
  # Sembla que la simplificació és possible.


# (c) Regressio per components principals

# Sempre convé centrar les variables, i a més en el nostre cas es tenen variables
# amb unitats diferents -> optaríem per escalar les vars

# 1. Ho fem amb les vars originals

library(pls)
pca <- pcr(siri ~ ., data = train, validation="CV", ncomp=14)
summary(pca)

validationplot(pca, legendpos = "topright")
plot(RMSEP(pca), legendpos = "topright")

min(RMSEP(pca, "CV")$val[1:15])  # El mínim RMSE
  # 4.598189
which.min(RMSEP(pca, "CV")$val[1:15]) # On es produeix aquest mínim
# Amb 11 components tenim el mínim
# Amb 7 ja s'obte un valor notable.

# Model PCA
model3 <- lm(train$siri ~ pca$scores[,1:7])

summary(model3)
  # Regressio significativa
  # Ajust del 0.72
  # Casi cap coefs 0 -> totes les vars menys una significatives

# Ajust del model3
  # Per a la mostra de training
rmse(model3$fit, train$siri)  # 4.399395
  # Per a la mostra de test
rmse(predict(pca, test, ncomp=7), test$siri) # 4.346266

# 2. Ho fem per a les variables centrades

trainx <- scale(train[-1], center = T, scale = F)
pca_centrat <- pcr(train$siri ~ trainx, validation="CV", ncomp=14)
summary(pca_centrat)

validationplot(pca_centrat, legendpos = "topright")
plot(RMSEP(pca_centrat), legendpos = "topright")

min(RMSEP(pca_centrat, "CV")$val[1:15])  # El mínim RMSE
# 4.563464
which.min(RMSEP(pca_centrat, "CV")$val[1:15]) # On es produeix aquest mínim
# Amb 11 components tenim el mínim
# Amb 7 ja s'obte un valor notable.

# Ajust del model pca centrat
# Per a la mostra de training
rmse(predict(pca_centrat, train, ncomp=7), train$siri)  # 4.399395
# Per a la mostra de test
mm <- apply(train[,-1],2,mean)
tx <- as.matrix(sweep(test[,-1],2,mm))
nx <- tx %*%  pca_centrat$loadings[,1:7]
pv <- cbind(1,nx) %*% coefficients(lm(siri ~ pca_centrat$scores[,1:7], train))
rmse(pv, test$siri)  # 4.346266

# Em dóna el mateix que si no centrem les variables

# Intent d'interpretacio (3 primeres PC)
matplot(1:14,pca_centrat$loadings[,1:3],type="l",xlab="Frequency",ylab="")


# (d) Partial least squares.
# No centrem les vars

pls <- plsr(siri ~ ., data=train, ncomp=14, validation="CV")
summary(pls)

validationplot(pls, legendpos = "topright")
plot(RMSEP(pls), legendpos = "topright")

min(RMSEP(pls, "CV")$val[1:15])  # El mínim RMSE
# 4.59284
which.min(RMSEP(pls, "CV")$val[1:15]) # On es produeix aquest mínim
# Amb 4 components tenim el mínim

# Model PLS
model4 <- lm(siri ~ pls$scores[,1:4], train)
summary(model4)
  # Regressio significativa
  # Ajust del 0.735
  # Cap coefs 0 -> totes les vars significatives

# Ajust del model4
# Per a la mostra de training
rmse(model4$fit, train$siri)  # 4.344006
# Per a la mostra de test
rmse(predict(pls, test, ncomp=4), test$siri) # 4.393


# (e) Ridge regression.
library(MASS)

# Primer centrem la variable resposta i les regressores per al GRUP TRAINING
trainx <- as.data.frame(scale(train, scale = FALSE)) # Centra les variables

# Fem la Ridge regression per un rang de valors de lambda
gridge <- lm.ridge(siri ~ .,lambda = seq(0,5e-4,1e-9), data =trainx) 

matplot(gridge$lambda,t(gridge$coef),type="l",lty=1,xlab=expression(lambda),ylab=expression(hat(beta)))

select(gridge)
abline(v=0.000270109, lwd = 3)

# Ajust per al grup training
which.min(gridge$GCV)
  # Coefs de la columna 270110

# Per fer les prediccions cal escalar les regressores (ja estan centrades) i afegir la mitjana
# (La Ridge Regression es fa amb totes les variables escalades i centrades)
ypred <- scale(trainx[,-1],center=FALSE,scale=gridge$scales)%*% gridge$coef[,270110] + mean(train$siri)
# I mirem l'ajust
rmse(ypred, train$siri)
  #  4.178651

# Ajust per al grup test
testx <-  as.matrix(sweep(test[,-1],2,mm)) # centro les observacions del grup test amb les mitjanes del grup training
ytpred <- scale(testx,center=F,scale=gridge$scales) %*% gridge$coef[,270110] + mean(train$siri)
rmse(ytpred, test$siri)
  # 4.395527