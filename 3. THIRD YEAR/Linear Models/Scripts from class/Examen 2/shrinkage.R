library(faraway)
data(fat,package="faraway")
plot(neck ~ knee, fat)
plot(chest ~ thigh, fat)
plot(hip ~ wrist, fat)
cfat <- fat[,9:18]
prfat <- prcomp(cfat) #permet calcular les components principals
dim(prfat$rot)
dim(prfat$x)
summary(prfat)
round(prfat$rot[,1],2) #combinacions lineals que ens dona la component principal 

#a vegades és necessari reescalar (estandaritzar), sobretot quan hi ha diferents unitats entre les variables
prfatc <- prcomp(cfat, scale=TRUE) 
summary(prfatc) 
round(prfatc$rot[,1],2) #per la primera component
round(prfatc$rot[,2],2) #per la segona component, la interpretacio de tamany i forma segueix sent molt semblant tot i que ara el tamany ha quedat molt repartit

#Convé veure si hi ha outliers i treure'ls abans de començar a aplicar el mètode -> és complicat quan tenim tantes dimensions 
#una possibilitat és calcular la distancia de mahalanobis de cada punt a un punt central
#Mahalanobis i no euclidia perquè, ja que la Mahalanobis té en compte la relació estadística entre les variables
#Com a cente podriem agafar les mitjanes de totes les variables
#com a sigma podriem agafar la matriu de variancies i covariancies
require(MASS) #conté una funcio que cov.rob calcula tant el valor central com la matriu de variancies i covariancies amb un metode robust
robfat <- cov.rob(cfat)
md <- mahalanobis(cfat, center=robfat$center, cov=robfat$cov) #punt central: robfat$center, matriu de variancies i covariancies: robfat$cov
#aquesta funcio torna el quadrat de la distancia de mahalanobis
boxplot(sqrt(md)) #observem outliers
#aquestes distancies al quadrat han de seguir una distribucio chi-quadrat
n <- nrow(cfat);p <- ncol(cfat)
plot(qchisq(1:n/(n+1),p), sort(md), xlab=expression(paste(chi^2," quantiles")), ylab="Sorted Mahalanobis distances")
abline(0,1) #observem que hi ha un conjunt de valors que s'aparten de la chi-quadrat
#Com a mínim els quatre últims s'haurien d'eliminar

lmoda <- lm(fat$brozek ~ ., data=cfat) 
sumary(lmoda)
lmodpcr <- lm(fat$brozek ~ prfatc$x[,1:2])
sumary(lmodpcr)
lmodr <- lm(fat$brozek ~ scale(abdom) + I(scale(ankle)-scale(abdom)), data=cfat)
sumary(lmodr)


data(meatspec, package="faraway")
trainmeat <- meatspec[1:172,]
testmeat <- meatspec[173:215,]
modlm <- lm(fat ~ ., trainmeat)
summary(modlm)$r.squared
rmse <- function(x,y) sqrt(mean((x-y)^2)) #arrel quadrada del mse
rmse(fitted(modlm), trainmeat$fat)
rmse(predict(modlm,testmeat), testmeat$fat) #es fa evident que el metode dels minims qudarats ajusta perfectament quan es tracta de les dades que m han donat, però en el el moment que tinc unes altres dades el metode no serveix
modsteplm <- step(modlm)
rmse(modsteplm$fit, trainmeat$fat)
rmse(predict(modsteplm,testmeat), testmeat$fat)
meatpca <- prcomp(trainmeat[,-101])
round(meatpca$sdev,3)
matplot(1:100, meatpca$rot[,1:3], type="l", xlab="Frequency", ylab="", col=1) #el colze em diu el numero d'elements que necessito


require(pls)
pcrmod <- pcr(fat ~ ., data=trainmeat, ncomp=50)
rmse(predict(pcrmod, ncomp=4), trainmeat$fat)
plot(modlm$coef[-1],xlab="Frequency",ylab="Coefficient",type="l") #coeficients pcr per las 100 variables (estan distribuits aleatoriament)
coefplot(pcrmod, ncomp=4, xlab="Frequency",main="")
plot(meatpca$sdev[1:10],type="l",ylab="SD of PC", xlab="PC number") #continuitat en aquests coeficienta, alguns son negatius, altres positius, i alguns son o estan aprop de ser nuls.
rmse(predict(pcrmod, testmeat, ncomp=4), testmeat$fat)


pcrmse <- RMSEP(pcrmod, newdata=testmeat)
plot(pcrmse,main="")
which.min(pcrmse$val)
pcrmse$val[28]
set.seed(123)
pcrmod <- pcr(fat ~ ., data=trainmeat, validation="CV", ncomp=50) #cv propi conjunt de dades com test i training a la vegada
pcrCV <- RMSEP(pcrmod, estimate="CV")
plot(pcrCV,main="")
which.min(pcrCV$val)
ypred <- predict(pcrmod, testmeat, ncomp=18)
rmse(ypred, testmeat$fat)

#PCR millor per entendre la regressió
#PLS millor per interpretacio (predir)


set.seed(123)
plsmod <- plsr(fat ~ ., data=meatspec[1:172,], ncomp=50,  validation="CV") 
coefplot(plsmod, ncomp=4, xlab="Frequency")
plsCV <- RMSEP(plsmod, estimate="CV")
plot(plsCV,main="")
ypred <- predict(plsmod,ncomp=15)
rmse(ypred, trainmeat$fat)
ytpred <- predict(plsmod, testmeat, ncomp=15)
rmse(ytpred, testmeat$fat)

# RIDGE REGRESSION
#minimitzar els errors quadratics tal que la suma dels errors al quadrat estigui limitada
#si lambda és 0 tenim la regrewsió ordinaria de sempre
#a mesura que lambda augmentar la restricció és més dura
#si sumem lamda estem trencant que l'esperança sigui beta->PROBLEMA: PER ARREGLAR EL PROBLEMA DE LA VARIANCIA EL QUE FA ES INTRODUIR BIAIX
require(MASS)
rgmod <- lm.ridge(fat ~ ., trainmeat, lambda = seq(0, 5e-8, len=21))
matplot(rgmod$lambda, coef(rgmod), type="l", xlab=expression(lambda),ylab=expression(hat(beta)),col=1)
which.min(rgmod$GCV)
abline(v=1.75e-08)
ypred <- cbind(1,as.matrix(trainmeat[,-101])) %*% coef(rgmod)[8,]
rmse(ypred, trainmeat$fat)
ypred <- cbind(1,as.matrix(testmeat[,-101])) %*% coef(rgmod)[8,]
rmse(ypred, testmeat$fat)
c(ytpred[13],ypred[13],testmeat$fat[13])
rmse(ypred[-13], testmeat$fat[-13])
require(lars)
data(state)
statedata <- data.frame(state.x77,row.names=state.abb)
lmod <- lars(as.matrix(statedata[,-4]),statedata$Life)
plot(lmod)
set.seed(123)
cvlmod <- cv.lars(as.matrix(statedata[,-4]),statedata$Life)
cvlmod$index[which.min(cvlmod$cv)]
predict(lmod,s=0.65657,type="coef",mode="fraction")$coef
coef(lm(Life.Exp ~ Population+Murder+HS.Grad+Frost, statedata))
trainy <- trainmeat$fat
trainx <- as.matrix(trainmeat[,-101])
lassomod <- lars(trainx,trainy)
set.seed(123)
cvout <- cv.lars(trainx,trainy)
cvout$index[which.min(cvout$cv)]
testx <- as.matrix(testmeat[,-101])
predlars <- predict(lassomod,testx,s=0.0101,mode="fraction")
rmse(testmeat$fat, predlars$fit)
predlars <- predict(lassomod, s=0.0101, type="coef", mode="fraction")
plot(predlars$coef,type="h",ylab="Coefficient")
sum(predlars$coef != 0)

#LASS necesita una MATRIU amb les variables regressores i la variable resposta