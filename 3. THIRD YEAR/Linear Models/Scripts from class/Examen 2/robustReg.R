library(faraway)

# OLS (Regressio minimo cuadratica ordinaria (la des sempre))
data(gala) #tenim 30 illes i 7 variables. Species és la var resposta
gl <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,gala)
summary(gl)
  # es detecten dos...elvation i adjacent

# Huber method
library(MASS)
gr <- rlm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,gala) #robust lineal model
summary(gr)
  #no té sentit calcular p-valors
  #t-value= quocient entre el valor i l'error estandar (no es una veritable t-student)
  #7.1320 i -4.7648 no són valors normals
  #La regressio robusta pel metode de huber ens confirma que aquestes dos variables son les més significatives en aquesta regressio

# Tobar les illes que tenen un pes més petit
wts <- gr$w
names(wts) <- row.names(gala)
head(sort(wts), 10)
  #hi ha algunes illes que tenen un valor de 1 (contribueix tota la illa en la regressió)
  #SantaCruz -> la seva contribució en la regressio robusta es poca

# La M-estimacio no funciona bé amb punts de alt leverage
# No podem substituir el metode ordinari per un metode robust. pero serveixen per confirmar

# LAD regression
library(quantreg)
attach(gala)
gq <- rq(Species ~Area+Elevation+Nearest+Scruz+Adjacent)
summary(gq) #si atrapa el 0 no es significatiu -> la unica important és Elevation. Adjacent és no significativa
detach(gala)


# Least Trimmed Squares (LTS) -> fa servir el metode dels minims quadrats sense tenir en compte els residus més grans (els elimina)
    #en funcio de q farem que el metode sigui més o menys "resistent"
library(MASS)
set.seed(123)
g <- ltsReg(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,gala)
coef(g)

library(robustbase)
g <- ltsReg(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,gala)
  #ltsReg depen del packet MASS
coef(g)
# They strongly recommend using lmrob() instead of ltsReg
g <- lmrob(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)
coef(g)
# This function computes an MM-type regression estimator as described in Yohai (1987)
# and Koller and Stahel (2011). By default it uses a bi-square redescending score function,
# and it returns a highly robust and highly efficient estimator (with 50% breakdown point
# and 95% asymptotic efficiency for normal errors). 

ltsmod <- ltsreg(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,gala, nsamp="exact")
coef(ltsmod)

# Si volem intervals de confiança hauriem de utilitzar un métode "bustra" #també es podria haver fet amb la lmrob
bcoef <- matrix(0, 1000, 6)
for(i in 1:1000){
  newy <- predict(ltsmod) + residuals(ltsmod)[sample(30, rep=T)] #residuals=permutacio dels 30 valors amb repetició
  brg <- ltsreg(newy ~ Area + Elevation + Nearest + Scruz + Adjacent, gala, nsamp="best") #best per evitar que els temps de computació siguin molt elevats
  #els coeficients s'han de guardar dintre de la matriu:
  bcoef[i, ] <- brg$coef
}

plot(density(bcoef[,6])) #també podria ser un histigrama
# es tracta de calcular quins son els valors que deixen 2.5 per dalt o per baix
qq <- quantile(bcoef[ ,6], probs=c(0.025, 0.975)) #interval de confiança al 95%
abline(v=qq[1], lty=2)
abline(v=qq[2], lty=2)

#CAS EN QUE ELS ERROS NO TENE COVARIANCIA 0 I LA VARIANCIA DE TOTS ELLS NO ES LA MATEIXA
#TINDREM UNS MINIMS QUADRAT GENERALITZATS QUAN la matriu de variancies covariancies sigma^2*SIGMA
#SIGMA matriu coneguda (M tumbada)
#Descomposició de Choleski t(S)S (M tumbada)
