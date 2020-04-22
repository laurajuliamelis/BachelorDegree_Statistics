############### Informacio util ###############

# Varialbe resposta: var dependent
# Variable regressora: var independent
# MODELS : lm(VarDependent ~ VarIndependent, data= dades)

# Si p-valor < 0.05 -> REBUTJEM NUL LA (=si p-valor > 0.05 no hi ha prou rao per rebutjar la hipotesi de igualtat(H0))

############### Carregar dades ###############
# DADES ".RData"
load("longnose.Rdata")

# DADES ".xls"
library(gdata)
dades <- read.table('clipboard', header =T, dec=",")

# DADES ".txt"
dades <- read.table("bloodpress_A.txt", header=T)

#DADES del "faraway"
library(faraway)
data(sat)

#DADES del "car"
library(car)
data(Ornstein)

# DADES en pagines web
link <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wpbc.data"
dades <- read.csv(link, header=FALSE, na.strings="?")

# *************** Si la base de dades conte una columna amb el "ID" del individus, eliminarla tot comencar (dades <- dades[,-1])
# ------------------------------------------------------------------------------------------------------------------------------#
################ 
## PREGUNTA 1 ## 
################
# DONAT UN PROBLEMA CONCRET:

## (A) CALCULEU L HIPERPLA DE REGRESSIO I EL COEFICIENT DE CORRELACIO MULTIPLE DE AV.POWER SOBRE LES ALTRES VARIABLES.
##     QUINA ES LA VARIANCIA ESTIMADA DE L ERROR?

g<- lm(AV.Power ~ ., data= dades) # Hiperpla
# ** NO AV.Power, nomes regressio lineal multiple sobre una variable en concret de les altres variables:
g <- lm(variableconcreta ~ ., data= dades)

ss <- summary(g)
sqrt(ss$r.squared) # El coef de correl multiple es equivalent a l arrel quadrada del coef de determinacio

ss$sigma^2 # var error


## (B) ES UN MODEL AMB UN BON AJUST? VOL DIR AIXO QUE ES SIGNIFICATIVA LA REGRESSIO? EXPLICA.
# ** Comentar tambe el COEFICIENT DE DETERMIANCIO AJUSTAT:
ss$adj.r.squared

# El coeficient de determinacio es R^2 = "Multiple R-squared" del ss (quan R^2 proper a 1 o major a 0.5, esta bastant be, 
# si es baix no). A mes, el R^2 ajustat encara es mes baix/alt -  estranya que suigui tan diferent.

# La regressio es significativa ja que el p-valor es < 0.05. Aixo significa que els coeficients de les variables regressores 
# no son tots zero i per tant, la prediccio pot ser util. No te res a veure amb l'ajust del model.

## QUINA ES LA HIPOTESI?
# Beta_i=0 --> totes les variables independents=0 (EX.  H0: sex=status=incom=verbal=0)


## (C) DIAGNOSI DEL MODEL EN ELS SEGUENTS PUNTS:

# i) VARIANCIA CONSTANT DELS ERRORS (= HOMOCEDASTICITAT)
plot(g, which = 3)
# BE: Esta be ja que s observa una recta horitzontal (pe. si hi ha alguna curva petita dir que es perq hi ha poques dades)
# MALAMENT: No s'observa una curva tan marcada com en el primer grafic, pero lo ideal es una linia horitzontal recta.

summary(lm(abs(residuals(g)) ~ fitted(g)))
#p-valor > 0.05 --> acceptem homocedesticitat (no sembla que hi hagi cap problema d'heterocedasticitat)

# ii) HIPOTESI DE NORMALITAT.
# Grafic: QQ-plot
plot(g, which = 2)
# Test formal
shapiro.test(residuals(g))
# p-value < 0.05 -> Rebutgem normalitat

# ** Probar si p-v < 0.05: "Segurament el problema es causat pels outliers"
which(rownames(dades) %in% c("875634", "859223")) # punts mes allunyats que suerten al grafic q-q. Suposem que ens torna 12, 22
shapiro.test(residuals(g)[-c(12,22)]) # Llevant els punts ens surtira p-valor > 0.05

# iii) PUNTS AMB INFLUENCIA POTENCIAL (LEVERAGE)
plot(g, which = 5)
plot(hatvalues(g), type="h")

par <- sum(hatvalues(g)) # nombre varialbes (parametres)
cutoff <- 2* par/nrow(dades)  # nrow(dades) = num observacions (N)
which(hatvalues(g) > cutoff) # Aquests punts son POTENCIALMENT INFLUENTS.
# SOL: Veiem que la observacio 21 te una influencia potencial elevada / No hi ha punts amb un leverage alt.


# iv) OUTLIERS
plot(g, which = 3)
plot(abs(rstudent(g)))
abline(h = 2, col = "red") # fixar la linea segons veiem

which(abs(rstudent(g)) > 2) # comparar amb al valor fixat abans
# SOL: Els punts 234787 i 676457 son atipics.

# --- ALTERNATIVA: 
library(car)
outlierTest(g)  #Els punts 14 i 16 son atipics.


# v) PUNTS INFLUENTS
# Influencia real:
# - sobre els coeficients:       # - sobre les prediccions:
# - DISTANCIA DE COOK            # - DISTANCIA DE COOK
# - DFBETAS                      # - DFFITS

# DISTANCIA DE COOK
C <- cooks.distance(g) 
n <- nrow(dades) # Nun obeservacions
k <- length(g$coefficients)  # Num parametres
ptall <- 4/(n-k-2) # punt de tall

plot(g, which = 4) # criteri: com que no es clar, el millor es dibuixar-ho.
abline(h=ptall)

which (c > ptall)

# -- ALTERNATIVA/CORROBORAR:
library(faraway)
halfnorm(cooks.distance(g), 2, ylab = "Distancies de Cook")
influence.measures(g) #Sembla evident que l'observacio 21 es influent.

# - DFBETAS
dfbetas(g)
l <- 2/sqrt(n) # limit 2/sqrt(n)

which(abs((dfbetas(g))[,1])>l)  
which(abs((dfbetas(g))[,2])>l)  
which(abs((dfbetas(g))[,3])>l)  
which(abs((dfbetas(g))[,4])>l)  
which(abs((dfbetas(g))[,5])>l)  

# - DFFITS
dffits(g)
l <- 2*sqrt((k + 1)/n)# limit 2*sqrt((k + 1)/n)

which(abs((dffits(g)))>l) 


# vi) PROBLEMES DE MULTICOLINEALITAT?
# 1. Deteccio mirant la correlacio entre les variables regressores
round(cor(teengamb[,-1]),3) # li traiem la variable independent. Comentar si son molt altes o no, quantes, etc.

# 2. Deteccio pels FIVS
vif(g) # hem de mirar si son menors de 10 (DE 4!!!!!!)
# SOL: hi ha molts/pocs/deu vif mes grans que 10. Tenim un problema greu de multicolinealitat.



## (D) QUE PODEM DIR DEL PUNT 21? EN QUE MILLORE EL MODEL SI L ELIMINEM? I QUE PODEM DIR DEL 12?
# 1. Primer diem tot el que sabem del punt eliminat:
# El punt 21 es un: outlier pero no un punt influent / un punt d influencia potencial i real molt alta (per lo que es un
# punt que esta lluny de la resta i la seva presencia modifica molt l estimacio dels parametres)

# Si el punt es outlier i l eliminem, guanyarem en l ajust. 
# Si es un punt influent i l'eliminem, el model sera mes robust. 

# 2. creem un nou model eliminant el punt
g2 <- lm(gamble ~ ., data = teengamb[-24,]) # g2 <- lm(AV.Power ~ .,data = teengamb[-24,]) 
summary(g2) #mirem possibles canvis com var.est.error o l'ajust... 

# Residus
plot(g2, which = 1)

# Mirem la normalitat
shapiro.test(residuals(g2)) # p-value > 0.05 -> ACCEPTEM NORMALITAT

# Mirem homoscedasticitat
ncvTest(g2) # non-constant error variance test

# (E) CONTRASTEU SI ELS COEFICIENTS DE REGRESSIO DE LES VARIABLES VAR1 I VAR2 SON IGUALS.
# Hipotesi:  H0: beta_var1 = beta_var2
# Contrastem:
g3 <- update(g, .~. - var1 -var2+I(var1+var2)) # g3 <- lm(AV.Power ~ var3 + var4 + etc + I(var1 +var2), data =dades)
anova(g,g3) # si p-valor > 0.05 no hi ha prou rao per rebutjar la hipotesi de igualtat.

# (F) SON SIGNIFICATIVES CONJUNTAMENT LES VARIABES VAR 1 I VAR 2 O PODEM PRESCINDIR DE ELLES?
g4 <- update(g, .~. - var1 -var2)
anova(g, g4) # si p-valor > 0.05 no semblen significatives i podem prescindir de elles i treballar amb un model mes simple

# (G) PREDICCIO EN FORMA D IC DE NOVES DADES QUE ENS DONIN
#A.
predict(g, newdata = data.frame(sex = 0, status = 60, income = 10, verbal = 11), interval = 'prediction')

#B.
confint(g, level=0.95) #per defecte el fa de 0.95, si volem nomes d'una variable
#confint(model,'variableselec')

# (H) CALCULEU LA CORRELACIO PARCIAL ENTRE AV.POWER I VAR1 SI ELIMINEM LA INFORMACIO DE LA RESTA DE VARIABLES (P.Tlac, DMax, P.Tlac.ll, P.4mM)
e1 <- residuals(lm(AV.Power ~ P.Tlac + DMax + P.Tlac.ll + P.4mM, data = dades))
e2 <- residuals(lm(var1 ~ P.Tlac + DMax + P.Tlac.ll + P.4mM, data = dades))
cor(e1, e2) # 0.2602 la correlacio parcial entre aquestes dues variables es molt baixa.

# ------------------------------------------------------------------------------------------------------------------------------#
################ 
## PREGUNTA 2 ## 
################
# DONADA UNA BASE DE DADES PER A UN PROBLEMA:

# ATENCIO!
# Si s ens diu "considereu el model linal OLS" abans, caldra fer:

# 1. Model de regressio OLS
g <- lm(vardependent ~ ., data = dades)
summary(g)

## (A) TROBEU EL MILLOR PODEL PER DOS METODES DIFERENTS DE SELECCIO DE VARIABLES.
#1. Stepwise
sg <- step(g)
summary(sg) 

#2. Metode Cp de Mallows
install.packages("leaps")
library(leaps)
b <- regsubsets(AV.Power ~ ., data = dades) # El model que tinguem
rs <- summary(b)
# Comentar si un model ens dona massa variables, moltes mes que l altre. Ens quedarem el mes simple.

## (A1) QUINES SON LES VARIABLES SELECCIONADES?
# Ens quedem les variables que ens dona el summary a l apartat "coeficients:"

## (A2) COEFICIENTS DE DETERMINACIO AJUSTATS D AQUETS MODELS? COMPAREULOS AMB EL DEL MODEL COMPLET.
# Model complet (totes les variables):
ss$adj.r.squatrd # recordar que: ss <- summary(g)

# Model de k variables. (k= numero variables que s han seleccionat en A1)
rs$adjr2[k]

# Comparem:
# Primer mirem els dos valors obtinguts si el model amb k variables te un coficient major, diem que "L ajust del model amb
# k variables millore el coeficient de determinacio ajustat"

# Despres, mirem si corregeix problema de multicolinealitat:
gs <- lm(variableconcreta ~ var1 + var2 + etc, data =dades) # nomes amb les variables seleccionades a A1
vif(gs) # ha d haver valors menors de 10. 
# SOL: "Corregeix el problema de multi/ no el corregeis per la qual cosa s hauria de reudir encara mes el nombre de variables)

## (A3) CALCULA IC 95% PER AL COEFICIENT DE REGRESSIO DE LA VARIABLE VAR1 EN TOTS ELS MODELS (COMPLET I SELECCIONAT). 
confint(g, "VAR1") # o be, confint(g)["VAR1", ]
confint(gs, "VAR1") # Si es mes estret diem que "Hem guanyat (molt/poc) en eficiencia"

## (B) UNA ALTRA POSSIBILITAT ES FER SERVIR LA Ridge Regression. QUINS SON ELS COEF OBTINGUTS? EXPLICA AVANTATGES/INCONVENIENTS.
library(MASS)
gridge <- lm.ridge(AV.Power ~ ., data = dades, lambda = seq(0, 50, 1)) # o  lambda = seq(0, 300, 1)
matplot(gridge$lambda, t(gridge$coef), type = "l", lty = 1, xlab = expression(lambda), ylab = expression(hat(beta)))
select(gridge)
## modified HKB estimator is 1.207 
## modified L-W estimator is 1.454 
## smallest value of GCV  at 4=k!!!!!!!!          # ATENCIO! CANVIAR AQUETS VALORS PER CONTINUAR

abline(v = k)
gridge <- lm.ridge(AV.Power ~ ., data = dades, lambda = k)
coef(gridge)

# Aquest metode permet conservar totes les variables en el model. Elimina el problema de la manca d eficiencia de les
# estimacions, a canvi d un petit increment en el biaix.
# En aquest problema sembla que es millor un metode de seleccio de variables.

# (C) AMB EL MODEL REDUIT AIC EL PUNT 97655 ENCARA FA NOSA(ES PROBLEMATIC). AJUSTEU UN MODEL PER UN METODE ROBUST PER ARREGLARHO
gs <- step(g) # o be, g <- lm(AV.Power ~var1+ var2 + var3, dades) (les variables del model reduit obtingut) 
plot(gs, which=4) 

# OPCIO 1: Metode least trimmed squares(LTS) 
gr <- ltsreg(eval(gs$call[[2]]), data = dades)
coef(gr)

#library(robustbase) ->   gr <- lmrob(eval(gs$call[[2]]), data = dades)   -> coef(gr)


# OPCIO 2: Metode de Huber
gr <- rlm(AV.Power ~ P.Tlac.ll + DMax + Rise.1.PB, data = dades)
summary(gr)

## (D) AJUSTEU UN MODEL PER A CADASCU DEL SEGUENTS METODES: (MODELS ROBUSTOS)
#1. MINIMS QUADRATS OLS
g_ols <- lm(vardependent ~ ., data = dades) # Sense el punt 24: data = dades[-24,]
summary(g_ols)
# SI DEMANEN EXPLICACIO: Significacio global bona/dolenta. R^2 bo.
# Cal fer un analisi dels residus per veure si hi ha problemes:
plot(g_ols) 

#2. METODE DE HUBER
library(MASS)
g_huber <- rlm(vardependent ~ ., data = dades)
summary(g_huber) # Mirem possibles canvis: "Lleugera modificacio dels valors dels coeficients, mateixes conclusions respecte a la significacio de les vars"
plot(g_huber) #surten varios grafics

#3. LEAST TRIMMED SQUARES  (LTS)
g_lts <- ltsreg(vardependnet ~ ., data = dades, nsamp = "exact")
coef(g_lts)
# Posem 'nsamp = "exact"' ja que aixi es demana una cerca mes exhaustiva, tot i que per a grans conjunts de dades 
# el temps de computacio es alt.

# mirem possibles canvis en els coeficients

plot(fitted(g_lts),residuals(g_lts),xlab="Fitted",ylab="Residuals")


# 4. LEAST ABSOLUT DEVIATIONS (LAD)
library(quantreg)
g_lad <- rq(vardependnet ~ ., data = dades)
summary(g_lad)

# Canvien bastant els valors d alguns coeficients:
# - VAR1 -> passa de 3.24 a 0.45
# - VAR2 -> passa de 1.29 a 0.57 (es redueix la meitat)

# A mes, ara tots son significatius mentre que amb OLS, la VAR1 no ho era.

# ------------------------------------------------------------------------------------------------------------------------------#
################ 
## PREGUNTA 3 ## 
################
# ENUNCIAT:
# En un Analisi de la Variancia un dels pitjors problemes que ens podem trobar es la manca d igualtat de les variancies 
# entre situacions experimentals, es a dir, la heterocedasticitat. En el cas concret d un ANOVA d un factor (one-way ANOVA)
# hi ha diverses solucions possibles com: (i) fer transformacions de la variable resposta, (ii) utilitzar la correccio de 
# Welch, (iii) solucio boostrap, etc. 
# Considerem les seguents dades amb tres situacions experimentals (grup):
# L'objectiu es comparar els tres grups (amb presencia d'heterocedasticitat).

yA <- c(0.178, 0.195, 0.225, 0.294, 0.315, 0.341, 0.36, 0.363, 0.371, 0.398, 0.407, 0.409, 0.432, 0.494, 0.719)
yB <- c(0.11 , 0.111, 0.204, 0.416, 0.417, 0.441, 0.492, 0.965, 1.113, 1.19, 1.233, 1.505, 1.897)
yC <- c(0.106, 0.114, 0.143, 0.435, 0.448, 0.51 , 0.576, 0.588, 0.608, 0.64, 0.658, 0.788, 0.958)

## (A) Dibuixeu un grafic que mostri les diferencies en la dispersio de les dades. Calculeu les variancies per a cada grup
# i comproveu que la rao entre la variancia mes gran i la mes petita es superior a 4. Donat que la variancia es sensible 
# a outliers, potser es millor calcular el rang interquartilic IQR per a cada grup.
y <- c(yA, yB, yC)
nA <- length(yA) 
nB <- length(yB)
nC <- length(yC)
grup <- as.factor(c(rep("A", nA), rep("B", nB), rep("C", nC))) 

# 1. Boxplot multiple (per mostrar les diferencies en la dispersio):
boxplot(y ~ grup) # S observa clarament que hi ha un grup mes dipers que els altres.

# 2. Calcular variancies per cada grup:
varA <- var(yA)
varB <- var(yB)
varC <- var(yC)
rao <- varB/varA # > 4
# ** Si la rao entre la variancia mes gran i mes petita es superior a 4 tindrem un problema d'heterocedasticitat. 

# 3. Rangs interquantilics:
# La variancia es sensible a outliers, per tant, pot ser millor treballar amb el rang interquartilic (funcio iqr)
iqrA <- IQR(yA)
iqrB <- IQR(yB)
iqrC <- IQR(yC)

## (B) Construiu el vector y de respostes i el factor grup i apliqueu un test de Levene per contrastar la homocedasticitat 
##     de les dades. Quina es la conclusio?

library(car)
leveneTest(y ~ grup)  # Si ens surt significatiu (p-valor < 0.05)-> rebutgem la homocedasticitat.

# El test de Levene per a la homogeneitat de la variancia surt significatiu, per tant, rebutgem la hipotesi d'homocedasticitat.
# CONCLUSIO: 
# El model lineal no funciona perque no compleix les condicions de gauss markov.
# Solucio: fer servir minims cuadrats ponderats.


## (C) Ara aplicarem la solucio de minims quadrats ponderats (weighted least squares):

# PASOS:
# w1 invers de la variancia de cada grup repetida 15 vegades, 13 vegades, 13 vegades (vector de pesos)
# Aplicar el model lm pero amb el parametre weights
# Fer el mateix amb l'invers del rang intercuartilic

# (i) Construiu un vector de pesos w1 amb l'invers de la variancia del grup per a cada observacio. Calculeu el model lineal
#     amb el parametre weights=w1 i doneu la taula ANOVA. Quina es la conclusio? Compareu el tercer grafic de l'analisi dels 
#     residus del model sense pesos i amb pesos.
w1 <- c(rep(1/varA, nA), rep(1/varB, nB), rep(1/varC, nB))
g1 <- lm(y ~ grup, weights=w1)
anova(g1)

g <- lm(y ~ grup)

par(mfrow=c(1,2))
plot(g1, which=3)
plot(g, which=3)

# (ii) Construiu un vector de pesos w2 amb l'invers del rang interquartilic IQR del grup per a cada observacio. Calculeu 
#      el model lineal amb el parametre weights=w2 i doneu la taula ANOVA. Quina es la conclusio?
w2 <- c(rep(1/iqrA, nA), rep(1/iqrB, nB), rep(1/iqrC, nB))
g2 <- lm(y ~ grup, weights=w2)

anova(g2)



## (D) Calculeu un model robust amb la funcio rlm del paquet MASS i el parametre weights=w2. Calculeu la taula ANOVA amb
##     la funcio Anova del paquet car. Quina es la conclusio?
library(MASS)
grlm <- rlm(y ~ grup, weights=w2)

# funcio ANOVA del paquet car -> no es la funcio anova habitual. Ens diu si hi ha diferencies entre el tres grups.
library(car)
Anova(grlm) # Rebutgem la hipotesis de igualtat entre els tres grups (p-valor < 0.05?)

## (E) Calculeu un model robust rlm1 amb la funcio lmrob del paquet robustbase i fixeu el parametre weights=w2.

## Considereu el model rlm0 <- lmrob(y ~ 1, weights=w2) i compareu-lo amb l'anterior amb anova(rlm1, rlm0). 
## Quina es la conclusio del test de Wald?

# ATENCIO!: 
# Test de wald: alternativa que calcula un model robust diferent. Es vol veure quin es el resultat en aquest altre sistema.
# S'ha de comparar el model complet amb el model que nomes te la constant com a regresora (el model 0) per veure si hi ha diferencies entre els 3 grups
library(robustbase)
rlm1 <- lmrob(y ~ grup, weights=w2)

rlm0 <- lmrob(y ~ 1, weights=w2)

anova(rlm1, rlm0)
# Acabem de comparar el model complet amb el model que nomes te la constant per veure si hi ha diferencies entre els 3
# grups (Test de Wald). Aquest test surt significatiu, per tant, es confirma que si que hi ha diferencies entre els 
# tres grups.

# ------------------------------------------------------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------------------------------------------------------#

################ 
## PROBLEMA 1 ## 
################
# POSSIBLES PREGUTNES EXTRES A L EXERCICI 1
library(faraway)
data(sat)

# a) Feu un resum numeric i grafic de les dades i comenteu tot el que penseu que es rellevant
summary(sat)


# b) Ajusteu un model de regressio amb la puntuacio total com a variable resposta i les variables expend, ratio i salary
#    com predictores. Contrasteu la hipotesi βsalary = 0. Contrasteu la hipotesi βsalary = βratio = βexpend = 0. 
#    Hi ha alguna predictora que tingui efecte sobre la resposta?
g1 <- lm(total ~ expend + ratio + salary, sat)
plot(g1)

summary(g1) # La regressio es significativa (F-statistic: 4.066 on 3 and 46 DF,  p-value: 0.01209)

# beta_Salary = 0
# salary        -8.823      4.697  -1.878   0.0667 (depen del nivell de significacio es diu que es significativa o que no)

# Predictora amb efecte sobre la resposta?
# NO. Nomes salary i depen del alpha escollit


# c) Afegiu la variable takers com a regressora. Contrasteu ara la hip`otesi βsalary = 0. Compareu aquest model amb el
#    model anterior amb un test F. Aquest darrer test  ́es equivalent a un test t, quin  ́es aquest test?
g2 <- lm(total ~ expend + ratio + salary + takers, sat)

summary(g2) # S'accepta beta_salary = 0

anova(g1, g2) # Acceptem el model 2 (Es equivalent a beta_takers = 0)


# d) Proporcioneu un interval de confian ̧ca al 95% per al coeficient βsalary en el darrer model.
confint(g2, "salary")


# ------------------------------------------------------------------------------------------------------------------------------#
################ 
## PROBLEMA 2 ## 
################
# POSIBLE PREGUNTA 4
# ENUNCIAT: Feu servir la resta d observacions com a mostra d’ajust (training sample) per ajustar els seguents models:
library(faraway)
data(fat)

# Escollim la mostra training i la test
index <- seq(10, 250, by = 10)

# Extraiem les dades i eliminem les vars que no son regressores
train <- fat[-index, -c(1,3,8)]
test <- fat[index, -c(1,3,8)]

# Funcio per comprovar l ajust dels models
rmse <- function(x,y) sqrt(mean((x-y)^2))

# MODELS!!!!

# (A) Regressio lineal amb totes les predictores.
model1 <- lm(siri ~. , data = train)
summary(model1) # Regressio significativa // Ajust del 0.74 //  Molts coefs 0 -> moltes vars no significatives

vif(model1) # Hi han alguns majors de 10 -> normal amb 14 regressores

# Ajust del model1:

# Per a la mostra de training
rmse(model1$fit, train$siri)  # 4.179

# Per a la mostra de test
rmse(predict(model1, test), test$siri) # 4.395559

# SOLUCIO: Son del mateix ordre. L error real es aproximadament igual al que el model es suggereix.


# (B) Regressio lineal amb les variables seleccionades per AIC.
model2 <- step(model1, direction = "both") # (siri ~ age + weight + neck + abdom + thigh + forearm + wrist, data = train)
# Ens quedem amb 7 regressores de les 14 que teniem

summary(model2) # Regressio significativa //  Ajust del 0.74 // Cap coefs 0 -> totes les vars significatives

# Ajust del model2

# Per a la mostra de training
rmse(model2$fit, train$siri)  # 4.217687

# Per a la mostra de test
rmse(predict(model2, test), test$siri) # 4.342456

# SOLUCIO:  Del mateix ordre. Casi igual al model1 i amb 7 variables menys. Sembla que la simplificacio es possible.


# (C) Regressio per components principals
# Sempre conve centrar les variables, i a mes en el nostre cas es tenen variables amb unitats diferents -> optarem per escalar les vars

# 1. Ho fem amb les vars originals
library(pls)
pca <- pcr(siri ~ ., data = train, validation="CV", ncomp=14)
summary(pca)

validationplot(pca, legendpos = "topright")
plot(RMSEP(pca), legendpos = "topright")

min(RMSEP(pca, "CV")$val[1:15])  # El minim RMSE (4.598189)

which.min(RMSEP(pca, "CV")$val[1:15]) #On es produeix aquest minim -> Amb 11 components tenim el minim, amb 7 ja s'obte un valor notable.

# Model PCA
model3 <- lm(train$siri ~ pca$scores[,1:7])

summary(model3) # Regressio significativa //Ajust del 0.72 // Casi cap coefs 0 -> totes les vars menys una significatives

# Ajust del model3

# Per a la mostra de training
rmse(model3$fit, train$siri)  # 4.399395

# Per a la mostra de test
rmse(predict(pca, test, ncomp=7), test$siri) # 4.346266


# 2. Ho fem per a les variables centrades.
trainx <- scale(train[-1], center = T, scale = F)
pca_centrat <- pcr(train$siri ~ trainx, validation="CV", ncomp=14)
summary(pca_centrat)

validationplot(pca_centrat, legendpos = "topright")
plot(RMSEP(pca_centrat), legendpos = "topright")

min(RMSEP(pca_centrat, "CV")$val[1:15])  # El minim RMSE (4.563464)
which.min(RMSEP(pca_centrat, "CV")$val[1:15]) # On es produeix aquest minim. Amb 11 components tenim el menim, amb 7 ja s'obte un valor notable.

# Ajust del model pca centrat

# Per a la mostra de training
rmse(predict(pca_centrat, train, ncomp=7), train$siri)  # 4.399395

# Per a la mostra de test
mm <- apply(train[,-1],2,mean)
tx <- as.matrix(sweep(test[,-1],2,mm))
nx <- tx %*%  pca_centrat$loadings[,1:7]
pv <- cbind(1,nx) %*% coefficients(lm(siri ~ pca_centrat$scores[,1:7], train))
rmse(pv, test$siri)  # 4.346266

# Em dona el mateix que si no centrem les variables.

# Intent d'interpretacio (3 primeres PC)
matplot(1:14,pca_centrat$loadings[,1:3],type="l",xlab="Frequency",ylab="")


# (D) Partial least squares.
# No centrem les vars
pls <- plsr(siri ~ ., data=train, ncomp=14, validation="CV")
summary(pls)

validationplot(pls, legendpos = "topright")
plot(RMSEP(pls), legendpos = "topright")

min(RMSEP(pls, "CV")$val[1:15])  # El minim RMSE (4.59284)
which.min(RMSEP(pls, "CV")$val[1:15]) # On es produeix aquest minim. Amb 4 components tenim el minim

# Model PLS
model4 <- lm(siri ~ pls$scores[,1:4], train)
summary(model4)  # Regressio significativa  // Ajust del 0.735 // Cap coefs 0 -> totes les vars significatives

# Ajust del model4
# Per a la mostra de training
rmse(model4$fit, train$siri)  # 4.344006

# Per a la mostra de test
rmse(predict(pls, test, ncomp=4), test$siri) # 4.393


# (E) Ridge regression.
library(MASS)

# Primer centrem la variable resposta i les regressores per al GRUP TRAINING
trainx <- as.data.frame(scale(train, scale = FALSE)) # Centra les variables

# Fem la Ridge regression per un rang de valors de lambda
gridge <- lm.ridge(siri ~ .,lambda = seq(0,5e-4,1e-9), data =trainx) 

matplot(gridge$lambda,t(gridge$coef),type="l",lty=1,xlab=expression(lambda),ylab=expression(hat(beta)))

select(gridge)
abline(v=0.000270109, lwd = 3)

# Ajust per al grup training
which.min(gridge$GCV) # Coefs de la columna 270110

# Per fer les prediccions cal escalar les regressores (ja estan centrades) i afegir la mitjana
# (La Ridge Regression es fa amb totes les variables escalades i centrades)
ypred <- scale(trainx[,-1],center=FALSE,scale=gridge$scales)%*% gridge$coef[,270110] + mean(train$siri)

# I mirem l'ajust
rmse(ypred, train$siri) # 4.178651

# Ajust per al grup test
testx <-  as.matrix(sweep(test[,-1],2,mm)) # centro les observacions del grup test amb les mitjanes del grup training
ytpred <- scale(testx,center=F,scale=gridge$scales) %*% gridge$coef[,270110] + mean(train$siri)
rmse(ytpred, test$siri) # 4.395527


# ------------------------------------------------------------------------------------------------------------------------------#

################ 
## PROBLEMA 3 ## 
################
# POSIBLE PREGUNTA 4: TEST RESET!!
# En estadıstica, el test de Ramsey (1969) o Ramsey Regression Equation Specification Error Test (RESET)  es un test per 
# contrastar la linealitat d un model de regressio. La idea es contrastar si combinacions de potencies del vector de
# prediccions ens ajuden a explicar la variable resposta. La intuıcio darrera el test es que si aquestes combinacions
# no lineals de les variables predictives tenen cap poder d explicacio en la resposta, llavors el model lineal inicial no
# esta ben definit. Aixı doncs, el contrast RESET(k) per a un valor de k > 1 es_

# H0 : Y =β0 + β1x1 +···+ βpxp + ε
# H1 : Y =β0 + β1x1 +···+ βpxp + γ_2y^2 + ··· + γ_ky^k + ε
# on y^ es el vector de prediccions del model lineal multiple inicial.

# Amb la base de dades teengamb del paquet faraway considerem el model lineal amb la variable gamble com a resposta i les 
# variables sex, income i verbal com a regressores.
# Contrasteu la linealitat d aquest model amb el test RESET1 per a k = 2, 3 i 4 (s ́on tres contrastos, un per a cada valor
# de k). A quina conclusio arribem?
library(faraway)
data(teengamb)

# Model OLS
g <- lm(gamble  ~ sex + income + verbal, data = teengamb)

summary(g) #  Regressio significativa //  Ajust del 0.50%
plot(g)    # Alguns punts critics en el suposit d'homoscedasticitat // Patro una mica estrany dels residus.

yp <- fitted(g) # Prediccions amb el model OLS

# (A) Models.
# 1. Model de la nul la = model OLS
g1 <- g

# 2. Model de la alternativa
g2 <- lm(gamble  ~ sex + income + verbal + I(yp^2), data = teengamb)                     # k = 2
g3 <- lm(gamble  ~ sex + income + verbal + I(yp^2) + I(yp^3), data = teengamb)           # k = 3
g4 <- lm(gamble  ~ sex + income + verbal + I(yp^2) + I(yp^3) + I(yp^4), data = teengamb) # k = 4


# (B) Contrastos.
# Contrast 1. g1 vs. g2
anova(g1, g2) # Rebutgem la nul la.  Ens quedem amb el model 2

# Contrast 2. g1 vs. g3
anova(g1, g3) # Rebutgem la nul la. Ens quedem amb el model 3

# Contrast 3. g1 vs. g4
anova(g1, g4) # Rebutgem la nul la. Ens quedem amb el model 4

# Altres
anova(g2, g3)

# Per tant, es rebutja la linealitat del model


# ALTERNATIVA: (?)
install.packages("lmtest")
library(lmtest)
help(resettest)

resettest(g, power = 2, type = "fitted")    # Si dona el mateix
resettest(g, power = 2:3, type = "fitted")  # Si dona el mateix
resettest(g, power = 2:4, type = "fitted")  # Si dona el mateix


# ------------------------------------------------------------------------------------------------------------------------------#

################ 
## PROBLEMA 4 ## 
################
# POSIBLE PREGUNTA 4: (TEST DE BREUSCH-PAGAN!!)

# Sota les condicions de Gauss-Markov, que inclou l'homocedasticitat, els minims  quadrats ordinaris proporcionen el millor
# estimador lineal no esbiaixat (BLUE), es a dir, no esbiaixat i eficient. Malgrat aixo, l'eficiencia es perd en presencia
# d'heterocedasticitat. Per estudiar la presencia d'heterocedasticitat podem ferservir el test de Breusch-Pagan.

# Donat el model Y = Xbeta + epsilon assumim que l'heterocedasticitat pren la forma:

# E(epsilon_i) = 0      sigma^2_i = E(epsilon^2_i) = h(z_i'alpha)     , per a tota i = 1,...,n

## (A) Contrasteu l'homocedasticitat d'aquest model amb el test de Breusch-Pagan segons el procediment explicat i amb les 
##     mateixes regressores per al model auxiliar que pel primer model.  Calculeu l'estadistic i el seu p-valor.
library(car)
data(Ornstein)

# item 1
go <- lm(interlocks ~ ., data = Ornstein)
e <- residuals(go)
var.errors <- sum(e^2)/248

# item 2
y <- e^2/var.errors
go.aux <- lm(y ~ assets + sector + nation, data = Ornstein)
ESS <- sum((predict(go.aux) - mean(y))^2)

# item 3
BP <- ESS/2
pchisq(BP, 13, lower.tail = F) # 1.066e-10


## (B) Compareu el resultat amb la funcio "ncvTest" del paquet "car". Configureu correctament el parametre "var.formula".
library(car)
ncvTest(go, var.formula = ~assets + sector + nation)


## (C) Calculeu el test amb la funcio "bptest" del paquet "lmtest". Configureu correctament el parametre "studentize".
library(lmtest)
bptest(go, studentize = F)


# ------------------------------------------------------------------------------------------------------------------------------#

################ 
## PROBLEMA 5 ## 
################
# POSIBLE PREGUNTA 4: (TEST RAINBOW!!)

# En estadıstica, el test Rainbow (Utts, 1982) es un test per contrastar la linealitat d’un model de regressio. La idea
# es comparar el model de regressio complet (full) amb un model identic pero calculat nomes amb els punts de baixa
# influencia (low leverage points), es a dir, els punts centrals de les dades. Seleccionarem el subconjunt de les dades 
# amb baixa influencia o regio central de les dades amb el criteri:
#                                               leverage < median(leverage) 
# Llavors calcularem el seguent estadıstic:
#                               F = (SSEFULL − SSECENTRAL)/(n − m) SSECENTRAL/(m − 2)
# on n es el numero total d observacions en el model complet i m es el numero de punts de baixa influencia.

# Si la hipotesi nul.la de linealitat es certa, llavors l’estad ́ıstic F segueix una distribucío F de Fisher amb n − m,
# m − 2 graus de llibertat.

# Amb la base de dades teengamb del paquet faraway considerem el model lineal amb la variable gamble com a resposta i les 
# variables sex, income i verbal com a regressores.

# Contrasteu la linealitat d aquest model amb el test Rainbow2. A quina conclusio arribem?


# 1. Anem a aplicar el test Rainbow a la base de dades teengamb.
library(faraway)
data(teengamb)
g.full <- lm(gamble ~ sex + income + verbal, data=teengamb)
hh <- hatvalues(g.full)
g.central <- update(g.full, subset= hh < median(hh))
sse.full <- sum(summary(g.full)$residuals^2)
sse.central <- sum(summary(g.central)$residuals^2)
n <- dim(teengamb)[1]
m <- sum(hh < median(hh))
f.est <- ((sse.full - sse.central)/(n-m))/(sse.central/(m-4))
p.valor <- pf(f.est, n-m, m-4, lower.tail = F) # 0.01952873

# SOLUCIO: Segons aquest test, hem de rebutjar la hipòtesi nul·la de linealitat.

# ALTERNATIVA:
library(faraway)
data(teengamb)
g.full <- lm(gamble ~ sex + income + verbal, data=teengamb)

library(lmtest)
raintest(g.full, order.by="mahalanobis")


# ------------------------------------------------------------------------------------------------------------------------------#

#
############### 
## PROBLEMA 6 ## 
################
# POSIBLE PREGUNTA 4: (Semblant al problema 2 de 2017)

# Experiment sobre la sexualitat sobre les mosques de la fruita es vol estudiar si hi ha relacio entre el que viu una
# mosca en funcio de la seva activitat sexual se sap que en funcio del tamany de la mosca, la mosca viu mes o menys
# longitud del thorax: mesura de la grandaria de la mosca
# Es una ANCOVA perque tenim variable regresora (concumitant) que es el thorax, i una variable factor que te varis nivells

# 5 Nivells del factor activity
# - besic: la mosca es verge 
# - mosca amb parella
# - mosca amb varies parelles 
# - mosca amb molts parelles 
# - high
# hem de codificar els 5 estats d'alguna manera amb 0's i 1's

# CODIFICACIO (hi ha mes maneres de codificar):
# verge 0 0 0 0
# one   1 0 0 0
# low   0 1 0 0
# many  0 0 1 0
# high  0 0 0 1

library(faraway)
data(fruitfly)
g <- lm(longevity ~ thorax * activity, fruitfly)  #la verge no apareix perque es 0 0 0 0
# variable a variable veiem que el producte (la interaccio) no es significativa
summary(g)

model.matrix(g)[1:3, ]
anova(g) # ANOVA secuencial 

# Esta clar que la interaccio entre el thorax i la activity es 0, no hi ha interaccio, 
# Les rectes seran paral.leles, podem eliminar la interaccio
# La activity es significativa -> les rectes son paral.leles no coincidents
# El thorax es significatiu (ja ho sabiem)
# CONCLUSIO: la activitat sexual es significativa. Pero, en positiu o negatiu? ho podem saber mirant els coeficients

#activityone           6.5172       
#activitylow          -7.7501        
#activitymany         -1.1394       
#activityhigh        -11.0380    

#una mosca que visqui aillada viuria 136.12-50.24 dies
#una mosca que visqui one viura 136.12-50.24+6.5172*1 dies
#una mosca que visqui low viura 7.7501 dies menys que la a?llada
#una mosca que visqui many viura 1.1394 dies menys que la a?llada

#136.1268: pendent de totes les rectes

g <- lm(longevity ~ thorax + activity, fruitfly)




# ------------------------------------------------------------------------------------------------------------------------------#
################ 
## PROBLEMA 7 ## 
################
# POSIBLE PREGUNTA 4: (COMPARACIO TRACTAMENTS)

# PLANTEJAMENT: 
# EXISTEIXEN 2 NIVELLS DE DESPARASITAR: estandar y intensiu (mes fort). 

#variable resposta: aument de pes
#variable concomitant/regressora : pes inicial

# OBJECTIU: estudiar si hi ha diferencies entre el tractament intensiu i el tractament estandar.

# Un ANCOVA no es problema de regressio, es un problema de comparacio

# 1. DADES.
goats <- read.table("http://www.statlab.uni-heidelberg.de/data/ancova/goats.data", skip=1)
names(goats) <- c("treatment", "weightgain", "initial.wt")


# 2. ------------
by(goats, goats$treatment, summary) #pel que fa al pes inicial, els dos grups son semblants (ha de ser aixi)

# El guany de pes es mes gran en el tractament intensiu que en el estandar (es veu a simple vista), tot i que no sabem si
# hi ha una diferencia significativa

# 3. ------------
op<-par(mfrow = c(1,2),pty="s")
boxplot(weightgain ~ treatment, goats)
plot(weightgain ~ initial.wt, pch=ifelse(treatment=="standard",1,16), data=goats)
par(op)

#Relacio entre el guany de pes i el pes inicial. 
#Pendent negatiu: aquelles cabres que pesen mes al principi guanyen menys pes, aquelles cabres mes primes guanyen mes pes
#les del tractament intensiu (negre) estan per sobre

#Com que tenim dos nivells podriem fer un test t-student



# 4. ------------
boxplot(weightgain ~ treatment, goats)


# 5. ------------
plot(weightgain ~ initial.wt, pch=ifelse(treatment=="standard",1,16), data=goats)
legend(27.8, 11, levels(goats$treatment), pch=c(16,1))


# 6. ------------
t.test(weightgain ~ treatment, data=goats) #p-valor<0.05 --> significatiu
#el test t-student no contempla que el guany de pes depen del pes inicial
#com que el pes inical influeix en el guany de pes, hem d'incorporar aquesta informaci? en el model
#en aquest cas el test t-student, per tant, ?s inapropiat


# 7. ------------
#tractament estandar es el control -> 1r nivell (podem fer servir la funcio relevel)
#tractament intensiu es l'experiment -> 2n nivell
goats$treatment <- relevel(goats$treatment, ref="standard")


# 8. ------------
#incoporem una variable dicot?mica (tipus de tractament) en un model de regressi?
#tres variables regresiores: la x, la d i el producte de x*d: y ~ x + d + x:d o tambi? y ~ x * d (interaccio)
g1 <- lm(weightgain ~ initial.wt * treatment, data=goats)
summary(g1)
#l ordre ha de ser aquest primer la variable concomitant i despres el tractament (si ho fessim al reves no aniria be)
#m interessa veure si el producte de x*d es significatiu o no. El p-valor no es significatiu i per tant deduim que beta3 es 0
#com beta3 ?s 0, ens quedem amb el model additiu

# 9. ------------
model.matrix(g1)[1:3, ]
model.matrix(g1)[38:40, ]


# 10. ------------
g2 <- lm(weightgain ~ initial.wt + treatment, data=goats) #model additiu
summary(g2)
#el coeficient beta2 (coeficient de la dicotomica) es significatiu -> hi ha diferencies entre els dos tractaments
#el coeficient beta1 (pendent de la recta) ?s significatiu -> es natural, ja sabem que beta1 es el pendent de la recta de regressio
#si beta3 no fos 0 les rectes no serien paral.leles, es creuarian
#les dos rectes tenen el mateix pendent que ?s beta1 (pendent=-0.35137)
#la diferencia entre les dues rectes es beta2
#quan la dicotomica val 0, el terme de intercpico seria beta0
#quan la dicotomica val 1, el terme de intercepcio es beta0+beta2


# 11. ------------
plot(weightgain ~ initial.wt, pch=ifelse(treatment=="standard",1,16), data=goats)
legend(27.8, 11, levels(goats$treatment), pch=c(1,16))
abline(14.96661, -0.35137) 
abline(14.96661-1.26486, -0.35137, lty=2)

# Si fem servir el tractmaent intensiu guanyem 1.26 kg (en general, sigui quin sigui el pes inicial de les cabres)
#aquest 1.26 es significatiu (si no ho fos les rectes serien coincidents)

# 12. ------------
confint(g2)[3,]


# 14. ------------
plot(g2)

#tres situacions:
## beta3 pot ser 0 o no (significatiu o no)
## si beta3 es 0, les rectes son paraleles o coincidents
## si beta3 no es 0, les rectes es creuen i hem acabat perque no es poden treure conclusions

## si beta2 es significatiu, hi ha diferencies entre els tractaments









