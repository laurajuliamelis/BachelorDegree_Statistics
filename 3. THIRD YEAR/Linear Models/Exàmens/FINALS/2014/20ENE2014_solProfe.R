#Examen de Models Lineals (20 de gener de 2014)

##############
# Problema 1 #
##############

# Una cadena d'empaquetatge està formada per cinc treballadors identificats 
# amb els núumeros 1 al 5, més un capatàs que treballa tot el temps. Cada dia
# registrem \( X_j = 1 \) si el treballador j és de servei i 0 en cas 
# contrari, \( Y = \) és el número de paquets despatxats. La següent taula 
# ens mostra les dades recollides. També les teniu en el fitxer "paquets.csv".
paquets <- read.table("E:/FINAL ML/Visto/Finales/EF_20ENE2014/paquets.csv", header = T)
paquets


#(a) Presenteu algun objectiu per recollir i analitzar aquestes dades. 
#Plantegeu un model lineal que pugui assolir aquest objectiu.

#L'objectiu és estudiar la relació entre els treballadors i el seu capataç 
#amb el número de paquets despatxats i la contribució de cada treballador. 
#El model lineal proposat és Y = beta_0 + beta_1*X_1 + ... + beta_5*X_5 + 
#epsilon on epsilon ha de ser un error que verifiqui les condicions de 
#Gauss-Markov.


#(b) Feu la regressió de Y sobre X_1,...,X_5. És possible que l'ordinador 
#proporcioni algun resultat inesperat. Expliqueu-ne els motius. Trobeu una 
#solució que eviti el problema anterior.

#La regressió demanada és:
g <- lm(y ~ ., data = paquets)
summary(g)
#Hi ha un coeficient de regressió que no es pot estimar(beta_5=NA). El rang 
#de la matriu de disseny no és màxim. La solució és reparametritzar el model
#per ajustar el número de paràmetres al rang. Per això hi ha vàries 
#possibilitats. Una és fer beta_5=0. Una altra és fer servir g-inverses. 
#A més, també podríem afegir alguna observació que augmentés el rang, 
#però això no ho podem fer ara.


#c) Comproveu que el rang de la matriu de disseny és 5 i identifiqueu les 
#funcions paramètriques estimables.

#El càlcul del rang es pot fer de vàries formes, la següent n'és una:
mm <- model.matrix(g)  # matriu de disseny
qr(mm)$rank
#La submatriu formada per les files 1,2,4,5,17 té rang 5.
qr(mm[c(1, 2, 4, 5, 17), ])$rank

#Les dues formes donen rang 5. De forma que per identificar les funcions 
#paramètriques estimables a_0*beta_0 + a_1*beta_1 + ... + a_5*beta_5 ho 
#podem fer amb aquestes files. El resultat és una única restricció a_1=a_5.


#(d) Podem saber la contribució estimada del capatàs? Quin és l'error 
#estàndard d'aquesta estimació?

#La contribució del capatàs està dins del paràmetre beta_0, de forma que 
#ens preguntem si aquest paràmetre és estimable. El vector (1,0,0,0,0,0) 
#verifica la condició de l'apartat anterior a_1=a_5=0 de forma que és 
#estimable. L'estimació de beta_0 és el Intercept del model lineal:
coef(g)[1]
#L'estimació és de 104.2 amb un error estàndard de 3.339.


#(e) Feu el contrast d'igualtat en la contribució dels treballadors 2,3 i 4.

#En primer lloc cal escriure la hipòtesi que ens demanen: 
#H_0: beta_2=beta_3=beta_4 que en realitat són dues funcions paramètriques: 
#beta_2 - beta_3 = 0 i beta_2 - beta_4 = 0 i les dues són f.p.e. ja que els 
#vectors (0,0,1,-1,0,0) i (0,0,1,0,-1,0) verifiquen la condició de l'apartat
#c). El contrast es pot fer de vàries formes però la més simple és:
g0 <- lm(y ~ x1 + I(x2 + x3 + x4) + x5, data = paquets)
anova(g0, g)

#Com el pvalor es significatiu 3.679e-11, rebutjem la hipòtesi d'igualtat.
#La contribució d'aquests tres treballadors és diferent.



#(f) Creeu una nova variable S = X_1+ ... +X_5 i feu la regressió de Y sobre S. Quines són les 
#estimacions dels paràmetres? Compareu els resultats amb els de l'apartat (d). Quina de les dues 
#estimacions de la contribució del capatàs és correcte?
#La variable S és el número de treballadors cada dia.
paquets$s <- paquets$x1 + paquets$x2 + paquets$x3 + paquets$x4 +paquets$x5
paquets$s <- apply(paquets[, -6], 1, sum)
g1 <- lm(y ~ s, data = paquets)
summary(g1)

#Segons aquesta recta de regressió, per cada treballador s'augmenta en quasi 50 paquets la feina del 
#dia. Aparentment, quan no hi ha cap treballador el número de paquets seria 47.4, en part per la 
#contribució del capatàs en contradicció amb l'estimació de beta_0 en el model anterior. Recordeu que 
#cap estimació del terme independent té explicació si les dades no inclouen el zero. El paràmetre 
#beta_0 inclou la contribució del capatàs, però representa la mitjana global d'un dia qualsevol amb 
#alguns treballadors.



##############
# Problema 2 #
##############

#La concentració de lactat en sang serveix frequentment per predir la resistència dels atletes.
#La identicació d'alguns predictors de la resistència pot ajudar als entrenadors i atletes a avaluar 
#els canvis del seu rendiment. Per identificar aquests predictors es van seleccionar vint-i-quatre 
#dones ciclistes ben entrenades i se'ls va practicar una prova física en un cicloergonòmetre. La prova
#física va consistir en realitzar etapes de tres minuts fins l'esgotament. Als 30 segons d'haver 
#finalitzat cada etapa de tres minuts, se'ls va treure sang capilar del dit índex per analitzar el 
#lactat plasmàtic i obtenir els valors predictors següent: llindar de lactat (P-Tlac), DMax (DMax), 
#LT-log-log (P-Tlac.II), P-4 mmol.L\( ^1 \) (P-4mM) i taxa de treball corresponent a un increment de 
#1mM del valor basal (Rise.1.PB). El rendiment de resistència (AV.Power) s'avaluà 7 dies més tard 
#mitjançant una prova de bicicleta, d'una hora de durada, en la que les atletes havien d'aconseguir la
#potència de sortida més alta possible. Els resultats es troben en el fitxer "CyclingPower.xls".


#1. Calculeu l'hiperplà de regressió i el coeficient de correlació múltiple de AV.Power sobre les 
#altres variables. Quina és la variància estimada de l'error?
library(gdata)
CyclingPower <- read.table('clipboard',header=T,dec=",") 
gh <- lm(AV.Power ~ ., data = CyclingPower)
(ss <- summary(gh))

#El coeficient de correlació múltiple és l'arrel quadrada del coeficient de determinació és 0.8564:
sqrt(ss$r.squared)

#i la variància estimada de l'error és 121.4:
ss$sigma^2


#2. És un model amb un bon ajust? Vol dir això que és significativa la regressió? Concreta què 
#significa cada pregunta.

#El coeficient de determinació és R^2=0.7334 que està força bé. Estranya una mica que el coeficient 
#ajustat Adjusted R-squared=0.6594 sigui tan diferent. Sembla que tenim un problema.
#La regressió és significativa ja que el test F global així ho confirma. El seu p-valor és 0.0001111.
#Això significa que els coeficients de les variables regressores no són tots zero i no té res a veure 
#amb l'ajust.


#3. Amb els gràfics o els estadístics adients, investigueu la diagnosi d'aquest model en el seguents
#punts

#(i) Variància constant dels errors.
plot(gh, which = 3)
summary(lm(abs(residuals(gh)) ~ fitted(gh)))
#p-value: 0.2513, no sembla que hi hagi cap problema d'heterocedasticitat.

#(ii) Hipòtesi de normalitat.
plot(gh, which = 2)
shapiro.test(residuals(gh))
#p-value = 0.5117, cap problema respecte a la normalitat.

#(iii) Punts amb infuència potencial (leverage).
plot(gh, which = 5)
plot(hatvalues(gh), type = "h")
sum(hatvalues(gh))
cutoff <- 2 * 6/24  # =2*paràmetros/n
abline(h = cutoff, col = "red")
which(hatvalues(gh) > cutoff)
#Veiem que la observació 21 té una una influencia potencial elevada.

#(iv) Outliers.
plot(gh, which = 3)
plot(abs(rstudent(gh)))
abline(h = 2, col = "red")
which(abs(rstudent(gh)) > 2)
## 14 16 
## 14 16
library(car)
# outlierTest(gh)
#Els punts 14 i 16 són atipics.

#(v) Punts influents.
plot(gh, which = 5)
plot(gh, which = 4)
infIndexPlot(gh, id.n=10)
cooks.distance(gh)
library(faraway)
halfnorm(cooks.distance(gh), 2, ylab = "Distàncies de Cook")
influence.measures(gh)
#Sembla evident que l'observació 21 és influent.

#(vi) Creieu que pot haver un problema de multicolinealitat? En què us baseu?
cor(CyclingPower[, -1])
#Les correlaciones entre les variables regressores són molt altes. Hi ha quatre vif
#més grans que 10. Tenim un problema greu de multicolinealitat.


#4. Què podem dir del punt 21? En què millora el model si eliminem aquest punt de 
#les dades? I què podeu dir del 14?

#El punt 21 és un punt d'influència potencial i real molt alta. Això vol dir que és
#un punt que està lluny dels altres i, a més, la seva presència modificar 
#substancialment l'estimació dels paràmetres. Si l'eliminem, guanyarem en l'ajust 
#una mica però, sobre tot, el model serà més robust.
#El punt 14 és un outlier i influent. Si el treiem guanyarem en l'ajust.

#En tot cas, el problema principal és la multicolinealitat. Mentre no arreglem això,
#no podem millorar el model.


#5. Contrasteu si els coeficients de regressió de les variables P-Tlac i P-Tlac.II 
#són iguals.

#Segons l'ordre de les regressores, aquesta hipòtesi és H_0: beta_2=beta_3
#i per contrastar-la fem:
gh0 <- lm(AV.Power ~ P.4mM + I(P.Tlac + P.Tlac.ll) + DMax + Rise.1.PB, data = CyclingPower)
anova(gh0, gh)
#Se acepta la hipòtesis de igualdad.


#6. Si una corredora té uns valors de P-Tlac=175, DMax=158, P-Tlac.II=131, 
#P-4mM=206 i Rise.1.PB=182, quina és l'estimació del seu rendiment de resistència 
#(AV.Power) i l'error estàndard d'aquesta estimació.

#Amb una probabilitat del 0.95, entre quins valors es troba el rendiment de 
#resistència (AV.Power) d'una corredora concreta que té els valors anteriors?
m.data <- data.frame(P.Tlac = 175, DMax = 158, P.Tlac.ll = 131, P.4mM = 206,                     Rise.1.PB = 182)
predict(gh, newdata = m.data, interval = "prediction", se.fit = T, level = 0.95)


#7. Calculeu la correlació parcial entre AV.Power i Rise.1.PB si eliminem la 
#informació de P-Tlac, DMax, P-Tlac.II i P-4mM.
e1 <- residuals(lm(AV.Power ~ P.Tlac + DMax + P.Tlac.ll + P.4mM, data = CyclingPower))
e2 <- residuals(lm(Rise.1.PB ~ P.Tlac + DMax + P.Tlac.ll + P.4mM, data = CyclingPower))
cor(e1, e2)
# 0.2602 la correlació parcial entre aquestes dues variables és molt baixa.


##############
# Problema 3 #
##############
#Amb la mateixa base de dades del problema anterior i el mateix model de partida, 
#sembla que tenim un problema de multicolinealitat.

#1. Trobeu el "millor" model per dos mètodes diferents de selecció de variables 
#com, per exemple, AIC i Cp de Mallows.

# Stepwise
step(gh)

# Mètode Cp de Mallows
library(leaps)
b <- regsubsets(AV.Power ~ ., data = CyclingPower)
(rs <- summary(b))
#En el cas de la selecció Stepwise el model que ha quedat conté 3 variables: 
#P.Tlac.ll, DMax, Rise.1.PB.
#En el gràfic de Cp observem que podem escollir entre el model de 3 paràmetres 
#(2 variables) i el de 4 paràmetres (3 variables) que són les mateixes.

#(a) Quines són les variables seleccionades?

#Amb els dos criteris seleccionem les variables P.Tlac.ll, DMax i Rise.1.PB.

#(b) Quins són els coeficients de determinació ajustats d'aquests models? 
#Compareu-los amb el del model complet. Llavors, què hem guanyat?
plot(2:6, rs$adjr2, xlab = "Número de paràmetres")
# model complet
rs$adjr2[5]
#0.6594

# model de 3 variables
rs$adjr2[3]
#0.6841
gs <- lm(AV.Power ~ P.Tlac.ll + DMax + Rise.1.PB, data = CyclingPower)
vif(gs)
#L'ajust del model amb 3 variables millora el coeficient de determinació ajustat i,
#a més, corregeix el problema de multicolinealitat.

#c) Calculeu l'interval de confiança al 95% per al coeficient de regressió de la 
#variable Rise.1.PB en els models, el complet i els seleccionats.

confint(gh)["Rise.1.PB", ]
##   2.5 %  97.5 % 
## -0.2549  0.8634

confint(gs)["Rise.1.PB", ]
##  2.5 % 97.5 % 
## 0.1253 0.7124

#Hem guanyat amb eficiència.

#2. Un altre possibilitat és fer servir la Ridge Regression. Quins són els 
#coeficients obtinguts? Expliqueu breument les avantatges i inconvenients d'aquest 
#mètode front a la selecció de variables.
library(MASS)
gridge <- lm.ridge(AV.Power ~ ., data = CyclingPower, lambda = seq(0, 50, 1))
matplot(gridge$lambda, t(gridge$coef), type = "l", lty = 1, xlab = expression(lambda), 
    ylab = expression(hat(beta)))
select(gridge)
## modified HKB estimator is 1.207 
## modified L-W estimator is 1.454 
## smallest value of GCV  at 4
abline(v = 4)
gridge <- lm.ridge(AV.Power ~ ., data = CyclingPower, lambda = 4)
coef(gridge)
#Aquest mètode permet conservar totes les variables en el model. Elimina el 
#problema de la manca d'eficiència de les estimacions, a canvi d'un petit increment 
#en el biaix.
#En aquest problema sembla que és millor un mètode de selecció de variables.

#3. Amb el model reduit de tres variables regressores (P.Tlac.ll, DMax i Rise.1.PB) 
#el punt 21 encara fa nosa. Ajusteu un model per un mètode robust adient.

plot(gs, which = 4)
library(MASS)

# Mètode de Huber
gr <- rlm(AV.Power ~ P.Tlac.ll + DMax + Rise.1.PB, data = CyclingPower)
summary(gr)


##############
# Problema 4 # Test de Breusch-Pagan
##############
#Sota les condicions de Gauss-Markov, que inclou l'homocedasticitat, els mínims 
#quadrats ordinaris proporcionen el millor estimador lineal no esbiaixat (BLUE), 
#és a dir, no esbiaixat i eficient. Malgrat això, l'eficiència es perd en presència
#d'heterocedasticitat. Per estudiar la presència d'heterocedasticitat podem fer 
#servir el test de Breusch-Pagan.
#Donat el model Y = Xbeta + epsilon assumim que l'heterocedasticitat pren la forma:
#E(epsilon_i) = 0 qquad sigma^2_i = E(epsilon^2_i) = h(z_i'alpha) qquad text{per a 
#tota $i = 1,dots,n$}

#(a) Contrasteu l'homocedasticitat d'aquest model amb el test de Breusch-Pagan 
#segons el procediment explicat i amb les mateixes regressores per al model 
#auxiliar que pel primer model.

#Calculeu l'estadístic i el seu p-valor.
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
pchisq(BP, 13, lower.tail = F)
## [1] 1.066e-10
(b) Compareu el resultat amb la funció "ncvTest" del paquet "car". Configureu correctament el paràmetre "var.formula".
library(car)
ncvTest(go, var.formula = ~assets + sector + nation)
## Non-constant Variance Score Test 
## Variance formula: ~ assets + sector + nation 
## Chisquare = 74.74    Df = 13     p = 1.066e-10
c) Calculeu el test amb la funció "bptest" del paquet "lmtest". Configureu correctament el paràmetre "studentize".
library(lmtest)
bptest(go, studentize = F)
## 
##  Breusch-Pagan test
## 
## data:  go
## BP = 74.74, df = 13, p-value = 1.066e-10


