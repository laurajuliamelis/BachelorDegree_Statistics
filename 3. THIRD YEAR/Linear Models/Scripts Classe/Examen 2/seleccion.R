y  <- c(78.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,93.1,115.9,83.8,113.3,109.4)
x1 <- c(7,1,11,11,7,11,3,1,2,21,1,11,10)
x2 <- c(26,29,56,31,52,55,71,31,54,47,40,66,68)
x3 <- c(6,15,8,8,6,9,17,22,18,4,23,9,8)
x4 <- c(60,52,20,47,33,22,6,44,22,26,34,12,12)

gc <- lm(y ~ x1+x2+x3+x4 ) 
g0 <- lm(y ~ 1) #el model incial l'escogeixo jo com investigador (no té per què ser aquest)
gc.formula <- formula(gc) #tope maxim del model amb una regressio forward

## Atenció! step no decideix en funció del test F
step(g0, scope=gc.formula, direction="forward", test="F") #test="F" per que t'ensenyi la F
g.forward <- lm(y ~ x1 + x2 + x4)
# El p-valor ha de ser significatiu (<0.05), ens hem de fixar que si el p-valor no és signifcatiu 
# no em d'afegir aquella variable encara que R ho fagi

step(gc, scope=gc.formula, direction="backward", test="F")
g.backward <- lm(y ~ x1 + x2 + x4)
# En aquest cas R acaba massa depresa.

step(gc, scope=gc.formula, direction="both", test="F") ## Stepwise selection

# Si els models són comparables
#anova(g.forward, g.backward)

#adjusted R^2
summary(g.forward)$adj.r.square; summary(g.backward)$adj.r.square

#AIC
AIC(g.forward); AIC(g.backward)

## Introducció progressiva (Forward selection)
model <- g0 
add1(model, scope = gc.formula, test="F") #add1 fa el pas de l'stepwise (afegeix una)
model <- update(g0, ~ . + x4) #agafem el model inicial i li afegim x4
add1(model, scope = gc.formula, test="F") 
model <- update(model, ~ . + x1)
add1(model, scope = gc.formula, test="F")

## Eliminació progressiva (Backward selection)
model <- gc; drop1(model, test="F")
model <- update(model, ~ . - x3); drop1(model, test="F") #dropp1 fa el pas de l'stepwise (treu una)
model <- update(model, ~ . - x4); drop1(model, test="F")

## SelecciÃ³ pas a pas (Stepwise selection)

# F_in >= F_out (alpha = 0.05, per ambdos)

model <- g0
abs(c(cor(y,x1),cor(y,x2),cor(y,x3),cor(y,x4)))
model <- update(model, ~ . + x4)
summary(model) # x4 entra en el model

gy4 <- lm(y ~ x4); g14 <- lm(x1 ~ x4); g24 <- lm(x2 ~ x4); g34 <- lm(x3 ~ x4)
abs(c(cor(residuals(gy4),residuals(g14)), cor(residuals(gy4),residuals(g24)), cor(residuals(gy4),residuals(g34))))
model <- update(model, ~ . + x1)
summary(model) # x1 entra en el model i x4 no surt

gy.14 <- lm(y ~ x1 + x4); g2.14 <- lm(x2 ~ x1 + x4); g3.14 <- lm(x3 ~ x1 + x4)
abs(c(cor(residuals(gy.14),residuals(g2.14)), cor(residuals(gy.14),residuals(g3.14))))
model <- update(model, ~ . + x2)
summary(model) # x2 no entra en el model

# partial correlations
# library(ggm)
# misdatos <- data.frame(y,x1,x2,x3,x4)
# pcor(c("y", "x1", "x4"), var(misdatos))
# partial corr between a and b controlling for x, y, z 

## Datos de Faraway
library(faraway)
data(state)
statedata <- data.frame(state.x77, row.names=state.abb)
str(statedata)

g <- lm(Life.Exp ~ ., data=statedata)
summary(g)

step(g, test="F") # Backward selection (si no hi ha scope)

lmod <- update(g, ~. - Area)
summary(lmod)

drop1(g, test="F")

library(faraway)
library(leaps)
statedata <- data.frame(state.x77, row.names=state.abb)
b <- regsubsets(Life.Exp ~ ., data=statedata)
rs <- summary(b)
rs$which #matriu que em diu cada model quin és el model millor si considerem només un número concret de variables

AIC <- 50*log(rs$rss/50) + (2:8)*2 #n=50, 2:8 numero de parametres
plot(AIC ~ I(1:7), ylab="AIC", xlab="Number of Predictors")

plot(2:8, rs$adjr2, xlab="Nº of Parameters", ylab="Adjusted R2") #El model 5 és el que té el R2 més gran
which.max(rs$adjr2) # model que considera 4 variables, en el gràfic és el 5
max(rs$adjr2)

plot(2:8, rs$acp, xlab="Nº of Parameters", ylab="Cp statistic")
abline(0,1)

lmod <- lm(Life.Exp ~ ., data=statedata)
h <- lm.influence(lmod)$hat
names(h) <- state.abb
plot(h, type="h")
cutoff_alt_leverage <- 2*8/50
abline(h=cutoff_alt_leverage, col="red") #el segon(AK=Alaska) té un leverage molt gran
rev(sort(h))
statedata[2,]

# Què passaria si treiessim Alaska del model??
b <- regsubsets(Life.Exp ~ ., data=statedata, subset=(state.abb!="AK"))
b <- regsubsets(Life.Exp ~ ., data=statedata[-2, ]) #una altra manera
rs <- summary(b)
plot(2:8, rs$adjr2, xlab="Nº of Parameters", ylab="Adjusted R2") #El millor model està entre 5 i 6


rs$which[which.max(rs$adjr),]

# Què passa quan fem un canvi d 'escala? Que li passa a la regressió quan a  una var li sumenm una constant a i li dividim una constant b
data(saving, package="faraway")
lmod <- lm(sr ~ pop15+pop75+dpi+ddpi,savings)
summary(lmod)

lmod2 <- lm(sr ~ pop15+pop75+I(dpi/1000)+ddpi,savings)
summary(lmod2)
#Conlcusio: si divideixo per una constnat una variable en el model el coeficient de la variable quedara multiplicada per aquella constant
            #ni el RSE ni el R2 ni la t ni la F varian

#Què passa si fem el mateix però amb y(la resposta)
srn <- savings$sr/1000 
lmod3 <- lm(srn ~ pop15+pop75+dpi+ddpi,savings)
summary(lmod3)
#Conlcusio: si divideixo per una constnat la resposta en el model TOTS els coeficientS queden dividits per aquella constant
# L'estimacio de sigma també queda dividada per 1000
#ni el R2 ni la t ni la Fv arian

#REESCALACIó
#Què passa si estandaritzem les variables (també ho fem amb la reposta)? -> els coeficients deixen de tenir unitats i son comparables entre ells 

#suposem que tenim una variable x1 que pren 0 amb prob 1/2 i 1 amb prob 1/2. L'esperança seria 1/2 i la variancia seria 1/4
#si vull rescalar seria (0-1/2)/arrel(1/4) i (1-1/2)/arrel(1/4). Les altres variables s'haurien de reescalar dividint dos cops per la seva variancia
#Es a dir, (x2-E(x2))/(2*sd(x2)).
#Agafem com a variable dicotomica -1 i 1 en comptes de 0 i 1 i no tindrem aquest problema
  


#COLINEAL (COLLINEALITY)
#Per que una matriu x sigui de rang maxim cap columna pot ser c.l de les altres
#imaginem que la col 10 és c.l. de la 2, 8 i 7, però per un valor no és => estictmanet el rang és maxim, pero a la practica la col 10 si que és c.l de les altres
#quan passa que una col es casi casi c.l de les altres és convenient enimar-la. Sino augment variancia i pèrdua d'eficiencia

#Com es detecta el problema de colinealitat
# 1r: calcul de la matriu de correlacio de les variables regressores. Si observem valors alts de correlacio 2 a 2  vol dir que una columna depen de un altre.
# 2n: estudiar R2 entre una regressora i les altres regressores. Si observem que un d'ells és molt gran, una de les vars sera casi casi c.l de les altres i per tant l'hauriem de treure del model
# 3r: calcul del numero de condicio de la matriu xtx. Quan xtx té algun valor propi 0, directament el seu rang no és màxim
#     Quan els valors propis son tots positus pero algun és molt molt molt petit (a la practica és 0=el rang no és maxim a la practica)
#     Comparar el més gran amb el més petit: k=sqrt(lamda1/lambdap), si k>30(gran) es consideraria que tenim un problema de definicio de la matriu (colinealitat)
# VIF = 1/(1-R2i) #R2i de una var regressora respecte les altres #quan tinguem VIF>4(estricte), VIF>10 (laxe) grans és quan tenim un problema de colinealitat

#EXEMPLE: estudi de la colinealitat
data(seatpos, package="faraway")
lmod <- lm(hipcenter ~., seatpos)
summary(lmod)

cor(seatpos[,-9]) #sense la resposta
# HtShoes i Ht estan fortament correlacionadas

x <- model.matrix(lmod)
xtx <- t(x)%*%x

#Calculs dels vectors i valors propis
ee <- eigen(xtx) 
ee$values #valors propis: tots son positius pero si agafem el major valor propi i el dividem pel més petit...
ee$values/ee$values[9]
sqrt(ee$values/ee$values[9])[1] #num de condicio. Els primers valors son elavadisims -> problema claríssim de colinealitat

solve(xtx) #calcular la inversa no és un problema, el problema és l'exactitud del calcul de la inversa

#Calcul dels VIF's del model
vif(lmod)
#criteri 4 -> eliminem 5
#criteri 10 -> eliminem 2 (utilitzarem aquest)
