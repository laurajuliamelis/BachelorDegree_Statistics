library(faraway)
data(savings)

#no cal fer un attachs

M1 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data=savings)
(M1_sum <- summary(M1))

fitted(M1) #calcula els valors ajustats (valors de predicció) (y amb sombrero)
residuals(M1)
which.max(abs(residuals(M1)))

# Validació del model

plot(fitted(M1), residuals(M1), xlab="Fitted", ylab="Residuals") #prediccions vs residus 
abline(h=0, col="red")

plot(M1, which=1) #és el mateix (apareix una linea vermella ajustada als punts)
# amb aquest gràfic mirem la variablitat dels residus i si hi ha alguna anomàlia

plot(fitted(M1), abs(residuals(M1)), xlab="Fitted", ylab="|Residuals|") 

plot(M1, which=3)

summary(lm(abs(residuals(M1)) ~ fitted(M1))) #no significatiu (recta plana) => la variancia és constant

plot(M1, which=2)

boxplot(rstandard(M1)) # no sembla que hi hagin valors atípics

which(rstudent(M1) > 2)

data("airquality")
M2 <- lm(Ozone ~ Solar.R + Wind + Temp, data=airquality, na.action=na.exclude)

summary(M2)

#transformació logaritmica de la variable resposta
M2log <- lm(log(Ozone) ~ Solar.R + Wind + Temp, airquality, na.action=na.exclude) 
summary(M2)

plot(M2log)

plot(residuals(M2log)[-153], residuals(M2log)[-1], xlab=expression(hat(epsilon)[i]), ylab=expression(hat(epsilon)[i+1]))
identify(residuals(M2log)[-153], residuals(M2log)[-1], n=4)

summary(lm(residuals(M2log)[-1] ~ -1 + residuals(M2log)[-153]))
#no hi ha terme independent peque els residus tenen mitjana 0. La recta de regressió ha de passar obligatoriament pel (0,0), motiu del -1

cor(residuals(M2log)[-1], residuals(M2log)[-153], use="complete.obs")

M1_inf <- influence(M1)
M1_inf$hat #leverage, vector diagonal de la matriu hat
#la suma de la diagonal de la matriu hauria de coincidir amb el rang
total_leverage <- rank(Hat) <- 5 
sum(M1_inf$hat)
summary(M1_inf$hat) #la mitjana és 5/50 (r/n) #alt leverage es considera dues vegades aquesta quantitat

cutoff_alt_leverage <- 2*5/50

plot(M1_inf$hat, type="h")
abline(h=cutoff_alt_leverage, col="red")
#plot(M1, which=4)

which(M1_inf$hat > cutoff_alt_leverage)

# una altra criteri per establir quines son aquelles observacions que estan fora del que es considera habitual (leverage):
countries <- rownames(savings)
halfnorm(M1_inf$hat, labs=countries, ylab="Leverages")

# Outliers:

rstandard(M1) # residus studentitzats
rstudent(M1) # residus studentitzats externament
plot(abs(rstudent(M1)), type="h")
abline(h=2, col="red") #obs 7 (Chile) i 47 (ZAmbia) són outliers segons aquest criteri

which(abs(rstudent(M1)) > 2) 

#Chile no té un alt leverage però és un outlier -> "no es comporta com el model de regressio"
#ZAmbia està lluny de la mitjana de valors dels paisos i a més a més és outlier

# Volem saber si aquests són valors significatius respecte la t-student
rr <- 50 - 5 - 1 #n-r-1=n-k-2
qt(0.975, df=44)
qt(0.975/(50*2), df=44) #bonferroni

library(car)
outlierTest(M1) # Aquest criteri només considera Zambia outlier

(cook <- cooks.distance(M1)) 
countries <- rownames(savings) 
halfnorm(cook, 3, labs=countries, ylab="Cook's distance") 
which.max(cook)

library(car)
plot(cookd(M1), type="h")

plot(M1, which=6) #es barreja el leverage i la distancia de cook

M1_L <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data=savings, subset=(cook < max(cook)))
summary(M1_L) # linear model estimates without Libya

# Influence Plot
library(car)
influencePlot(M1, id.method="identify", main="Influence Plot", sub="Circle size i proportional to Cook's Distance")
# Zambia i Chile són outliers
# Japan, Ireland, United States i Libya tenen alt leverage
# Cercle gros = ditància de Cook gran (Libya, Japan, Zambia)

influence.measures(M1)

#Checking the structure of the model
# El que hem fet fins ara s'ha basat unicament en chaquejar l'error a través dels residus
# l'estructura del model també s'hauria de chequejar
# La pregunta és: el model adequat és x*beta? La recta de regressió és la millor de les curves possibles?
# 1r metode Mateix metode dos noms: regressio parcial i gràfics de variable afegida
# 2n metode Gràfics de residus parcials
# tractar d'estudiar la regressio múltiple com si fossin varies regressions simples
# estudiar la influencia d'una variable regressora en el model

avPlots(M1) #1r metode
# no es veu cap patologia evident

termplot(M1, partial.resid = T, terms=1) #2n metode #terms=1 et fa per la primera variable
# sugereix pensar en dos regressions en funcio de la varible pop15 (pop15<35 i pop15>35)
cr.plots(M1, variable="pop15")
cr.plots(M1)
