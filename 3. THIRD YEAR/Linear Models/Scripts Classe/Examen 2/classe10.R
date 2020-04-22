#COMPARACIÓ DE RECTES

#Ancova (Anilisi de la covariancia)

#REgressio quan les explicatives son numeriques
#ANOVA quan les explicatives son factors o qualitatives (disseny d'experiments)
#ANCOVA quan les explicatives son numeriques i factor alhora
#la variable resposta sempre es numerica (normal)
#quan la variable resposta sigui d'un altre tipus parlarem de model generalitzat

#tenim dos mostres, dos poblacions
#recta1 alpha1 beta1
#recta2 alpha2 beta2

#Aquestes rectes son paraleles? -> h0: beta1 = beta2
#les rectes son iguals? -> h0: alpha1 = alpha2, beta1 = beta2

#Per constrastar ->Construir un model geneeral i un model que s'avingui a la hipotesi nul.la

#EXEMPLE: exercici 1.4, ens podem preguntar si les rectes són paral.leles

dist <- c(100, 200, 400, 800, 1500, 5000, 10000, 42192)
th <- c(9.84, 19.32, 43.19, 102.58, 215.78, 787.96, 1627.34, 7956.00)
td <- c(10.94, 22.12, 48.25, 117.73, 240.83, 899.88, 1861.63, 8765.00)

mod.h <- lm(log(th) ~ I(log(dist)))
mod.d <- lm(log(td) ~ I(log(dist)))            
coef(mod.h)
coef(mod.d)

#aparanment són valors molt propers

#tot i que la unica manera de comprovarho és fer un contrast d'hipótesis

#Model general:
#X=(alpha1, beta1, alpha2, beta2) matriu de disseny
#Y=(tempshomes)
#  (tempsdones)

alpha1 <- c(rep(1,8), rep(0,8))
beta1 <- c(log(dist), rep(0,8))
alpha2 <- c(rep(0,8), rep(1,8))
beta2 <- c(rep(0,8), log(dist))
temps <- c(th,td)
g <- lm(log(temps) ~ 0 +alpha1 + beta1 + alpha2 + beta2) #al ficar el 0 treiem la columna de 1's
coef(g)
# dona molt diferent beta1 a beta2 perque no és el temps sino el logaritme del temps

model.matrix(g) #estimació conjunta dels paràmetres
qr(model.matrix(g))$rank #rank de la matriu de disseny

#H0: beta1 = beta2 #test de paral·lelisme
#Xtilde = (alpha1, beta1, alpha2)
g0 <- lm(log(temps) ~ 0 + alpha1 + I(beta1+beta2) + alpha2)
anova(g0,g)

#H0: alpha1 = alpha2 #test de concurrència

#1. Contrast t-Student
  #H0: alpha1 = alpha2 -> alpha1 - alpha2 = 0 -> (1,0,-1,0)*(alpha1,beta1,alpha2,beta2)' és fpe
  #rank de la matriu de disseny general (model.matrix) és 4 (es fa fent la descomposició qr)
  #alpha1 - alpha2 = -2.886 -(-2.76)

#2. Contrast de models
  #Xtilde = (alpha, beta1, beta2) alpha1=alpha2(=alpha)
g0 <- lm(log(temps) ~ beta1 + beta2) #la columna de uns no cal posarle perk en un lm sempre hi 
                                     #ha una columna de 1's que represneta la interseccio
anova(g0,g) #test F-fisher
  #acceptem la H0

#H0: alpha1 = alpha2, beta1 = beta2 #test de coincidencia
  #Camí1: 1r beta1 = beta2
         #2n alpha1 = alpha2 (tenint en compte que són paral.leles)
  #Camí2: test F directe

#test F directe (2n camí)
#H0 diu que tenim una unica recta (considerem només un núvol de punts)
g0 <- lm(log(temps) ~ I(beta1+beta2))
model.matrix(g0)

anova(g0,g)
  #rebutgem H0 -> no son la mateixa recta

#Suposem que són paral.leles (1r camí)
  #el model general ja no serà el model inicial
g <- lm(log(temps) ~ 0 + alpha1 + I(beta1+beta2) + alpha2)
g0 <- lm(log(temps) ~ I(beta1+beta2))

anova(g0,g)
  #rebutgem H0 -> no son la mateixa recta

#Arribem al mateix resultat per ambdós camins. Podem assegurar que les rectes són paral·leles pero no coincidents

#Si no hi ha igualtat de variancies entre el homes i les dones, el model conjunt no és vàlid
var.test(residuals(mod.h), residuals(mod.d)) #test F que es basa en la normalitat de les dades
  #Acceptem la H0 d'igualtat de variàncies 

#Si les dades no fossin normals no podriem utilitzar el test F

#ALTERNATIVA: test de Barlett, packet stats (és paramètric)

#ALTERNATIVA: test de Levene

install.packages("car")
library(car)
residus <- c(residuals(mod.h),residuals(mod.d))
grup <- factor(c(rep(1,8),rep(2,8)))
leveneTest(residus,grup)
  #Acceptem la H0 d'igualtat de variàncies

data("anscombe")
str(anscombe)
attach(anscombe)

plot(x1,y1)
r1 <- lm(y1 ~ x1)
abline(r1)

plot(x2,y2)
r2 <- lm(y2 ~ x2)
abline(r2)

plot(x3,y3)
r3 <- lm(y3 ~ x3)
abline(r3)

plot(x4,y4)
r4 <- lm(y4 ~ x4)
abline(r4)

#en el primer grafic la recta ha de ser plana
plot(r1)
plot(r2) #els erros no són aleatoris, l'estructura del model falla
plot(r3) #els erros no són aleatoris, l'estructura del model falla
plot(r4)
