#COMPARACI? DE RECTES

#Ancova (Anilisi de la convariancia)

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

#EXEMPLE: exercici 1.4, ens podem preguntar si les rectes s?n paral.leles

dist <- c(100, 200, 400, 800, 1500, 5000, 10000, 42192)
th <- c(9.84, 19.32, 43.19, 102.58, 215.78, 787.96, 1627.34, 7956.00)
td <- c(10.94, 22.12, 48.25, 117.73, 240.83, 899.88, 1861.63, 8765.00)

mod.h <- lm(log(th) ~ I(log(dist)))
mod.d <- lm(log(td) ~ I(log(dist)))            
coef(mod.h)
coef(mod.d)

#aparanment s?n valors molt propers

#tot i que la unica manera de comprovarho ?s fer un contrast d'hip?tesis

#Model general:
#X=(alpha1, beta1, alpha2, beta2) matriu de disseny
#Y=(tempshomes)
#  (tempsdones)

alpha1 <- c(rep(1,8), rep(0,8))
beta1 <- c(log(dist), rep(0,8))
alpha2 <- c(rep(0,8), rep(1,8))
beta2 <- c(rep(0,8), log(dist))
temps <- c(th,td)
g <- lm(log(temps) ~ 0 +alpha1 + beta1 + alpha2 + beta2) # modelo general
model.matrix(g)
coef(g)

# paralelas -> beta1=beta2
# dona molt diferent beta1 a beta2 perque no ?s el temps sino el logaritme del temps
#H0: beta1 = beta2
#Xtilde = (alpha1, beta1, alpha2)

g0 <- lm(log(temps) ~ 0 + alpha1 + I(beta1+beta2) + alpha2) # modelo de la hipotesis nula
anova(g0,g)


#H0=alpha1-alpha2
#Xtilde= (alpha1,beta1,beta2)
# concurrentes -> alpha1 = alpha2 en el origen de cordenadas las dos rectas se cortan
#  t-student -> alpha1-alpha2 = 0 // (1,0,-1,0) (alpha1,beta1,alpha2,beta2)t

qr(model.matrix(g))$rank ## para calcular el rango de model matrix 
g0 <- lm(log(temps) ~ beta1 + beta2)
anova(g0,g)

#test de coincidencia, opciones:
#1? contrastar si las rectas son paralelas beta1=beta2, si lo son ->
#2? contrastar si son coincidentes alpha1=alpha2 suponiendo que son paralelas 
# o bien, hacer un test F directo
# tenemos que juntas los datos de las dos rectas en una unica recta

g0 <- lm (log(temps) ~ I(beta1+beta2))
model.matrix(g0)
anova(g0,g)

# vemos que no es la misma recta, respecto al modelo general hemos aceptado que son paralelas y concurrentes 
# pero respecto al modelo paralelo rechazamos que sean coincidentes

# suponemos que son paralelas
# la I(x,x) junta en una sola columna

g <- lm(log(temps) ~ 0 + alpha1 + I(beta1+beta2) + alpha2)
g0 <- lm(log(temps) ~ I(beta1+beta2))
anova(g0,g)

# rechazamos que son coincidentes


# para comparar las varianzas usamos el test F que en R es:
# H0 = sigma1^2 = sigma2^2

var.test(residuals(mod.h),residuals(mod.d))

# p-valor = 0.9198 aceptamos H0

# test f que se basa en la normalidad de los datos, si no fueran normales no se podr?a hacer con el test F ni bartlett
# si hubiesen 3 o 4 rectas y no dos tendriamos que hacer otro test que es una extension del F, test Bartlett 
# bartlett.test
# tambien tenemos el test de Levene que es mas robusto

library(car)
residus<- c(residuals(mod.h),residuals(mod.d))
grup <- factor(c(rep(1,8),rep(2,8)))
leveneTest(residus,grup)


# del tema 6

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

# analisis de residuos
plot(r1, which=1)
plot(r1, which=2)
plot(r1, which=3)
plot(r1, which=4)

#1? grafico, grafico ideal, errores aleatorios
# 3? grafico, esta bien aunque no salga todo horizontal porque la curva que se ve puede ser asi porque hay pocos datos
plot(r2, which=1)
plot(r2, which=2)
plot(r2, which=3)
plot(r2, which=4)
# 1? grafico, hay una curva, prohibido!! los errores no son aleatorios, la estructura del modelo falla
# 2? grafico, sobre la normalidad no pasa nada
# 3? grafico, no se ve la patologia que se veia en el primero, lo ideal es una linea horizontal recta
# 4? aun no lo podemos comentar
plot(r3)
# 1? grafico 
# 2? grafico, indica claramente que el punto 3 es un out lier?
plot(r4)
# 4? grafico, nos preocupa, la distancia de cook es elevada en algun punto


