#EJERCICIO2
datos<-read.table("CyclingPower.csv",sep=";",dec=",",header=TRUE)
datos

#1)
g <- lm(AV.Power ~ ., data = datos)
summary(g)
# Varianza estimada del error
summary(g)$sigma^2
#> summary(g)$sigma^2
#[1] 121.3527

#2)Una buena medida de ajuste es calcular la correlación:
summary(g)
#Multiple R-squared: 0.7334
#Podemos decir que tiene un buen ajuste, aunque no del todo exacto

#regresión significativa
summary(g)
#F-statistic: 9.905 on 5 and 18 DF,  p-value: 0.0001111 
# La regresión es significativa.
#Que sea significativa quiere decir que es útil, que tiene sentido calcularla
#y que las regresoras afectan a la respuesta y nos ayudan a hacer predicciones.
#Si la regresión es significativa rechazamos la siguiente hipótesis:
#betaP.4mM=betaP.Tlac=betaP.Tlac.ll=betaDMax=betaRise.1.PB= 0


#3)

#a)
#varianza constante(Homocedasticidad)
# Recta de regressió entre valores absolutos y valores ajustados.
summary(lm(abs(residuals(g)) ~ fitted(g)))
#p-value: 0.2513 
#La regresión no es significativa, porlo tanto el pendiente de la recta = 0 (Varianza constante)
# Método John Fox.
ncvTest(g)
#p = 0.3452906 
#Por este método también vemos que la varianza es constante

#b) 
#Hipótesis de normalidad
# Comprobamos gráficamente y analíticamente
#Grafico: QQ-plot
plot(g, which = 2)
# Tenemos 3 puntos bastante alejados
# Test formals
shapiro.test(residuals(g))
#p-value = 0.5117   aceptamos normalidad

# c) Puntos con influencia potencial
leverage <- hatvalues(g)
# El criterio a seguir será: h > 2(k+1)/n
k <- 5 # k num vars
which(leverage > 2*(k+1)/24)
# Nos da solo el punto 21

# d) Outliers
# Criterio: |t| > 2
t <-rstudent(g)
which(abs(t)>2)
# Da los puntos 14 y 16
#Si probamos con Bonferroni vemos que solo nos da el punto 14
outlierTest(g)
# 14
#Gráficamente
Boxplot(t)

# e) Puntos influentes
#Método DFBETAS
dfbetas(g)

# calculamos el límite 2/sqrt(n)
k <- 2/sqrt(24)
which(abs((dfbetas(g))[,1])>k)  # 24 35
which(abs((dfbetas(g))[,2])>k)  # 5 24
which(abs((dfbetas(g))[,3])>k)  # 5 24
which(abs((dfbetas(g))[,4])>k)  # 24 39
which(abs((dfbetas(g))[,5])>k)  # 24 27 35
#Utilizamos el método DFFITS
dffits(g)
# Calculamos el límite
k <- 2*sqrt((5 + 1)/24)
which(abs((dffits(g)))>k) 
# Los puntos influyentes son 14 16 21 24 

# f) Problemes de multicolinealitat
#Miramos la correlación entre las variables regresoras
round(cor(datos[,1:5]),3)
# Tenemos un 0.952 entre  P.Tlac y Dmax, que es el más alto
# Miramos los FIV's
vif(g)
# Son menores que 10, por lo tanto no podemos hablar de multicolinealidad






# 4) Quitamos el punto 21
# El punto 21 es un punto influente (influencia potencial)
g2 <- lm(AV.Power ~ ., data = datos[-21])
summary(g2)
summary(g2)$sigma^2
#[1] 121.3527 #Vemos que se mantiene
#Vemos que el ajuste aumenta un poco
plot(g2, which = 2)

# Miramos la normalidad
shapiro.test(residuals(g2))
# p-value = 0.5117   el hecho de quitar el punto 21 no afecta a la normalidad

# Ahroa mriamos la homocedasticidad
ncvTest(g2)
# p = 0.3452906 , seguimos considerando cierta la homocedasticidad, aunque ahora el p valor es más alto que con el punto 21.
#EL punto 14 vemos que es outlier y punto influyente

#5)


# 6) IC
predict(g, newdata = data.frame(P.4mM = 200,  P.Tlac = 175,	P.Tlac.ll = 131,	DMax = 152,	Rise.1.PB = 180), interval = 'prediction')

#fit      lwr      upr
#1 180.6768 153.6813 207.6722

#7)
g3 <- lm(AV.Power ~ Rise.1.PB, data = datos)
summary(g3)
round(cor(datos[,1:2]),3)
#Correlación = 0.803



#EJERCICIO3
datos<-read.table("CyclingPower.csv",sep=";",dec=",",header=TRUE)
datos

#1)
g <- lm(AV.Power ~ ., data = datos)
summary(g)


# Selección por AIC
g_AIC <- step(g)
#Vemos dos opciones (diferentes AIC)
#Step:  AIC=118.44
#AV.Power ~ P.4mM + P.Tlac.ll + DMax + Rise.1.PB
#ó
#Step:  AIC=116.99
#AV.Power ~ P.Tlac.ll + DMax + Rise.1.PB


# Selección por Cp de Mallows
#No me sale

# Intento hacerlo en vez de Cp de Mallows, selección del modelo por Forward Stepwise
# Fórmula del modelo completo
gc.formula <- formula(g)
# Model simple
g0 <- lm(AV.Power ~ 1, data = datos)
# Forward stepwise
modelo <- g0
add1(modelo, scope = gc.formula, test="F")
#Añado la variable con la F mayor(Rise.1.PB)
model <- update(g0, ~ . + Rise.1.PB)
# Al modeol g0 le sumo Rise.1.PB
add1(modelo, scope = gc.formula, test="F")
#Single term additions
#Model:
#  AV.Power ~ Rise.1.PB
#Df Sum of Sq    RSS    AIC F value Pr(>F)
#<none>                 2704.7 117.39               
#P.4mM      1    29.086 2675.6 119.13  0.2283 0.6377
#P.Tlac     1    38.019 2666.7 119.05  0.2994 0.5900
#P.Tlac.ll  1   140.432 2564.3 118.11  1.1501 0.2957
#DMax       1   137.501 2567.2 118.14  1.1248 0.3009
#Añado la variable con la F mayor(P.Tlac.ll)
model <- update(modelo, ~ . + P.Tlac.ll)
# A el modelo ñe sumo P.Tlac.ll
add1(modelo, scope = gc.formula, test="F")
# Ya no tengo que añadir más

g_forward <- modelo

# Coeficients de determinación de los modelos

summary(g)$adj.r.squared   
#[1] 0.6593934
summary(g_AIC)$adj.r.squared 
#[1] 0.6840969
summary(g_forward)$adj.r.squared 
#0 #está mal...

#c)
# Intervalos de confianza
confint(g, "income")
confint(g_AIC, "income")
confint(g_forward, "income")



# 3) Ajuste del modelo mediante el método LTS(Least trimmed squares)
library(MASS)
g_lts <- ltsreg(datos~ P.Tlac.ll + DMax + Rise.1.PB, data = datos, nsamp = "exact")
coef(g_lts)
