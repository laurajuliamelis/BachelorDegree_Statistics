# ******************************************************************************
#                   ESTUDIO DE SIMULACIÓN DEL ESTADÍSTICO
#                       t de Student para una muestra
# ******************************************************************************
# Tamaño muestral:
n <- 5

# Parámetros de las muestras a generar:
media <- 10
sigma <- 2

# Tamaño muestral de la simulación:
m <- 20000

# Función que implementa el estadístico t para una única muestra:
tStat <- function(x, mu = 0) sqrt(length(x)) * (mean(x) - mu) / sd(x)

# ¡Para simular necesitamos especificar COMPLETAMENTE el modelo de procedencia
# de los datos: distribución, parámetros...!)

# Generación de una muestra iid N(media, sigma) de tamaño n:
rnorm(n, media, sigma)
# Cálculo del estadístico t sobre una muestra N(media, sigma) de tamaño n
# (evidentemente, la muestra será distinta de la anterior):
tStat(rnorm(n, media, sigma), media)

# Fijamos una semilla aleatoria inicial para que el experimento sea repetible:
set.seed(127)

# Simulamos la obtención de m valores del estadístico t bajo normalidad y
# los valores de media, sigma y n fijados previamente:
t.sim <- replicate(m, tStat(rnorm(n,media,sigma), media))

# Los m valores de t obtenidos por simulación nos proporcionan una buena idea
# empírica de la verdadera distribución muestral de este estadístico.
# Algunas gráficas en este sentido:

# Histograma de los m valores obtenidos por simulación:
hist(t.sim, breaks=70, freq=F, xlim=c(-10,10), ylim=c(0,0.50))
# Eje de abcisas que vamos a representar:
eje.x <- seq(from=-10, to=10, by=0.25)
# ¿Es adecuado suponer que este estadístico tiene distribucion N(0,1)? 
lines(eje.x, dnorm(eje.x), type="l", col="red")
# ¿Es más adecuada la distribución t de Student con n - 1 g.d.l?
lines(eje.x, dt(eje.x, df=n-1), type="l", col="blue")

# Las conclusiones anteriores en principio solamente son válidas bajo las
# condiciones en las que hemos realizado el experimento de simulación: 
# iid bajo normalidad y valores concretos de media, sigma y n.

# Sería necesario repetir las simulaciones anteriores bajo otras condiciones,
# para estudiar hasta que punto los resultados son generales. 

# Para ello vamos a encapsular los procesos anteriores en una única función:
# (Esta función permite "jugar" con el tamaño muestral, con los parámetros de
# la distribución..., no con la distribución de los datos, que en todo momento
# va a ser normal)

sim.tNorm <- 
function(m = 20000, # tamaño muestral de la simulación
        n = 5, # tamaño de las muestras generadas en cada réplica de simulación
        media = 10, sigma = 2, # parámetros de las v.a. a generar
        null.return = TRUE)
{
  # Simulación propiamente dicha:
  t.sim <- replicate(m, tStat(rnorm(n,media,sigma), media))
  # Análisis gráfico de los resultados de la simulación:
  hist(t.sim, breaks=70, freq=F, xlim=c(-10,10), ylim=c(0,0.50))
  eje.x <- seq(from=-10, to=10, by=0.25)
  lines(eje.x, dnorm(eje.x), type="l", col="red")
  lines(eje.x, dt(eje.x, df=n-1), type="l", col="blue")
  title(sub=paste("N(",media,",",sigma,")   n = ",n))
  if (null.return)
    return()
  else
    return(t.sim)
}

# Repetimos exactamente la misma simulación anterior:
set.seed(123) # Para que sea repetible inicializamos la secuencia alatoria
sim.tNorm()

# Bajo el mismo tamaño muestral n = 5, probamos otras medias y desviaciones
# estándar para los datos simulados:
set.seed(123)
sim.tNorm(media = 10, sigma = 2)
set.seed(123)
sim.tNorm(media = 10, sigma = 4)
set.seed(123)
sim.tNorm(media = 10, sigma = 8)
set.seed(123)
sim.tNorm(media = 10, sigma = 16)
set.seed(123)
sim.tNorm(media = 100, sigma = 2)
set.seed(123)
sim.tNorm(media = 100, sigma = 4)
set.seed(123)
sim.tNorm(media = 100, sigma = 8)
set.seed(123)
sim.tNorm(media = 100, sigma = 16)
# etc...
# Parece que la distribución muestral de t siempre es la misma

# En cambio, si experimentamos con distintos tamaños muestrales n:
set.seed(123)
sim.tNorm(n = 5)
set.seed(123)
sim.tNorm(n = 10)
set.seed(123)
sim.tNorm(n = 20)
set.seed(123)
sim.tNorm(n = 30)
set.seed(123)
sim.tNorm(n = 100)
set.seed(123)
# etc...
# Parece que la dispersión de la distribución muestral de t va disminuyendo
# (¡como debe ser, sabemos que t sigue una distribución de Student con n - 1
# grados de libertad!)

# Muchas veces nuestro interés se centra en algún(os) aspecto(s) concreto(s)
# de la distribución muestral de un estadístico (p.e. la media, la varianza, la
# probabilidad en una cola) y no tanto en su forma general.
# Consideremos el caso de la probabilidad en determinada cola de la distribución:

# Bajo una districión normal, este valor z dejaría una probabilidad 0.025
# a su derecha:
alfa <- 0.025
z <- qnorm(alfa, lower.tail = F)
z

# Pero, en realidad ¿qué proporción de valores de t quedan a la derecha de z?:
set.seed(127)
t.sim <- replicate(m, tStat(rnorm(n,media,sigma), media))
alfa.real <- sum(t.sim > z) / m
alfa.real

# ¿Podemos concluir que la verdadera probabilidad en esta cola es en realidad
# superior a lo esperable según la normal (0.025)? (Es decir, que si para hacer
# inferencia sobre t empleamos las tablas de la normal, en realidad estamos
# empleando unas tablas equivocadas.)
# ¿En concreto podemos afirmar que es 0.06166?
# 
# Cuando simulamos estocásticamente, NUNCA DEBEMOS OLVIDAR que en realidad
# estamos realizando un EXPERIMENTO ALEATORIO, sujeto a error analizable
# estadísticamente.
# Fijémonos que si repetimos la simulación a partir de semillas distintas,
# LOS RESULTADOS NO SON CONSTANTES, TIENEN UNA VARIABILIDAD:

# (Ahora la semilla aleatoria global de R está en el valor donde la dejaron las
# n*m generaciones de variables normales a partir de set.seed(127))
t.sim <- replicate(m, tStat(rnorm(n,media,sigma), media))
alfa.real <- sum(t.sim > z) / m
alfa.real
# (Y ahora está en otro estado, tras n*m nuevas generaciones)
t.sim <- replicate(m, tStat(rnorm(n,media,sigma), media))
alfa.real <- sum(t.sim > z) / m
alfa.real

# Por ello, siempre se debería justificar los resultados de las simulaciones y
# las afirmaciones que se realizan a partir de ellos mediante los análisis
# estadísticos adecuados.
# Por ejemplo, un intervalo de confianza al 95% (basado en simple teoría
# asintótica normal, "m es muy grande"):

paste(alfa.real, "±", qnorm(.975)*sqrt(alfa.real*(1-alfa.real)/(m-1)))

# "Tabla" de valores críticos de la distribución t obtenida empíricamente
# mediante simulación:
probs <- c(0.999, 0.995, 0.975, 0.95, 0.925, 0.9)
quantile(t.sim, probs)
# Valores "verdaderos" (según una distribución t de Student con n-1 g.d.l.)
# que tendríamos que emplear:
qt(probs, df=n-1)

# EJERCICIO:
# El empleo de la distribución t de Student, ¿Sigue siendo válido bajo no
# normalidad? (Por ejemplo, bajo datos con distribución exponencial.)


sim.tGeneral <-
function(m = 20000, # tamaño muestral de la simulación
        n = 5, # tamaño de las muestras generadas en cada réplica de simulación
        media = 10, sigma = 2, # parámetros de las v.a. a generar
        generador = rnorm, # generador aleatorio, siempre debe tener la misma
                           # interfaz que 'rnorm': generador(n, mean, sd)
        null.return = TRUE)
{
  # Simulación propiamente dicha:
  t.sim <- replicate(m, tStat(generador(n,media,sigma), media))
  # Análisis gráfico de los resultados de la simulación:
  hist(t.sim, breaks=70, freq=F, xlim=c(-10,10), ylim=c(0,0.50))
  eje.x <- seq(from=-10, to=10, by=0.25)
  lines(eje.x, dnorm(eje.x), type="l", col="red")
  lines(eje.x, dt(eje.x, df=n-1), type="l", col="blue")
  title(sub=paste("(",media,",",sigma,")   n = ",n))
  if (null.return)
    return()
  else
    return(t.sim)
}

runiform <- function(n, mean=0, sd=1) {
  # Genera n "uniformes estándar" E(U) = 0, var(U) = 1
  b <- sqrt(3)
  u <- runif(n, -b, b)
  # convierte estas uniformes a la escala deseada:
  return(sd * u + mean)
}

x <- runiform(100000, media, sigma)
mean(x)
sd(x)
rm(x)

set.seed(123)
sim.tGeneral(media = 10, sigma = 2, generador = runiform)
set.seed(123)
sim.tGeneral(media = 10, sigma = 4, generador = runiform)
set.seed(123)
sim.tGeneral(media = 10, sigma = 8, generador = runiform)
set.seed(123)
sim.tGeneral(media = 10, sigma = 16, generador = runiform)
set.seed(123)
sim.tGeneral(media = 100, sigma = 2, generador = runiform)
set.seed(123)
sim.tGeneral(media = 100, sigma = 4, generador = runiform)
set.seed(123)
sim.tGeneral(media = 100, sigma = 8, generador = runiform)
set.seed(123)
sim.tGeneral(media = 100, sigma = 16, generador = runiform)
# etc...
# Parece que la distribución t de Student sigue siendo aproximadamente válida
# para datos uniformes (distribución simétrica): método t robusto

# Y tanto mejor aumentando los tamaños muestrales n:
set.seed(123)
sim.tGeneral(n = 5, generador = runiform)
set.seed(123)
sim.tGeneral(n = 10, generador = runiform)
set.seed(123)
sim.tGeneral(n = 20, generador = runiform)
set.seed(123)
sim.tGeneral(n = 30, generador = runiform)
set.seed(123)
sim.tGeneral(n = 100, generador = runiform)
# etc...

rexpon <- function(n, mean=0, sd=1) {
  # Genera n "exponenciales estándar" E(Y) = 0, var(Y) = 1
  y <- rexp(n) - 1
  # convierte estas uniformes a la escala deseada:
  return(sd * y + mean)
}

x <- rexpon(100000, media, sigma)
mean(x)
sd(x)
hist(x)
rm(x)


set.seed(123)
sim.tGeneral(media = 10, sigma = 2, generador = rexpon)
set.seed(123)
sim.tGeneral(media = 10, sigma = 4, generador = rexpon)
set.seed(123)
sim.tGeneral(media = 10, sigma = 8, generador = rexpon)
set.seed(123)
sim.tGeneral(media = 10, sigma = 16, generador = rexpon)
set.seed(123)
sim.tGeneral(media = 100, sigma = 2, generador = rexpon)
set.seed(123)
sim.tGeneral(media = 100, sigma = 4, generador = rexpon)
set.seed(123)
sim.tGeneral(media = 100, sigma = 8, generador = rexpon)
set.seed(123)
sim.tGeneral(media = 100, sigma = 16, generador = rexpon)
# etc...
# Parece que la distribución t de Student no es válida
# para datos exponenciales (distribución asimétrica): método t no robusto
# frente a asimetría

# Y tanto mejor aumentando los tamaños muestrales n:
set.seed(123)
sim.tGeneral(n = 5, generador = rexpon)
set.seed(123)
sim.tGeneral(n = 10, generador = rexpon)
set.seed(123)
sim.tGeneral(n = 20, generador = rexpon)
set.seed(123)
sim.tGeneral(n = 30, generador = rexpon)
set.seed(123)
sim.tGeneral(n = 100, generador = rexpon)
# etc...

# ******************************************************************************
#                   ESTUDIO DE SIMULACIÓN DEL ESTADÍSTICO
#               t de Student para dos muestras independientes
# ******************************************************************************

# Para disponer de la función 'levene.test' por si queremos hacer pretest
# para igualdad de varianzas:
library(car)

# Para analizar una situación algo más compleja, vamos a estudiar mediante 
# simulación el estadístico t de Student para dos muestras independientes.
# Ayuda de t.test
?t.test
# ¿Es adecuado emplear unas tablas de la normal cuando se calcula el estadístico
# t para 2 muestras?

n1 <- 5
n2 <- 5
N <- n1 + n2
media1 <- 10
media2 <- 10
sigma1 <- 2
sigma2 <- 2

m <- 20000

muestra1 <- rnorm(n1,media1,sigma1)
muestra2 <- rnorm(n2,media2,sigma2)
t.test(muestra1, muestra2, var.equal = T)
t.test(muestra1, muestra2, var.equal = T)$statistic
t.test(muestra1, muestra2, var.equal = T)$p.value

# Posible pretest (Levene) para igualdad de varianzas:
grupo <- factor(c(rep(1,n1), rep(2,n2)))
var.iguales <- leveneTest(c(muestra1, muestra2), grupo)
var.iguales
# p-valor del test de Levene:
var.iguales[["Pr(>F)"]][1]
var.iguales$"Pr(>F)"[1]

set.seed(127)
t.sim2 <- replicate(m, 
  t.test(rnorm(n1,media1,sigma1), rnorm(n2,media2,sigma2), var.equal = T)$statistic
)

eje.x <- seq(from=-10, to=10, by=0.25) 
hist(t.sim2, breaks=70, freq=F, xlim=c(-10,10), ylim=c(0,0.50))
lines(eje.x, dnorm(eje.x), type="l", col="red")
lines(eje.x, dt(eje.x, df=N-2), type="l", col="blue")

# Otra manera de representar la muestra: en lugar de dos vectores una fórmula
grupo <- factor( rep(c("grupo1","grupo2"), times=c(n1,n2)), levels = c("grupo1","grupo2"))

set.seed(127)
t.sim2 <- replicate(m,
  t.test(c(rnorm(n1,media1,sigma1), rnorm(n2,media2,sigma2)) ~ grupo, var.equal = T)$statistic
)

eje.x <- seq(from=-10, to=10, by=0.25) 
hist(t.sim2, breaks=70, freq=F, xlim=c(-10,10), ylim=c(0,0.50))
lines(eje.x, dnorm(eje.x), type="l", col="red")
lines(eje.x, dt(eje.x, df=N-2), type="l", col="blue")


# Empleando las tablas adecuadas si los datos fuesen normales (es decir, las de la t de Student)
# ¿Hasta que punto el método es robusto si...
# ...los datos no son normales?
# ...las varianzas no son iguales?