
cdrate = read.table("cdrate.dat", 
  col.names = c("interes","tipus.entitat"),
  colClasses = c("numeric", "factor"))

# Gràfics en una finestra separada:
windows(21,21)

interes = cdrate[,"interes"]

# Histograma:
hist(interes, 
  ylab = "densitat", main = "Histograma d'interes", 
  breaks = 7)
# Si li deixem triar els intervals:
hist(interes, 
     ylab = "densitat", main = "Histograma d'interes")


# Polígon de freqüències:

# Per dibuixar polígon de freqüències:
require(ggplot2)
x = seq(from = 7.4, to = 9, by = 0.01)
qplot(x, data = data.frame(x = interes), geom = "freqpoly", binwidth = 0.2)

# Estimacions més suaus de la densitat:
# S'aniran col·locant successivament en forma d'una matriu 3 x 2:
par(mfrow = c(3,2))

# Ajust normal:
fx = dnorm(x, mean = mean(interes), sd = sd(interes))
plot(x, fx,
  type = "l",
  xlab = "interes", ylab = "densitat", main = "Ajust normal")

# Ajust normal per bancs i caixes per separat:
mitjanes = tapply(interes, cdrate[,"tipus.entitat"], mean)
desviacions = tapply(interes, cdrate[,"tipus.entitat"], sd)
f.bancs = dnorm(x, mean = mitjanes[1], sd = desviacions[1])
f.caixes = dnorm(x, mean = mitjanes[2], sd = desviacions[2])
plot(x, f.bancs,
  type = "l", col = "blue",
  xlab = "interes", ylab = "densitat", main = "Ajust normal per separat",
  ylim = c(0,1.7))
lines(x, f.caixes)

# Mixtura de dues normals:
proporcions = tapply(interes, cdrate[,"tipus.entitat"], length) / length(interes)
f.mixtura = proporcions[1] * f.bancs + proporcions[2] * f.caixes
plot(x, f.mixtura,
  type = "l",
  xlab = "interes", ylab = "densitat", main = "Mixtura de dues normals")

# Estimació no paramètrica de la densitat:
f.noparam = density(interes)
plot(f.noparam,
  xlab = "interes", ylab = "densitat", main = "Ajust no parametric (nucli gaussia)")
  
# Determinació de l'amplada de finestra 'h' o "bandwidth".
# Es pot donar un valor mitjançant l'argument 'bw' de la funció 'density'.
# Si no s'especifica, per defecte s'avalua com s'indica a les transparències
# (presentació "Estimacio_no_parametrica_densitat") de teoria per a nucli
# gaussià, però amb el factor 0.9 en lloc d'1.059.
# Anteriorment no hem especificat el valor de l'argument 'bw'. El valor escollit
# per defecte, que coincideix amb la desviació típica del nucli, és:
valor.h = f.noparam$bw
valor.h
# variància del nucli:
var.h = valor.h^2
var.h

# L'argument 'bw' també pot ser un string indicant el mètode d'ajust de
# l'amplada de finestra. Per a que coincideixi amb el mètode explicat a les
# transparències (factor 1.059) cal indicar:
f.noparam = density(interes, bw = "nrd")
plot(f.noparam,
  xlab = "interes", ylab = "densitat", 
  main = "Ajust no parametric (nucli gaussia)", sub = "bw sgons Scott(1992)")

valor.h = f.noparam$bw
valor.h
# variància del nucli:
var.h = valor.h^2
var.h
# Per saber més sobre altres mètodes de determinació de l'amplada de finestra,
# mireu l'ajuda de 'density'

# Prova de diversos nuclis:
par(mfrow=c(3,3))
nuclis <- c("gaussian", "epanechnikov", "rectangular",
  "triangular", "biweight", "cosine", "optcosine")
sapply(nuclis, 
  function(nom.nucli) plot(density(interes,kernel=nom.nucli),main=nom.nucli)
)
par(mfrow=c(1,1))


# Generació de valors segons una densitat estimada no paramètricament:
# P.e. generació segons 'f.noparam' amb nucli normal i amplada (bandwidth) 

set.seed(213)
# escollim a l'atzar un element de la mostra (generem la distribució empírica):
xi = sample(interes, size = 1)
xi
# generem una perturbació normal de mitjana 0 i desviació típica 'valor.h'
# (igual a la "bandwith" amb que R ha creat la densitat):
z = rnorm(1, sd = valor.h)
z
# sumem tots dos valors:
xi + z

# Generació d'una remostra bootstrap quan l'estimació de la distribució
# és l'anterior (bootstrap semiparamètric):
sample(interes, replace = TRUE) + rnorm(length(interes), sd = valor.h) 

#               Interval de confiança bootstrap percentil
#            per a la veritable variància del tipus d'interès

# Hem de generar B remostres bootstrap semiparamètric nucli gaussià:
B = 10000
x.boots = replicate(B,
            sample(interes, replace = TRUE) + 
            rnorm(length(interes), sd = valor.h))
# Ara tenim una gran matriu de 69 files i 10000 columnes, 
# cada columna correspon a una remostra bootstrap.

# A continuació hem d'estimar el paràmetre (aquí, variància poblacional)
# sobre cada remostra bootstrap.
# Si ho féssim de manera poc reflexiva, segurament faríem:
var.boots = apply(x.boots, 2, var)
# Ja que la funció 'var' de R implementa un bon estimador (no esbiaixat)
# de la variància. Per tant, l'interval de confiança bootstrap percentil
# seria:
quantile(var.boots, probs = c(0.025, 0.975))

# L'anterior no és que estigui terriblement malament, però no verifica el
# que caldria segons pura ortodoxia bootstrap. Recordem que de la mateixa
# manera que la variància poblacional és una funció de la veritable distribució
# F de les dades (amb la que en realitat no podem fer res en ser desconeguda),
# hauríem d'estimar la variància amb l'expressió que resulti de substituir F
# per la corresponent estimació d'ella, ara una estimació nucli.

# Quina seria l'expressió de la variància si substituíssim la veritable
# distribució (desconeguda) F dels interessos per l'estimació nucli gaussià?
# Fixem-nos que correspondria a la variància d'una variable aleatòria amb
# distribució empírica + la variància d'una normal N(0, sigma = valor.h),
# totes dues independents.
# És a dir, tindríem la variància mostral sense corregir + valor.h^2.
# (No oblidem que tal com l'hem obtingut, 'valor.h' també és un valor aleatori,
# dependent de la mostra, ja que ha estat ajustat per la funció 'density'
# a partir de la mostra.) 

# Variància mostral no corregida o "quasivariància":
var.biased <- function(x) {
  inv.n = 1 / length(x)
  return(sum((x - sum(x) * inv.n)^2) * inv.n)
}

# Variància associada a la distribució nucli:
var.nucli = function(x, bw = "nrd") {
  return(var.biased(x) + density(x, bw = bw)$bw^2)
}

# Càlcul de la variància nucli per cada remostra:
var.boots = apply(x.boots, 2, var.nucli)

# IC percentil bootstrap 95%:
quantile(var.boots, probs = c(0.025, 0.975))

# Comparem-lo amb l'IC percentil bootstrap no paramètric:
# Novament, seria més correcte (ortodòxia bootstrap!) calcular la variància
# mostral esbiaixada sobre cada remostra no paramètrica:
var.boots = apply(replicate(B, sample(interes, replace = TRUE)), 2, var.biased)                                
quantile(var.boots, probs = c(0.025, 0.975))
