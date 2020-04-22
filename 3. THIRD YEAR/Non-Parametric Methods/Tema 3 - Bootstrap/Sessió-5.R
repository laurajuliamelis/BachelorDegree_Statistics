### R code from vignette source 'sessio5.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: sessio5.Rnw:39-68
###################################################
# Considerem aquestes dades "reals":
x1 = c(4.44, 2.88, 0.91, 0.44, 0.15, 5.70, 2.95, 1.05, 1.63, 1.39, 0.38, 0.33)
x2 = c(7.66, 27.85, 20.67, 4.50, 23.38, 7.83, 36.29, 29.38, 38.30, 15.93, 13.16, 
       4.30, 53.23, 30.02, 15.63, 2.80, 11.24)

n1 = length(x1)  # 12
n2 = length(x2)  # 17

# Considerem el problema d'estimar 'theta' 

# De vegades utilitzem el bootstrap per estimar alguna característica de
# la distribució d'un estadístic o un estimador com ara la seva variància.

# Per estimar theta un podria pensar que un estimador molt raonable és el
# quocient de les respectives variàncies mostrals no esbiaixades, var(x1)/var(x2).

# De vegades interessa estimar la variància de var(x1)/var(x2) com estimador # de theta = sigma1^2/sigma2^2. 

# Generem mitjançant bootstrap no paramètric B rèpliques d'aquest estimador:
B = 10000
set.seed(23771)
theta.boots = replicate(B, {
  x1.sim = sample(x1, replace = TRUE)
  x2.sim = sample(x2, replace = TRUE)
  var(x1.sim) / var(x2.sim)
})

# Calculem la variància mostral d'aquests B valors:
var(theta.boots)


###################################################
### code chunk number 2: sessio5.Rnw:93-112
###################################################
# O també ens pot interessar el biaix d'un estimador.
# Per exemple:
#   var(x1) és un estimador no esbiaixat de sigma1^2
#   var(x2) és un estimador no esbiaixat de sigma2^2
# però:
#   var(x1)/var(x2) ho és de theta = sigma1^2/sigma2^2 ???

# El veritable biaix seria  E{var(x1)/var(x2)} - theta
# E{var(x1)/var(x2)} l'estimem mitjançant la mitjana mostral dels B valors
# que acabem d'obtenir:
Eboot = mean(theta.boots)
# theta.estim al "món bootstrap" juga el paper de theta al "món real",
# per tant, l'estimació bootstrap del biaix serà:
Eboot - var(x1)/var(x2)
# O millor: en pura ortodoxia bootstrap seria s'hauria de restar el quocient 
# de les variàncies mostrals esbiaixades (raó?)
theta.estim = (((n1 - 1) / n1) * var(x1)) / (((n2 - 1) / n2) * var(x2))
theta.estim
Eboot - theta.estim


###################################################
### code chunk number 3: sessio5.Rnw:118-212
###################################################

# Aquesta mostra fa el paper d'unes "dades reals" però en realitat sabem que
# procedeix d'una N(15, 3)
# ATENCIÓ: AIXÒ ÉS AIXÍ PER QUE ESTEM EN UNA SITUACIÓ "DE LABORATORI", AMB
# DADES REALS LÒGICAMENT DESCONEIXERÍEM COMPLETAMENT ELS PARÀMETRES REALS

x <- c(15.54, 21.06, 16.52, 13.62, 16.14, 10.98, 13.53, 16.02, 16.79, 15.90)

n <- length(x)

# mitjana retallada de les dades "reals":
mtrim <- mean(x, trim = 0.2)

# valors jackknife, suprimint cada vegada un valor:
mtrim_i <- numeric(length = n)
for (i in 1:n) mtrim_i[i] <- mean(x[-i], trim = 0.2)
# possiblement, una manera més eficient:
mtrim_i = vapply(1:n, function(i) mean(x[-i], trim = 0.2), FUN.VALUE = 0.0)

mtrim. <- mean(mtrim_i)

# estimació jackknife de la variància de la mitjana retallada:
varJ <- ((n - 1) / n) * sum((mtrim_i - mtrim.)^2)

# estimació jackknife del biaix de la mitjana retallada:
bJ <- (n - 1) * (mtrim. - mtrim)

# mitjana retallada "corregida pel biaix" (mtrim - bJ):
mtrimJ <- n * mtrim - (n - 1) * mtrim.


# Enfoc bootstrap

# una mostra aleatòria amb reemplaçament de x
# (equivalent a dir: una mostra a partir de la distribució empírica Fn),
# és a dir, una remostra bootstrap:
sample(x, replace = TRUE)

# nombre de rèpliques bootstrap:
B <- 10000

# determinació la variància i el biaix de la mitjana retallada 20%

# mitjana retallada d'una (re)mostra bootstrap:
mean(sample(x, replace = TRUE), trim = 0.2)

# B valors de la mitjana retallada a partir de B remostres bootstrap:
mtrim.boots <- replicate(B, mean(sample(x, replace = TRUE), trim = 0.2))
# les 10 primeres:
mtrim.boots[1:10]

# Estimació bootstrap (no paramètrica) de la variància de mitjana retallada:
var(mtrim.boots)

# Estimació bootstrap (no p.) del biaix
mean(mtrim.boots) - mean(x)
# o bé: ????
mean(mtrim.boots) - mean(x, trim = 0.2)
# discutir...


# Simulació per determinar la veritable variància i el veritable biaix
# de la mitjana retallada del 0.2,
# quan les dades procedeixen d'una normal de mitjana 15 i variància 9

# simulem 1000000 de valors de la mitjana retallada:
m <- 1000000
mtrim.sims <-  replicate(m, mean(rnorm(10, mean = 15, sd = 3), trim = 0.20))


# variància:
varMtrim <- var(mtrim.sims)
varMtrim

# i biaix:
bMtrim <- mean(mtrim.sims) - 15
bMtrim


# és nul el biaix?
Lower<-bMtrim - (qnorm(0.975) * sqrt(varMtrim / m))
Upper<-bMtrim + (qnorm(0.975) * sqrt(varMtrim / m))
IC<-c(Lower, Upper)
IC


# La simulació no descarta el valor 0 de biaix

# Bootstrap paramètric. Ara estem en una situació d'inferència paramètrica:
# podem assumir que les dades procedeixen d'una distribució normal però
# desconeixem els paràmetres d'aquesta distribució; 
# els hem d'estimar a # partir de la pròpia mostra
var(replicate(B, mean(rnorm(n, mean = mean(x), sd = sd(x)), trim = 0.20)))



