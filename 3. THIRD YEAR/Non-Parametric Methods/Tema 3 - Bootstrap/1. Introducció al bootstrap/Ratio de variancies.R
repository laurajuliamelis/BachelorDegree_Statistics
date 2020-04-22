# Considerem aquestes dades "reals":
x1 = c(4.44, 2.88, 0.91, 0.44, 0.15, 5.70, 2.95, 1.05, 1.63, 1.39, 0.38, 0.33)
x2 = c(7.66, 27.85, 20.67, 4.50, 23.38, 7.83, 36.29, 29.38, 38.30, 15.93, 13.16, 
       4.30, 53.23, 30.02, 15.63, 2.80, 11.24)

n1 = length(x1)  # 12
n2 = length(x2)  # 17

# Considerem el problema d'estimar el paràmetre 'theta' igual a la ratio o quocient
# de les seves, desconegudes, variàncies poblacionals:
#                     theta = sigma1^2 / sigma2^2

# Devegades utilitzem el bootstrap per estimar alguna característica de
# la distribució d'un estadístic o un estimador com ara la seva variància.

# Per estimar theta un podria pensar que un estimador molt raonable és el
# quocient de les respectives variàncies mostrals no esbiaixades, var(x1)/var(x2).

# Devegades interessa estimar la variància de var(x1)/var(x2) com estimador 
# de theta = sigma1^2/sigma2^2. Pel mètode bootstrap això es pot fer així:
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

# Interval de confiança per theta.
# Ens caldrà conèixer quina és la distribució mostral de q = (var(x1) / var(x2)) / theta.

# Mides mostrals a simular
# (És important que ens quedem amb la distribució obtinguda per mides
# mostrals 12 i 17 respectivament, ja que l'exemple bootstrap amb dades
# "reals" té aquestes mides precisament)
n1 = 12
n2 = 17

# Simulació per determinar quina seria la veritable distribució de
# q = (var(x1) / var(x2)) / theta, on theta = sigma1^2/sigma2^2, la ratio
# de les "veritables" variàncies poblacionals, 
# sota una distribució exponencial:

# Ens inventem uns valors poblacionals, podem canviar els valors de
# mu1 i mu2 per comprovar el caràcter pivotal de 'q':
mu1 = 4
rate1 = 1 / mu1
sigma1 = mu1
mu2 = 20
rate2 = 1 / mu2
sigma2 = mu2

theta = sigma1^2 / sigma2^2
theta

# Simulació:
nsim = 100000

set.seed(127)
q.sims = replicate(nsim, {
  x1.sim = rexp(n1, rate = rate1)
  x2.sim = rexp(n2, rate = rate2)
  (var(x1.sim) / var(x2.sim)) / theta
})

windows(21, 21)

# Estimació suavitzada de la funció de densitat:
maxRatio = quantile(q.sims, probs = 0.99)
dens.sim = density(q.sims, from = 0, to = maxRatio)
# La comparem amb la distribució F(n1-1,n2-1):
dens.F = df(dens.sim$x, df1 = n1 - 1, df2 = n2 - 1)

plot(dens.sim$x, dens.F, col = "red", type = "l")
lines(dens.sim$x, dens.sim$y, col = "green")

# Té caràcter pivotal (com podríem observar jugant amb diversos valors
# dels paràmetres), però evidentment la seva distribució
# no és una F(n1-1,n2-1)

# Estimació bootstrap no paramètric de la distribució de
# (var(x1) / var(x2)) / theta:
B = 10000

theta.estim = var(x1) / var(x2)
# Encara que en pura ortodoxia bootstrap seria dividint les
# variàncies mostrals esbiaixades (raó?)
theta.estim = (((n1 - 1) / n1) * var(x1)) / (((n2 - 1) / n2) * var(x2))
theta.estim

# Generació de B rèpliques bootstrap de q = (var(x1) / var(x2)) / theta:
set.seed(23771)
q.boots = replicate(B, {
  x1.boot = sample(x1, replace = TRUE)
  x2.boot = sample(x2, replace = TRUE)
  (var(x1.boot) / var(x2.boot)) / theta.estim
})

# Gràfica de la funció de densitat de q, estimada mitjançant bootstrap:
dens.boot = density(q.boots, from = 0, to = maxRatio)
lines(dens.boot$x, dens.boot$y)

# Interval de confiança bootstrap no paramètric per theta (ratio de variàncies
# poblacionals):
ic = (var(x1) / var(x2)) / quantile(q.boots, probs = c(0.975, 0.025))
names(ic) = NULL
attr(ic, "conf.level") = 0.95
ic


# Estimació bootstrap paramètric-exponencial de la distribució de
# q = (var(x1) / var(x2)) / theta:

# Si suposem distribució exponencial per les dades tenim que 
# rate la podem estimar com 1 / (mitjana mostral).
# A mé, com que sigma^2 = 1 / rate^2, podem estimar la variància
# simplement com (mitjana mostral)^2
 
x1.barra = mean(x1)
x2.barra = mean(x2)
rate1.estim = 1 / x1.barra
rate2.estim = 1 / x2.barra
# Per tant, estimem var com 1 / (rate.estim)^2 = x.barra^2
# i theta com (x1.barra / x2.barra)^2
theta.estim = (x1.barra / x2.barra)^2
theta.estim

set.seed(23771)
q.paramBoots = replicate(B, {
  x1.boot = rexp(n1, rate = rate1.estim)
  x2.boot = rexp(n2, rate = rate2.estim)
  (var(x1.boot) / var(x2.boot)) / theta.estim
})

# Estimació bootstrap paramètric exponencial de la densitat de la ratio:
dens.paramBoot = density(q.paramBoots, from = 0, to = maxRatio)
# Gràfica de la funció de densitat de la ratio estimada mitjançant  
# bootstrap paramètric exponencial:
lines(dens.paramBoot$x, dens.paramBoot$y, col = "blue")

legend("topright", 
       legend = c("F(11,16)", "Veritable", "Bootstrap no par.", "Bootstrap par."),
       text.col = c("red", "green", "black", "blue"))

# Interval de confiança bootstrap paramètric-exponencial per theta
# (ratio de variàncies poblacionals):
ic = (var(x1) / var(x2)) / quantile(q.paramBoots, probs = c(0.975, 0.025))
names(ic) = NULL
attr(ic, "conf.level") = 0.95
ic

