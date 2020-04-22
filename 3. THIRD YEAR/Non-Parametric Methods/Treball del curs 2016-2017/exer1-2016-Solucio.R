
# Per a disposar de la mostra de temps d'espera, simplement executa:
x = c(4.38, 3.12, 2.25, 1.37, 1.29, 3.85, 0.23, 0.01, 0.71, 0.49, 0.36, 0.12, 0.17, 0.5, 0.18, 
      2.57, 0.16, 2.09, 0.98, 0.51, 9.43, 0.2, 1.11, 1.29, 1.24, 0.88, 0.36, 0.08, 0.57, 1.6, 
      0.11, 0.57, 0.21, 0.6, 0.33, 0.1, 0.74, 0.17, 1, 0.3, 0.73, 4.01, 0.79, 0.26, 2.65, 2.4, 
      0.78, 0.48, 1.32, 0.57, 1.2, 0.23, 0.29, 0.12, 2.42, 0.93, 2.62, 1.02, 3.85, 3.51, 0.7, 
      1.18, 0.95, 0.98, 1.22, 0.08, 1.37, 1.05, 5.14, 0.57, 0.35, 0.41, 0.63, 2.83, 0.82, 0.08, 
      3.07, 1.02, 0.09, 1.45, 0.64, 1.72, 0.52, 0.91, 0.89, 0.87, 0.95, 5.28, 1, 0.76, 0.24, 
      0.46, 2.01, 0.3, 0.51, 1.61, 0.24, 0.32, 0.54, 0.17, 0.48, 0.39, 0.57, 0.23, 0.05, 0.11, 
      3.36, 0.86, 0.19, 0.97, 1.32, 1.81, 2.1, 1.74)
# o, alternativament, executa aquesta instrucció amb l'arxiu "datos.RDa" a la carpeta de treball:
load("datos.RDa")

n = length(x)

# Per a poder dibuixar la veritable funció de densitat de l'estadístic V, executa:
load("trueDens.RDa")
# amb el fitxer "trueDens.RDa" a la carpeta de treball.
# Aquesta instrucció carregarà en memòria l'objecte 'dens' de classe "density".

# Funció que calcula l'estimació esbiaixada de la variància, també
# coneguda com variància mostral no corregida o "quasivariància":
var.biased <- function(x) {
  inv.n = 1 / length(x)
  return(sum((x - sum(x) * inv.n)^2) * inv.n)
}

# Funció que calcula l'estadístic pivotal V:
pivotV = function(x, sigma2) {
  return((length(x) - 1) * var(x) / sigma2)
}

# 1) Estimació de la distribució mostral de V mitjançant bootstrap
# no paramètric:

# Nombre de rèpliques bootstrap "B" (aquí en direm 'nboot'):
nboot = 20000

# Generem 'nboot' remostres bootstrap (no paramètric) i sobre cadascuna
# calculem V.
# Ara la distribució que fa el paper de distribució de les dades, la que estem
# simulant, és la distribució empírica -recordeu "principi plug-in".
# La seva variància és la variància mostral esbiaixada, la que retorna 'var.biased'
# (i no el valor que retorna 'var'!!!)

# Valor que repetidament passarem a l'argument 'sigma2' de la funció 'pivotV'
# dins la iteració bootstrap, ara fa el paper de variància poblacional:
s2 = var.biased(x)

# Simulació bootstrap no paramètric:
set.seed(211763) # Per a que sigui repetible...
v.bootsNopar = replicate(nboot,
  pivotV(sample(x, replace = TRUE), sigma2 = s2)
)

# Representació gràfica de la densitat estimada mitjançant bootstrap i
# comparació amb altres densitats possibles de V:
windows(21,21)
# Histograma dels B valors bootstrap:
hist(v.bootsNopar, freq = FALSE, xlim = c(0,300), ylim = c(0,0.030))
# Eix d'abcisses a representar:
eix.x <- seq(from=0, to=300, by=0.5)
# És adequat suposar que V segueix una distribució khi-quadrat(n-1)?
lines(eix.x, dchisq(eix.x, df=n-1), type="l", col="blue")
# Veritable distribució mostral de V si les dades fossin exponencials:
lines(dens$x, dens$y, col = "green") 

# 2) Taula de quantils de la distribució bootstrap no paramètric de V:
quant.noPar = quantile(v.bootsNopar, 
                       probs = c(.01, .025, .05, .1, .9, .95, .975, .99))
quant.noPar

# 3) Igual que 1) però amb bootstrap paramètric exponencial:
# Caldrà generar les remostres bootstrap segons una distribució exponencial.
# Aquesta distribució estarà caracteritzada per un paràmetre, diguem-ne "lambda",
# que correspondria a l'argument "rate" de les funcions rexp, dexp, etc.
# Aquest paràmetre és desconegut i caldrà estimar-lo a partir de la mostra.
# Normalment s'agafa l'estimació de màxima versemblança:
promig = mean(x)
lambda.estim = 1 / promig

# Si assumim que les dades segueixen una exponencial de paràmetre "lambda",
# la mitjana i la variància segons aquesta distribució serien 1 / lambda i
# 1 / (lambda^2) respectivament. En altres paraules, la variància segons
# una distribució de paràmetre "lambda.estim" (valor que haurem de passar 
# repetidament a l'argument 'sigma2' de la funció 'pivotV' dins la iteració 
# bootstrap) serà:
s2 = promig^2

# Simulació bootstrap paramètric exponencial:
set.seed(211763) # Per a que sigui repetible...
v.bootsPar = replicate(nboot,
  pivotV(rexp(n, rate = lambda.estim), sigma2 = s2)
)

# Representació gràfica de la densitat estimada mitjançant bootstrap i
# comparació amb altres densitats possibles de V:
windows(21,21)
# Histograma dels B valors bootstrap:
hist(v.bootsPar, freq = FALSE, xlim = c(0,300), ylim = c(0,0.030))
# És adequat suposar que V segueix una distribució khi-quadrat(n-1)?
lines(eix.x, dchisq(eix.x, df=n-1), type="l", col="blue")
# Veritable distribució mostral de V si les dades fossin exponencials:
lines(dens$x, dens$y, col = "green") 

# 4) Taula de quantils de la distribució bootstrap paramètric exponencial de V:
quant.parExp = quantile(v.bootsPar, 
                        probs = c(.01, .025, .05, .1, .9, .95, .975, .99))
quant.parExp

# 5) Interval de confiança bootstrap no paramètric percentil i BCa:
# Percentil:

# En principi, seguint estrictament la teoria desenvolupada per Efron,
# en substituir la distribució desconeguda F de les dades per la distribució
# empírica Fn, l'estimador de la variància que obtenim és la variància mostral
# esbiaixada, la que ens dóna la funció 'var.biased' que hem creat abans.
# Per tant, primer hauríem d'obtenir 'nboot' rèpliques bootstrap de l'estimació
# del paràmetre problema (la variància poblacional):
s2 = var.biased(x)
var.bootsNopar = replicate(nboot, var.biased(sample(x, replace = TRUE)))

# IC percentil:
icPerc.Nopar = quantile(var.bootsNopar, probs = c(0.025, 0.975))
names(icPerc.Nopar) = NULL
attr(icPerc.Nopar, "conf.level") = 0.95
icPerc.Nopar

# IC BCa:
# Rèpliques jackknife de la mostra original i càlcul de 'var.biased' sobre
# cadascuna:
var_i = numeric(n)
for (i in 1:n) var_i[i] <- var.biased(x[-i]) 

# Estimació jackknife de la constant d'acceleració:
var_i <- mean(var_i) - var_i
a = sum(var_i^3) / (6 * sum(var_i^2)^1.5)
a

# Càlcul de la constant de correcció de biaix:
z0 = qnorm(sum(var.bootsNopar <= s2) / nboot)
z0

zalpha = - qnorm(0.025)
zalpha

icBCa.Nopar = quantile(var.bootsNopar, 
                c(
                  pnorm(z0 + (z0 - zalpha) / (1 - a * (z0 - zalpha))), 
                  pnorm(z0 + (z0 + zalpha) / (1 - a * (z0 + zalpha)))
                )
)
names(icBCa.Nopar) = NULL
attr(icBCa.Nopar, "conf.level") = 0.95
icBCa.Nopar

# 5) Interval de confiança bootstrap paramètric exponencial percentil i BCa:
# Percentil:

# Novament, seguint estrictament la teoria desenvolupada per Efron,
# en substituir la distribució desconeguda F de les dades per la distribució
# Exp(1/mean(x)), l'estimador de la variància que obtenim és mean(x)^2.
# Primer hauríem d'obtenir 'nboot' rèpliques bootstrap de l'estimació
# del paràmetre problema (la variància poblacional):
s2 = promig^2
var.bootsPar = replicate(nboot, mean(rexp(n, rate = lambda.estim))^2)

# IC percentil:
icPerc.Par = quantile(var.bootsPar, probs = c(0.025, 0.975))
names(icPerc.Par) = NULL
attr(icPerc.Par, "conf.level") = 0.95
icPerc.Par

# IC BCa:
# Rèpliques jackknife de la mostra original i càlcul de promig^2 sobre
# cadascuna:
var_i = numeric(n)
for (i in 1:n) var_i[i] <- mean(x[-i])^2

# Estimació jackknife de la constant d'acceleració:
var_i <- mean(var_i) - var_i
a = sum(var_i^3) / (6 * sum(var_i^2)^1.5)
a

# Càlcul de la constant de correcció de biaix:
z0 = qnorm(sum(var.bootsPar <= s2) / nboot)
z0

# zalpha = - qnorm(0.025)
# zalpha

icBCa.Par = quantile(var.bootsPar, 
                c(
                  pnorm(z0 + (z0 - zalpha) / (1 - a * (z0 - zalpha))), 
                  pnorm(z0 + (z0 + zalpha) / (1 - a * (z0 + zalpha)))
                )
)
names(icBCa.Par) = NULL
attr(icBCa.Par, "conf.level") = 0.95
icBCa.Par

# NOTA:
# En alguns treballs no s'han tingut en compte aquestes consideracions sobre quin és 
# l'estimador "plug-in" adequat. En aquests casos, quan calia un estimador de la variància,
# segurament s'haurà utilitzat sempre la funció 'var'.
# En concret, al fer la simulació bootstrap per determinar la distribució mostral
# del pívot V, al denominador s'haurà posat el valor de var(x).
# Per obtenir els intervals de confiança percentil i BCa, és possible
# que directament s'hagi fet alguna cosa com:
# Al cas no paramètric:
s2 = var(x)
var.bootsNopar = replicate(nboot, var(sample(x, replace = TRUE)))
# Al cas paramètric exponencial:
var.bootsPar = replicate(nboot, var(rexp(n, rate = lambda.estim)))
# I que als càlculs d'a i de z0 s'hagi utilitzat també els valors proporcionats per
# 'var'.
# Segons el principi plug-in no és el que caldria fer, denota que no s'ha entès 
# completament o no s'ha aplicat estrictament allò explicat a classe.
# Possiblement, coses d'aquestes són les que han provocat en alguns treballs que,
# per exemple, la distribució mostral sota bootstrap paramètric exponencial de V no 
# fos del tot similar a la "veritable" que s'ha proporcionat.
# 
# Aquests treballs no estan completament bé, encara que tampoc no s'han considerat
# malament. Però els que han tingut en compte l'aplicació del principi plug-in s'han
# considerat una mica millors.


# 7) Interval de confiança bootstrap basat en el caràcter pivotal de V

# A l'interval de confiança que estaria basat en V (vegeu l'enunciat),
# l'única cosa que no es mantindria si fallés la condició de normalitat de les
# dades però encara fos acceptable considerar que V és pivotal, 
# seria el denominador, ja que els quantils khi-quadrat que hi anirien 
# depenen de la validesa de la suposició de normalitat, que fa que V tingui
# una distribució mostral khi-quadrat.
# Sota normalitat l'interval de confiança seria:
(n - 1) * var(x) / qchisq(c(0.975, 0.025), n - 1)
# Però de forma més general podem estimar aquests quantils a partir de
# l'estimació bootstrap de la distribució de V, que ja hem obtingut als
# apartats 1-2) i 3-4). Per tant,

# Interval de confiança bootstrap no paramètric:
icNopar = (n - 1) * var(x) / quant.noPar[c("97.5%", "2.5%")]
names(icNopar) = NULL
attr(icNopar, "conf.level") = 0.95
icNopar

# Interval de confiança bootstrap paramètric exponencial:
icPar = (n - 1) * var(x) / quant.parExp[c("97.5%", "2.5%")]
names(icPar) = NULL
attr(icPar, "conf.level") = 0.95
icPar

# 8) Interval de confiança bootstrap percentil SEMIPARAMÈTRIC, a partir d'una
# estimació nucli gaussià de la funció de densitat

# Primer necessitarem obtenir l'estimació de la distribució mostral
# a partir de la qual farem bootstrap, és a dir, a partir de la qual
# simularem noves (re)mostres:
fNucli = density(x, bw = "nrd")
windows(21,21)
plot(fNucli,
     xlab = "x", ylab = "densitat", 
     main = "Ajust no parametric (nucli gaussia)", sub = "bw segons Scott(1992)")
# L'argument 'bw' solament fa que l'amplada de finestra coincideixi amb allò
# explicat a classe.

valor.h = fNucli$bw
valor.h
# variància del nucli:
var.h = valor.h^2
var.h

# Recordem que la distribució nucli anterior es pot interpretar com 
# la composició de la distribució empírica amb una distribució normal,
# per tant podem generar 'nboot' remostres bootstrap com:
x.boots = replicate(nboot,
                    sample(x, replace = TRUE) + rnorm(length(x), sd = valor.h))
# (encara que l'enunciat parla de 10000 remostres, mantenim l'anterior 20000)

# Ara tenim una gran matriu de 'n' files i 20000 columnes, 
# cada columna correspon a una remostra bootstrap.

# Segons el principi plug-in, l'estimació de la variància d'una variable aleatòria
# amb la distribució anteriorment estimada seria la variància de la distribució 
# empírica + la variància d'una normal N(0, sigma = valor.h).
# És a dir, tindríem la variància mostral esbiaixada + valor.h^2.
# (No oblidem que tal com l'hem obtingut, 'valor.h' també és un valor aleatori
# ja que ha estat ajustat per la funció 'density' a partir de la mostra.) 

# Sobre cada remostra (columna, segona dimensió) avaluem la variància anterior.
# Primer definim una funció per calcular-la amb més comoditat:
var.nucli = function(x, bw = "nrd") {
  return(var.biased(x) + density(x, bw = bw)$bw^2)
}

# Apliquem la funció anterior sobre cada remostra semiparamètrica
# (serà bastant lent, implica molt de càlcul, 'nboots' estimacions de la densitat):
var.boots = apply(x.boots, 2, var.nucli)

# IC percentil bootstrap 95%:
icSemiPar = quantile(var.boots, probs = c(0.025, 0.975))
names(icSemiPar) = NULL
attr(icSemiPar, "conf.level") = 0.95
icSemiPar

# NOTA: aquí seria aplicable un comentari similar al fet abans. Si deixem de
# banda les consideracions anteriors sobre el principi plug-in, és raonable
# (però no del tot correcte per demostrar que s'entèn la idea bootstrap!) 
# obtenir les 20000 estimacions bootstrap de la variància com:
var.boots = apply(x.boots, 2, var)


