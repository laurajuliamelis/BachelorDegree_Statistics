lawSchool <- read.table(file = "Law_School.txt", header = TRUE)

n <- nrow(lawSchool)
sample(n, replace = TRUERUE)
# Una remostra bootstrap de 'lawSchool'
lawSchool[sample(1:n, replace = TRUE), ]

# B = 2 remostres bootstrap:
B = 2
replicate(B, lawSchool[sample(1:n, replace = TRUE), ], simplify = FALSE)

# Càlcul de la correlació sobre la mostra original:
r <- cor(lawSchool)[2,1]
cor(lawSchool[,1], lawSchool[,2])

# Remostra bootstrap i càlcul d'r sobre ella:
cor(lawSchool[sample(n, replace = TRUE),])[2,1]

# B = 5 remostres bootstrap i directament calculem sobre elles
# la correlació:
B = 5
replicate(B, cor(lawSchool[sample(n, replace = TRUE),])[2,1])

# Basarem els intervals de confiança bootstrap en 10000 remostres
B <- 10000
alpha <- 0.05
confLevel = 1 - alpha

# ******************************************************************************
# Intervalo bootstrap-t
# ******************************************************************************

# Correlación sobre la muestra original:
r
# Error estándar de la correlación muestral sobre la muestra original
# (basado en enfoque paramétrico normal bivariante):
se.r = (1 - r * r) / sqrt(n - 3)
se.r

# Si t = (r - rho) / se.r  y  t(alfa/2), t(1 - alfa/2) representan valores
# críticos de la distribución muestral de este estadístico t, la forma 
# general del IC bootstrap-t ser???: 
#           [r - t(1 - alfa/2) * se.r, r - t(alfa/2) * se.r]

# Aproximaremos los valores críticos t(1 - alfa/2) y t(alfa/2)
# mediante bootstrap no paramétrico:
set.seed(123)
# Simulación de B valores t*:
t.boot = replicate(B,
{
  r.boot = cor(lawSchool[sample(n, replace = TRUE),])[2,1]
  (r.boot - r) / (1 - r.boot * r.boot)
}
)
# para no tener que repetir la raiz cuadrada en cada réplica bootstrap:
t.boot = t.boot * sqrt(n - 3) 
t.boot[1:20]

# Intervalo de confianza:
icBoot.t = r - quantile(t.boot, probs = c(1 - alpha/2, alpha/2)) * se.r
names(icBoot.t) = NULL
attr(icBoot.t, "conf.level") = confLevel
icBoot.t

# ******************************************************************************
# Intervalo bootstrap-t simetrizado
# ******************************************************************************

t.alpha = quantile(abs(t.boot), probs = confLevel)
icBoot.t.sym = r - c(t.alpha, -t.alpha) * se.r
names(icBoot.t.sym) = NULL
attr(icBoot.t.sym, "conf.level") = confLevel
icBoot.t.sym

# podríamos compararlos con el IC paramétrico para normal bivariante:
cor.test(lawSchool[,1], lawSchool[,2])$conf.int

# --------------------------------------------------------------------------
# Exercici
# --------------------------------------------------------------------------
# Calcula les versions "bootstrap paramètric" dels dos intervals de
# confiança anteriors
# (Pista: pot ser ???til carregar la llibreria "mvtnorm" i utilitzar
# la funci??? 'rmvnorm')
require(mvtnorm)

set.seed(123)
# Simulación de B valores t*:
mitjanes = colMeans(lawSchool)
varcovs = cov(lawSchool)
t.boot = replicate(B,
{
  r.boot = cor(rmvnorm(n, mean = mitjanes, sigma = varcovs))[2,1]
  (r.boot - r) / (1 - r.boot * r.boot)
}
)

# para no tener que repetir la raiz cuadrada en cada réplica bootstrap:
t.boot = t.boot * sqrt(n - 3) 


# Intervalo de confianza bootstrap-t:
icBoot.t = r - quantile(t.boot, probs = c(1 - alpha/2, alpha/2)) * se.r
names(icBoot.t) = NULL
attr(icBoot.t, "conf.level") = confLevel
icBoot.t

# ******************************************************************************
# Intervalo bootstrap-t simetrizado
# ******************************************************************************

t.alpha = quantile(abs(t.boot), probs = confLevel)
icBoot.t.sym = r - c(t.alpha, -t.alpha) * se.r
names(icBoot.t.sym) = NULL
attr(icBoot.t.sym, "conf.level") = confLevel
icBoot.t.sym

# podríamos compararlos con el IC paramétrico para normal bivariante:
cor.test(lawSchool[,1], lawSchool[,2])$conf.int

# Fins ara, pel denominador de l'estadístic t o "estudentitzat", hem utilitzat
# l'error estàndard de la correlació mostral estimat mitjançant l'aproximació
# (1 - r^2) / sqrt(n - 3) basada en teoria normal
# ******************************************************************************
# ................Error estàndard pel mètode jackknife..........................

# Matriu de dades sense fila 4:
lawSchool[-4,]

# Obtenció de les n rèpliques jackknife del coeficient de correlació
r_i <- numeric(n)
for (i in 1:n) r_i[i] <- cor(lawSchool[-i,])[2,1] 

# Una manera suposadament més eficient:
r_i = vapply(1:n, function(i) cor(lawSchool[-i,])[2,1], FUN.VALUE = 0)

# Variància jackknife:
((n - 1) / n) * sum((r_i - mean(r_i))^2)
# Error estàndard jackknife:
seJ.r = sqrt(((n - 1) / n) * sum((r_i - mean(r_i))^2))
seJ.r
# El comparem amb l'estimació paramètrica normal:
se.r


# ******************************************************************************
# Intervalo bootstrap percentil
# ******************************************************************************

# B valors de r*
r.boot <- replicate(B, 
            cor(lawSchool[sample(1:n, replace = TRUE),])[2,1])
r.boot[1:10]

# Interval de confiança percentil bootstrap 95%:
icBoot.perc = quantile(r.boot, probs = c(alpha/2, 1 - alpha/2))
names(icBoot.perc) = NULL
attr(icBoot.perc, "conf.level") = confLevel
icBoot.perc

# ******************************************************************************
# Intervalo bootstrap percentil BCa
# ******************************************************************************

# Càlcul de la constant d'acceleració:
# (Recordem:) Obtenció de les n rèpliques jackknife del coeficient de correlació
r_i <- numeric(n)
for (i in 1:n) r_i[i] <- cor(lawSchool[-i,])[2,1] 

# Una manera suposadament més eficient:
r_i = vapply(1:n, function(i) cor(lawSchool[-i,])[2,1], FUN.VALUE = 0)

# a <- sum(mean(r_i) - (r_i)^3) / (6 * sum((mean(r_i) - r_i)^2)^(3/2))
# Càlcul una mica més eficient:
r_i <- mean(r_i) - r_i
a <- sum(r_i^3) / (6 * sum(r_i^2)^1.5)
a

# Càlcul de la constant de correcció de biaix:
z0 <- qnorm(sum(r.boot <= r) / B)
z0

zalpha <- - qnorm(alpha/2)
zalpha

# IC BCa: com un percentil però en unes noves probabilitats a
# cada cua:
quantile(r.boot, 
         c(
           pnorm(z0 + (z0 - zalpha) / (1 - a * (z0 - zalpha))), 
           pnorm(z0 + (z0 + zalpha) / (1 - a * (z0 + zalpha)))
         )
)
