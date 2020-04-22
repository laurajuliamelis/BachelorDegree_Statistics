# ===================================================================================
# ===================================================================================

# Aquesta mostra fa el paper d'unes "dades reals" però en realitat sabem que
# procedeix d'una N(15, 3)
# ATENCIÓ: AIXÒ ÉS AIXÍ PER QUE ESTEM EN UNA SITUACIÓ "DE LABORATORI", AMB
# DADES REALS LÒGICAMENT DESCONEIXERÍEM COMPLETAMENT ELS PARÀMETRES REALS

x <- c(15.54, 21.06, 16.52, 13.62, 16.14, 10.98, 13.53, 16.02, 16.79, 15.90)

n <- length(x)

# Verdadera procedencia de los datos anteriores: N(mu=15, sigma=3).
# En este caso lo sabemos dado que es una situación "de laboratorio informático".
# Lógicamente en una situación real desconoceríamos los parámetros de la distribución
# de procedencia de los datos (aunque tal vez podríamos hacer alguna conjetura sobre
# su forma, p.e. normal)
mu <- 15
sigma <- 3


# ===================================================================================
# Per la mostra normal anterior, estudi bootstrap de la distribució de l'estadístic t:
# ===================================================================================
tStud <- function(x, mitjana)
{
	(mean(x) - mitjana) / sqrt(var(x)/length(x))
}

# Mitjana mostral, estimació de la veritable mitjana (que recordem que és 15)
xBarra <- mean(x)
xBarra
# Desviació típica mostral, estimació de la veritable sigma (que és 3)
s.x <- sqrt(var(x))
s.x

# Veritable valor de l'error estàndard de la mitjana mostral:
sigma.xBarra <- sigma/sqrt(n)
sigma.xBarra
# Estimació de l'error estàndard de la mitjana mostral:
s.xBarra <- s.x/sqrt(n)
s.xBarra

t.x <- tStud(x, mu)
t.x

rang.t <- seq(from=-4, to=+4, by=0.1)
dens.veritat <- dt(rang.t, df=n-1)
windows(21,21)
plot(rang.t, dens.veritat, type="l", col="green", ylim=c(0,0.4))

# A menudo, nos interesa conocer ciertos "valores críticos" de esta distribución
# muestral.
# Valores críticos según la "verdadera" distribución muestral de t
tCritStud <- qt(c(0.975, 0.025), df = n - 1)
tCritStud
# Por ejemplo para calcular un intervalo de confianza paramétrico normal, 
# basado en t(n - 2):
xBarra - tCritStud * s.xBarra

dens.normAprox <- dnorm(rang.t)
lines(rang.t, dens.normAprox, type="l", col="blue")

# valores críticos según aproximación normal:
tCritNorm = qnorm(c(0.975, 0.025))
tCritNorm
# intervalo de confianza según aproximación normal:
xBarra - tCritNorm * s.xBarra


# ---------------------------------------------------------------------------------
# Bootstrap no paramètric:
B <- 10000

sample(x, replace = TRUE)

set.seed(127)

mostres.bootstrap <- matrix(sample(x, replace=T, size=B*n), ncol=B)
mostres.bootstrap[,1:10]
t.bootstrap <- apply(mostres.bootstrap, 2, tStud, mitjana=xBarra)
t.bootstrap[1:10]

dens.bootstrap <- density(t.bootstrap,from=-4, to=+4)
lines(dens.bootstrap, type="l", col="red")

# valores críticos según bootstrap no paramétrico
tCritBoot = quantile(t.bootstrap, probs = c(0.975, 0.025))
# intervalo de confianza bootstrap no paramétrico:
xBarra - tCritBoot * s.xBarra

# ---------------------------------------------------------------------------------
# Si en realidad nos podemos fiar de que la forma de la distribución es normal:
# Bootstrap paramètric normal:
# Una sola remostra:
rnorm(n, mean=xBarra, sd=s.x)
B <- 10000
set.seed(127)
mostres.bootstrap <- matrix( rnorm(B*n, mean=xBarra, sd=s.x), ncol=B)
t.bootstrap.param <- apply(mostres.bootstrap ,2, tStud, mitjana=xBarra)

dens.bootstrap.param <- density(t.bootstrap.param,from=-4, to=+4)
lines(dens.bootstrap.param, type="l", col="brown")

# valores críticos según bootstrap paramétrico
tCritBoot.param = quantile(t.bootstrap.param, probs = c(0.975, 0.025))
# intervalo de confianza bootstrap paramétrico:
xBarra - tCritBoot.param * s.xBarra


# ---------------------------------------------------------------------------------
# Càlcul d'una probabilitat a la cua dreta: P[t > valor]

# Suposem que per decidir quin és el valor que a la seva dreta i deixa 0.025
# de probabilitat, hem decidit utilitzar les taules de la normal
# (és a dir, hem considerat que t té distribució N(0,1)):
z0.05 <- qnorm(0.025, lower.tail = FALSE)  # 1.959964
# En teoria tindríem:
pnorm(z0.05, lower.tail = FALSE)
# Però la veritable probabilitat és:
pt(z0.05, df = n - 1, lower.tail = FALSE)
# Si utilitzem l'aproximació a la veritable distribució de t que ens ha
# proporcionat el bootstrap no paramètric, aquesta probabilitat és:
length(t.bootstrap[t.bootstrap > z0.05])/B
# Si utilitzem l'aproximació a la veritable distribució de t que ens ha
# proporcionat el bootstrap paramètric, aquesta probabilitat és:
length(t.bootstrap.param[t.bootstrap.param > z0.05])/B

# ===================================================================================
# Mostra exponencial, estadístic t:
# ===================================================================================
sigma <- mu
x <- c(8.51,  8.71, 69.19, 10.05, 23.64, 8.67, 1.51, 20.36, 1.23, 5.27)
n = length(x)
sigma.xBarra <- sigma/sqrt(n)
xBarra <- mean(x)
s.x <- sqrt(var(x))
s.xBarra <- s.x/sqrt(n)
t.x <- tStud(x, mu)
rang.t <- seq(from=-4, to=+4, by=0.1)

# Ara és difícil determinar analíticament quina és la veritable distribució
# mostral de l'estadístic t quan les dades són exponencials.
# Aproximació mitjançant simulació a la veritable distribució mostral.
# Fem una simulació utilitzant els VERITABLES VALORS DELS PARÀMETRES POBLACIONALS
# (en què es diferecia del que fem amb bootstrap?)
m <- 100000
mostra.t <- apply(matrix(rexp(m*n, rate=1/mu), ncol=m), 2, tStud, mitjana=mu)
dens.veritat <- density(mostra.t,from=-4, to=+4)
# Obrim una altra finestra gràfica independent:
windows(21,21)
plot(dens.veritat, type="l", col="black", ylim=c(0,0.4))

# ¿Es pivotal? Probar con distintos valores de "mu": ¿se mantiene estable la distribución
# muestral?
color = 0
for (otherMu in c(1,2,5,10,20,50)) {
  color = color + 1
  lines(
    density(
      apply(matrix(rexp(m*n, rate=1/otherMu), ncol=m), 2, tStud, mitjana=otherMu), 
      from=-4, to=+4
    ), 
    type="l", col=color, ylim=c(0,0.4)
  )
}

# Parece que sí...

# L'aproximació normal sembla que no seria gaire adequada:
dens.normAprox <- dnorm(rang.t)
lines(rang.t, dens.normAprox, type="l", col="blue")
# Ni tampoc la t de Student:
lines(rang.t, dt(rang.t, df = n - 1), type="l", col="green", ylim=c(0,0.4))

# ---------------------------------------------------------------------------------
# Bootstrap no paramètric:
B <- 10000
mostres.bootstrap <- matrix(sample(x, replace=T, size=B*n), ncol=B)
t.bootstrap <- apply(mostres.bootstrap ,2, tStud, mitjana=xBarra)

dens.bootstrap <- density(t.bootstrap,from=-4, to=+4)
lines(dens.bootstrap, type="l", col="brown", ylim=c(0,0.4))
# ---------------------------------------------------------------------------------
# Bootstrap paramètric exponencial:
mostres.bootstrap <- matrix(rexp(n=B*n, rate=1/xBarra), ncol=B)
t.bootstrap.param <- apply(mostres.bootstrap ,2, tStud, mitjana=xBarra)

dens.bootstrap.param <- density(t.bootstrap.param,from=-4, to=+4)
lines(dens.bootstrap.param, type="l", col="red", ylim=c(0,0.4))

# ---------------------------------------------------------------------------------
# Càlcul d'una probabilitat a la cua dreta: P[t>1.96]
z0.05 <- qnorm(0.025, lower.tail = FALSE)  # 1.959964
1 - pnorm(z0.05)  # ... aproximació normal
1 - pt(z0.05, df=n-1)  # ... aproximació t
length(mostra.t[mostra.t > z0.05])/m  # ... veritable probabilitat (simulació 100000)
length(t.bootstrap[t.bootstrap > z0.05])/B  # ... bootstrap no paramètric
length(t.bootstrap.param[t.bootstrap.param > z0.05])/B  # ... bootstrap paramètric

# ---------------------------------------------------------------------------------
# Càlcul d'una probabilitat a la cua esquerra: P[t < -1.96]
pnorm(-z0.05)  # ... aproximació normal
pt(-z0.05, df=n-1)  # ... aproximació t
length(mostra.t[mostra.t < -z0.05])/m  # ... "veritable" probabilitat
length(t.bootstrap[t.bootstrap < -z0.05])/B  # ... bootstrap no paramètric
length(t.bootstrap.param[t.bootstrap.param < -z0.05])/B  # ... bootstrap paramètric

# ---------------------------------------------------------------------------------
# Càlcul d'una probabilitat bilateral: P[|t| > 1.96]
pnorm(-z0.05) + (1 - pnorm(z0.05))
pt(-z0.05, df=n-1) + (1 - pt(z0.05, df=n-1))
length(mostra.t[abs(mostra.t) > z0.05])/m
length(t.bootstrap[abs(t.bootstrap) > z0.05])/B
length(t.bootstrap.param[abs(t.bootstrap.param) > z0.05])/B



# ===================================================================================
# ===================================================================================
# Mostra exponencial, estadístic t, n=40:
set.seed(127)
n <- 40
sigma <- mu
x <- rexp(n, rate=1/mu)
sigma.xBarra <- sigma/sqrt(n)
xBarra <- mean(x)
s.x <- sqrt(var(x))
s.xBarra <- s.x/sqrt(n)
t.x <- tStud(x, mu)
rang.t <- seq(from=-4, to=+4, by=0.1)
# Aproximació mitjançant simulació a la veritable distribució mostral:
m <- 10000
mostra.t <- apply(matrix(rexp(m*n, rate=1/mu), ncol=m) ,2, tStud, mitjana=mu)
dens.veritat <- density(mostra.t,from=-4, to=+4)
plot(dens.veritat, type="l", col="green", ylim=c(0,0.4))

dens.normAprox <- dnorm(rang.t)
lines(rang.t, dens.normAprox, type="l", col="blue")
# ---------------------------------------------------------------------------------
# Bootstrap no paramètric:
B <- 10000
mostres.bootstrap <- matrix(sample(x, replace=T, size=B*n), ncol=B)
t.bootstrap <- apply(mostres.bootstrap ,2, tStud, mitjana=xBarra)

dens.bootstrap <- density(t.bootstrap,from=-4, to=+4)
lines(dens.bootstrap, type="l", col="brown")
# ---------------------------------------------------------------------------------
# Bootstrap paramètric exponencial:
mostres.bootstrap <- matrix(rexp(n=B*n, rate=1/xBarra), ncol=B)
t.bootstrap.param <- apply(mostres.bootstrap ,2, tStud, mitjana=xBarra)

dens.bootstrap.param <- density(t.bootstrap.param,from=-4, to=+4)
lines(dens.bootstrap.param, type="l", col="red")

# ---------------------------------------------------------------------------------
# Càlcul d'una probabilitat a la cua dreta: P[t>1.96]
z0.05 <- 1.959964
1 - pnorm(z0.05)  # ... aproximació normal
1 - pt(z0.05, df=n-1)  # ... aproximació t
length(mostra.t[mostra.t > z0.05])/m  # ... veritable probabilitat
length(t.bootstrap[t.bootstrap > z0.05])/B  # ... bootstrap no paramètric
length(t.bootstrap.param[t.bootstrap.param > z0.05])/B  # ... bootstrap paramètric

# ---------------------------------------------------------------------------------
# Càlcul d'una probabilitat a la cua esquerra: P[t < -1.96]
pnorm(-z0.05))  # ... aproximació normal
pt(-z0.05, df=n-1)  # ... aproximació t
length(mostra.t[mostra.t < -z0.05])/m  # ... "veritable" probabilitat
length(t.bootstrap[t.bootstrap < -z0.05])/B  # ... bootstrap no paramètric
length(t.bootstrap.param[t.bootstrap.param < -z0.05])/B  # ... bootstrap paramètric

# ---------------------------------------------------------------------------------
# Càlcul d'una probabilitat bilateral: P[|t| > 1.96]
pnorm(-z0.05) + (1 - pnorm(z0.05))
pt(-z0.05, df=n-1) + (1 - pt(z0.05, df=n-1))
length(mostra.t[abs(mostra.t) > z0.05])/m
length(t.bootstrap[abs(t.bootstrap) > z0.05])/B
length(t.bootstrap.param[abs(t.bootstrap.param) > z0.05])/B
