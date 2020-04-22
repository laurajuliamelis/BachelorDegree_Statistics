### R code from vignette source 'Boot_sessio_2_3_17_18.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: Boot_sessio_2_3_17_18.Rnw:38-43
###################################################
# Lectura de les dades PROCEDENTS D'UNA NORMAL
x <- c(15.54, 21.06, 16.52, 13.62, 16.14, 10.98, 13.53, 16.02, 16.79, 15.90)
n<-length(x)
mu <- 15
sigma <- 3


###################################################
### code chunk number 2: Boot_sessio_2_3_17_18.Rnw:50-54
###################################################
tStud <- function(x, mitjana)
{
	(mean(x) - mitjana) / sqrt(var(x)/length(x))
}


###################################################
### code chunk number 3: Boot_sessio_2_3_17_18.Rnw:68-91
###################################################
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

# Calculem l'estadístic t i el grafiquem:
t.x <- tStud(x, mu)
t.x

rang.t <- seq(from=-4, to=+4, by=0.1)
dens.veritat <- dt(rang.t, df=n-1)
windows(21,21)
plot(rang.t, dens.veritat, type="l", col="green", ylim=c(0,0.4))


###################################################
### code chunk number 4: Boot_sessio_2_3_17_18.Rnw:100-102
###################################################
tCritStud <- qt(c(0.975, 0.025), df = n - 1)
tCritStud


###################################################
### code chunk number 5: Boot_sessio_2_3_17_18.Rnw:106-107
###################################################
xBarra - tCritStud * s.xBarra


###################################################
### code chunk number 6: Boot_sessio_2_3_17_18.Rnw:112-114
###################################################
dens.normAprox <- dnorm(rang.t)
lines(rang.t, dens.normAprox, type="l", col="blue")


###################################################
### code chunk number 7: Boot_sessio_2_3_17_18.Rnw:118-120
###################################################
tCritNorm = qnorm(c(0.975, 0.025))
tCritNorm


###################################################
### code chunk number 8: Boot_sessio_2_3_17_18.Rnw:124-125
###################################################
xBarra - tCritNorm * s.xBarra


###################################################
### code chunk number 9: Boot_sessio_2_3_17_18.Rnw:129-142
###################################################
B <- 10000

sample(x, replace = TRUE)

set.seed(127)

mostres.bootstrap <- matrix(sample(x, replace=T, size=B*n), ncol=B)
mostres.bootstrap[,1:10]
t.bootstrap <- apply(mostres.bootstrap, 2, tStud, mitjana=xBarra)
t.bootstrap[1:10]

dens.bootstrap <- density(t.bootstrap,from=-4, to=+4)
lines(dens.bootstrap, type="l", col="red")


###################################################
### code chunk number 10: Boot_sessio_2_3_17_18.Rnw:149-150
###################################################
tCritBoot = quantile(t.bootstrap, probs = c(0.975, 0.025))


###################################################
### code chunk number 11: Boot_sessio_2_3_17_18.Rnw:154-155
###################################################
xBarra - tCritBoot * s.xBarra


###################################################
### code chunk number 12: Boot_sessio_2_3_17_18.Rnw:162-173
###################################################
# Si en realidad nos podemos fiar de que la forma de la distribución es normal
# Bootstrap paramètric normal:
# Una sola remostra:
rnorm(n, mean=xBarra, sd=s.x)
B <- 10000
set.seed(127)
mostres.bootstrap <- matrix( rnorm(B*n, mean=xBarra, sd=s.x), ncol=B)
t.bootstrap.param <- apply(mostres.bootstrap ,2, tStud, mitjana=xBarra)

dens.bootstrap.param <- density(t.bootstrap.param,from=-4, to=+4)
lines(dens.bootstrap.param, type="l", col="brown")


###################################################
### code chunk number 13: Boot_sessio_2_3_17_18.Rnw:176-178
###################################################
# valors crítics segons bootstrap paramètric
tCritBoot.param = quantile(t.bootstrap.param, probs = c(0.975, 0.025))


###################################################
### code chunk number 14: Boot_sessio_2_3_17_18.Rnw:181-182
###################################################
xBarra - tCritBoot.param * s.xBarra


###################################################
### code chunk number 15: Boot_sessio_2_3_17_18.Rnw:191-192
###################################################
z0.05 <- qnorm(0.025, lower.tail = FALSE)  # 1.959964


###################################################
### code chunk number 16: Boot_sessio_2_3_17_18.Rnw:196-197
###################################################
pnorm(z0.05, lower.tail = FALSE)


###################################################
### code chunk number 17: Boot_sessio_2_3_17_18.Rnw:201-203
###################################################
# Però la veritable probabilitat és:
pt(z0.05, df = n - 1, lower.tail = FALSE)


###################################################
### code chunk number 18: Boot_sessio_2_3_17_18.Rnw:208-209
###################################################
length(t.bootstrap[t.bootstrap > z0.05])/B


###################################################
### code chunk number 19: Boot_sessio_2_3_17_18.Rnw:215-216
###################################################
length(t.bootstrap.param[t.bootstrap.param > z0.05])/B


###################################################
### code chunk number 20: Boot_sessio_2_3_17_18.Rnw:221-230
###################################################
sigma <- mu
x <- c(8.51,  8.71, 69.19, 10.05, 23.64, 8.67, 1.51, 20.36, 1.23, 5.27)
n = length(x)
sigma.xBarra <- sigma/sqrt(n)
xBarra <- mean(x)
s.x <- sqrt(var(x))
s.xBarra <- s.x/sqrt(n)
t.x <- tStud(x, mu)
rang.t <- seq(from=-4, to=+4, by=0.1)


###################################################
### code chunk number 21: Boot_sessio_2_3_17_18.Rnw:242-248
###################################################
m <- 100000
mostra.t <- apply(matrix(rexp(m*n, rate=1/mu), ncol=m), 2, tStud, mitjana=mu)
dens.veritat <- density(mostra.t,from=-4, to=+4)
# Obrim una altra finestra gràfica independent:
windows(21,21)
plot(dens.veritat, type="l", col="black", ylim=c(0,0.4), main="Exponencial dades, t-stat")


###################################################
### code chunk number 22: Boot_sessio_2_3_17_18.Rnw:257-268
###################################################
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


###################################################
### code chunk number 23: Boot_sessio_2_3_17_18.Rnw:276-281
###################################################
par(mfrow=c(2,2))
plot(dens.veritat, type="l", col="black", ylim=c(0,0.4),main="dens.veritat versus")
dens.normAprox <- dnorm(rang.t)
lines(rang.t, dens.normAprox, type="l", col="blue")
legend("topright", legend=c("Normal"))


###################################################
### code chunk number 24: Boot_sessio_2_3_17_18.Rnw:286-289
###################################################
plot(dens.veritat, type="l", col="black", ylim=c(0,0.4),main="dens.veritat versus")
lines(rang.t, dt(rang.t, df = n - 1), type="l", col="green", ylim=c(0,0.4))
legend("topright", legend=c("t-Stud"))


###################################################
### code chunk number 25: Boot_sessio_2_3_17_18.Rnw:294-303
###################################################
# Bootstrap no paramètric:
B <- 10000
mostres.bootstrap <- matrix(sample(x, replace=T, size=B*n), ncol=B)
t.bootstrap <- apply(mostres.bootstrap ,2, tStud, mitjana=xBarra)

dens.bootstrap <- density(t.bootstrap,from=-4, to=+4)
plot(dens.veritat, type="l", col="black", ylim=c(0,0.4),main="dens.veritat versus")
lines(dens.bootstrap, type="l", col="brown", ylim=c(0,0.4))



###################################################
### code chunk number 26: Boot_sessio_2_3_17_18.Rnw:306-314
###################################################
# Bootstrap paramètric exponencial:
mostres.bootstrap <- matrix(rexp(n=B*n, rate=1/xBarra), ncol=B)
t.bootstrap.param <- apply(mostres.bootstrap ,2, tStud, mitjana=xBarra)

dens.bootstrap.param <- density(t.bootstrap.param,from=-4, to=+4)
plot(dens.veritat, type="l", col="black", ylim=c(0,0.4),main="dens.veritat versus")
lines(dens.bootstrap.param, type="l", col="red", ylim=c(0,0.4))
legend("topright", legend=c("Boot Parm"))


###################################################
### code chunk number 27: Boot_sessio_2_3_17_18.Rnw:322-329
###################################################
# Càlcul d'una probabilitat a la cua dreta: P[t>1.96]
z0.05 <- qnorm(0.025, lower.tail = FALSE)  # 1.959964
1 - pnorm(z0.05)  # ... aproximació normal
1 - pt(z0.05, df=n-1)  # ... aproximació t
length(mostra.t[mostra.t > z0.05])/m  # ... veritable probabilitat (simulació 100000)
length(t.bootstrap[t.bootstrap > z0.05])/B  # ... bootstrap no paramètric
length(t.bootstrap.param[t.bootstrap.param > z0.05])/B  # ... bootstrap paramètric


###################################################
### code chunk number 28: Boot_sessio_2_3_17_18.Rnw:332-337
###################################################
pnorm(-z0.05)  # ... aproximació normal
pt(-z0.05, df=n-1)  # ... aproximació t
length(mostra.t[mostra.t < -z0.05])/m  # ... "veritable" probabilitat
length(t.bootstrap[t.bootstrap < -z0.05])/B  # ... bootstrap no paramètric
length(t.bootstrap.param[t.bootstrap.param < -z0.05])/B  # ... bootstrap paramètric


###################################################
### code chunk number 29: Boot_sessio_2_3_17_18.Rnw:341-347
###################################################
# Càlcul d'una probabilitat bilateral: P[|t| > 1.96]
pnorm(-z0.05) + (1 - pnorm(z0.05))
pt(-z0.05, df=n-1) + (1 - pt(z0.05, df=n-1))
length(mostra.t[abs(mostra.t) > z0.05])/m
length(t.bootstrap[abs(t.bootstrap) > z0.05])/B
length(t.bootstrap.param[abs(t.bootstrap.param) > z0.05])/B


###################################################
### code chunk number 30: Boot_sessio_2_3_17_18.Rnw:352-369
###################################################
# Mostra exponencial, estadístic t, n=40:
set.seed(127)
n <- 40
sigma <- mu
x <- rexp(n, rate=1/mu) #SIMULEM DADES MODEL EXPONENCIAL
sigma.xBarra <- sigma/sqrt(n)
xBarra <- mean(x)
s.x <- sqrt(var(x))
s.xBarra <- s.x/sqrt(n)
t.x <- tStud(x, mu)
rang.t <- seq(from=-4, to=+4, by=0.1)
# Aproximació mitjançant simulació a la veritable distribució mostral:
m <- 10000
mostra.t <- apply(matrix(rexp(m*n, rate=1/mu), ncol=m) ,2, tStud, mitjana=mu)
par(mfrow=c(2,2))
dens.veritat <- density(mostra.t,from=-4, to=+4)
plot(dens.veritat, type="l", col="green", ylim=c(0,0.4), main="Simulem dades Exponencials")


###################################################
### code chunk number 31: Boot_sessio_2_3_17_18.Rnw:374-379
###################################################
# Aproximació mitjançant simulació a la veritable distribució mostral:
dens.normAprox <- dnorm(rang.t)
plot(dens.veritat, type="l", col="green", ylim=c(0,0.4), main="Simulem dades Exponencials")
lines(rang.t, dens.normAprox, type="l", col="blue")
legend("topright", legend=c("Aprox Normal"))


###################################################
### code chunk number 32: Boot_sessio_2_3_17_18.Rnw:383-392
###################################################
# Bootstrap no paramètric:
B <- 10000
mostres.bootstrap <- matrix(sample(x, replace=T, size=B*n), ncol=B)
t.bootstrap <- apply(mostres.bootstrap ,2, tStud, mitjana=xBarra)

dens.bootstrap <- density(t.bootstrap,from=-4, to=+4)
plot(dens.veritat, type="l", col="green", ylim=c(0,0.4), main="Simulem dades Exponencials")
lines(dens.bootstrap, type="l", col="brown")
legend("topright", legend=c("Boot No param"))


###################################################
### code chunk number 33: Boot_sessio_2_3_17_18.Rnw:395-403
###################################################
# Bootstrap paramètric exponencial:
mostres.bootstrap <- matrix(rexp(n=B*n, rate=1/xBarra), ncol=B)
t.bootstrap.param <- apply(mostres.bootstrap ,2, tStud, mitjana=xBarra)

dens.bootstrap.param <- density(t.bootstrap.param,from=-4, to=+4)
plot(dens.veritat, type="l", col="green", ylim=c(0,0.4), main="Simulem dades Exponencials")
lines(dens.bootstrap.param, type="l", col="red")
legend("topright", legend=c("Boot Param Exponencial"))


###################################################
### code chunk number 34: Boot_sessio_2_3_17_18.Rnw:411-418
###################################################
# Càlcul d'una probabilitat a la cua dreta: P[t>1.96]
z0.05 <- 1.959964
1 - pnorm(z0.05)  # ... aproximació normal
1 - pt(z0.05, df=n-1)  # ... aproximació t
length(mostra.t[mostra.t > z0.05])/m  # ... veritable probabilitat
length(t.bootstrap[t.bootstrap > z0.05])/B  # ... bootstrap no paramètric
length(t.bootstrap.param[t.bootstrap.param > z0.05])/B  # ... bootstrap paramètric


###################################################
### code chunk number 35: Boot_sessio_2_3_17_18.Rnw:421-427
###################################################
# Càlcul d'una probabilitat a la cua esquerra: P[t < -1.96]
pnorm(-z0.05)  # ... aproximació normal
pt(-z0.05, df=n-1)  # ... aproximació t
length(mostra.t[mostra.t < -z0.05])/m  # ... "veritable" probabilitat
length(t.bootstrap[t.bootstrap < -z0.05])/B  # ... bootstrap no paramètric
length(t.bootstrap.param[t.bootstrap.param < -z0.05])/B  # ... bootstrap paramètric


###################################################
### code chunk number 36: Boot_sessio_2_3_17_18.Rnw:431-439
###################################################
# Càlcul d'una probabilitat bilateral: P[|t| > 1.96]
pnorm(-z0.05) + (1 - pnorm(z0.05))
pt(-z0.05, df=n-1) + (1 - pt(z0.05, df=n-1))
length(mostra.t[abs(mostra.t) > z0.05])/m
length(t.bootstrap[abs(t.bootstrap) > z0.05])/B
length(t.bootstrap.param[abs(t.bootstrap.param) > z0.05])/B




