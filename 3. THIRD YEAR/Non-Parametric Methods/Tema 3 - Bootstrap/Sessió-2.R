### R code from vignette source 'Boot_sessio_2_17_18.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: Boot_sessio_2_17_18.Rnw:38-43
###################################################
# Lectura de les dades PROCEDENTS D'UNA NORMAL
x <- c(15.54, 21.06, 16.52, 13.62, 16.14, 10.98, 13.53, 16.02, 16.79, 15.90)
n<-length(x)
mu <- 15
sigma <- 3


###################################################
### code chunk number 2: Boot_sessio_2_17_18.Rnw:50-54
###################################################
tStud <- function(x, mitjana)
{
	(mean(x) - mitjana) / sqrt(var(x)/length(x))
}


###################################################
### code chunk number 3: Boot_sessio_2_17_18.Rnw:68-91
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
### code chunk number 4: Boot_sessio_2_17_18.Rnw:100-102
###################################################
tCritStud <- qt(c(0.975, 0.025), df = n - 1)
tCritStud


###################################################
### code chunk number 5: Boot_sessio_2_17_18.Rnw:106-107
###################################################
xBarra - tCritStud * s.xBarra


###################################################
### code chunk number 6: Boot_sessio_2_17_18.Rnw:112-114
###################################################
dens.normAprox <- dnorm(rang.t)
lines(rang.t, dens.normAprox, type="l", col="blue")


###################################################
### code chunk number 7: Boot_sessio_2_17_18.Rnw:118-120
###################################################
tCritNorm = qnorm(c(0.975, 0.025))
tCritNorm


###################################################
### code chunk number 8: Boot_sessio_2_17_18.Rnw:124-125
###################################################
xBarra - tCritNorm * s.xBarra


###################################################
### code chunk number 9: Boot_sessio_2_17_18.Rnw:129-142
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
### code chunk number 10: Boot_sessio_2_17_18.Rnw:149-150
###################################################
tCritBoot = quantile(t.bootstrap, probs = c(0.975, 0.025))


###################################################
### code chunk number 11: Boot_sessio_2_17_18.Rnw:154-155
###################################################
xBarra - tCritBoot * s.xBarra


###################################################
### code chunk number 12: Boot_sessio_2_17_18.Rnw:162-173
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
### code chunk number 13: Boot_sessio_2_17_18.Rnw:176-178
###################################################
# valors crítics segons bootstrap paramètric
tCritBoot.param = quantile(t.bootstrap.param, probs = c(0.975, 0.025))


###################################################
### code chunk number 14: Boot_sessio_2_17_18.Rnw:181-182
###################################################
xBarra - tCritBoot.param * s.xBarra


###################################################
### code chunk number 15: Boot_sessio_2_17_18.Rnw:191-192
###################################################
z0.05 <- qnorm(0.025, lower.tail = FALSE)  # 1.959964


###################################################
### code chunk number 16: Boot_sessio_2_17_18.Rnw:196-197
###################################################
pnorm(z0.05, lower.tail = FALSE)


###################################################
### code chunk number 17: Boot_sessio_2_17_18.Rnw:201-203
###################################################
# Però la veritable probabilitat és:
pt(z0.05, df = n - 1, lower.tail = FALSE)


###################################################
### code chunk number 18: Boot_sessio_2_17_18.Rnw:208-209
###################################################
length(t.bootstrap[t.bootstrap > z0.05])/B


###################################################
### code chunk number 19: Boot_sessio_2_17_18.Rnw:215-216
###################################################
length(t.bootstrap.param[t.bootstrap.param > z0.05])/B


