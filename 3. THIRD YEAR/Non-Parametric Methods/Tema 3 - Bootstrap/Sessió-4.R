### R code from vignette source 'Boot_sessio_4_17_18.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: Boot_sessio_4_17_18.Rnw:37-41
###################################################
# Lectura de les dades PROCEDENTS D'UNA NORMAL
x <- c(15.54, 21.06, 16.52, 13.62, 16.14, 10.98, 13.53, 16.02, 16.79, 15.90)
n <- length(x)
sqrt.n <- sqrt(n)


###################################################
### code chunk number 2: Boot_sessio_4_17_18.Rnw:56-63
###################################################
# Estimació de la mitjana sobre la muestra "real" x:
media.estim <- mean(x)
media.estim
# Estimació de la desviació estàndar sobre la mostra "real" x:
sd(x)
# Estimació de l'error estàndard del estimador de la mitjana:
sd(x)/sqrt.n


###################################################
### code chunk number 3: Boot_sessio_4_17_18.Rnw:76-86
###################################################
B <- 10000

t.boot <- replicate(B,
	{
		x.boot <- sample(x, replace = TRUE)
		sqrt.n * (mean(x.boot) - media.estim) / sd(x.boot)
	}
)

t.boot[1:10]


###################################################
### code chunk number 4: Boot_sessio_4_17_18.Rnw:94-104
###################################################
alfa <- 0.05

# Valors crítics que deixan probabilidad alfa/2 
# en la cua esquerraa i dreta de la distribució
# bootstrap de l'estadístic t:

quantile(t.boot, probs=c(alfa/2, 1 - alfa/2))

# Comparem-ho amb els crresponents valors t de Student:
qt(c(alfa/2, 1 - alfa/2), df = n - 1)


###################################################
### code chunk number 5: Boot_sessio_4_17_18.Rnw:123-139
###################################################
# Interval de confiança bootstrap-t de CUES EQUIPROBABLES, amb nivell de
# confiança nominal 1 - alfa (= 0.95)
# Fixeu-vos el canvi d'ordre dels quantils

icBoot.t <- media.estim - 
  quantile(t.boot, probs=c(1 - alfa/2, alfa/2)) * sd(x) / sqrt.n

names(icBoot.t) <- NULL
attr(icBoot.t, "conf.level") <- 0.95
icBoot.t

# Comparació AMB  Intervalo de Confiança per la mitjama a partir 
# de suposició de normalitad --> distribución muestral t de Student

t.test(x, var.equal = TRUE)$conf.int



###################################################
### code chunk number 6: Boot_sessio_4_17_18.Rnw:149-169
###################################################
# Aquest Interval de Confianza bootstrap-t que 
# anomenarem {\bf simetritzat} (simétric respecte de l'estimació 
# del paràmetre), PERO AMB LES CUES de la 
# distribució de t* desiguals NO EQUIPROBABLES.
# Determinaremos el valor "t_alfa" tal que
# P*{|t*| <= t_alfa} = 1 - alfa
t_alfa <- quantile(abs(t.boot), probs = 1 - alfa)
t_alfa

# Interval de Confiança Bootstrap-t simétric:
icBoot.t.sim <- media.estim - c(t_alfa, -t_alfa) * sd(x) / sqrt.n
names(icBoot.t.sim) <- NULL
attr(icBoot.t.sim, "conf.level") <- 0.95
icBoot.t.sim

# Comparació AMB  Intervalo de Confiança per la mitjama
# a partir de suposició de normalitat --> distribució mostral t de Student

t.test(x, var.equal = TRUE)$conf.int



###################################################
### code chunk number 7: Boot_sessio_4_17_18.Rnw:179-201
###################################################
sd.x = sd(x)
t.boot <- replicate(B,
{
  x.boot <- rnorm(n, mean = media.estim, sd = sd.x)
  sqrt.n * (mean(x.boot) - media.estim) / sd(x.boot)
}
)

# Interval de Confianza bootstrap-t de CUES EQUIPROBABLES 
# amb nivell de confiança nominal 1 - alfa (= 0.95)

icBootParam.t <- media.estim - 
  quantile(t.boot, probs=c(1 - alfa/2, alfa/2)) * sd(x) / sqrt.n
names(icBootParam.t) <- NULL
attr(icBootParam.t, "conf.level") <- 0.95
icBootParam.t

# Comparació AMB Interval de Confiança per la mitjama
# a partir de  suposició de normalitad --> distribución muestral t de Student

t.test(x, var.equal = TRUE)$conf.int



###################################################
### code chunk number 8: Boot_sessio_4_17_18.Rnw:211-222
###################################################
# Interval de confiança bootstrap-t simetritzat.
# Trobarem el valor "t_alfa" tal que
# P*{|t*| <= t_alfa} = 1 - alfa

t_alfa <- quantile(abs(t.boot), probs = 1 - alfa)
icBootParam.t.sim <- media.estim - c(t_alfa, -t_alfa) * sd(x) / sqrt.n
names(icBootParam.t.sim) <- NULL
icBootParam.t.sim

# Recordem, IC paramétric t de Student:
t.test(x, var.equal = TRUE)$conf.int


###################################################
### code chunk number 9: Boot_sessio_4_17_18.Rnw:235-239
###################################################
# Lectura de les dades PROCEDENTS D'UNA EXPONENCIAL
x <- c(8.51,  8.71, 69.19, 10.05, 23.64, 8.67, 1.51, 20.36, 1.23, 5.27)
n = length(x)
sqrt.n <- sqrt(n)


###################################################
### code chunk number 10: Boot_sessio_4_17_18.Rnw:251-254
###################################################
# Estimació de la mitjana sobre la muestra "real" x:
media.estim <- mean(x)
media.estim


###################################################
### code chunk number 11: Boot_sessio_4_17_18.Rnw:259-282
###################################################

t.boot <- replicate(B,
	{
		x.boot <- sample(x, replace = TRUE)
		sqrt.n * (mean(x.boot) - media.estim) / sd(x.boot)
	}
)

# Valors crítics en la "tabulació" de la distribució 
# mostral bootstrap de l'estadístic:

quantile(t.boot, probs = c(alfa/2, 1 - alfa/2))
# NO S'ASSEMBLEN alss corresponents valors de la distribució t de Student:
qt(c(alfa/2, 1 - alfa/2), df = n - 1)

# Interval de confiança bootstrap-t
icBoot.t <- media.estim - quantile(t.boot, probs=c(1 - alfa/2, alfa/2)) * sd(x) / sqrt.n
names(icBoot.t) <- NULL
attr(icBoot.t, "conf.level") <- 0.95
icBoot.t

# Molt diferent de l'interval de confiança t de Student
t.test(x, var.equal = TRUE)$conf.int


###################################################
### code chunk number 12: Boot_sessio_4_17_18.Rnw:289-306
###################################################
# Interval de confiança bootstrap-t simetritzat
t_alfa <- quantile(abs(t.boot), probs = 1 - alfa)

# A cada cua deixa la següent probabilitat:
sum(t.boot < -t_alfa) / B
sum(t.boot > t_alfa) / B

# En realitat fixeu-vos que la probabilitat es concentra en la cua esquerra

icBoot.t.sim <- media.estim - c(t_alfa, -t_alfa) * sd(x) / sqrt.n
names(icBoot.t.sim) <- NULL
attr(icBoot.t.sim, "conf.level") <- 0.95
icBoot.t.sim

# Molt diferent de l'interval de confiança t de Student
t.test(x, var.equal = TRUE)$conf.int



###################################################
### code chunk number 13: Boot_sessio_4_17_18.Rnw:314-337
###################################################

t.boot <- replicate(B,
{
  x.boot <- rexp(n, rate = 1 / media.estim)
  sqrt.n * (mean(x.boot) - media.estim) / sd(x.boot)
}
)

# Valors crítics en la "tabulació" de la distribució 
# mostral bootstrap de l'estadístic:

quantile(t.boot, probs = c(alfa/2, 1 - alfa/2))
# NO S'ASSEMBLEN alss corresponents valors de la distribució t de Student:
qt(c(alfa/2, 1 - alfa/2), df = n - 1)

# Interval de confiança bootstrap-t
icBoot.t <- media.estim - quantile(t.boot, probs=c(1 - alfa/2, alfa/2)) * sd(x) / sqrt.n
names(icBoot.t) <- NULL
attr(icBoot.t, "conf.level") <- 0.95
icBoot.t

# Molt diferent de l'interval de confiança t de Student
t.test(x, var.equal = TRUE)$conf.int


###################################################
### code chunk number 14: Boot_sessio_4_17_18.Rnw:347-364
###################################################
# Interval de confiança bootstrap-t simetritzat
t_alfa <- quantile(abs(t.boot), probs = 1 - alfa)

# A cada cua deixa la següent probabilitat:
sum(t.boot < -t_alfa) / B
sum(t.boot > t_alfa) / B

# En realitat fixeu-vos que la probabilitat es concentra en la cua esquerra

icBoot.t.sim <- media.estim - c(t_alfa, -t_alfa) * sd(x) / sqrt.n
names(icBoot.t.sim) <- NULL
attr(icBoot.t.sim, "conf.level") <- 0.95
icBoot.t.sim

# Molt diferent de l'interval de confiança t de Student
t.test(x, var.equal = TRUE)$conf.int



###################################################
### code chunk number 15: Boot_sessio_4_17_18.Rnw:393-463
###################################################
nsim = 1000 # COMPTE HAURIA DE SER 10.000
# Generació de 'nsim' mostres procedents dels següents paràmetres
# coneguts, fixats per nosaltres: 
media <- 15
expRate <- 1 / media
# Sobre cadascuna d'aquestes mostres es calculessin diversos intervals de
# confiança bootstrap. Aquests intervals requeriran la generació de
# 'B' remuostres bootstrap:
B <- 100 # COMPTE HAURIA DE SER 1000

# És a dir, estem fent una simulació de grandària total
# 10000 x 1000. Trigarà bastant........

# Estudiarem les propietats d'intervals de nivell de confiança
# "nominal" (és a dir, que almenys en teoria és) 0.95.
alfa <- 0.05
alfa1 <- alfa / 2
alfa2 <- 1 - alfa1
probColas <- c(alfa2, alfa1)
conf.level <- 1 - alfa

set.seed(4317)
coverages <- replicate(nsim,
	{
		x.sim <- rexp(n, rate = expRate)
	  media.estim <- mean(x.sim)
    se.media <- sd(x.sim) / sqrt.n
	  # Bootstrap no paramètric:
	  t.boot <- replicate(B,
      {
        x.boot <- sample(x.sim, replace = TRUE)
        sqrt.n * (mean(x.boot) - media.estim) / sd(x.boot)
       }
	  )
    # IC boot-t
	  icBoot.t <- media.estim - quantile(t.boot, probs = probColas) * se.media
	  # IC boot-t simetritzat:
	  t_alfa <- quantile(abs(t.boot), probs = conf.level)
	  icBoot.t.sim <- media.estim - c(t_alfa, -t_alfa) * se.media
	  
	  # Bootstrap paramètric:
    estim.expRate = 1 / media.estim
	  t.boot.param <- replicate(B,
      {
        x.boot <- rexp(n, rate = estim.expRate)
        sqrt.n * (mean(x.boot) - media.estim) / sd(x.boot)
      }
	  )
	  # IC boot-t paramètric
	  icBootParam.t.sim <- media.estim - quantile(t.boot.param, probs = probColas) * se.media
	  # IC boot-t simetritzat paramètrico:
	  t_alfa <- quantile(abs(t.boot.param), probs = conf.level)
	  icBootParam.t.sim <- media.estim - c(t_alfa, -t_alfa) * se.media
    
    # IC t de Student:
    ic.t <- t.test(x.sim, var.equal = TRUE)$conf.int
    
	  # Recubriment?:
	  c(media >= c(icBoot.t[1], icBoot.t.sim[1], icBootParam.t[1], 
	               icBootParam.t.sim[1], ic.t[1])) &
	  c(media <= c(icBoot.t[2], icBoot.t.sim[2], icBootParam.t[2], 
	               icBootParam.t.sim[2], ic.t[2]))
	}
  
)
rownames(coverages) <- c("boot-t NP", "boot-t NP SIM",
  "boot-t P", "boot-t P SIM", "t Student")
coverages[,1:10]
# Estimació del veritable nivell de confiança:
apply(coverages, 1, mean)


###################################################
### code chunk number 16: Boot_sessio_4_17_18.Rnw:476-489
###################################################
family <- factor(c("Ceratops","Hadro","Pachycephalo","Tyranno","Ornithomo",
                   "Sauronith"),
                 levels=c("Ceratops","Hadro","Pachycephalo","Tyranno",
                          "Ornithomo","Sauronith"))

dinos <- data.frame(
  upper = c(50, 29, 3, 3, 4, 1),
  middle= c(53, 51, 2, 3, 8, 6),
  lower = c(19,  7, 1, 2, 1, 3)
)

rownames(dinos) <- family
dinos


###################################################
### code chunk number 17: Boot_sessio_4_17_18.Rnw:514-520
###################################################
# 
shannon <- function(freq.abs)
{
	freq.rels = freq.abs / sum(freq.abs)
	- sum(ifelse(freq.rels > 0, freq.rels * log2(freq.rels), 0))
}


###################################################
### code chunk number 18: Boot_sessio_4_17_18.Rnw:524-531
###################################################

# Diversity over a given period:
shannon(dinos[,"middle"])
shannon(dinos[,2])
# Diversity for all periods:
apply(dinos, 2, shannon)
vapply(dinos, shannon, FUN.VALUE = 0.0)


###################################################
### code chunk number 19: Boot_sessio_4_17_18.Rnw:547-567
###################################################
shannon <- function(freq.abs, compute.se = TRUE)
{
  n <- sum(freq.abs)
  freq.rels = freq.abs / n
	log2freq.rels <- ifelse(freq.rels > 0, log2(freq.rels), 0)
	result <- - sum(freq.rels * log2freq.rels)
	if (compute.se) {
    attr(result, "se") <- sqrt((sum(freq.rels * log2freq.rels^2) - result^2) / n)
	}
	return(result)
}

biodivs <- lapply(dinos, shannon)
biodivs
unlist(biodivs)
sapply(biodivs, attr, "se")

biodivs <- sapply(dinos, shannon, compute.se = FALSE)
biodivs
sapply(biodivs, attr, "se")


###################################################
### code chunk number 20: Boot_sessio_4_17_18.Rnw:584-599
###################################################
freq.abs <- dinos[,"upper"]
freq.abs
shannon.sample <- shannon(freq.abs)
shannon.sample
n <- sum(freq.abs)
n
freq.rels <- freq.abs / n
freq.rels

# One bootstrap multinomial resample:

set.seed(12345)
freq.boot <- rmultinom(1, size = n, prob = freq.rels)
freq.boot
# Caution: it is a 'k x one-column matrix' with k = length(freq.rels)


