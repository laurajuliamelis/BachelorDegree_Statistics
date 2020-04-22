# EXEMPLE 5.3.2.

y<-c(17,34,26,10,19,17,8,16,13,11,
        17,41,26,3,-6,-4,11,16,16,4,
       21,20,11,26,42,28,3,3,16,-10,
        10,24,32,26,52,28,27,28,21,42)


alpha<-c(rep(1,10),rep(0,10),rep(0,10),rep(1,10))
beta<-c(rep(0,10),rep(1,10),rep(1,10),rep(0,10))
gamma<-c(rep(0,10),rep(1,10),rep(0,10),rep(1,10))

x <- c(rep(1,40),alpha,beta,gamma) #matriu de disseny
x <- matrix(x, ncol=4)

xtx<- t(x) %*% x

library(MASS)
betas<- ginv(xtx) %*% t(x) %*% y 

residus <- y - x %*% betas
RSS <- sum(residus^2)


#El mateix pel model reduit
xtilde <- c(rep(1,40), rep(1,40), gamma)
xtilde <- matrix(xtilde, ncol=3)
xtildetxtilde <- t(xtilde) %*% xtilde
betas0 <- ginv(xtildetxtilde) %*% t(xtilde) %*% y

residus0 <- y - xtilde %*% betas0
RSS_H <- sum(residus0^2)

q <- 1
n <- 40
r <- 3
numF <- (RSS_H - RSS)/q
denF <- RSS/(n-r)

F<- numF/denF

qf(0.95,1, 40-3) #el 4.71 esta a la zona de rebuig, rebutgem la hip nula, els farmacs actuen de forma diferent

#EXERCICI: Calcular la t-student per alpha-beta i veure que al quadrat es 4.71

crossover.lm <- lm(y ~ alpha + beta + gamma) #no cal afegir la mu per que per defecte lm porta una columna 
                                             #de punts anomenada intersect

summary(crossover.lm)

  #R no pot calcular mu, aplha, beta, gamma perque tinc 4 parametres i el rang és 3. Per això, elimina un paràmetre.
  #Estima els paràmetres com si beta no hi fos.

  #Afegim una ultima fila (restricció) a la matriu de disseny per que el rang sigui 4  
  #(això ho hem fet per comprovar el que fa R):

x <- model.matrix(crossover.lm) #aquesta funcio retorna la matriu de disseny del model lineal
xa <- rbind(x,c(0,0,1,0)) #la restriccio és: gamma=0
xa
ya <- c(y,0)

betas <- solve(t(xa) %*% xa) %*% t(xa) %*% ya
betas #dona el mateix resultat que el summary #MODEL DEL DISSENY EXPERIMENTAL

#ARA NECESSITO EL MODEL DE LA HIPOTESI NULA
#Contrastarem si H_0: alpha=beta
  #quan aplha=beta => la columna de aplha són tot 1's
crossover.lm0 <- lm( y ~ gamma )
summary(crossover.lm0)

#per cotrastar la hipotesis he de calcular la F
q <- 1
n <- 40
r <- 3

RSS <- sum(residuals(crossover.lm)^2)
RSS_H <- sum(residuals(crossover.lm0)^2)

s1 <- summary(crossover.lm)
s1$sigma #estimacio de la desviacio estandar dels errors
varEst <- (s1$sigma)^2 #variancia estimada
RSS <- s1$sigma^2 * (n-r)

s0 <- summary(crossover.lm0)
RSS_H <- s0$sigma^2 * (n-2)

numF <- (RSS_H - RSS)/q
denF <- RSS/(n-r)

F<- numF/denF
F

qf(0.95,1, 40-3) #el 4.71 esta a la zona de rebuig, rebutgem la hip nula, els farmacs actuen de forma diferent

#tot aixo no calia fer-ho perque tenim la funcio anova
anova(crossover.lm0, crossover.lm)

#MODEL ON LA INTERACCIÓ ÉS DIFERENT AB I BA

gamma.ab <- c(rep(0,10),rep(1,10),rep(0,10),rep(0,10))
gamma.ba <- c(rep(0,10),rep(0,10),rep(0,10),rep(1,10))

crossover.lm2 <- lm(y ~ alpha + beta + gamma.ab + gamma.ba)

# H_0: gamma.ab = gamma.ba

anova(crossover.lm, crossover.lm2)

# en els informes fer servir lletra monoespaiada, ex: Couriel

# L'estadístic del test ANOVA és F=3.8978 amb 1 i 36 graus de llibertat.
# llavors el p-valor del test és 0.056 > al nivell de significació 0.05.
# En conseqüència acceptem la hipòtesi nul.la (no queda molt sofisticat).
# (millor dir:) No hi ha raons estadístiques per rebutjar la hipòtesi nul.la.