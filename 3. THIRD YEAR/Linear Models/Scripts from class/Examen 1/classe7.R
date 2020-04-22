install.packages("faraway")
library(faraway)
data(gala, package="faraway") #Cada fila representa una illa, i cada illa té set mesures
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz  + Adjacent, gala)
#És aquest model de regressió útil? Serà útil sempre i quan algun dels coeficients sigui no nul. 
#Si tots els coeficients de la regressió fosin zero, exclòs el pertinent a la resposta, el model seria inútil
#Hem de descartar que el model sigui inútil en primer lloc

nullmod <- lm(Species ~ 1, gala) #Aquest model hem diu que tots els coeficients són zero excepte el terme independent

anova(nullmod, lmod) #Contrastem el model de la hipòtesi que no hi ha cap variable regresora front el model complet
#Com a resultat, s'obté un p.valor molt petit, per tant, es rebutja la hipòtesi nul.la. La regressió és significativa
#ja que s'ha rebutjat que tots els coeficients siguin zero excepte el terme independent

(rss0 <- deviance(nullmod)) #Permet calcular la residual sum of squares d'un model (la funció deviance)
(rss <- deviance(lmod))
(df0 <- df.residual(nullmod)) #Graus de llibertat del model(n-r)(df.residual)
(df <- df.residual(lmod))
(fstat <- ((rss0-rss)/(df0-df))/(rss/df)) #Ens dona l'estadístic de contrast
1-pf(fstat, df0-df, df) #Pvalor

summary(lmod) #La última fila del summary ens diu si el model és útil
#F-statístic = 15.7 , pvalor=6.838e-07

lmods <- lm(Species ~  Elevation + Nearest + Scruz + Adjacent, gala) #Llevem la variable
#Area per veure si és una variable regressora rellevant

anova(lmods, lmod) #Com que el pvalor és gran (0.2963), acceptem la hipòtesi nul.la d'aquest contrast
#La hipòtesi nul.la d'aquest contrast és el model senzill, o sigui, llevant la variable Àrea, per tant,
#Aquesta variable no és rellevant, i es pot treure

sumary(lmod) #Cada fila representa un contrast en el que es contrasta si el coeficient és zero o no
#Si el pvalor d'una fila és major al nivell de significació, significa que no és una variable rellevant
#L'àrea sembla que no és important perquè estan les altres variables. La presència de les altres variables
#fa que l'àrea sigui una variable no important. Però podria ser que en un altre contrast en què no estiguesin
#totes les altres variables, si que fos important. La variable àrea, no explica més perque estan les altres variables
#Això no vol dir que no expliqui res, sinó que estant les altres, no és important.

sumary(lm(Species ~ Area, gala))

lmods <- lm(Species ~  Elevation + Nearest + Scruz, gala) #Model en que àrea i adjacent són iguals a zero

anova(lmods, lmod) #P.valor és molt petit, per tant estem rebutjant el model més senzill (hipòtesi nul.la)
#Rebutgem que l'area i l'adjacent siguin zero

lmods <- lm(Species ~  I(Area+Adjacent) + Elevation + Nearest + Scruz, gala) #Pregunta si Area = Adjacent
anova(lmods, lmod) #Rebutgem ja que pvalor=0.02.. per tant no acceptem la hipòtesi nul.la que Area i adjacent són iguals


lmods <- lm(Species ~  Area+ offset(0.5*Elevation) + Nearest + Scruz + Adjacent, gala) #Ens pregunta si la beta d'Elevation
#és igual a 0.5 . Offset vol dir que introdueixi aquest vector columna sense que hi haja el paràmetre beta. O sigui l'estimació
#beta d'elevació, serà 0.5.

anova(lmods, lmod) #El resultat és rebutjar que Beta d'elevació sigui 0.5 ja que pvalor és molt petit

#El mateix amb t.student

(tstat <- (0.31946-0.5)/0.05366) #L'estimació d'elevation menys 0.5 (valor el qual penses que val l'estimació)i dividir entre standard error
2*pt(tstat, 24)#P.valor
tstat^2 #Coincideix amb l'estadístic de la F

#Necessitem normalitat de l'error i verificarles tres hipòtesis de Gauss-Markov per tal de poder contrastar aquestes hipòtesis
qqnorm(gala$Species) #Perquè volem veure si és normal
qqline(gala$Species) #Aparentment hi ha uns quants valors que s'allunyen bastant de la normalitat
#Normalment, la normalitat falla a les cues. No ha de fallar als valors centrals
#Hi ha dubtes de que la variable resposta (Species) sigui normal. Això significa que el model no és normal si la variable
#resposta no és normal. Això significa que els contrastos t i Fisher no valen

#El test F mesura lo lluny que estan les sumes de quadrats de dos models lineals. Pensarem en el criteri de si les sumes
#de quadrats són semblants o diferents

#Test de permutacions: permutarem el resultat de la variable y (Species)
#Si realment el model no funciona, permutar el resultat trobat, no serviria de res. En canvi, si el model funciona, aquesta
#permutació m'inpediria trobar la regressió


lmod <- lm(Species ~ Nearest + Scruz, gala) #Agafem el model en que només Nearest i Scruz són les dues variables regresores
lms <- summary(lmod)

lms$fstatistic #Valor del f.statistic per saber si el model és útil o inútil. 
#És util amb valor 0.60 i pvalor igual a la linea de baix
1-pf(lms$fstatistic[1],lms$fstatistic[2],lms$fstatistic[3]) #P.valor
#Major al nivell de significació, per tant, és útil

###ARA AMB PERMUTACIONS###

nreps <- 4000
set.seed(123)
fstats <- numeric(nreps)
for(i in 1:nreps){
  lmods <- lm(sample(Species) ~ Nearest+Scruz, gala)
  fstats[i] <- summary(lmods)$fstat[1]
}
hist(fstats) #Hauria de tenir forma de F-Fisher , i la té.
#De totes aquestes possibilitats, hi ha una que és la real de la F sense permutar (0.60 d'abans)
#Aquest histograma s'ha convertit o representa la distribució de l'estadístic F. Per tant, amb
#aquest histograma es pot decidir si la f que he trobat de veritat(0.60) està a la zona d'accteptació
#o de rebuitg. Si està a la cua rebutjaré i si està a la part central (cap al 0) acceptarem
#A ull es pot veure que el 0.6 està en la zona d'acceptar

mean(fstats > lms$fstat[1]) #Quantes F són superiors a la F que he trobat real(0.6) -> Cua de la dreta
#True false... com fiquem mean calculem la freqüència relativa, i per tant, tenim el p.valor
#Aquest valor és 0.5645, molt superior a 0.05 de significació, per tant, estic en la zona d'acceptació
#Per tant, en qualsevol permutació que pugui fer, la F que he trobat, és un valor acceptable, no és un valor extrem

#El test de permutacions ha consistit en substituir la distribució teòrica que és la F de Fisher en una distribució
#empírica que és un histograma. Això es pot utilitzar quan no es coneix la distribució de l'estadístic i veure si el 
#valor que hem trobat és un valor normal o un valor extrem per rebutjar o acceptar.

summary(lmod)$coef[3,]
tstats <- numeric(nreps)

set.seed(123)
for(i in 1:nreps){
  lmods <- lm(Species ~ Nearest+sample(Scruz), gala)
  tstats[i] <- summary(lmods)$coef[3,3]
}
mean(abs(tstats) > abs(lms$coef[3,3]))
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz  + Adjacent, gala)
sumary(lmod)
qt(0.975, 30-6)
-0.02394 + c(-1,1) *  2.0639 * 0.02242
-0.07480 + c(-1,1) *  2.0639 * 0.01770

confint(lmod) #confint funcio que calcula els intervals de confianza
require(ellipse)
plot(ellipse(lmod,c(2,6)),type="l",ylim=c(-0.13,0))
points(coef(lmod)[2], coef(lmod)[6], pch=19)
abline(v=confint(lmod)[2,],lty=2)
abline(h=confint(lmod)[6,],lty=2)
sample(10,rep=TRUE)
set.seed(123)
nb <- 4000
coefmat <- matrix(NA,nb,6)
resids <- residuals(lmod)
preds <- fitted(lmod)
for(i in 1:nb){
  booty <- preds + sample(resids, rep=TRUE)
  bmod <- update(lmod, booty ~ .)
  coefmat[i,] <- coef(bmod)
}
colnames(coefmat) <- c("Intercept",colnames(gala[,3:7]))
coefmat <- data.frame(coefmat)
apply(coefmat,2,function(x) quantile(x,c(0.025,0.975)))
require(ggplot2)
ggplot(coefmat, aes(x=Area)) + geom_density() + geom_vline(xint=c(-0.0628, 0.0185),lty=2)
ggplot(coefmat, aes(x=Adjacent)) + geom_density() + geom_vline(xint=c(-0.104, -0.0409),lty=2)

# interval de confianza de beta_area + beta_elevacio (beta1 + beta2)

#var(beta_Est) = sigma_est^2 * (X'X)^-1
summary(lmod)$sigma^2
x<-model.matrix(lmod)
crossprod(x)

varBeta_est <- summary(lmod)$sigma^2 * solve(crossprod(x))
varBeta_est #aqui trobem els numeros per calcular var(beta1 + beta2) = var(beta1) + var(beta2) + 2cov(beta1,beta2)
