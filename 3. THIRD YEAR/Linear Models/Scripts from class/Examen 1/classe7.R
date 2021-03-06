install.packages("faraway")
library(faraway)
data(gala, package="faraway") #Cada fila representa una illa, i cada illa t� set mesures
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz  + Adjacent, gala)
#�s aquest model de regressi� �til? Ser� �til sempre i quan algun dels coeficients sigui no nul. 
#Si tots els coeficients de la regressi� fosin zero, excl�s el pertinent a la resposta, el model seria in�til
#Hem de descartar que el model sigui in�til en primer lloc

nullmod <- lm(Species ~ 1, gala) #Aquest model hem diu que tots els coeficients s�n zero excepte el terme independent

anova(nullmod, lmod) #Contrastem el model de la hip�tesi que no hi ha cap variable regresora front el model complet
#Com a resultat, s'obt� un p.valor molt petit, per tant, es rebutja la hip�tesi nul.la. La regressi� �s significativa
#ja que s'ha rebutjat que tots els coeficients siguin zero excepte el terme independent

(rss0 <- deviance(nullmod)) #Permet calcular la residual sum of squares d'un model (la funci� deviance)
(rss <- deviance(lmod))
(df0 <- df.residual(nullmod)) #Graus de llibertat del model(n-r)(df.residual)
(df <- df.residual(lmod))
(fstat <- ((rss0-rss)/(df0-df))/(rss/df)) #Ens dona l'estad�stic de contrast
1-pf(fstat, df0-df, df) #Pvalor

summary(lmod) #La �ltima fila del summary ens diu si el model �s �til
#F-stat�stic = 15.7 , pvalor=6.838e-07

lmods <- lm(Species ~  Elevation + Nearest + Scruz + Adjacent, gala) #Llevem la variable
#Area per veure si �s una variable regressora rellevant

anova(lmods, lmod) #Com que el pvalor �s gran (0.2963), acceptem la hip�tesi nul.la d'aquest contrast
#La hip�tesi nul.la d'aquest contrast �s el model senzill, o sigui, llevant la variable �rea, per tant,
#Aquesta variable no �s rellevant, i es pot treure

sumary(lmod) #Cada fila representa un contrast en el que es contrasta si el coeficient �s zero o no
#Si el pvalor d'una fila �s major al nivell de significaci�, significa que no �s una variable rellevant
#L'�rea sembla que no �s important perqu� estan les altres variables. La pres�ncia de les altres variables
#fa que l'�rea sigui una variable no important. Per� podria ser que en un altre contrast en qu� no estiguesin
#totes les altres variables, si que fos important. La variable �rea, no explica m�s perque estan les altres variables
#Aix� no vol dir que no expliqui res, sin� que estant les altres, no �s important.

sumary(lm(Species ~ Area, gala))

lmods <- lm(Species ~  Elevation + Nearest + Scruz, gala) #Model en que �rea i adjacent s�n iguals a zero

anova(lmods, lmod) #P.valor �s molt petit, per tant estem rebutjant el model m�s senzill (hip�tesi nul.la)
#Rebutgem que l'area i l'adjacent siguin zero

lmods <- lm(Species ~  I(Area+Adjacent) + Elevation + Nearest + Scruz, gala) #Pregunta si Area = Adjacent
anova(lmods, lmod) #Rebutgem ja que pvalor=0.02.. per tant no acceptem la hip�tesi nul.la que Area i adjacent s�n iguals


lmods <- lm(Species ~  Area+ offset(0.5*Elevation) + Nearest + Scruz + Adjacent, gala) #Ens pregunta si la beta d'Elevation
#�s igual a 0.5 . Offset vol dir que introdueixi aquest vector columna sense que hi haja el par�metre beta. O sigui l'estimaci�
#beta d'elevaci�, ser� 0.5.

anova(lmods, lmod) #El resultat �s rebutjar que Beta d'elevaci� sigui 0.5 ja que pvalor �s molt petit

#El mateix amb t.student

(tstat <- (0.31946-0.5)/0.05366) #L'estimaci� d'elevation menys 0.5 (valor el qual penses que val l'estimaci�)i dividir entre standard error
2*pt(tstat, 24)#P.valor
tstat^2 #Coincideix amb l'estad�stic de la F

#Necessitem normalitat de l'error i verificarles tres hip�tesis de Gauss-Markov per tal de poder contrastar aquestes hip�tesis
qqnorm(gala$Species) #Perqu� volem veure si �s normal
qqline(gala$Species) #Aparentment hi ha uns quants valors que s'allunyen bastant de la normalitat
#Normalment, la normalitat falla a les cues. No ha de fallar als valors centrals
#Hi ha dubtes de que la variable resposta (Species) sigui normal. Aix� significa que el model no �s normal si la variable
#resposta no �s normal. Aix� significa que els contrastos t i Fisher no valen

#El test F mesura lo lluny que estan les sumes de quadrats de dos models lineals. Pensarem en el criteri de si les sumes
#de quadrats s�n semblants o diferents

#Test de permutacions: permutarem el resultat de la variable y (Species)
#Si realment el model no funciona, permutar el resultat trobat, no serviria de res. En canvi, si el model funciona, aquesta
#permutaci� m'inpediria trobar la regressi�


lmod <- lm(Species ~ Nearest + Scruz, gala) #Agafem el model en que nom�s Nearest i Scruz s�n les dues variables regresores
lms <- summary(lmod)

lms$fstatistic #Valor del f.statistic per saber si el model �s �til o in�til. 
#�s util amb valor 0.60 i pvalor igual a la linea de baix
1-pf(lms$fstatistic[1],lms$fstatistic[2],lms$fstatistic[3]) #P.valor
#Major al nivell de significaci�, per tant, �s �til

###ARA AMB PERMUTACIONS###

nreps <- 4000
set.seed(123)
fstats <- numeric(nreps)
for(i in 1:nreps){
  lmods <- lm(sample(Species) ~ Nearest+Scruz, gala)
  fstats[i] <- summary(lmods)$fstat[1]
}
hist(fstats) #Hauria de tenir forma de F-Fisher , i la t�.
#De totes aquestes possibilitats, hi ha una que �s la real de la F sense permutar (0.60 d'abans)
#Aquest histograma s'ha convertit o representa la distribuci� de l'estad�stic F. Per tant, amb
#aquest histograma es pot decidir si la f que he trobat de veritat(0.60) est� a la zona d'accteptaci�
#o de rebuitg. Si est� a la cua rebutjar� i si est� a la part central (cap al 0) acceptarem
#A ull es pot veure que el 0.6 est� en la zona d'acceptar

mean(fstats > lms$fstat[1]) #Quantes F s�n superiors a la F que he trobat real(0.6) -> Cua de la dreta
#True false... com fiquem mean calculem la freq��ncia relativa, i per tant, tenim el p.valor
#Aquest valor �s 0.5645, molt superior a 0.05 de significaci�, per tant, estic en la zona d'acceptaci�
#Per tant, en qualsevol permutaci� que pugui fer, la F que he trobat, �s un valor acceptable, no �s un valor extrem

#El test de permutacions ha consistit en substituir la distribuci� te�rica que �s la F de Fisher en una distribuci�
#emp�rica que �s un histograma. Aix� es pot utilitzar quan no es coneix la distribuci� de l'estad�stic i veure si el 
#valor que hem trobat �s un valor normal o un valor extrem per rebutjar o acceptar.

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
