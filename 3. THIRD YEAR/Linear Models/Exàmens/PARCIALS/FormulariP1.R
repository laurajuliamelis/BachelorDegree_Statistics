################                                                                                                   si p-valor < 0.05 -> RECHAZAMOS NULA
## PREGUNTA 1 ## 
################

## (A) QUINA CONDICIO HA DE VERIFICAR UNA FUNCIO PARAMETRICA PER A QUE SIGUI ESTIMABLE EN AQUEST MODEL?

# En el cas de nomes un valor possible per funcio (6 equacions, 6 valors despres de la igualtat - 1 a cada una): 
y <- c(10.98, 11.03, 0.95, 4.03, 6.94, 7.02)
x <- c(2, 1, 3,
       2, 1, 3,
       1,-1, 0,
       1, 0, 1,
       1, 1, 2,
       1, 1, 2)
x <- matrix(x, ncol=3, byrow = TRUE)
qr(x)$rank 

# En el cas de diveros valors per cada funcio (4 equacions, 12 valors despres de la igualtat - 3 a cada una):
y <- c(2.95, 3.01, 2.98, 6.03, 6.01, 5.99, 8.88, 8.97, 9, 3.02, 3.10, 2.98)
x <- c(rep(c(1,-1,1),3),rep(c(1,2,1),3),rep(c(2,1,2),3),rep(c(0,3,0),3))
x <- matrix(x,ncol=3, byrow=T)
x
qr(x)$rank

# OPCIONAL: El rang de la matriu de disseny, x, es 2, i eliminem la fila 1 (ja que f1=f4+f5), la 2 (ja que f2=f1), la 4 (f4= f1-f5)i la 6 (f6=f5).
# Tenim 3 parametres i el rang de la matriu de disseny es 2, llavors per trobar les funcions parametriques estimables cal resoldre el seguent sistema d'equacions:
# $(a_1, a_2, a_3) = \lambda_1(2, 1, 3) + \lambda_2(1, -1, 0)$
# De manera que una combinacio lineal $\psi = a_1\alpha + a_2\beta + a_3\gamma$ sera f.p.e si $a_1= -(a_2) + a_3$.


# (B) INDIQUEU SI LES FUNCIONS PARAMETRIQUES SEGUENTS SON ESTIMABLES I CALCULEU L ESTIMADOR MQ QUAN SIGUI POSSIBLE.

# Per mirar si son estimables: 
# Essent alpha + 2beta + 3gamma + delta = (1,2,3,1), sera estimable si es complei que: 1 = -2 + 3 + 1.
# (i) $\alpha + \gamma = (1,0,1): 1 = -0+1$, per tant, si que es estimable.
# (ii) $\gamma = (0,0,1): 0 \neq -0+1$, per tant, no es estimable.

# Per calcular l'estimador MQ:
library(MASS)
xtx  <- t(x) %*% x
xtxi <- ginv(xtx)
betas <- xtxi %*% t(x) %*% y #formula per calcular les estimacions

# Sumem les betas dels parametres que apareixen en la funció. (En la funcio alpha + gamma:)
betas[1]+ betas[3]   


# (C.1) CALCULEU L ESTIMACIO DE LA COVARIANCIA ENTRE ELS ESTIMADORS LINEALR OPTIMS ($\alpha + \gamma$) I ($\beta + \gamma$), 
# I LA VARIANCIA DE L ESTIMADOR LINEAL OPTIM DE ($\alpha + \gamma$).

# Comprobem que son funcions estimables (si no han surtit abans).
n <- length(y)
r <- qr(x)$rank
pred  <- x %*% betas   # Calculem les prediccions
resid <- y - pred      # Calculem els residus: diferencia entre les observacions i el model
RSS   <- sum(resid^2)  # Residual Sum of Squares
MSE   <- RSS/(n-r)     # ECM = RSS/(6-2) (sigma^2)

# Ens demanen COV(a,b) i VAR(a)=COV(a,a).
# Formules: cov(a'betas, b'betas) = sigma^2 * a'(X'X)^-b // var(a'betas) = sigma^2 a'(X'X)^-a
a <- c(1, 0, 1) # a'beta = (1,0,1) *(alpha, beta, gamma)
b <- c(0, 1, 1) # b'beta = (0,1,1) *(alpha, beta, gamma)

cov <- MSE * t(a) %*% xtxi %*% b
cov

var <- MSE * t(a) %*% xtxi %*% a
var

# (C.2) CALCULEU L ESTIMACIO DE LES VARIANCIES DELS ESTIMADORS LINEALR OPTIMS DE LES FUNCIONS PARAMETRIQUES ESTIMABLES
# ($\alpha + \gamma$) I ($\beta + \gamma$), I LA COVARIANCIA ENTRE ELLS.
n <- length(y)
r <- qr(x)$rank
pred  <- x %*% betas   # Calculem les prediccions
resid <- y - pred      # Calculem els residus: diferencia entre les observacions i el model
RSS   <- sum(resid^2)  # Residual Sum of Squares
MSE   <- RSS/(n-r)     # ECM = RSS/(6-2) (sigma^2)

# Ens demanen VAR(a)=COV(a,a), VAR(b)=COV(b,b)  I COV(a,b).
# Formules: var(a'betas) = sigma^2 a'(X'X)^-a //cov(a'betas, b'betas) = sigma^2 * a'(X'X)^-b
a <- c(1, 0, 1) # a'beta = (1,0,1) *(alpha, beta, gamma)
b <- c(0, 1, 1) # b'beta = (0,1,1) *(alpha, beta, gamma)

varA <- MSE * t(a) %*% xtxi %*% a
varA

varB <- MSE * t(b) %*% xtxi %*% b
varB

cov <- MSE * t(a) %*% xtxi %*% b
cov



# (**) CALCUEU UN INTERVAL DE CONFIANCA AL 95% PER A $\alpha + \gamma$.
# Formula IC: alphaSombrero(betas[1]) + gammaSombrero +- tstudent sub n-a(alpha) * se(alphaSombrero + gammaSombrero)
se <- sqrt(var)
(betas[1]+betas[3]) + c(-1,1) * (qt(p = 0.975, df = n-r) * se)



# (D) FEU EL CONTRAST D HIPOTESI $H_0: \alpha + \gamma = 4$  I TAMBE  $H_O: \alpha + \gamma = 4$, $\beta + \gamma = 3$.
#tenint el Ic, si el 4 cau a dintre, acceptem i sino no.

# Per al primer contrast, considerem l'estadistic t-student, ja que la hipotesis nomes conte una equacio.
t.est <- as.numeric((sum(betas[c(1,3)]) - 4)/sqrt(var))
(pt(t.est, df= n - r, lower.tail = F) * 2)  #p-valor

# El segon contrast, en canvi, requereix un test F:
A <- c(1,0,1,
       0,1,1)
A <- matrix(A, ncol=3, byrow=T)
qr(A)$rank

Abetas <- A%*% betas - c(4,3) # c(4,3): valors despres de la igualtat
atai <- solve (A %*% xtxi %*% t(A))
num <- t(Abetas) %*% atai %*% (Abetas/2)
F.est <- num / MSE
(p.value <- pf(F.est, df1= 2, df2= n-r, lower.tail = F))  # df1=2, perque ens donen dues equacions
# Com que p valor= 0.868 > 0.05, es clar que acceptem la hipotesi nulla.


################
## Problema 2 ##
################

## CARREGAR DADES ".RData"
load("longnose.Rdata")
str(Data)

Data <- read.table("nom.txt", header=TRUE, sep=",")
head(Data)

Data <- read.csv2("nom.csv")
head(Data)


# (A) FER RESUM NUMERIC DE LES VARIABLES IMPLICADS EN LA REGRESSIO.FER UN GRAFIC QUE RELACIONI LES VARIABLES DOS A DOS. 

# Resum numeric
summary(Data)

# Grafic que relaciona les variables dos a dos
pairs(Data)

# Variables mes correlacionades amb la variable resposta:
cor(Data)  
# Mirar la fila que correspon a la variable dependent, i mirem quines columnes tenen valors grans 
# (aquelles seran les variables independents mes correlacionades).
# RESULTAT: Aixi doncs, la maxima correlacio es dona per X i X, amb valors superiors a X.


# (B) ESTUDIEU LA NORMALITAT DE LA VARIABLE RESPOSTA. FEU UN HISTOGRAMA DE LES DADES D AQUESTA VARIABLE
# I PINTEU UNA CORBA DE DENSITAT ESTIMADA

#Graficament:
qqnorm(Data$Longnose)
qqline(Data$Longnose) # Per a que surti la linea a comparar

# Numericament: amb el test de shapiro
shapiro.test(Data$Longnose) #p-valor < 0.05, les dades son normals

# Amb el test de normalitat de D'Agostino:
library(moments)
agostino.test(Data$Longnose)

# Histograma:
hist(Data$Longnose, freq =F, ylim=c(0,0.025))
lines(density(Data$Longnose), col="red")

# (B.b) APLICANT LOGARITMES:
log <- log(Data$Longnose)

#Graficament:
qqnorm(log)
qqline(log) # Per a que surti la linea a comparar

# Numericament: amb el test de shapiro
shapiro.test(log) #p-valor < 0.05, les dades son normals

# Amb el test de normalitat de D'Agostino:
library(moments)
agostino.test(log)

# Histograma:
hist(log, freq =F, ylim=c(0,0.5))
lines(density(log), col="red")


# (C) OBTENIU L ESTIMACIO DELS PARAMETRES DEL MODEL ($\beta_0, \beta_1, \sigma^2$) I CALCULEU EL COEFICIENT DE DETERMINACIO.
# ES SIGNIFICATIVA LA REGRESSIO? QUE SIGNIFICA AIXO ULTIM?
g <- lm(Longnose ~ Temp, data=Data)
sg <- summary(g)
coef(g)      # Estimacio parametres: $\beta_0 = -0.8769 i \beta_Temp =2.1032$
(sg$sigma)^2 # Estimacio sigma2: $\sigma^2 = 2225.75$
# La regressio es significativa perque l estadistic F es significatiu, ja que F= sg$fstatistic[1] i el seu pvalor es inferior 
# a 0.05. Aixo significa que la variable X te una influencia significativa (relevant o important) en Y.


# (D) INVESTIGUEU LA VALIDESA DEL MODEL LINEAL. HI HA ALGUN RESIDU QUE POGUEM CLASSIFICAR D ATIPIC? FEU UNA DEFINICIO DEL CONCEPTE
# DADA ATIPICA (OUTLIER) EN AQUEST CONTEXT. FEU UN GRAFIC ADIENT. 

plot(g, which=1) # Residuals vs Fitted
# BIEN: Linia horitzontal y punts aleatoris (errors aleatoris), sembla que el l estructura del model es correcte perque es compleix
# la primera condicio de Markov ($E(e_i)=0$).
# MAL: Es fa una curva, per la qual cosa els errors no son aleatoris ($E(e_i) \neq 0$) i per tant l estructura del model falla.

plot(g, which=2) # Normal Q-Q plot
# BIEN: els punts es troben majoritariament sobre la recta.
# MAL:  observa com els punts no es troben sobre la recta.

plot(g, which=3) # Scale-Location
# BIEN: Esta be ja que s'observa una recta horitzontal (pe. si hi ha alguna curva petita dir que es perq hi ha poques dades)
# MAL: No s'observa una curva tan marcada com en el primer grafic, pero lo ideal es una linia horitzontal recta.

plot(g, which=4) # Ens pot preocupar perque s observa una distancia de Cook elevada en alguns punts.

#Resultat final: cal esmenar els residus 2, 5 y 13 com a atipics, son molt grans


# (E) DONEU ELS INTERVALS DE CONFIANCA AL 90% DE $\beta_0$, $\beta_1$ (O betaNomVariable) i de  $\sigma^2$. COM INTERPRETEM $\beta_0$, 
# $\beta_1$ I $\sigma^2$? EN QUINES UNITATS ESTAN ELS COEFICIENTS?
confint(g, level=0.90)

#Per a la variancia del model, cal ferho a ma:
RSS <- sum(sg$residuals^2)
c(RSS/qchisq(0.95, ss$df[2]), RSS/qchisq(0.05, ss$df[2]))

# Com s'interpreten:
#Beta0: es el intersect, per tant es el valor del model en el origen de coordenades, o sigui, quan totes les variables regresores valen 0, el numero de peixos d'aquesta especie seria beta0. 
#BetaTemp: per cada increment d'un grau de temperatura, quants peixos tindria de mes? Increment o decrement per unitat de temperatursa, es la relacio entre la temperatura i el num de peixos.
#Sigma2: es la variancia del error, la variabilitat del error en aquest model.


# (F) CONTRASTEU LA HIPOTESIS $H_0: \beta_Temp = 3$. CONTRASTEU TAMBE $H_0: \beta_DO2 = \beta_NO3$. UTILITZEU alpha=0.05.
# Si tenim l interval de confianca de $\beta_Temp$, si el tres cau a dintre, admetrem $H_0$.

# Quan nomes tenim una fpe, tenim dues opcions:
# Opcio 1: tstudent.
t <-  (coefficients(g)[[2]] - 3)/sqrt(vcov(g)[2,2])
p_value <- 2*pt(t, n-r, lower.tail = TRUE)

#Opcio 2: contrast de models.
g <- lm(Longnose ~ Temp, data=Data)
g0 <- lm(Longnose ~ offset(Temp), data=Data)
anova(g0, g)
# p_value = 0.6262, Acceptem H0.

# Quan tenim dos equacions: contrast de models.
g0 <- lm(Longnose ~ Acerage + Maxdepth + SO4 + Temp + I(DO2 + NO3), data= Data)
g1 <- lm(Longnose ~ Acerage + DO2 + Maxdepth  + NO3 + SO4 + Temp,  data= Data)
anova(g0, g1) # g0 es el model de la Ho y g1, el model general.
# p_value = 0.8141, acceptem H0.



# (G) FEU UNA PREDICCIO DE NUM DE PEIXOS SI AUGMENTEM UN GRAU LA TEMPERATURA. FEU UNA PREDICIO CONCRETA AMB IC al 90%.
predict(g, newdata= data.frame(Temp <- Temp+1), interval = "prediction", level = 0.9)
# Si ens demanen una prediccio concreta: interval ="prediction", si volen la prediccio mitjana: interval ="confidence".

#Tambe:
Data$Temp <- Data$Temp + 1
p <-predict(g, Data, interval = "prediction", level = 0.9)
c(mean(p[, 2]), mean(p[, 3]))

# (H) FEU UN TEST DE PERMUTACIONS DE LA HIPOTESI DE NO SIGNIFICACIO DE LA REGRESSIO (TOTS ELS COEFICIENTS SON 0 EXCEPTE EL TERME D INTERCEPCIO)
lmod <- lm(Longnose ~ Acerage + DO2 + Maxdepth  + NO3 + SO4 + Temp,  data= Data)
lms <- summary(lmod)
nreps <- 1000
set.seed(123)
fstats <- numeric(nreps)

for (i in 1:nreps) {
  lmods <- lm(sample(Longnose) ~ 0+0+0+0+0+0+ I(Acerage + DO2 + Maxdepth  + NO3 + SO4 + Temp), Data)
  fstats[i] <- summary(lmods)$fstat[1]
}

mean(fstats)
summary(fstats)

#########################
## Problema 2. OPCIO 2 ##
#########################

# (A2) FEU UN GRAFIC DE DISPERSIO DE LES VARIABLES I UNA PRIMERA REGRESSIO LINEAL PER IDENTIFICAR RESIDUS MOLT GRANS.L ANALISI DE RESIDUS US POT AJUDAR.
g <- lm(Longnose ~ Temp, data=Data)
with(Data, plot(Longnose, Temp))
abline(g)

#Analisi grafica dels residus:
plot(g, which=1)
# S observen 2 residus molt grans, que es corresponen amb BIG_ELK_CR i MEADOW_BR.

# Si diuen d eliminar els valors anomals de la base de dades:
which(rownames(NOM_VARIABLE) == "BIG_ELK_CR") # [1] 2
which(rownames(NOM_VARIABLE) == "MEADOW_BR")  # [1] 61

Data0<- Data[-c(2,61), ]

# (B2) OBTENIU L ESTIMACIO DELS PARAMETRES DEL MODEL ($\beta_o, \beta_1, \sigma^2$) I CALCULEU EL COEFICIENT DE DETERMINACIO.
# ES SIGNIFICATIVA LA REGRESSIO?

# Estimacio dels parametres:
g <- lm(Longnose ~ Temp, data=Data0) # En el cas que s'hagin eliminat dades.
sg <- summary(g)
coef(g)      # Estimacio parametres
(sg$sigma)^2 # Estimacio sigma2
# La regressio es significativa ja que l estadistic F es significatiu, ja que F= sg$fstatistic[1] i el seu pvalor es inferior a 0.05.


# (C2) HI HA ALGUNA RAO PER DUBTAR DE LA NORMALITAT DELS RESIDUS? LA NORMALITAT GARANTEIX LA MINIMA VARIANCIA DELS ESTIMADORS LINEALS
# EN UN MODEL LINEAL COM AQUEST? EXPLICA BREUMENT COM S ACONSEGUEIX LA MINIMA VARIANCIA DE LES ESTIMACIONS EN ELS MODELS LINEALS.

# La normalitat dels residus es pot comprovar amb un grafic.
plot(g, which =2)
# A simple vista tot sembla normal, farem un test per confirmar-ho:
shapiro.test(residuals(g))
# Com p valor > 0.05, no hi ha cap rao per dubtar de la normalitat. La normalitat no és cap garantia de mínima variància. El teorema de 
# Gauss-Markov diu que el métode dels mínims quadrats amb les tres hipòtesis de Gauss-Markov (no cal la normalitat) és el que garantitza 
# la mínima variància dels estimadors dels paràmetres o f.p.e.


# (G2) ARA CONSIDEREM DUES RECTES DE REGRESSIO SEPARADES PER TEMPERATURA(MAJOR Q 20 I MENOR O IGUAL Q 20) I COMPAREULES (TEST DE 
# PARALELISME I DE COINCIDENCIA). FEU ELS CONTRASTOS I ELS GRAFICS ADIENTS.
r1 <- lm(Longnose ~ Temp, data= Data, subset = Temp>20) # Si cal separarho segons una variable categorica: subset=type=="Ordinari/No ordinary"
r2 <- lm(Longnose ~ Temp, data= Data, subset = Temp<=20)

# Feiem el grafic:
idx <- Data$Temp > 20
plot(Data$Longnose, Data$Temp, pch=ifelse(idx, 1, 20), xlab="Temperatura", ylab="Longnose")
abline(r1)
abline(r2, lty=2)


