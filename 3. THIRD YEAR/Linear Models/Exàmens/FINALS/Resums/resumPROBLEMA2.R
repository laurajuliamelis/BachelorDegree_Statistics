### PROBLEMA 2 ###
library(faraway) #--> data(nom de les dades)
library(car)
library(MASS)
library(leaps)
datos<-read.table("CyclingPower.csv",sep=";",dec=",",header=TRUE)
head(datos)
attach(datos)

###REGRESSIO LINEAL MULTIPLE SOBRE UNA VARIABLE EN CONCRET DE LES 
###ALTRES VARIABLES

g<-lm(variablconcreta ~ .,data=data) #el punto es para decir que es
#de todas las demas varibles
summary(g)


### ESTIMACI? DE LA VARIANCIA DE L'ERROR ###

summary(g)$sigma^2


### COEFICIENT DE DETERMIANCI? AJUSTAT (adjusted R-squared) ###

summary(g)$adj.r.squared


### SIGNIFICACI? DE LA REGRESSI? ###

#Que la regressi? sigui significativa vol dir que ?s ?til, que t? 
#sentit calcular-la, que les regressores afecten a la resposta i 
#ajuden a predir-la
#la hipotesis ?s: Beta_i=0 --> totes les variables independents=0

#al summary mirem el F-statistic i el p-value
#F-statistic:
#p-value: <0,05 --> significativa


### DIAGNOSI DEL MODEL ###
 

# a. HOMOCEDASTICITAT #

plot(g, which = 1)
summary(lm(abs(residuals(g)) ~ fitted(g)))
#p-valor > 0.05 --> acceptem homocedesticitat

# - Residuals vs. Fitted
# - Residuals (en valor absolut) vs. Fitted
# - "Test" de Faraway
# - John Fox: test i gr?fic

# - Residuals vs. Fitted (marcant el 0) i Residuals (en valor absolut) vs. Fitted
par(mfrow = c(1, 2))
plot(g, which = 1)
plot(fitted(g),abs(residuals(g)),xlab="Fitted",ylab="|Residuals|")
par(mfrow = c(1, 1))

# Hi ha alguns punts cr?tics
# L?nia vermella una mica quadr?tica
# punts amb patr? estrany... agrupats

# - "Test" per comprovar si la vari?ncia ?s constant
# Regressi? entre el valor absolut dels residus i els valors ajustats
summary(lm(abs(residuals(g)) ~ fitted(g)))
# La regressi? ?s significativa, el que implica que seria una recta amb pendent != 0
# Els errors no s?n constants
# Dibuixem la recta
plot(fitted(g),abs(residuals(g)),xlab="Fitted",ylab="|Residuals|")
abline(summary(lm(abs(residuals(g)) ~ fitted(g))))

# - John Fox.
# non-constant error variance test
ncvTest(g)
# p < 0.05  -> Rebutgem homoscedasticitat

# plot studentized residuals vs. fitted values
spreadLevelPlot(g)
# Dibuixa una recta de regressi? que indica el nivell de const?ncia
# Si no ?s horitzontal, sin? que t? pendent -> els residus van creixent

# Com que es rebutja la homoscedasticitat i es violen les suposicions
# de Gauss-Markov, es podria fer alguna transformaci? de variables
# per intentar solucionar aquest problema.


# b. HIP?TESI DE NORMALITAT #

# Gr?fic: QQ-plot
plot(g, which = 2)


# Test formal
shapiro.test(residuals(g))
# p-value <0.05 -> Rebutgem normalitat


# c. PUNTS AMB INFLUENCIA POTENCIAL (leverage) #
leverage <- hatvalues(g)

# criteri: h_ii > 2(k+1)/n on k = nombre de variables
which(leverage > 2(k+1)/n)
# Aquests punts s?n POTENCIALMENT INFLUENTS


# d. OUTLIERS #

  # A. Criteri: |t_i| > 2

t <-rstudent(g)
which(abs(t)>2)

plot(1:n, t, type = 'h')
abline(h = c(2, 0, -2))

# Gr?ficament
Boxplot(t)


  # B. Ajust del criteri amb Bonferroni
outlierTest(g)


  # c. qqPlot d'ajust als quantils te?rics d'una t-Student
qqPlot(g, main="QQ Plot", id.n = 3)


# e. PUNTS INFLUENTS #

# Influ?ncia real:
  # - sobre els coeficients:
    # - DIST?NCIA DE COOK
    # - DFBETAS
  # - sobre les prediccions:
    # - DIST?NCIA DE COOK
    # - DFFITS

# - DIST?NCIA DE COOK

  C <- cooks.distance(g)
  # criteri: com que no ?s clar, el millor ?s dibuixar-ho.
  n<-47
  k<-4
  ptall<-4/(n-k-2) #creem el punt de tall perque sí!
  
  # criteri: com que no ?s clar, el millor ?s dibuixar-ho.
  plot(g, which = 4)
  abline(h=ptall)


# - DFBETAS
  
  dfbetas(g)
  # limit 2/sqrt(n)
  l <- 2/sqrt(n)

  which(abs((dfbetas(g))[,1])>l)  
  which(abs((dfbetas(g))[,2])>l)  
  which(abs((dfbetas(g))[,3])>l)  
  which(abs((dfbetas(g))[,4])>l)  
  which(abs((dfbetas(g))[,5])>l)  

  
# - DFFITS
  
  dffits(g)
  # limit 2*sqrt((k + 1)/n)
  l <- 2*sqrt((k + 1)/n)
  which(abs((dffits(g)))>l) 
  


# f. PROBLEMES DE MULTICOLINEALITAT #
  
  # 1. Detecci? mirant la correlaci? entre les vars regressores
    round(cor(teengamb[,1:k]),3) #k=nombre de vars
    # mirem el m?s alt
  
  # 2. Detecci? pels FIVS
      vif(g)
      # hem de mirar si son menors de 10
      

      
### ESTUDI DEL MODEL DESPRES D'ELIMINAR UN PUNT A LES DADES ###
      
      # Primer diem tot el que sabem del punt eliminat.
      g2 <- lm(gamble ~ ., data = teengamb[-24,]) #creem un nou model eliminant el punt
      summary(g2)
      #mirem possibles canvis com var.est.error o l'ajust... 
     
      
      # Residus
      plot(g2, which = 1)
      
      # Mirem la normalitat
      shapiro.test(residuals(g2))
      # p-value > 0.05 -> ACCEPTEM NORMALITAT
      
      # Mirem homoscedasticitat
      # non-constant error variance test
      ncvTest(g2)

      
### PREDICCI? EN FORMA D'IC DE NOVES DADES QUE ENS DONIN ###
      
#A.
    predict(g2, newdata = data.frame(sex = 0, status = 60, income = 10, verbal = 11), interval = 'prediction')

#B.
    confint(g, level=0.95) #per defecte el fa de 0.95, si volem nomes d'una variable
    #confint(model,'variableselec')

### ANCOVA ###  ?????????????????

  teengamb2 <- teengamb
  teengamb2$sex <- as.factor(teengamb2$sex)
  levels(teengamb2$sex) <- c("Noi", "Noia")
  g_anc <- lm(gamble ~ income * sex, data=teengamb2)
  summary(g_anc)
  # La interaccio es significativa
  # beta3 != 0 ??
  # Les rectes no son paral?les
  # En funcio del sex, la recta ?s una o una altra
  
  # Gr?fic amb les rectes de cadascun dels grups
  plot(gamble ~ income, pch=ifelse(sex=="Noi",1,16), data=teengamb2)
  legend("topright", levels(teengamb2$sex), pch=c(1,16))
  # Nois
  abline(coefficients(g_anc)[1], coefficients(g_anc)[2])
  # Noies
  abline(coefficients(g_anc)[1]+coefficients(g_anc)[3], coefficients(g_anc)[2]+coefficients(g_anc)[4], lty=2)
  
        



