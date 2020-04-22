# EXAMEN FINAL GENER 2013

# EXERCICI 2

library(faraway)
library(car)

# Dades
data(teengamb)

# 1) Model de regressio multiple
g <- lm(gamble ~ ., data = teengamb)

summary(g)

# Estimacio de la variància de l'error
summary(g)$sigma^2
  # 514.8516

# Coef de determinacio ajustat
summary(g)$adj.r.squared
  # 0.4816495

# 2) Significació de la regressió
summary(g)
  # F-statistic: 11.69 on 4 and 42 DF,  p-value: 1.815e-06 
  # La regressió és significativa
  # La Hipòtesis es:
  # beta_sex = beta_status = beta_income = beta_verbal = 0
  # Que sigui significativa vol dir que és útil, que té sentit, que les regressores
  # afecten a la resposta i ajuden a predir-la


# 3) Diagnosi del model

# a. Homoscedasticitat

# - Residuals vs. Fitted
# - Residuals (en valor absolut) vs. Fitted
# - "Test" de Faraway
# - John Fox: test i gràfic

# - Residuals vs. Fitted (marcant el 0) i Residuals (en valor absolut) vs. Fitted
par(mfrow = c(1, 2))
plot(g, which = 1)
plot(fitted(g),abs(residuals(g)),xlab="Fitted",ylab="|Residuals|")
par(mfrow = c(1, 1))

# Hi ha alguns punts crítics
# Línia vermella una mica quadràtica
# punts amb patró estrany... agrupats


# - "Test" per comprovar si la variància és constant
# Regressió entre el valor absolut dels residus i els valors ajustats
summary(lm(abs(residuals(g)) ~ fitted(g)))
# La regressió és significativa, el que implica que seria una recta amb pendent != 0
# No són constants
# Podríem dibuixar si volguéssim la recta que ens ha sortit.
abline(summary(lm(abs(residuals(g)) ~ fitted(g))))

# - John Fox.
# non-constant error variance test
ncvTest(g)
# p = 8.284638e-07  -> Rebutgem homoscedasticitat

# plot studentized residuals vs. fitted values
spreadLevelPlot(g)
# Dibuixa una recta de regressió que indica el nivell de constància
# No és horitzontal sinó que té pendent -> els residus van creixent

# b. Normalitat

# Gràfic: QQ-plot
plot(g, which = 2)
  # Hi ha 3 o 4 punts molt allunyats

# Test formals
shapiro.test(residuals(g))
# p-value = 8.16e-05 -> Rebutgem normalitat

# c. Leverage

leverage <- hatvalues(g)

# criteri: h_ii > 2(k+1)/n
k <- 4 # Es tenen 4 vars
which(leverage > 2*(k+1)/47)
# Aquests punts són POTENCIALMENT INFLUENTS: 31 33 35 42

# d. Outliers

# A. Criteri: |t_i| > 2

t <-rstudent(g)
which(abs(t)>2)
  # 24 36 39

plot(1:47, t, type = 'h')
abline(h = c(2, 0, -2))

# Gràficament
Boxplot(t)
# 24 36 39

# B. Ajust del criteri amb Bonferroni
outlierTest(g)
  # 24

# c. qqPlot d'ajust als quantils teòrics d'una t-Student
qqPlot(g, main="QQ Plot", id.n = 3)
# 24 36 39

# e. Punts influents

# Influència real:
  # - sobre els coeficients:
    # - DISTÀNCIA DE COOK
    # - DFBETAS
  # - sobre les prediccions:
    # - DISTÀNCIA DE COOK
    # - DFFITS

# - DISTÀNCIA DE COOK
C <- cooks.distance(g)
# criteri: com que no és clar, el millor és dibuixar-ho.
plot(1:47, C, type = 'h')
# Es veuen, si tallem al 0.10, com a mínim 2 valors.

plot(g, which = 4)
# 5 24 39

# - DFBETAS
dfbetas(g)

# limit 2/sqrt(n)
k <- 2/sqrt(47)

which(abs((dfbetas(g))[,1])>k)  # 24 35
which(abs((dfbetas(g))[,2])>k)  # 5 24
which(abs((dfbetas(g))[,3])>k)  # 5 24
which(abs((dfbetas(g))[,4])>k)  # 24 39
which(abs((dfbetas(g))[,5])>k)  # 24 27 35

# - DFFITS

dffits(g)

# limit 2*sqrt((k + 1)/n)
k <- 2*sqrt((4 + 1)/47)
which(abs((dffits(g)))>k) 
# 24 39


# f. Problemes de multicolinealitat

# 1. Detecció mirant la correlació entre les vars regressores
round(cor(teengamb[,1:4]),3)
# Hi ha un 0.53 entre status i verbal, que és el més alt

# 2. Detecció pels FIVS
vif(g)
# Són petits, molt menors que 10

# 3. Detecció mirant els VAP's i calculant el número de condició (kappa)
X <- model.matrix(g)[,-1]
e <- eigen(t(X) %*% X)
vaps <- e$val
kappa <- sqrt(vaps[1]/vaps[4])
# El kappa = 105.3152 --> MOLT SUPERIOR A 30


# 4) Model sense el punt 24
# Punt 24: es outlier i es infuent
g2 <- lm(gamble ~ ., data = teengamb[-24,])
summary(g2)
  # summary(g2)$sigma^2 = 280.1236
  # Disminueix molt
  # L'ajust augmenta una mica
plot(g2, which = 1)

  # Mirem la normalitat
  shapiro.test(residuals(g2))
  # p-value = 0.4603 -> ACCEPTEM NORMALITAT

  # Mirem homoscedasticitat
  # non-constant error variance test
  ncvTest(g2)
  # Seguim rebutjant, pero ara el p-valor p = 0.0004163668 és mes gran

# 5) IC
predict(g, newdata = data.frame(sex = 0, status = 60, income = 10, verbal = 11), interval = 'prediction')
#        fit      lwr      upr
#   42.75504 -7.94659 93.45668

predict(g2, newdata = data.frame(sex = 0, status = 60, income = 10, verbal = 11), interval = 'prediction')
#      fit      lwr      upr
# 41.55484 4.126987 78.98269

# 6) ANCOVA
teengamb2 <- teengamb
teengamb2$sex <- as.factor(teengamb2$sex)
levels(teengamb2$sex) <- c("Noi", "Noia")
g_anc <- lm(gamble ~ income * sex, data=teengamb2)
summary(g_anc)
  # La interaccio es significativa
  # beta3 != 0
  # Les rectes no son paral·les
  # En funcio del sex, la recta és una o una altra

# Gràfic amb les rectes de cadascun dels grups
plot(gamble ~ income, pch=ifelse(sex=="Noi",1,16), data=teengamb2)
legend("topright", levels(teengamb2$sex), pch=c(1,16))
# Nois
abline(-2.6596, 6.5181)
# Noies
abline(-2.6596 + 5.7996, 6.5181 - 6.3432, lty=2)
