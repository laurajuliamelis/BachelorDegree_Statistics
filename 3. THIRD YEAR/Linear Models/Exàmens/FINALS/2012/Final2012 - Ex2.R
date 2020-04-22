# EXAMEN FINAL GENER 2012

# EXERCICI 2

library(faraway)

# Dades
data(sat)

# a) Resum gràfic i numèric
summary(sat)


# b) Model i contrastos
g1 <- lm(total ~ expend + ratio + salary, sat)
plot(g1)

summary(g1)
  # La regressió és significativa
    # F-statistic: 4.066 on 3 and 46 DF,  p-value: 0.01209
  # beta_Salary = 0
    # salary        -8.823      4.697  -1.878   0.0667
    # depen del nivell de significacio es diu que és significativa o que no
  # Predictora amb efecte sobre la resposta?
    # NO. Només salary i depen del alpha escollit


# c) Model amb takers
g2 <- lm(total ~ expend + ratio + salary + takers, sat)

summary(g2)
  # S'accepta beta_salary = 0

anova(g1, g2)
  # Acceptem el model 2
  # És equivalent a beta_takers = 0


# d) IC 95% per a beta_salary en el model 2
confint(g2, "salary")
  #    2.5 %   97.5 %
  # -3.170247 6.446081


# e) Diagnosi del model

# 1. Homoscedasticitat

# - Residuals vs. Fitted
# - Residuals (en valor absolut) vs. Fitted
# - "Test" de Faraway
# - John Fox: test i gràfic

# - Residuals vs. Fitted (marcant el 0)
plot(g2, which = 1)
  # Residus en forma de paràbola
  # Línia vermella no plana sinó quadràtica
  # Marca 3 punts però respexte a varis criteris

# - Residuals (en valor absolut) vs. Fitted
plot(fitted(g2),abs(residuals(g2)),xlab="Fitted",ylab="|Residuals|")
  # Sembla que hi ha dos grups

# - "Test" per comprovar si la variància és constant
# Regressió entre el valor absolut dels residus i els valors ajustats
summary(lm(abs(residuals(g2)) ~ fitted(g2)))
# La regressió no és significativa, el que implica que seria una recta plana
# Acceptem la hipòtesi de que el pendent és 0.
# Podríem dibuixar si volguéssim la recta que ens ha sortit.
abline(summary(lm(abs(residuals(g2)) ~ fitted(g2))))

# - John Fox.
# non-constant error variance test
ncvTest(g2)
  # p = 0.4037221 -> Acceptem homoscedasticitat

# plot studentized residuals vs. fitted values
spreadLevelPlot(g2)
# Dibuixa una recta de regressió que indica el nivell de constància

# 2. Normalitat

# Gràfic: QQ-plot
plot(g2, which = 2)

# Test formals
shapiro.test(residuals(g2))
  # p-value = 0.4304 -> Acceptem normalitat

# 3. Leverage

leverage <- hatvalues(g2)

# criteri: h_ii > 2(k+1)/n
k <- 4 # Es tenen 4 vars
which(leverage > 2*(k+1)/50)
# Aquests punts són POTENCIALMENT INFLUENTS
#  California Connecticut  New Jersey        Utah 
#           5           7          30          44

# 4. Outliers

# A. Criteri: |t_i| > 2

t <-rstudent(g2)
which(abs(t)>2)
  # New Hampshire  North Dakota          Utah West Virginia 
  #            29            34            44            48
plot(1:50, t, type = 'h')
abline(h = c(2, 0, -2))

# Gràficament
Boxplot(t)
  # 44 i 48

# B. Ajust del criteri amb Bonferroni
outlierTest(g2)
# West Virginia -> 48

# c. qqPlot d'ajust als quantils teòrics d'una t-Student
qqPlot(g2, main="QQ Plot", id.n = 4)
# "West Virginia" "Utah"          "North Dakota"  "New Hampshire"

# 5. Punts influents

# Influència real:
  # - sobre els coeficients:
    # - DISTÀNCIA DE COOK
    # - DFBETAS
  # - sobre les prediccions:
    # - DISTÀNCIA DE COOK
    # - DFFITS

# - DISTÀNCIA DE COOK
C <- cooks.distance(g2)
which(C > 0.1)

# criteri: com que no és clar, el millor és dibuixar-ho.
plot(1:50, C, type = 'h')
# Es veuen, si tallem al 0.10, com a mínim 2 valors.

plot(g2, which = 4)
# "West Virginia" "Utah"  "New Hampshire"


# - DFBETAS
dfbetas(g2)
which(abs((dfbetas(g2))[,1])>1)
which(abs((dfbetas(g2))[,2])>1)
which(abs((dfbetas(g2))[,3])>1)  # 44
which(abs((dfbetas(g2))[,4])>1)
which(abs((dfbetas(g2))[,5])>1)
 
# - DFFITS

dffits(g2)
which(abs((dffits(g2)))>1) 
# 44.


# 6. Problemes de colinealitat

# 1. Detecció mirant la correlació entre les vars regressores
round(cor(sat[,1:4]),3)
  # Hi ha un 0.87 entre expend i salary

# 2. Detecció pels FIVS
vif(g2)
  # Expend i salary gairebé 10 però no arriba
  # mitjana(vifs) = 5.72 << 10
sum(vif(g2))/4

# 3. Detecció mirant els VAP's i calculant el número de condició (kappa)
X <- model.matrix(g2)[,-1]
e <- eigen(t(X) %*% X)
vaps <- e$val
kappa <- sqrt(vaps[1]/vaps[4])
# El kappa = 113.7506 --> MOLT SUPERIOR A 30

# Potser sobra o expend o salary
g3 <- step(g2, direction = 'both')
  # ratio + salary + takers
  # Hem tret expend -> correlacionada amb salary