# EXAMEN FINAL GENER 2012

# EXERCICI 3

library(faraway)

# Dades
data(stackloss)

# a) OLS
g_ols <- lm(stack.loss ~ ., stackloss)

summary(g_ols)
  # Significació global regressió bona
  # R^2 bo

# Cal fer un anàlisi dels residus per veure si hi ha problemes
plot(g_ols)
  # Al residuals vs. Fitted hi ha un patró estrany.
  # Forma residus concentrada i línia vermella no recta

# b) LAD REGRESSION
library(quantreg)
g_lad <- rq(stack.loss ~ ., data = stackloss)

summary(g_lad)
  # Canvia bastant el valor dels coeficients per a:
  # - Water.Temp -> passa de 1.2953 a 0.57391 (es redueix a la meitat)
  # - Acid.Conc -> passa de --0.1521 a -0.06087 (es redueix a la més de la meitat)
  # Ara tots són significatius mentre que amb OLS Acid.Conc no ho era.

# c) HUBER
library(MASS)
g_huber <- rlm(stack.loss ~ ., stackloss)

summary(g_huber)
# Lleugera modificació dels valors dels coeficients
# Mateixes conclusions respecte a la significació de les vars

# d) LTS
g_lts <- ltsreg(stack.loss ~ ., stackloss, nsamp = "exact")
coef(g_lts)

# Amb LTS es troben diferències MOLT substancials en els coeficients.
# - Acid.Conc. = 0!
# (Intercept)      Air.Flow    Water.Temp    Acid.Conc. 
# -3.580556e+01  7.500000e-01  3.333333e-01  3.489094e-17

# Gràfic de dispersió   (NO té sentit, només per mirar)
plot(stack.loss ~ Air.Flow, stackloss)
plot(stack.loss ~ Water.Temp, stackloss)
plot(stack.loss ~ Acid.Conc., stackloss)


# Detecció de punts influents i outliers sobre el model OLS

library(car)
library(lmtest)

# 1. OUTLIERS
#############

# A. Criteri: |t_i| > 2

t <- rstudent(g_ols) # Residus estudentitzats externament

# 1. Càlcul
which(abs(t)>2)
  # Marca el 4 i el 21

# 2. Gràficament
Boxplot(t)
  # Marca només el 21

plot(1:21, t, type = 'h')
abline(h = c(2, 0, -2))
  # Hi ha dos punts que talen la frontera -> 4 i 21

# B. Ajust del criteri amb Bonferroni
outlierTest(g_ols)
  # Ens marca el 21

# c. qqPlot d'ajust als quantils teòrics d'una t-Student
qqPlot(g_ols, main="QQ Plot", id.n = 2)
  # Ens marca el 21 i el 4


# 2. INFLUÈNCIA
###############

# Influencia potencial: LEVERAGE
# Influència real:
  # - sobre els coeficients:
    # - DISTÀNCIA DE COOK
    # - DFBETAS
  # - sobre les prediccions:
    # - DISTÀNCIA DE COOK
    # - DFFITS

# LEVERAGE  (Influència potencial)

leverage <- hatvalues(g_ols)
# criteri: h_ii > 2(k+1)/n
k <- 3 # Es tenen 3 vars regressores
which(leverage > 2*(k+1)/21)
# Marca el 17: Aquests punt és POTENCIALMENT INFLUENT


# DISTÀNCIA DE COOK  (Influència real)

C <- cooks.distance(g_ols)
which(C > 0.1)
# criteri: com que no és clar, el millor és dibuixar-ho.
plot(1:21, C, type = 'h')
# Es veuen, si tallem al 0.10, 4 valors.
# 1  3  4 21

plot(g_ols, which = 4)
# Un cutoff (lloc per tallar) = 4/(n-k-1)
# 1 4 21

# Gràfic: Influence Plot
influencePlot(g_ols, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
  # 4 17 21

# DFBETAS   (Influència real)

dfbetas(g_ols)

which(abs((dfbetas(g_ols))[,1])>1)
  # Cap
which(abs((dfbetas(g_ols))[,2])>1)
  # 21
which(abs((dfbetas(g_ols))[,3])>1)
  # 21
which(abs((dfbetas(g_ols))[,4])>1) 
  # Cap

# DFFITS   (Influència real sobre les projeccions)

dffits(g_ols)
which(abs((dffits(g_ols)))>1)
  # 21

# Posant en comú els punts outliers i influents, s'eliminarien el 4 i el 21

# Eliminem els punts trobats
stackloss2 <- stackloss[-c(4, 21),]

# Recalculem el model OLS
g_ols2 <- lm(stack.loss ~ ., stackloss2)
summary(g_ols2)
  # Regressió significativa
# vs.
summary(g_ols)
  # Es millora l'ajust
  # Es redueix el RSE
  # canvia molt el coef de Water.Temp