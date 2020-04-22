# EXAMEN DE MODELS LINEALS
# ------------------------

# FINAL 1-feb-2013

# LAURA RIBA ARCHILLA

# PROBLEMES 2, 3 i 4

library(faraway)
library(car)
library(MASS)
library(leaps)

data(teengamb)

####################################################################

# PROBLEMA 2


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
# Els errors no són constants
# Dibuixem la recta
plot(fitted(g),abs(residuals(g)),xlab="Fitted",ylab="|Residuals|")
abline(summary(lm(abs(residuals(g)) ~ fitted(g))))

# - John Fox.
# non-constant error variance test
ncvTest(g)
# p = 8.284638e-07  -> Rebutgem homoscedasticitat

# plot studentized residuals vs. fitted values
spreadLevelPlot(g)
# Dibuixa una recta de regressió que indica el nivell de constància
# No és horitzontal sinó que té pendent -> els residus van creixent

# Com que es rebutja la homoscedasticitat i es violen les suposicions
# de Gauss-Markov, es podria fer alguna transformació de variables
# per intentar solucionar aquest problema.

# b. Normalitat

# Gràfic: QQ-plot
plot(g, which = 2)
  # Hi ha 3 o 4 punts molt allunyats

# Test formal
shapiro.test(residuals(g))
# p-value = 8.16e-05 -> Rebutgem normalitat

# c. Leverage
leverage <- hatvalues(g)

# criteri: h_ii > 2(k+1)/n on k = 4 ja que es tenen 4 vars
which(leverage > 2*(4 +1)/47)
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
plot(g, which = 4)
# 5 24 39

# - DFBETAS
dfbetas(g)

# limit 2/sqrt(n)
l <- 2/sqrt(47)

which(abs((dfbetas(g))[,1])>l)  # 24 35
which(abs((dfbetas(g))[,2])>l)  # 5 24
which(abs((dfbetas(g))[,3])>l)  # 5 24
which(abs((dfbetas(g))[,4])>l)  # 24 39
which(abs((dfbetas(g))[,5])>l)  # 24 27 35

# - DFFITS

dffits(g)

# limit 2*sqrt((k + 1)/n)
l <- 2*sqrt((4 + 1)/47)
which(abs((dffits(g)))>l) 
# 24 39

# f. Problemes de multicolinealitat

# 1. Detecció mirant la correlació entre les vars regressores
round(cor(teengamb[,1:4]),3)
# Hi ha un 0.53 entre status i verbal, que és el més alt

# 2. Detecció pels FIVS
vif(g)
# Són petits, molt menors que 10


# 4) Model sense el punt 24
# Punt 24: es outlier i es infuent
g2 <- lm(gamble ~ ., data = teengamb[-24,])
summary(g2)
  # summary(g2)$sigma^2 = 280.1236 -> Disminueix molt
  # L'ajust augmenta una mica

# Residus
plot(g2, which = 1)

# Mirem la normalitat
shapiro.test(residuals(g2))
  # p-value = 0.4603 -> ACCEPTEM NORMALITAT

# Mirem homoscedasticitat
# non-constant error variance test
  ncvTest(g2)
# Seguim rebutjant, pero ara el p-valor p = 0.0004163668 és mes gran

# 5) IC
# Utilitzem el model sense l'observació 24
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
abline(coefficients(g_anc)[1], coefficients(g_anc)[2])
# Noies
abline(coefficients(g_anc)[1]+coefficients(g_anc)[3], coefficients(g_anc)[2]+coefficients(g_anc)[4], lty=2)

#######################################################################

# PROBLEMA 3

# Apartat 1. Seleccio de variables

# 1) Model de regressio OLS
g <- lm(gamble ~ ., data = teengamb)
summary(g)


# 2) Model seleccionat per AIC
g_AIC <- step(g, direction = "both")
  # gamble ~ sex + income + verbal
  # Hem tret status
summary(g_AIC)


# 3) Model seleccionat per Cp de Mallows
# install.packages('leaps')
# library(leaps)
X <- as.matrix(teengamb[,-5])
y <- teengamb[,5]

g_Cp <- leaps(X, y, method = "Cp", names = c("sex", "status", "income", "verbal"))

# Si P = 3 (2 regressores més intercept)
g_Cp$which[g_Cp$Cp==min(g_Cp$Cp[g_Cp$size==3])]
  # sex + income

# Si P = 4 (3 regressores més intercept)
g_Cp$which[g_Cp$Cp==min(g_Cp$Cp[g_Cp$size==4])]
  # sex + income + verbal

# Gràficament
plot(g_Cp$size,g_Cp$Cp, ylim = c(0,6),xlab="P",ylab="C_P")
abline(0, 1)

  # El model que més s'apropa a Cp=P és el millor model
  # amb P = 3 (2 regressores més l'intercept)
  # Per tant, escolliriem sex + income

g_Mallows <- lm(gamble ~ sex + income, data = teengamb)

# Model seleccionat per FORWARD STEPWISE
# Fórmula del model complet
gc.formula <- formula(g)
# Model simple
g0 <- lm(gamble ~ 1, data = teengamb)
# Forward stepwise
model <- g0
add1(model, scope = gc.formula, test="F")
# Haig d'afegir aquella var amb la F més gran --> income
model <- update(g0, ~ . + income)
# Al model g0 li sumo income
add1(model, scope = gc.formula, test="F")
# Haig d'afegir aquella var amb la F més gran --> sex
model <- update(model, ~ . + sex)
# Al model "model" li sumo sex
add1(model, scope = gc.formula, test="F")
# Ja no tinc que afegir-ne cap
  # gamble ~ income + sex


# Coeficients de determinació dels models

summary(g)$adj.r.squared          # 0.4816
summary(g_AIC)$adj.r.squared      # 0.4933
summary(g_Mallows)$adj.r.squared  # 0.4787

# L'ajust no millora, pero aconseguim el mateix ajust amb menys variables

summary(g)$sigma        # 22.69
summary(g_AIC)$sigma     # 22.43416
summary(g_Mallows)$sigma  # 22.75428

# Intervals de confiança
confint(g, "income")
confint(g_AIC, "income")
confint(g_Mallows, "income")


# Apartat 2. Models robustos

# 1) Model de regressio multiple OLS sense el punt 24
g_ols <- lm(gamble ~ ., data = teengamb[-24,])
summary(g_ols)

# 2) HUBER
g_huber <- rlm(gamble ~ ., data = teengamb)

summary(g_huber)
# Modificació d'alguns valors dels coeficients
  # Intercept
  # Verbal
  # Mateixes conclusions respecte a la significació de les vars

plot(g_huber)

# 3) LTS
g_lts <- ltsreg(gamble ~ ., data = teengamb, nsamp = "exact")
coef(g_lts)
  #  (Intercept)          sex       status       income       verbal 
#    1.942662441  0.089223455  0.009302694  0.603803487 -0.340491284

# Posem 'nsamp = "exact"' ja que així es demana una cerca més
# exhaustiva, tot i que per a grans conjunts de dades el temps
# de computació és alt.

# Amb LTS es troben diferències MOLT substancials en els coeficients.

plot(fitted(g_lts),residuals(g_lts),xlab="Fitted",ylab="Residuals")

########################################################################

# PROBLEMA 4

# TEST RESET

# Model OLS
g <- lm(gamble ~ sex + income + verbal, data = teengamb)

# Prediccions amb el model OLS
yp <- fitted(g)

# Contrast de models

# Model de la nul·la = model OLS
g1 <- g

# Model de la alternativa

# k = 2
g2 <- lm(gamble  ~ sex + income + verbal + I(yp^2), data = teengamb)

# k = 3
g3 <- lm(gamble  ~ sex + income + verbal + I(yp^2) + I(yp^3), data = teengamb)

# k = 4
g4 <- lm(gamble  ~ sex + income + verbal + I(yp^2) + I(yp^3) + I(yp^4), data = teengamb)

# Contrastos

# Contrast 1. g1 vs. g2
anova(g1, g2)
  # Rebutgem la nul·la
  # Ens quedem amb el model 2

# Contrast 2. g1 vs. g3
anova(g1, g3)
  # Rebutgem la nul·la
  # Ens quedem amb el model 3

# Contrast 3. g1 vs. g4
anova(g1, g4)
  # Rebutgem la nul·la
  # Ens quedem amb el model 4

# Per tant, es rebutja la linealitat del model

#######################################################################