# EXAMEN FINAL GENER 2012

# EXERCICI 3

library(faraway)

# Dades
data(teengamb)

# Apartat 1. Seleccio de variables

# 1) Model de regressio multiple OLS
g <- lm(gamble ~ ., data = teengamb)
summary(g)


# 2) Model seleccionat per AIC
g_AIC <- step(g, direction = "both")
  # gamble ~ sex + income + verbal
  # Hem tret status


# 3) Model seleccionat per Forward Stepwise
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

g_forward <- model
  # gamble ~ income + sex


# 4) Model seleccionat per Cp de Mallows
install.packages('leaps')
library(leaps)
X <- as.matrix(teengamb[,-5])
y <- teengamb[,5]

g_Cp <- leaps(X, y, method = "Cp", names = c("sex", "status", "income", "verbal"))

# Si P = 3 (2 regressores més intercept)
g_Cp$which[g_Cp$Cp==min(g_Cp$Cp[g_Cp$size==3])]
  # sex + income

# Si P = 4 (3 regressores més intercept)
g_Cp$which[g_Cp$Cp==min(g_Cp$Cp[g_Cp$size==4])]
  # sex + income + verbal
plot(g_Cp$size,g_Cp$Cp, ylim = c(0,6),xlab="P",ylab="C_P")
abline(0, 1)

  # El que més s'apropa a Cp=P és el millor de P = 3
  # Escolliriem sex + income

# Coeficients de determinació dels models

summary(g)$adj.r.squared          # 0.4816
summary(g_AIC)$adj.r.squared      # 0.4933
summary(g_forward)$adj.r.squared  # 0.4787

# L'ajust no millora, pero aconseguim el mateix ajust amb menys variables

# Intervals de confiança
confint(g, "income")
confint(g_AIC, "income")
confint(g_forward, "income")


# Apartat 2. Models robustos

# 1) Model de regressio multiple OLS sense el punt 24
g_ols <- lm(gamble ~ ., data = teengamb[-24,])
summary(g_ols)

# 2) HUBER
library(MASS)
g_huber <- rlm(gamble ~ ., data = teengamb)

summary(g_huber)
# Modificació d'alguns valors dels coeficients
  # Intercept
  # Verbal
  # Mateixes conclusions respecte a la significació de les vars?? comporvar

# 3) LTS
g_lts <- ltsreg(gamble ~ ., data = teengamb, nsamp = "exact")
coef(g_lts)
  #  (Intercept)          sex       status       income       verbal 
#    1.942662441  0.089223455  0.009302694  0.603803487 -0.340491284

# Amb LTS es troben diferències MOLT substancials en els coeficients.
