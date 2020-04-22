# TEST RESET

# EXERCICI "NEW"

library(faraway)

# Dades
data(teengamb)

# Model OLS
g <- lm(gamble  ~ sex + income + verbal, data = teengamb)

summary(g)
  # Regressio significativa
  # Ajust del 0.50%

plot(g)
  # Alguns punts crítics en el supòsit d'homoscedasticitat
  # Patró una mica estrany dels residus

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

# Altres
anova(g2, g3)

install.packages("lmtest")
library(lmtest)
help(resettest)

resettest(g, power = 2, type = "fitted")  # Si dóna el mateix
resettest(g, power = 2:3, type = "fitted")  # Si dóna el mateix
resettest(g, power = 2:4, type = "fitted")  # Si dóna el mateix
