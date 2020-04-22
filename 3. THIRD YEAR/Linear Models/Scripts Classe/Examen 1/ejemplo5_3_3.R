# EXEMPLE 5.3.3.

x <- c(17,34,26,10,19,17,8,16,13,11)
y <- c(21,20,11,26,42,28,3,3,16,-10)

# Sota normalitat, H_0: mitjana x = mitjana y
# considerem les variàncies iguals

t.test(x, y, var.equal = T)

# No hi ha raons per pensar que les mitjanes són diferents


# MATEIX PROBLEMA PLANTEJAT DES DEL PUNT DE VISTA DELS MODELS LINEALS

resposta <- c(x,y)

m1 <- c(rep(1,10),rep(0,10))
m2 <- c(rep(0,10),rep(1,10))
lmg <- lm(resposta ~ 0 + m1 + m2) #no hi ha mitjana general (l'intercept s'ha d'eliminar)

lm0 <- lm(resposta ~ 1) #sota la hipotesis nula de que les mitjanes són iguals

anova(lm0, lmg)

 0.20637^2 #T^2 = F