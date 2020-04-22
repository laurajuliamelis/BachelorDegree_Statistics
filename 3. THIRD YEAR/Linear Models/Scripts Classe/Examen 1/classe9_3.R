#CARACTER LINEAL DE LA REGRESSIO SIMPLE

#Es mira la luminositat de les bombetes després d'haver estat funcionant x hores

temps <- c(250, 250, 250, 500, 500, 750, 750, 750, 1000, 1000, 1250, 1250)
lum <- c(5460, 5475, 5400, 4000, 4700, 4500, 4600, 4520, 4320, 4300, 4000, 4010)

recta <- lm(lum ~ temps)
summary(recta)

plot(temps,lum)
abline(recta)

curva <- lm(lum ~ factor(temps)) #Anova 1 Factor (on cada nivell es com si fos una poblacio)

#temps realment és un factor de 5 nivells

anova(recta, curva)
#puc comparar les sumes de quadrats d'un model o de l altra 
# en R el test F és ANOVA

pol3 <- lm(lum ~ temps +I(temps^2) + I(temps^3))
summary(pol3)
coef(pol3)

curve(7.871040e+03 -1.375480e+01*x  +1.765249e-02*x^2 -7.316532e-06 *x^3, add=T, col='blue', xlim=c(250,1250))
