# EXAMEN DE MODELS LINEALS SETEMBRE de 2003

# PROBLEMA 3

# Dades
x <- c(5.5, 4.8, 7.8, 8.2, 8.6, 9.7, 9.6, 8.9, 11.4, 10.6, 12.7, 11.5, 11.4)
y <- c(1006, 1162, 1479, 805, 795, 747, 732, 683, 686, 493, 476, 386, 368)

# Recta de regressio
recta <- lm(y ~ x)
plot(x, y)

# Intervals de confiança per als parámetres
summary(recta)
confint(recta)
#                 2.5 %     97.5 %
#  (Intercept) 1211.3316 2268.23420
#       x      -161.3537  -50.72882

# Variància residual (var no explicada = SCR) + R^2 + r
   # Coeficient de determinació
     R_2 <- summary(recta)$adj.r.squared # = 0.583
   # Coeficient de correlacio
     r <- sqrt(R_2)   # r = 0.764
   # SCR
     RSS <- (summary(recta)$sigma)^2*(13-2)   # RSS = 458209.6
     RSS_ <- sum(residuals(recta)^2)

# Predicció + interval de confiança
pred <- coefficients(recta)[[1]] + coefficients(recta)[[2]]*13   # No fa falta perquè ens ho dona el predict
predict(recta, newdata = data.frame(x = 13), interval = 'prediction')
    #         fit       lwr      upr
    #      361.2468 -148.2122 870.7057