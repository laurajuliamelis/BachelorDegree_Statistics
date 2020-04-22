# EJEMPLO CROSS-OVER:
# Crossover:los datos que se estan estudiando son diferencias (son datos apareados)

X = read.csv(url('https://goo.gl/R1V6Ey'), sep=';')
interaction.plot(X$Per, X$T, X$FC)

# OPCIÓN UNO:
# Estimar el efecto del tratamiento
# Hacer la inferencia para estimar el efecto período