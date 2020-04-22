# Estatus

# Vd. ha recopilado los resultados de 14 ensayos clÃ­nicos publicados sobre la enfermedad pulmonar
# obstructiva crÃ³nica (EPOC), y la eficacia de cierto tratamiento, planteado con criterios similares. 
# La variable respuesta es el cambio observado en la prueba de la distancia recorrida andando durante 
# 6 minutos (6MWD). Los datos que contempla son los tamaÃ±os de cada grupo (intervenciÃ³n y control: t 
# y p), y las medias y desviaciones respectivas.	


bd <- read.csv("Metanalisis.csv", header=T, sep=";")
bd <- as.data.frame(bd)


# EJERCICIO 1. (sol: 0.83902685922291)
# Calcule el efecto tipificado (diferencia de medias estandarizada) que resulta del estudio "Salpeter71".	
bd$MD <- bd$Xt - bd$Xp

bd$nt_1<- bd$nt-1
bd$np_2<- bd$np-1
bd$st_1<- (bd$St)^2
bd$sp_2<- (bd$Sp)^2

bd$num <- (bd$nt_1*bd$st_1)+(bd$np_2*bd$sp_2)
bd$denom <- bd$nt+bd$np-2
bd$s <- sqrt(bd$num/bd$denom)
bd$SMD <- bd$MD/bd$s

bd$SMD[1] # Resultat 1


# EJERCICIO 2. (sol:0.0011544035581616)
# ¿Cuánto vale la variancia del efecto tipificado anterior?	
bd$var_SMD <- (sqrt((bd$nt+ bd$np)/(bd$nt*bd$np)))^2

bd$var_SMD[1] # Resultat 2


# EJERCICIO 3. (sol:3160.4281641356)
# Con el supuesto de efectos fijos, halle la suma acumulada de los pesos que se asignan a los estudios.
bd$W <- 1/bd$var_SMD

sum(bd$W) # Resultat 3

# EJERCICIO 4. (sol:0.99887174801455)
# ¿Cuánto vale el efecto global tipificado, estimado a partir del metaanÃ¡lisis? (dos decimales al menos)
bd$sum <- bd$W*bd$SMD

Y_hat <- sum(bd$sum)/sum(bd$W) # Resutat 4

# EJERCICIO 5.  (sol:0.96400727463382)
# Proporcione un intervalo de confianza 95% para el efecto global tipificado. Extremo inferior:
s <- sqrt(1/sum(bd$W))

IC_L <- Y_hat - (1.96*s) # Resutat 5


# EJERCICIO 6. (so: 1.0337362213953)
# Extremo superior:
IC_U <- Y_hat + (1.96*s) # Resutat 6



# EJERCICIO 7. (sol: 90.396509398636)
# El primer paso para sondear sobre la posibilidad de un efecto aleatorio (atribuible a la heterogeneidad de los estudios) es calcular el coeficiente Q. Â¿CuÃ¡nto vale en este caso?
Q <- sum(bd$W*(bd$SMD-Y_hat)^2) # Resutat 7
  
# EJERCICIO 8.
# Â¿CuÃ¡nto vale el p-valor de la prueba de heterogeneidad de estudios? (tres decimales al menos)

c <- nrow(bd)
pvalor <-  pchisq(0.05, c-1, lower.tail = T) # Resultat 8 (??)
  
# EJERCICIO 9. (sol: 0.030183011936301)
# Estime la variancia interestudios, usando el procedimiento de Dersimonian y Laird.
W_hat <- sum(bd$W)/c

s2_w<-(((sum(bd$W^2))) -(c*(W_hat^2)))/(c-1)
U <- (c-1)*(W_hat -(s2_w/(c*W_hat)))

D <- (Q-(c-1))/U # Resultat 9


# EJERCICIO 10. (SoluciÃ³: 1.0588461800131)
# Bajo el punto de vista del modelo de efectos aleatorios, obtenga la nueva estimaciÃ³n del efecto global tipificado.
bd$W2 <- 1/(D+(1/bd$W))
bd$sum2 <- bd$W2*bd$SMD

Y_hat_2 <- sum(bd$sum2)/sum(bd$W2) # Resultat 10
 
# EJERCICIO 11. (SoluciÃ³: 0.94467081610908)
# ¿Extremo inferior del IC (95%) para el efecto tipificado "promedio"?
s2 <- sqrt(1/sum(bd$W2))

IC_L2 <- Y_hat_2 - (1.96*s2) # Resutat 11


# EJERCICIO 12.
# 12. ¿Extremo superior del IC?

IC_U2 <- Y_hat_2 + (1.96*s2) # Resutat 11
