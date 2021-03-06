# Para determinar un posible efecto cardiovascular de Sildenafil durante el ejercicio en hombres
# con problemas coronarios, se ha dispuesto un ensayo cl�nico con intercambio, en la que la variable
# respuesta es un �ndice de fatiga medido tras una prueba de esfuerzo. El tratamiento (o el
# correspondiente placebo) se suministraba una hora antes de cada prueba, estando �stas separadas
# por un periodo de lavado de tres d�as. Los datos son: A1, A2, B1, B2, donde A indica "tratado con
# Sildenafil", B "tratado con placebo", y {1,2} se refiere al periodo.


# Dades
bd <- read.csv("Crossover.csv", header=T, sep=";")
bd <- as.data.frame(bd)


# 1. �Cu�l es el efecto estimado puntualmente de Sildenafil?	
n1 <- 9  # CANVIAR-HO SEGONS LES DADES!!
n2 <- 12 # CANVIAR-HO SEGONS LES DADES!!

d_i1 <- bd[1:n1,4]- bd[(n1+1):(2*n1),4]
d_i2 <- bd[(2*n1+1):((2*n1)+n2),4]- bd[((2*n1)+n2+1):nrow(bd),4]

d1 <- mean(d_i1)
d2 <- mean(d_i2)

efecto <- (d1-d2)/2
efecto # RESULTAT 1

# 2. Indique el extremo inferior del IC (95%) para el efecto de los tratados con Sildenafil.	
var1 <- var(d_i1)
var2 <- var(d_i2)

s2 <- (((n1-1)*var1)+((n2-1)*var2))/(n1+n2-2)
s <- sqrt(s2)

pvalor <-  qt(0.025, (n1+n2-2), lower.tail = F) 

IC_L <- efecto - (0.5*pvalor*s*sqrt((1/n1) + (1/n2)))
IC_L # RESULTAT 2 

# 3. Idem para el extremo superior.	
IC_U <- efecto + (0.5*pvalor*s*sqrt((1/n1) + (1/n2)))
IC_U # RESULTAT 3 

# 4. �Influye el hecho de haber realizado la prueba antes o despu�s? Obtenga ahora la estimaci�n 
# por IC del efecto periodo; extremo inferior:	
efecto_per�odo <- (d1 + d2)/2
efecto_per�odo 

IC_L_per�odo <- efecto_per�odo - (0.5*pvalor*s*sqrt((1/n1) + (1/n2)))
IC_L_per�odo # RESULTAT 4

# 5. Idem para el extremo superior.	
IC_U_per�odo <- efecto_per�odo + (0.5*pvalor*s*sqrt((1/n1) + (1/n2)))
IC_U_per�odo # RESULTAT 5

# 6. �Presenta efectos arrastrados el tratamiento, a pesar del periodo de lavado? Ya sabemos que 
# esta es una prueba con escasa potencia, pero estime el posible efecto tard�o del tratamiento; 
# extremo inferior:	
suma1 <- bd[1:n1,4]+bd[(n1+1):(2*n1),4]
suma2 <- bd[(2*n1+1):((2*n1)+n2),4]+ bd[((2*n1)+n2+1):nrow(bd),4]

efecto_tard�o <- mean(suma1)-mean(suma2)
efecto_tard�o

var1_tard�o <- var(suma1)
var2_tard�o <- var(suma2)

s2_tard�o <- (((n1-1)*var1_tard�o)+((n2-1)*var2_tard�o))/(n1+n2-2)
s_tard�o <- sqrt(s2_tard�o)

pvalor <-  qt(0.025, (n1+n2-2), lower.tail = F)  


IC_L_tard�o <- efecto_tard�o - (pvalor*s_tard�o*sqrt((1/n1) + (1/n2)))
IC_L_tard�o # RESULTAT 6 

# 7. Idem para el extremo superior.	
IC_U_tard�o <- efecto_tard�o + (pvalor*s_tard�o*sqrt((1/n1) + (1/n2)))
IC_U_tard�o # RESULTAT 7

# 8. �En cu�nto estima que vale la variancia intraindividuos?	
intra <- s2/2
intra # RESULTAT 8

# 9. Halle un valor estimado de la variancia entreindividuos.	
entre <- (s2_tard�o-(2*intra))/4
entre # RESULTAT 9

# 10. Finalmente, calcule la potencia de la prueba realizada sobre el efecto directo, 
# asumiendo que la desviaci�n intraindividuo vale 3.5 unid. y el posible efecto de Sildenafil 
# es incrementar la respuesta en 6 puntos.	
# Considere los tama�os por grupo obtenidos en esta prueba, y un riesgo bilateral del 5%.