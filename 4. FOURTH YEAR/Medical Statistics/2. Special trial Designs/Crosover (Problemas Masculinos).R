# Para determinar un posible efecto cardiovascular de Sildenafil durante el ejercicio en hombres
# con problemas coronarios, se ha dispuesto un ensayo clínico con intercambio, en la que la variable
# respuesta es un índice de fatiga medido tras una prueba de esfuerzo. El tratamiento (o el
# correspondiente placebo) se suministraba una hora antes de cada prueba, estando éstas separadas
# por un periodo de lavado de tres días. Los datos son: A1, A2, B1, B2, donde A indica "tratado con
# Sildenafil", B "tratado con placebo", y {1,2} se refiere al periodo.


# Dades
bd <- read.csv("Crossover.csv", header=T, sep=";")
bd <- as.data.frame(bd)


# 1. ¿Cuál es el efecto estimado puntualmente de Sildenafil?	
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

# 4. ¿Influye el hecho de haber realizado la prueba antes o después? Obtenga ahora la estimación 
# por IC del efecto periodo; extremo inferior:	
efecto_período <- (d1 + d2)/2
efecto_período 

IC_L_período <- efecto_período - (0.5*pvalor*s*sqrt((1/n1) + (1/n2)))
IC_L_período # RESULTAT 4

# 5. Idem para el extremo superior.	
IC_U_período <- efecto_período + (0.5*pvalor*s*sqrt((1/n1) + (1/n2)))
IC_U_período # RESULTAT 5

# 6. ¿Presenta efectos arrastrados el tratamiento, a pesar del periodo de lavado? Ya sabemos que 
# esta es una prueba con escasa potencia, pero estime el posible efecto tardío del tratamiento; 
# extremo inferior:	
suma1 <- bd[1:n1,4]+bd[(n1+1):(2*n1),4]
suma2 <- bd[(2*n1+1):((2*n1)+n2),4]+ bd[((2*n1)+n2+1):nrow(bd),4]

efecto_tardío <- mean(suma1)-mean(suma2)
efecto_tardío

var1_tardío <- var(suma1)
var2_tardío <- var(suma2)

s2_tardío <- (((n1-1)*var1_tardío)+((n2-1)*var2_tardío))/(n1+n2-2)
s_tardío <- sqrt(s2_tardío)

pvalor <-  qt(0.025, (n1+n2-2), lower.tail = F)  


IC_L_tardío <- efecto_tardío - (pvalor*s_tardío*sqrt((1/n1) + (1/n2)))
IC_L_tardío # RESULTAT 6 

# 7. Idem para el extremo superior.	
IC_U_tardío <- efecto_tardío + (pvalor*s_tardío*sqrt((1/n1) + (1/n2)))
IC_U_tardío # RESULTAT 7

# 8. ¿En cuánto estima que vale la variancia intraindividuos?	
intra <- s2/2
intra # RESULTAT 8

# 9. Halle un valor estimado de la variancia entreindividuos.	
entre <- (s2_tardío-(2*intra))/4
entre # RESULTAT 9

# 10. Finalmente, calcule la potencia de la prueba realizada sobre el efecto directo, 
# asumiendo que la desviación intraindividuo vale 3.5 unid. y el posible efecto de Sildenafil 
# es incrementar la respuesta en 6 puntos.	
# Considere los tamaños por grupo obtenidos en esta prueba, y un riesgo bilateral del 5%.