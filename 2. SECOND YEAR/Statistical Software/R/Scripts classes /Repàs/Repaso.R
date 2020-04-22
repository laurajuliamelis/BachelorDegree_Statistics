############################################
###  Software Estadístic, October 2016   ###
###  Review of Descriptive analysis		 ###
############################################
# Cargamos el data frame "states" del área de trabajo
load("States.RData")
head(states)

# Se asigna un missing a la primera y última observación de "lifex"
states$lifex[c(1, 50)] <- NA

# Se categoriza la variable numérica "inco" con la función cut2()
library(Hmisc)
states$inco2 <- cut2(states$inco, c(4000, 4500, 5000))

# Alternativa con la función cut()
states$inco2 <- cut(
	    x = states$inco,
	    breaks = c(min(states$inco), 4000, 4500, 5000, max(states$inco)),
	    include.lowest = TRUE,
	    right = FALSE,
	    dig.lab = 4)

# **********************************************************************
# RECORDATORIO: Comprobad siempre la existencia de missings al cargar un
# data frame. Pueden estar codificados como NA's o bajo otros formatos.
# **********************************************************************

## =========================================================================
##  EJERCICIO: Responded a las siguientes cuestiones relativas al data frame
##  "states" que contiene 2 NA's en la primera y última fila de "lifex"
## =========================================================================

# --------------------------------------------
# 1) ¿Cuántos NA's hay en la variable "lifex"?
# --------------------------------------------

# Introducción a la función: complete.cases()

complete.cases(states)		    # Vector lógico
sum(complete.cases(states)) 	# Número de observaciones (filas) completas

# Opción 1:
sum(is.na(states$lifex))				  # Hay 2 NA's

# Opción 2:
length(states$lifex) - sum(!is.na(states$lifex))  # Hay 2 NA's

# Opción 3:
sum(!complete.cases(states$lifex))			  # Hay 2 NA's

# ---------------------------------------------------------------------
# 2) Obtened el número de missings que hay en cada una de las variables
# ---------------------------------------------------------------------

# Opción 1: La más rudimentaria
summary(states)					    # 2 NA's en lifex

# Opción 2: La mejor
colSums(is.na(states))				# 2 NA's en lifex

# ------------------------------------------------------------------
# 3) Obtened el data frame (sin guardar el resultado) que resulta de
#    eliminar aquellas filas de "states" que contienen algún NA
# ------------------------------------------------------------------

# Introducción a la función: na.omit()

# Opción 1:
states[complete.cases(states), ]

# Opción 2:
na.omit(states)

# --------------------------------------------------------------
# 4) Eliminad las variables "hsg", "fro" del data frame "states"
# --------------------------------------------------------------

# Atención: Sólo ejecutad una de las siguientes 4 opciones

# Opción 1: La más básica
states$hsg <- NULL
states$fro <- NULL

# Opción 2: Un poco mejor
states$hsg <- states$fro <- NULL

# Opción 3: Una solución óptima
states <- subset(states, select = -c(hsg, fro))

# Opción 4: Otra solución óptima
states <- states[, !(names(states) %in% c("hsg", "fro"))]

# ---------------------------------------------------------------
# 5) Canviad el nombre de la variable "inco2" por el de "incocat"
# ---------------------------------------------------------------

# Opción 1: La más básica
names(states)[8] <- "incocat"

# Opción 2: Mejor, pues no require saber el número de columna en el
# que se ubica la variable "inco2"
names(states)[which(names(states) == "inco2")] <- "incocat"

# -------------------------------------------------------------------
# 6) ¿Cuántos estados de la región "South" tienen un salario >= 4500?
# -------------------------------------------------------------------

# Opción 1:
sum(states[states$reg == "South", ]$inco >= 4500)	# 4 estados

# Opción 2:
nrow(subset(states, reg == "South" & inco >= 4500))	# 4 estados

# ------------------------------------------------------------------
# 7) ¿Cuáles son los nombres de los estados de la pregunta anterior?
# ------------------------------------------------------------------

rownames(subset(states, reg == "South" & inco >= 4500))
# Answer: Delaware, Florida, Maryland, Virginia

# ----------------------------------------------------------------------
# 8) Tabla de contingencia para representar la distribución condicionada
#    de la región en función de las 4 posibles categorías salariales
# ----------------------------------------------------------------------

library(Epi)
tab <- stat.table(list(Cat_Income = incocat, Region = reg),
	              list(count(), percent(reg)), states, margins = T)

# ---------------------------------------------------------------------
# 9) ¿Cuáles son la media aritmética y la mediana de la  variable "pop"
#    en cada una de las cuatro posibles regiones? (sin decimales)
# ---------------------------------------------------------------------

# Opción 1:
library(doBy)
sumby <- summaryBy(pop ~ reg, states, FUN = c(mean, median))
names(sumby) <- c("Region", "Mean Population", "Median Population")
print(sumby, digits = 4) # data frame

# Opción 2:
pop.reg.mean.sd <- with(states,
                        tapply(pop, reg, function(x) {
				                          c(mean = mean(x), sd = sd(x))
				                         }))

class(pop.reg.mean.sd)		        # Es un tensor (array)
print(pop.reg.mean.sd, digits = 4)

# Opción 3: Nivel avanzado (funciones no explicadas en el curso)
mean.sd.op3a <- simplify2array(with(states,
						tapply(pop, reg, function(x) {
						  c(mean = mean(x), sd = sd(x))
						})))
round(mean.sd.op3a)			# Es una matriz

mean.sd.op3b <- do.call("rbind",
			            tapply(states$pop, states$reg, function(x) {
			                                           c(mean = mean(x), sd = sd(x))
			                                           }))
print(mean.sd.op3b, digits = 4) 	# Es una matriz

# -----------------------------------------------------------------
# 10) Ordenad "states" en orden descendente según la variable "pop"
#     y en orden ascendente segun la variable "incocat"
# -----------------------------------------------------------------

states[with(states, order(- pop, incocat)), ]
