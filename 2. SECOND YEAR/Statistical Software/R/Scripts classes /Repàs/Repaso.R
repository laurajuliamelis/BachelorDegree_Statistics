############################################
###  Software Estad�stic, October 2016   ###
###  Review of Descriptive analysis		 ###
############################################
# Cargamos el data frame "states" del �rea de trabajo
load("States.RData")
head(states)

# Se asigna un missing a la primera y �ltima observaci�n de "lifex"
states$lifex[c(1, 50)] <- NA

# Se categoriza la variable num�rica "inco" con la funci�n cut2()
library(Hmisc)
states$inco2 <- cut2(states$inco, c(4000, 4500, 5000))

# Alternativa con la funci�n cut()
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
##  "states" que contiene 2 NA's en la primera y �ltima fila de "lifex"
## =========================================================================

# --------------------------------------------
# 1) �Cu�ntos NA's hay en la variable "lifex"?
# --------------------------------------------

# Introducci�n a la funci�n: complete.cases()

complete.cases(states)		    # Vector l�gico
sum(complete.cases(states)) 	# N�mero de observaciones (filas) completas

# Opci�n 1:
sum(is.na(states$lifex))				  # Hay 2 NA's

# Opci�n 2:
length(states$lifex) - sum(!is.na(states$lifex))  # Hay 2 NA's

# Opci�n 3:
sum(!complete.cases(states$lifex))			  # Hay 2 NA's

# ---------------------------------------------------------------------
# 2) Obtened el n�mero de missings que hay en cada una de las variables
# ---------------------------------------------------------------------

# Opci�n 1: La m�s rudimentaria
summary(states)					    # 2 NA's en lifex

# Opci�n 2: La mejor
colSums(is.na(states))				# 2 NA's en lifex

# ------------------------------------------------------------------
# 3) Obtened el data frame (sin guardar el resultado) que resulta de
#    eliminar aquellas filas de "states" que contienen alg�n NA
# ------------------------------------------------------------------

# Introducci�n a la funci�n: na.omit()

# Opci�n 1:
states[complete.cases(states), ]

# Opci�n 2:
na.omit(states)

# --------------------------------------------------------------
# 4) Eliminad las variables "hsg", "fro" del data frame "states"
# --------------------------------------------------------------

# Atenci�n: S�lo ejecutad una de las siguientes 4 opciones

# Opci�n 1: La m�s b�sica
states$hsg <- NULL
states$fro <- NULL

# Opci�n 2: Un poco mejor
states$hsg <- states$fro <- NULL

# Opci�n 3: Una soluci�n �ptima
states <- subset(states, select = -c(hsg, fro))

# Opci�n 4: Otra soluci�n �ptima
states <- states[, !(names(states) %in% c("hsg", "fro"))]

# ---------------------------------------------------------------
# 5) Canviad el nombre de la variable "inco2" por el de "incocat"
# ---------------------------------------------------------------

# Opci�n 1: La m�s b�sica
names(states)[8] <- "incocat"

# Opci�n 2: Mejor, pues no require saber el n�mero de columna en el
# que se ubica la variable "inco2"
names(states)[which(names(states) == "inco2")] <- "incocat"

# -------------------------------------------------------------------
# 6) �Cu�ntos estados de la regi�n "South" tienen un salario >= 4500?
# -------------------------------------------------------------------

# Opci�n 1:
sum(states[states$reg == "South", ]$inco >= 4500)	# 4 estados

# Opci�n 2:
nrow(subset(states, reg == "South" & inco >= 4500))	# 4 estados

# ------------------------------------------------------------------
# 7) �Cu�les son los nombres de los estados de la pregunta anterior?
# ------------------------------------------------------------------

rownames(subset(states, reg == "South" & inco >= 4500))
# Answer: Delaware, Florida, Maryland, Virginia

# ----------------------------------------------------------------------
# 8) Tabla de contingencia para representar la distribuci�n condicionada
#    de la regi�n en funci�n de las 4 posibles categor�as salariales
# ----------------------------------------------------------------------

library(Epi)
tab <- stat.table(list(Cat_Income = incocat, Region = reg),
	              list(count(), percent(reg)), states, margins = T)

# ---------------------------------------------------------------------
# 9) �Cu�les son la media aritm�tica y la mediana de la  variable "pop"
#    en cada una de las cuatro posibles regiones? (sin decimales)
# ---------------------------------------------------------------------

# Opci�n 1:
library(doBy)
sumby <- summaryBy(pop ~ reg, states, FUN = c(mean, median))
names(sumby) <- c("Region", "Mean Population", "Median Population")
print(sumby, digits = 4) # data frame

# Opci�n 2:
pop.reg.mean.sd <- with(states,
                        tapply(pop, reg, function(x) {
				                          c(mean = mean(x), sd = sd(x))
				                         }))

class(pop.reg.mean.sd)		        # Es un tensor (array)
print(pop.reg.mean.sd, digits = 4)

# Opci�n 3: Nivel avanzado (funciones no explicadas en el curso)
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
# 10) Ordenad "states" en orden descendente seg�n la variable "pop"
#     y en orden ascendente segun la variable "incocat"
# -----------------------------------------------------------------

states[with(states, order(- pop, incocat)), ]
