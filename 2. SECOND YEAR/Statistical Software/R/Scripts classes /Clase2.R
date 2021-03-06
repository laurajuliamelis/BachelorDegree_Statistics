##########################################
###  Software Estad�stic, 22.09.2016   ###
###  Lecture 3: Some useful functions  ###
##########################################

## Abrir el �rea de trabajo ClasseR_Sep22.RData
## ============================================
# �Cu�l es el directorio de trabajo actual?
getwd()

# �Contiene ClasseR_Sep22.RData?
dir()
"ClasseR_Sep22.RData" %in% dir()

# If TRUE, then
load("ClasseR_Sep22.RData")
# If FALSE, then
setwd(...)                  # Hay que especificar el directorio de trabajo
load("ClasseR_Sep22.RData")

# Listado de los objetos presentes en mi actual �rea de trabajo
# -------------------------------------------------------------
ls()
objects()  # Alternativamente
ls.str()
summary(height)
summary(prov)
summary(dfram)


## Las funciones save y save.image
## ===============================
# �Cu�l es la diferencia entre las siguientes instrucciones?
save.image(file = "RLecture3.RData")
save(bmi, dfram, file = "RLecture3b.RData")

# �C�mo borrar objetos del �rea de trabajo?
rm(height)

# Si ahora me doy cuenta que quer�a borrar weight (y no height).
# �Qu� puedo hacer?
load("RLecture3.RData")
rm(weight)
height

# Cuestiones con las funciones round y trunc
# ==========================================
round(bmi, 3) # Ya no podemos redondear a 3 decimales, pues partimos de 2 decimales
round(bmi, 1) # S� podemos redondear a 1 s�lo decimal

# Diferencia entre truncar por la parte entera y reondear sin decimales "bmi"
# ---------------------------------------------------------------------------
trunc(bmi)
round(bmi)


# Cuestiones con la funci�n which
# ===============================
# �Cu�ntas personas tienen una altura igual a 174 cm?
sum(height == 174)
# �Qu� posiciones ocupan?
which(height == 174)

# �Cu�l es el m�nimo �ndice de masa corporal (IMC)?
min(bmi)
# �Qu� posici�n ocupa el m�nimo IMC?
which.min(bmi)
# �Posici�n que ocupa el m�ximo IMC?
which.max(bmi)

# �Ojo! En caso de m�ximos o m�nimos repetidos pasa lo siguiente
# --------------------------------------------------------------
max(height)
which.max(height)
# Pero:
sum(height == 188)
# �C�mo podemos ver todas las posiciones que ocupa el m�ximo?
which(height == max(height))


## Detecci�n de datos repetidos
## ============================
# (a) Uso de las funciones sort y table [Soluci�n no-�ptima]
# ----------------------------------------------------------
table(height)
sort(table(height))
sort(table(height), decreasing = T)
sort(table(height), decreasing = T)[1] > 1  # TRUE

# (b) Uso de las funciones unique y length [Soluci�n no-�ptima]
# -------------------------------------------------------------
unique(height)
length(unique(height)) < length(height)     # TRUE

# (c) Uso de las funciones duplicated y sum [Soluci�n mejor]
#                                     o any [Soluci�n �ptima]
# -----------------------------------------------------------
duplicated(height)
sum(duplicated(height))
sum(duplicated(height)) > 0     # TRUE
any(duplicated(height))         # TRUE


# Cuestiones con la funci�n is.na
# ===============================
# Hay alg�n missing in height o prov
is.na(height)
any(is.na(height))
all(complete.cases(height))
any(is.na(prov))
# �Cu�ntos hay en prov?
sum(is.na(prov))
# �Y en el data frame?
sum(is.na(dfram))
# El data frame sin missings
na.omit(dfram)


# Cuestiones con la funci�n cor
# =============================
# �Cu�l es el coef. de correlaci�n de Pearson entre la altura y el imc?
round(cor(height, bmi), 3)

# �Y si el �ltimo elemento del vector de alturas fuese un missing?
height.new <- c(height[1:14], NA)
height.new
summary(height.new)
mean(height.new, na.rm = T)

round(cor(height.new, bmi), 3)
round(cor(height.new, bmi, use = "complete.obs"), 3)

# �Cu�les son los coeficientes de correlaci�n entre las tres variables
#  num�ricas de dfram?
# --------------------------------------------------------------------
round(cor(dfram[, c("height", "weight", "bmi")]), 3)

# Borramos el primer y el �ltimo valor de height en dfram
dfram$height[c(1, nrow(dfram))] <- NA
dfram

# �Cu�l es la diferencia entre las siguientes dos instrucciones?
round(cor(dfram[, c("height", "weight", "bmi")], use = "complete.obs"), 3)
round(cor(dfram[, c("height", "weight", "bmi")], use = "pairwise.complete.obs"), 3)


# Un ejercicio con vectores: Cread el siguiente vector
# 1 2 3 4 5  2 3 4 5 6  3 4 5 6 7  4 5 6 7 8  5 6 7 8 9
# =====================================================
0:4 + rep(1:5, each = 5)


# Cuestiones con la funci�n sample
# ================================
prov
prov[6:7] <- c("Barcelona", "Girona")
# �C�mo construir un vector tal que las provincias aparezcan en orden aleatorio
# pero controlando la frecuencia absoluta asociada a cada una de ellas?

# Si se intenta de forma directa con sample:
# ------------------------------------------
prov.op1 <- sample(prov, 15, replace = T)
table(prov.op1) # NO se puede controlar el n�mero de veces que aparece cada provincia

# Ahora s� que funciona
# ---------------------
prov.op2 <- sample(prov, size = length(prov), replace = FALSE)
prov.op2
table(prov.op2)
# M�s sencillo a�n
prov.op2 <- sample(prov)
prov.op2
table(prov.op2)


# Se puede ejecutar un script sin tenerlo que abrir
# =================================================
source("Classe2_2016_09_20_DistNormal.R")
