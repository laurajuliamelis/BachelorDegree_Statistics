## Estadística Médica
## Curso 2018/19; 31.10.2018
## Cálculo de tasas de incidencia
## ==============================
load("Muga2007.RData")
library(Hmisc)

## Subconjunto de los datos de Muga et al. (2007)
## ==============================================
str(muga2007)
head(muga2007, 10)
tail(muga2007)
summary(muga2007)


## A) Reproducid los valores de las primeras 3 columnas
##    de la Tabla 2 de Muga et al. (2007) para las
##    variables Gender y Exposure Category
## ----------------------------------------------------
# 1ª columna



# 2ª columna



# 3ª columna




## B) Cálculo de las tasas de incidencia (Columna 4 de la Tabla 2)
## ---------------------------------------------------------------
library(epitools)
?pois.exact

# A nivel global
with(muga2007, pois.exact(...))

# Por sexo


# Por grupo de riesgo
