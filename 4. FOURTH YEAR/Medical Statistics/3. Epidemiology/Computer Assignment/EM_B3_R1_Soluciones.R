## Estadística Médica
## Curso 2018/19; 09.11.2018
## Cálculo de tasas de incidencia
## ==============================
load("Muga2007.RData")
library(Hmisc)

## Subconjunto de los datos de Muga et al. (2007)
## ==============================================
str(muga2007)
head(muga2007, 10)
tail(muga2007)
View(muga2007)

summary(muga2007)


## A) Reproducid los valores de las primeras 3 columnas
##    de la Tabla 2 de Muga et al. (2007) para las
##    variables Gender y Exposure Category
## ----------------------------------------------------
# 1ª columna
summary(muga2007)

# 2ª columna
with(muga2007, table(sex, tbcens))
with(muga2007, tapply(tbcens, risk, sum))

# Alternativa
#install.packages('Epi')
library(Epi)


#with (muga2007, list(Sex = sex, TB = tbcens))


?stat.table

stat.table(list(Sex = sex, TB = tbcens), # INDICAMOS LAS VARIABLES EN UNA LISTA
           list(count(), percent(tbcens)), # INDICAMOS LOS CÁLCULOS QUE QUEREMOS OBTENER (CONTAR Y %tbcens)
           muga2007, margin = T) # margin SI QUEREMOS INCLUIR LOS RESULTADOS MARGINALES



#stat.table(list(Sex = sex, TB = tbcens), # INDICAMOS LAS VARIABLES EN UNA LISTA
#           list(count(), percent(tbcens),percent(sex)), # INDICAMOS LOS CÁLCULOS QUE QUEREMOS OBTENER (CONTAR Y %tbcens)
#           muga2007, margin = F)


stat.table(list(Risk = risk, TB = tbcens),
           list(count(), percent(tbcens)), muga2007, margin = T)

# 3ª columna

sum(muga2007$hiv2tbc) #19991.2 + 3706.9
sum(muga2007$hiv2tbc[muga2007$sex=="Male"]) #19991.2 + 3706.9
sum(muga2007$hiv2tbc[muga2007$sex=="Female"]) #19991.2 + 3706.9

with(muga2007, round(tapply(hiv2tbc, sex, sum), 1)) # SUMAMOS LOS "TIEMPOS HASTA TUBERCULOSIS (hiv2tbc)" DE TODOS LOS PACIENTES
with(muga2007, round(tapply(hiv2tbc, risk, sum), 1))


## B) Cálculo de las tasas de incidencia (Columna 4 de la Tabla 2)
## ---------------------------------------------------------------
library(epitools)
# A nivel global
?pois.exact
with(muga2007, pois.exact(sum(tbcens), sum(hiv2tbc))) # CASOS NUEVOS (count), TIEMPOS A RIESGO (person-time at risk)
with(muga2007, pois.exact(sum(tbcens), sum(hiv2tbc)))[, 3:5]*1000 # 1000 p-y
with(muga2007, round(pois.exact(sum(tbcens), sum(hiv2tbc))[, 3:5]*1000, 1)) 

# Por sexo
with(subset(muga2007, sex == "Male"),
     round(pois.exact(sum(tbcens), sum(hiv2tbc))[, 3:5]*1000, 1))
with(subset(muga2007, sex == "Female"),
     round(pois.exact(sum(tbcens), sum(hiv2tbc))[, 3:5]*1000, 1))

# Por grupo de riesgo
with(subset(muga2007, risk == "IDUs"),
     round(pois.exact(sum(tbcens), sum(hiv2tbc))[, 3:5]*1000, 1))
with(subset(muga2007, risk == "Sex.Transmission"),
     round(pois.exact(sum(tbcens), sum(hiv2tbc))[, 3:5]*1000, 1))
with(subset(muga2007, risk == "Haemophilia"),
     round(pois.exact(sum(tbcens), sum(hiv2tbc))[, 3:5]*1000, 1))

# Los mismos cálculos usando un bucle
for (i in levels(muga2007$risk)) {
  cat("\nGrupo de riesgo:", i, "\n")
  print(with(subset(muga2007, risk == i),
             round(pois.exact(sum(tbcens), sum(hiv2tbc))[, 3:5]*1000, 1)))
}

# Los mismos cálculos con la función by
by(muga2007, muga2007$sex,
  function(x) round(pois.exact(sum(x$tbcens), sum(x$hiv2tbc))[, 3:5]*1000, 1))

by(muga2007, muga2007$risk,
   function(x) round(pois.exact(sum(x$tbcens), sum(x$hiv2tbc))[, 3:5]*1000, 1))
