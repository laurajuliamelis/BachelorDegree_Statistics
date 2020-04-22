########################################
###  Software Estadístic, 20.09.2016 ###
###  Lecture 2: Solution of Test 0   ###
########################################

### *********************
### RESOLUCIÓN APARTADO A
### *********************

set.seed(2009)	# Se fija una semilla para la reprodiucir resultados
height <- round(rnorm(15, 175, 10)) # En cm
height
weight <- round(height - 100 + rnorm(15, 0, 5)) # En Kg
weight


# Dos formas correctas de hallar el vector "bmi" (Kg/m^2)
# ------------------------------------------------------
bmi <- round(weight/(height/100)^2, 2)
bmi <- round(weight/(height*height/10000), 2) # Otra forma de obtener bmi
bmi

## Importante: si trabajamos con dos vectores de longitud distina,
## se "recicla" el vector más corto.
x <- c(1, 4, 9)
y <- c(2, 0, -2)
xy <- c(x, y)
xy2 <- c(xy, 2)
x/y             # Todo correcto
xy/x            # Tampoco no hay ningún problema
xy2/y           # Mensaje de advertencia!


### *************************
### RESOLUCIÓN DEL APARTADO B
### *************************

# Ejemplo de forma correcta pero ineficiente para resolver el apartado, pues
# resulta demasiado complicado y además podría llegar a tener un elevado coste
# computacional al aumentar el tamaño de componentes del vector "bmi"
# ============================================================================
t <- 0
m <- 0
for (i in bmi){
 if (i <= 22.0){
  t <- t + 1
 }
 if (i < 25.0 & i > 23.0){
  m <- m + 1
 }
}
print(c(t, m))


# Mucho más sencillo y óptimo (puntuación máxima en el examen):
# =============================================================
sum(bmi < 22)

# Lo mismo, pero de otra forma:
sum(!bmi >= 22)

sum(bmi > 23 & bmi < 25) # Resultado: 5 individuos


# Cuestiones cortas de entrenamiento
# ==================================

# ¿Cuáles son los valores del vector "bmi" que cumplen la condición anterior?
# ---------------------------------------------------------------------------
bmi[bmi > 23 & bmi < 25]
# Lo siguiente NO es correcto:
bmi[bmi > 23 && bmi < 25]


# ¿Qué posición ocupan estos valores en el vector "bmi"?
# ------------------------------------------------------
indx <- which(bmi > 23 & bmi < 25)
indx


### *************************
### RESOLUCIÓN DEL APARTADO C
### *************************
median(weight[height > 175])


### **************************
### RESOLUCIÓN DEL APARTADO D
### **************************

# Alternativa correcta pero que resulta ineficiente
# =================================================
prov <- c("Barcelona", "Barcelona", "Barcelona", "Barcelona", "Barcelona", "Barcelona",
          "Girona", "Girona", "Girona", "Girona",
          "Lleida", "Lleida", "Lleida",
          "Tarragona", "Tarragona")

# Alternativa que mejora la anterior
# ==================================
prov <- c(rep("Barcelona", 6), rep("Girona", 4), rep("Lleida", 3), rep("Tarragona", 2))

# Solución óptima en el examen: Sólo usa una vez el comando rep()
# ===============================================================
prov <- rep(c("Barcelona", "Girona", "Lleida", "Tarragona"), c(6, 4, 3, 2))
prov
table(prov)
summary(prov)

# Atención: El vector "prov" construido es actualmente un carácter
# =================================================================
class(prov)

# ======================================================================
# UTILIZACIÓN DE FACTORES PARA EL TRATAMIENTO DE VARIABLES CATEGÓRICAS:
# Los factores son variables en R que pueden tomar únicamente un limitado
# número de valores distintos. Este tipo de variables, denominadas por lo
# general como categóricas o cualitativas, tienen una amplia presencia en
# las bases de datos acompañadas de las convencionales variables de tipo
# numérico ó cuantitativo. En consecuencia, para que R pueda trabajar con
# ellas en modo numérico, de forma interna se organizan en niveles.
# Mucho mejor que trabajar con caracteres, es trabajar con factores
# (Forma de conseguir la máxima puntuación en el examen)
# =======================================================================

prov2 <- factor(rep(c("Barcelona", "Girona", "Lleida", "Tarragona"), c(6, 4, 3, 2)))
prov2
table(prov2)
summary(prov2)
levels(prov2)      # Categorías o niveles del factor
class(prov2)       # "factor"
as.numeric(prov2)  # Valor numérico que R asigna a cada nivel del vector "prov2"


### **************************
### RESOLUCIÓN DEL APARTADO E
### **************************

# Resolución del apartado E si el vector de provincias es un caracter
# ===================================================================
range(height[prov == "Barcelona"])

range(weight[prov == "Lleida" | prov == "Tarragona"])

# Alternativa (un poco más eficiente)
range(weight[prov %in% c("Lleida", "Tarragona")])

# La siguiente instrucción NO es correcta
range(weight[prov == c("Lleida", "Tarragona")])

# Resolución del apartado E si el vector de provincias es un factor
# =================================================================
range(height[prov2 == "Barcelona"])
range(height[as.numeric(prov2) == 1])

range(weight[as.numeric(prov2) %in% 3:4])
range(weight[as.numeric(prov2) > 2])


### **************************
### RESOLUCIÓN DEL APARTADO F
### **************************

# Resolución del apartado F si el vector de provincias es un caracter
# ===================================================================

# Forma muy ineficiente (aunque correcta)
# ---------------------------------------
prov_new <- prov
prov_new[prov_new == "Barcelona"] <- "BAR"
prov_new[prov_new == "Girona"] <- "GIR"
prov_new[prov_new == "Lleida"] <- "LLE"
prov_new[prov_new == "Tarragona"] <- "TAR"
prov_new

# Mucho mejor:
# PASO 1: Me quedo con los elementos 1 a 3 de cada caracter del vector
# PASO 2: Convierto los caracteres a letras mayúsculas
# IMPORTANTE: Asignar un nombre al nuevo vector (como se consigue la máxima
# puntuación en el examen).
# -------------------------------------------------------------------------
prov_new <- toupper(substr(prov, 1, 3))


# Resolución del apartado F si el vector de provincias es un factor
# =================================================================
prov2_new <- prov2
levels(prov2_new) <- c("BAR", "GIR", "LLE", "TAR")
prov2_new


### *************************
### RESOLUCIÓN DEL APARTADO G
### *************************

# Importante: Poner variable identificadora de individuo
# ------------------------------------------------------
dfram <- data.frame(id = paste("ID", 1:15, sep = "-"), height, weight, bmi, prov2_new)
dfram
# Nota: La variable id se puede crear también de la siguiente manera.
paste0("ID-", 1:15)
