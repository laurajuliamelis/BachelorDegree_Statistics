# Nom: Laura Julià Melis
# NIUB: 16810883
# ************************************************************
# Parámetros que se pueden cambiar:                          *
# ************************************************************

defectos <- 4       # Número de defectos inicial
n_muestras <- 10    # Tamaño de la muestra
N_total <- 50       # Número de muestras
N_salto <- 10       # Muestra en la que se produce el salto
def_salto <- 8      # Salto en el número de defectos

# ************************************************************
# Creación de un data frame con los datos definidos:         *
# ************************************************************

dad1 <- rbinom (N_salto, n_muestras, defectos/n_muestras)
dad2 <- rbinom ((N_total-N_salto), n_muestras, def_salto/n_muestras)
dades <- c(dad1,dad2)
dades

muestras <- rep(n_muestras, N_total)

df <- data.frame(Muestra = 1:N_total, Defectes = dades , Mida_mostra = muestras)

# ************************************************************
# Gráfico de control NP:                                     *
# ************************************************************
# Si no tenim el paquest "qcc" instal·lat:  install.packages("qcc")
library("qcc")
grafic_np <- qcc(data= df$Defectes , type = "np", sizes = df$Mida_mostra)

#Puntos fuera de control
grafic_np$violations
#Límites de control
grafic_np$limits

