#########################################
###  Software Estadístic, 18.12.2016  ###
###       Pràctica final amb R        ###
###      NOM: Laura Julià Melis       ###
###         NIUB: 16810883            ###
#########################################
 
## EXERCICI 2.
## -----------

#  Creació de la funció.

Ex2 <- function (dades, cvar) {
  
    #  Comprovem que dades sigui un data frame.
    if (is.data.frame(dades) == FALSE) { 
      return ("ERROR! Aquest objecte no és un data frame.")
    }
    
    #  Comprovem que dades contingui la variable categòrica cvar.
    if (any(names(dades) == cvar) == FALSE || is.character(cvar) == FALSE) { 
      return ("ERROR! Aquesta variable no és correcte.")
    }
    
  
    #  Comprovem que dades contingui almenys una variable numèrica.
    if ((any(lapply(dades, class) == "numeric") || 
         any(lapply(dades, class) == "integer")) == FALSE) {
      return ("ERROR! Aquest data frame no conté cap variable numèrica.")
    }
    
  
    #  Nombre de files i columnes. 
    cat(" El nombre de files és", nrow(dades), "i el de columnes,", 
        ncol(dades), ".\n\n","TAULES DE FREQÜÈNCIES:\n")
    
    #  Realitzem taules de freqüències, si no tenim instal·lat el paquet Epi, 
    #  haurem de fer-ho amb la comanda: install.packages("catspec"). 
    #  A continuació, carreguem el paquet:
    library(catspec)
    
    for (i in 1:ncol(dades)) {
      if (class(dades[, i]) == "numeric" || class(dades[, i]) == "integer"){
        cat("Taula de freqüències de la variable", names(dades[i]), ":\n\n")
        print(quantile(dades[, i], na.rm = T))
      } else {
      cat("Taula de freqüències de la variable", names(dades[i]), ":\n\n")
      print(ctab(dades[, i]))
      }
    }
    #  Calculem el nombre de missings per variable.
    cat("\n","Nombre de missings que hi ha a cada variable:\n")
    print(colSums(is.na(dades)))
    
    
    #  Calulem quina fila temés missings.
    cat("\n Fila o files amb més missings:\n")
    print(which(rowSums(is.na(dades))== max(rowSums(is.na(dades))))) 
    
    
    #  Calculem diferents indicadors numèrics per a les diferents categories
    #  de cvar i realitzem gràfics de mosaics en funció de cvar, per a la resta
    #  de variables categòriques.
    pdf("Gràfics_de_mosaics.pdf") # Guardem tots els gràfics en un document pdf
    par(font = 2, font.lab = 4, font.axis = 4, las = 1)
    
    cat("Alguns indicadors numèrics per a les diferents categories de la 
        variable", cvar, "\n")
    for (i in 1:ncol(dades)) {
      if (class(dades[, i]) == "numeric" || class(dades[, i]) == "integer") {
        cat("Amb la variable", names(dades[i]), ":\n\n")
        print(with(dades, tapply(dades[, i], dades[, cvar], summary)))
      } else if(names(dades[i]) != cvar) {
        mosaicplot(dades[, cvar] ~ dades[, i], col = rainbow(12), 
                   main = "Gràfic de mosaics", cex.axis = 1)
      }
     }
   
    return (cat("FI DE LA FUNCIÓ!\n"))
}

#  Aplicació de la funció a les dades de l'exercici 1.

dades <- read.table ("SALES.csv", header=TRUE, sep = ';', nrows = 50)
dades <- transform (dades, zip = NULL, state = NULL, sale_date = NULL)
dades <- transform (dades, row.names=street , street = NULL)
dades$beds[sample (1:50, 1)] <- NA
dades$baths[sample (1:50, 2)] <- NA

cvar <- "type"  #nom d'una de les variables

Ex2 (dades, cvar)

#  Comentari de la sortida de la funció.

#    En primer lloc, observem que la funció no ens ha retornat cap missatge 
#  d'error perquè compleix les condicions demanades. Llavors, veiem que ens
#  informa del nombre d'individus i de variables que hi ha a la base de dades.
#    Pel que fa a les taules de freqüències, s'observa la diferència entre les 
#  taules per a les variables numèriques i les categòriques. Mencionem alguns
#  resultats significatius: un 66% de les vivendes recollides a la nostra base
#  de dades es troba a la ciutat de Sacramento i el 84% són residencials, 
#  almenys el 50% de les cases tenen 3 dormitoris i/o dos banys, uns 1119,5 
#  peus quadrats de superfície i un preu de 125 820$.
#     El nombre de missings resulta ser els que s'han afegit aleatòriament a 
#  l'exercici 1, ja que la base de dades original no contenia valors perduts i, 
#  com no s'han asignat a una mateixa fila, quan s'informa de la fila amb més
#  missings, resulten ser 3, cada una amb un missing. 
#     Seguidament, la fució realtiza diversos resums numèrics de totes les 
#  variables numèriques, segons la variable categòrica introduïda, en aquest
#  cas, "type". El primer que s'observa es que la mediana de llits pels 
#  condominis és 2 i no 3 com en el cas general, i que una gran majoria 
#  d'immobles residencials disposa de 3 dormitoris. El preu i la superfície 
#  de les cases són majors per a les de tipus residencial, encara que no d'una 
#  manera considerable. Finalment, la localització de les vivendes no varia 
#  segons el tipus, tal com es pot veure amb els resums per les variables 
#  latitude i longitude.

