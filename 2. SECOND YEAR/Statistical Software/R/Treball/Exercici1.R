#########################################
###  Software Estadístic, 18.12.2016  ###
###       Pràctica final amb R        ###
###      NOM: Laura Julià Melis       ###
###         NIUB: 16810883            ###
#########################################

## EXERCICI 1.
## -----------

## APARTAT b.

#  Importació de les dades.
df <- read.table("SALES.csv", header = TRUE, sep = ';', nrows = 50)
df <- transform(df, zip = NULL, state = NULL, sale_date = NULL)
head(df)
class(df)

#  Substitució del nom de cada registre per la variable identificadora street.
df <- transform (df, row.names = street, street = NULL)
str(df)

#  Introducció de dades faltants aleatòriament.
df$beds[sample(1:50, 1)] <- NA
df$baths[sample(1:50, 2)] <- NA



##  APARTAT c. 
#  Resum numèric de totes les variables del data frame.
summary(df)

#  Obrim una finestra per als gràfics i establim els seus paràmetres.
windows (title = "Gràfiques de la descriptiva univariant", 
         width = 10, height = 5)
par(mfrow = c(2, 3), lwd = 1, font = 2, font.lab = 2, font.axis = 2, las = 1)

#  Creació d'un gràfic de barres de les freqüències de les ciutats 
#  on es troben les vivendes.
barplot(table(df$city), col = 3, las = 3, cex.names = 0.6, 
        ylab = "Freqüències absolutes", 
        main = "Diagrama de barres de la variable city") 

#  Creació d'un gràfic de barres de les freqüències del nombre d'habitacions.
barplot(table(df$beds), col = 4, xlab = "Nombre d'habitacions", 
        ylab = "Freqüències absolutes", 
        main = "Diagrama de barres de la variable beds")

#  Creació d'un gràfic de barres de les freqüències del nombre de banys.
barplot(table(df$baths), col = 5, xlab = "Nombre de banys", 
        ylab = "Freqüències absolutes", 
        main = "Diagrama de barres de la variable baths")

#  Creació d'un histograma de freqüències de la superfície.
hist(df$sq__ft, col = 2,freq = F, breaks = 15, 
     main = "Histograma de la variable sq__ft", 
     xlab = "Peus quadrats", ylab = "Freqüències relatives")
lines(density(df$sq__ft), lwd = 2)

#  Creació d'un diagrama de sectors del tipus de vivenda.
pie(table(df$type), col = heat.colors(4), 
    main = "Diagrama de sectors de type", border = NA)

#  Creació d'un histograma de freqüències del preu.
hist(df$price, col = gray.colors(20), 
     seq(40000, 180000, by = 10000), 
     xlab = "Preu", ylab = "Freqüència", 
     main = "Histograma de la variable price")

#  Obrim una nova finestra per als dos darrers gràfics 
#  i establim uns nous paràmetres.
windows (title = "Gràfiques de la descriptiva univariant", 
         width = 7, height = 4)
par(mfrow = c(1, 2), lwd = 1, font = 2, font.lab = 2, font.axis = 2, las = 1)

# Creació d'un gràfic de densitat de la latitud.
plot(density(df$latitude), xlab = "latitud", ylab = "Densitat", 
     main = "Gràfic de densitat de la latitud")
polygon(density(df$latitude), col = "red", border = "blue")

# Creació d'un gràfic de densitat de la longitud.
plot(density(df$longitude), xlab = "longitud", ylab = "Densitat", 
     main = "Gràfic de densitat de la longitud")
polygon(density(df$longitude), col = "blue", border = "red")

#  Tanquem les dues finestres.
graphics.off()


## APARTAT d. 

# Matriu de correlacions:
matR <- cor(df[c(2:4, 6:8)], use = "pairwise.complete.obs")
matR

#  Test de correlació entre superfície i nombre d'habitacions.
cor.test(df$sq__ft, df$beds)

#  Test de correlació entre superfície i preu de la vivenda:
cor.test(df$sq__ft, df$price)

#  Taula de contingència entre la ciutat i el tipus de vivenda.
with(df, table(city, type))

#  Descriptiva del preu mitjà de les cases segons la ciutat i el tipus
#  de vivenda, ordenades de major a menor preu.
sort(tapply(df$price, df$city, mean), decreasing = TRUE)
sort(tapply(df$price, df$type, mean), decreasing = TRUE)

# Creació d'un gràfic bivariant entre el preu i la superfície.
plot(sq__ft~price, data = df, pch = 20, las = 1, 
     xlab = "Preu de la vivenda", ylab = "Superfície (peus quadrats)")
title("Diagrama de punts entre el preu i la superfície")
abline(lm(sq__ft~price, data = df), col = 2)

# Creació d'un gràfic bivariant entre el preu i el nombre de dormitoris.
plot(beds~sq__ft, data = df, pch = 19, col = 2, yaxt = 'n', 
     xlab = "Preu de la vivenda", ylab = "Nombre de dormitoris")
title("Diagrama de punts entre el preu i el nombre de dormitoris")
abline(lm(beds~sq__ft, data = df))
axis(2, at = c(0, 1, 2, 3, 4))

# Creació d'un gràfic bivariant entre la latitud i la longitud.
plot(df$longitude~df$latitude, pch = 4, las = 1, xlab = "Latitud",
     ylab = "Longitud")
title("Diagrama de punts entre la latitud i la longitud")

#  Tanquem els gràfics.
dev.off()
