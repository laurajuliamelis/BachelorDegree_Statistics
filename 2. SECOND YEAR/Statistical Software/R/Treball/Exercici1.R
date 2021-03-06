#########################################
###  Software Estad�stic, 18.12.2016  ###
###       Pr�ctica final amb R        ###
###      NOM: Laura Juli� Melis       ###
###         NIUB: 16810883            ###
#########################################

## EXERCICI 1.
## -----------

## APARTAT b.

#  Importaci� de les dades.
df <- read.table("SALES.csv", header = TRUE, sep = ';', nrows = 50)
df <- transform(df, zip = NULL, state = NULL, sale_date = NULL)
head(df)
class(df)

#  Substituci� del nom de cada registre per la variable identificadora street.
df <- transform (df, row.names = street, street = NULL)
str(df)

#  Introducci� de dades faltants aleat�riament.
df$beds[sample(1:50, 1)] <- NA
df$baths[sample(1:50, 2)] <- NA



##  APARTAT c. 
#  Resum num�ric de totes les variables del data frame.
summary(df)

#  Obrim una finestra per als gr�fics i establim els seus par�metres.
windows (title = "Gr�fiques de la descriptiva univariant", 
         width = 10, height = 5)
par(mfrow = c(2, 3), lwd = 1, font = 2, font.lab = 2, font.axis = 2, las = 1)

#  Creaci� d'un gr�fic de barres de les freq��ncies de les ciutats 
#  on es troben les vivendes.
barplot(table(df$city), col = 3, las = 3, cex.names = 0.6, 
        ylab = "Freq��ncies absolutes", 
        main = "Diagrama de barres de la variable city") 

#  Creaci� d'un gr�fic de barres de les freq��ncies del nombre d'habitacions.
barplot(table(df$beds), col = 4, xlab = "Nombre d'habitacions", 
        ylab = "Freq��ncies absolutes", 
        main = "Diagrama de barres de la variable beds")

#  Creaci� d'un gr�fic de barres de les freq��ncies del nombre de banys.
barplot(table(df$baths), col = 5, xlab = "Nombre de banys", 
        ylab = "Freq��ncies absolutes", 
        main = "Diagrama de barres de la variable baths")

#  Creaci� d'un histograma de freq��ncies de la superf�cie.
hist(df$sq__ft, col = 2,freq = F, breaks = 15, 
     main = "Histograma de la variable sq__ft", 
     xlab = "Peus quadrats", ylab = "Freq��ncies relatives")
lines(density(df$sq__ft), lwd = 2)

#  Creaci� d'un diagrama de sectors del tipus de vivenda.
pie(table(df$type), col = heat.colors(4), 
    main = "Diagrama de sectors de type", border = NA)

#  Creaci� d'un histograma de freq��ncies del preu.
hist(df$price, col = gray.colors(20), 
     seq(40000, 180000, by = 10000), 
     xlab = "Preu", ylab = "Freq��ncia", 
     main = "Histograma de la variable price")

#  Obrim una nova finestra per als dos darrers gr�fics 
#  i establim uns nous par�metres.
windows (title = "Gr�fiques de la descriptiva univariant", 
         width = 7, height = 4)
par(mfrow = c(1, 2), lwd = 1, font = 2, font.lab = 2, font.axis = 2, las = 1)

# Creaci� d'un gr�fic de densitat de la latitud.
plot(density(df$latitude), xlab = "latitud", ylab = "Densitat", 
     main = "Gr�fic de densitat de la latitud")
polygon(density(df$latitude), col = "red", border = "blue")

# Creaci� d'un gr�fic de densitat de la longitud.
plot(density(df$longitude), xlab = "longitud", ylab = "Densitat", 
     main = "Gr�fic de densitat de la longitud")
polygon(density(df$longitude), col = "blue", border = "red")

#  Tanquem les dues finestres.
graphics.off()


## APARTAT d. 

# Matriu de correlacions:
matR <- cor(df[c(2:4, 6:8)], use = "pairwise.complete.obs")
matR

#  Test de correlaci� entre superf�cie i nombre d'habitacions.
cor.test(df$sq__ft, df$beds)

#  Test de correlaci� entre superf�cie i preu de la vivenda:
cor.test(df$sq__ft, df$price)

#  Taula de conting�ncia entre la ciutat i el tipus de vivenda.
with(df, table(city, type))

#  Descriptiva del preu mitj� de les cases segons la ciutat i el tipus
#  de vivenda, ordenades de major a menor preu.
sort(tapply(df$price, df$city, mean), decreasing = TRUE)
sort(tapply(df$price, df$type, mean), decreasing = TRUE)

# Creaci� d'un gr�fic bivariant entre el preu i la superf�cie.
plot(sq__ft~price, data = df, pch = 20, las = 1, 
     xlab = "Preu de la vivenda", ylab = "Superf�cie (peus quadrats)")
title("Diagrama de punts entre el preu i la superf�cie")
abline(lm(sq__ft~price, data = df), col = 2)

# Creaci� d'un gr�fic bivariant entre el preu i el nombre de dormitoris.
plot(beds~sq__ft, data = df, pch = 19, col = 2, yaxt = 'n', 
     xlab = "Preu de la vivenda", ylab = "Nombre de dormitoris")
title("Diagrama de punts entre el preu i el nombre de dormitoris")
abline(lm(beds~sq__ft, data = df))
axis(2, at = c(0, 1, 2, 3, 4))

# Creaci� d'un gr�fic bivariant entre la latitud i la longitud.
plot(df$longitude~df$latitude, pch = 4, las = 1, xlab = "Latitud",
     ylab = "Longitud")
title("Diagrama de punts entre la latitud i la longitud")

#  Tanquem els gr�fics.
dev.off()
