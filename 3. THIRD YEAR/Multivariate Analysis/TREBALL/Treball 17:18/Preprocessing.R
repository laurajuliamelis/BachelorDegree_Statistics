#####################################################################################
#####                                                                           #####   
#####  PRIMERA SESSIÓ DE PRÀCTIQUES:                                            #####  
#####	 INTRODUCCIÓ AL PAQUET R, RECOLLIDA DE DADES I DEPURACIÓ                  #####
#####                                                                           #####             
#####################################################################################



##### 1. IMPORTAR LES DADES

### 1.1. Canviar el directori del arxiu 

setwd("C:/Users/aleix.fibla/Desktop/multi")

### 1.2. Llegir les dades des de R

dd <- read.csv2("Booking_data.csv",sep=",")
dd$X <- NULL

### 2. CONÈIXER LA BASE DE DADES
# 2.1. Dimensions, estructura i columnes

View(dd) ### Veure la base de dades
class(dd) ### Classe
dim(dd) ### Dimensió
nrow(dd) ### Nombre de files
ncol(dd) ### Nombre de columnes
colnames(dd) ### Nom de les columnes
rownames(dd) ### Nom de les files
str(dd) ### Conèixer l’estructura de la base de dades

#2.2. Descripció de les variables
##https://www-eio.upc.edu/~karina/datamining/refmaterial/Dades/CREDSCO/credscoInfo.pdf

### 3. PRIMER CONTROL: MÀXIMS, MÍNIMS, ..... (ERRORS I VALORS ANÒMALS)
#3.1. Fer un resum
summary(dd)

# Respondre les tres preguntes: 
# 1) Quina és la variable resposta?
# 2) Quines variables són categòriques i quines són continues?
# 3) Hi han missings?

#3.2. Variable resposta 
summary(dd$Review_Positivity_Rate)
hist(dd$Review_Positivity_Rate) #Caldrà transformar-la a numèrica

#3.3. Missings (DE MOMENT NO SÉ QUE S'HA DE FER AQUI)
hist(dd$Ingresos,main=paste("Boxplot of", colnames(dd)[10]))
boxplot(dd$Ingresos, main=paste("Boxplot of", colnames(dd)[10]))
mean(dd$Ingresos)
sd(dd$Ingresos)
cv<-sd(dd$Ingresos)/mean(dd$Ingresos) ### Coefficient de variació
cv

### 4. CATEGORITZAR
#4.1. Identificar les variables categòriques
class(dd[,1])
sapply(dd, class)

#4.2. Declarar com a factor
dd$id <- as.numeric(as.character(dd$id))
dd$Hotel_lat<-as.numeric(as.character(dd$Hotel_lat))
dd$Hotel_lng<-as.numeric(as.character(dd$Hotel_lng))
dd$Businesses_100m<-as.numeric(dd$Businesses_100m)
dd$Businesses_1km<-as.numeric(dd$Businesses_1km)
dd$Businesses_5km<-as.numeric(dd$Businesses_5km)
dd$Stay_Duration<-as.numeric(as.character(dd$Stay_Duration))
dd$Is_Hotel_Holiday <- as.factor(dd$Is_Hotel_Holiday)
dd$Is_Reviewer_Holiday <- as.factor(dd$Is_Reviewer_Holiday)
dd$Total_Number_of_Reviews<-as.numeric(dd$Total_Number_of_Reviews)
dd$Review_Is_Positive <- as.factor(dd$Review_Is_Positive)
dd$Review_Positivity_Rate<-as.numeric(as.character(dd$Review_Positivity_Rate))
dd$Review_Total_Negative_Word_Counts<-as.numeric(as.character(dd$Review_Total_Negative_Word_Counts))
dd$Review_Total_Positive_Word_Counts<-as.numeric(as.character(dd$Review_Total_Positive_Word_Counts))
dd$Average_Score <- as.numeric(as.character(dd$Average_Score))
dd$Reviewer_Score<- as.numeric(as.character(dd$Reviewer_Score))
dd$Total_Number_of_Reviews_Reviewer_Has_Given<-as.numeric(as.character(dd$Total_Number_of_Reviews_Reviewer_Has_Given))
dd$Additional_Number_of_Scoring<-as.numeric(as.character(dd$Additional_Number_of_Scoring))
dd$Submitted_from_Mobile <- as.factor(dd$Submitted_from_Mobile)


#Detalls (a la variable Days_Since_Review cal treure-li "day" , i Review_Date cal passar-la a data )

dd$Days_Since_Review <- gsub("[^0-9]", "", dd$Days_Since_Review)
dd$Days_Since_Review <- as.numeric(dd$Days_Since_Review)

dd$Review_Date<- paste(substr(dd$Review_Date, 1, 5-1), "/", substr(dd$Review_Date, 5, nchar(dd$Review_Date)), sep = "")
dd$Review_Date <- paste(substr(dd$Review_Date, 1, 8-1), "/", substr(dd$Review_Date, 8, nchar(dd$Review_Date)), sep = "")
dd$Review_Date[which(dd$Review_Date ==  "NA/NA/")] <- NA
mode(dd$Review_Date)

dd$Review_Date <- as.Date(dd$Review_Date, "%Y/%m/%d")

### 5. MODALITATS (LEVELS)

#5.1. Revisar les modalitats (Binàries)

levels(dd$Is_Hotel_Holiday) 
levels(dd$Is_Reviewer_Holiday) 
levels(dd$Submitted_from_Mobile)
levels(dd$Review_Is_Positive)


#5.1. Revisar les modealitats (Politòmiques)

levels(dd$Hotel_Country)
    summary(dd$Hotel_Country)
    barplot(table(dd$Hotel_Country))
    pie(table(dd$Hotel_Country))
levels(dd$Hotel_City)
    summary(dd$Hotel_City)
    barplot(table(dd$Hotel_City))
    pie(table(dd$Hotel_City))
levels(dd$Guest_Type)
    summary(dd$Guest_Type)
    barplot(table(dd$Guest_Type))
    pie(table(dd$Guest_Type))
levels(dd$Trip_Type)
    summary(dd$Trip_Type)
    barplot(table(dd$Trip_Type))
    pie(table(dd$Trip_Type))
levels(dd$Room_Type_Level)
    summary(dd$Room_Type_Level)
    barplot(table(dd$Room_Type_Level))
    pie(table(dd$Room_Type_Level))


#5.2. REDEFINIR LES MODALITATS
#5.2. Redefinir les modalitats (Binàries)

levels(dd$Is_Hotel_Holiday) <- c("No","Yes")
levels(dd$Is_Reviewer_Holiday) <- c("No","Yes")
levels(dd$Submitted_from_Mobile) <- c("No","Yes")
levels(dd$Review_Is_Positive) <- c("No","Yes")

#5.2. Redefinir les modalitats (Politòmiques)

### ROOM_TYPE_LEVEL
attach(dd)
dd$Room_Type_Level <- as.factor(as.character(Room_Type_Level))
table(dd$Room_Type_Level)
dd$Room_Type_Level<- factor(Room_Type_Level, ordered=TRUE,  levels= c("Ambassadors","Art","Business","Business Class","City","Classic","Deluxe","Duplex","Executive","Family","Luxury","NULL","Premium","Privilege","Standard","Studio","Suite","Superior"))
newvalues <- c("Deluxe","Deluxe","Business","Business","Classic","Classic","Deluxe","Duplex","Deluxe","Family","Deluxe","Other","Deluxe","Deluxe","Standard","Classic","Deluxe","Deluxe")
dd$Room_Type_Level <- newvalues[ match(dd$Room_Type_Level,
                                    c("Ambassadors","Art","Business","Business Class","City","Classic","Deluxe","Duplex","Executive","Family","Luxury","NULL","Premium","Privilege","Standard","Studio","Suite","Superior"))]
table(dd$Room_Type_Level)
class(dd$Room_Type_Level)
dd$Room_Type_Level <- as.factor(dd$Room_Type_Level)

summary(dd$Room_Type_Level)
pie(table(dd$Room_Type_Level))
barplot(table(dd$Room_Type_Level),las=3,cex.names = 0.7)

### HOTEL_CITY
table(dd$Hotel_City) 
dd$Hotel_City<- factor(dd$Hotel_City, ordered=TRUE,  levels= c("Amsterdam","Amsterdam Zuidoost","Barcelona","Boulogne Billancourt","Donauinsel","El Prat de Llobregat","Fitzrovia","London","Milan","Paddington","Paris","Paris 06","Paris 12","Vienna","Vincennes","Woodford Green"))
newvalues2 <- c("Amsterdam","Amsterdam","Barcelona","Boulogne Billancourt","Vienna","Barcelona","London","London","Milan","London","Paris","Paris","Paris","Vienna","Vincennes","London")
dd$Hotel_City <- newvalues2[ match(dd$Hotel_City,
                                c("Amsterdam","Amsterdam Zuidoost","Barcelona","Boulogne Billancourt","Donauinsel","El Prat de Llobregat","Fitzrovia","London","Milan","Paddington","Paris","Paris 06","Paris 12","Vienna","Vincennes","Woodford Green"))]
table(dd$Hotel_City)
class(dd$Hotel_City)
dd$Hotel_City <- as.factor(dd$Hotel_City)

summary(dd$Hotel_City)
pie(table(dd$Hotel_City))
barplot(table(dd$Hotel_City),las=3,cex.names = 0.7)

#TRIP_TYPE
table(dd$Trip_Type)
dd$Trip_Type<- factor(dd$Trip_Type, ordered=TRUE,  levels= c("Business trip","Couple","Family with older children","Family with young children","Leisure trip","NULL","Solo traveler"))
newvalues3 <- c("Business trip","Couple","Family","Family","Leisure trip","Others","Solo traveler")
dd$Trip_Type <- newvalues3[ match(dd$Trip_Type,
                               c("Business trip","Couple","Family with older children","Family with young children","Leisure trip","NULL","Solo traveler"))]
table(dd$Trip_Type)
class(dd$Trip_Type)
dd$Trip_Type <- as.factor(dd$Trip_Type)

summary(dd$Trip_Type)
pie(table(dd$Trip_Type))
barplot(table(dd$Trip_Type),las=3,cex.names = 0.7)


###GUEST_TYPE

table(dd$Guest_Type)
dd$Guest_Type<- factor(dd$Guest_Type, ordered=TRUE,  levels= c("Couple","Family with older children","Family with young children","Group","Solo traveler","Travelers with friends","With a pet"))
newvalues3 <- c("Couple","Family","Family","Group","Solo traveler","Group","With a pet")
dd$Guest_Type <- newvalues3[ match(dd$Guest_Type,
                                  c("Couple","Family with older children","Family with young children","Group","Solo traveler","Travelers with friends","With a pet"))]
table(dd$Guest_Type)
class(dd$Guest_Type)
dd$Guest_Type <- as.factor(dd$Guest_Type)

summary(dd$Guest_Type)
pie(table(dd$Guest_Type))
barplot(table(dd$Guest_Type),las=3,cex.names = 0.7)


###HOTEL_COUNTRY

table(dd$Hotel_Country)
dd$Hotel_Country<- factor(dd$Hotel_Country, ordered=TRUE,  levels= c("AT","ES","FR","GB","IT","NL"))
newvalues3 <- c("Austria","España","Francia","Gran Bretaña","Italia","Holanda")
dd$Hotel_Country <- newvalues3[ match(dd$Hotel_Country,
                                   c("AT","ES","FR","GB","IT","NL"))]
table(dd$Hotel_Country)
class(dd$Hotel_Country)
dd$Hotel_Country <- as.factor(dd$Hotel_Country)

summary(dd$Hotel_Country)
pie(table(dd$Hotel_Country))
barplot(table(dd$Hotel_Country),las=3,cex.names = 0.7)


## IMPUTACIÓ Missings variables numèriques
## require(BaylorEdPsych)
##  nums <- subset_colclasses(dd, "numeric")

## Test de Little
## LittleMCAR(as.data.frame(nums))$p.value

require(class)

## indexos per a les variables que necessiten imputació

uncompleteVars<-c(5:9)

fullVariables <- c(13, 15, 18, 20, 23, 25, 26, 27, 28, 29)
aux<-dd[,fullVariables]
dim(aux)
names(aux)

for (k in uncompleteVars){
  aux1 <- aux[!is.na(dd[,k]),]
  dim(aux1) 
  aux2 <- aux[is.na(dd[,k]),]
  dim(aux2)
  
  RefValues<- dd[!is.na(dd[,k]),k]
  #Find nns for aux2
  knn.values = knn(aux1,aux2,RefValues)   
  
  #CARE: neither aux1 nor aux2 can contain NAs
  
  
  #CARE: knn.ing is generated as a factor. 
  #Be sure to retrieve the correct values
  
  dd[is.na(dd[,k]),k] = as.numeric(as.character(knn.values))
  fullVariables<-c(fullVariables, k)
  aux<-dd[,fullVariables]
}

dim(dd)
summary(dd)

### FER HISTOGRAMES DE COM QUEDEN LES VARIABLES SENSE MISSING!


### 6. GUARDAR LA BASE PROCESSADA
write.table(dd, file = "Booking_data_preprocessing_bona.csv", sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

### 7. INFORMACIÓ ADICIONAL (TREBALLAR DE MANERA EFICIENT)

# 7.1. Figures 
frecs<-table(dd[,8])
barplot(frecs)
pie(frecs)

# 7.2. Tractar totes les variables categòriques d'una vegada
dcat<- c(1,3,6,7,8)
dd[,dcat]<-lapply(dd[,dcat],factor)
lapply(dd[,dcat],levels)

# 7.3. Fer bucles gràfiques amb parades
varNum<-c(2,4,5,9:14)
par(ask=TRUE)
for(k in varNum){hist(dd[,k], main=paste("Histogram of", names(dd)[k]))}
par(ask=FALSE)

# 7.4. Guardar les gràfiques de sortida a pdf
pdf("informeDescripCredsCo.pdf")
for(k in varNum){hist(dd[,k], main=paste("Histogram of", names(dd)[k]))}
dev.off()

# 7.5. Guardar la sortida de resultats a doc
sink("informeDescripCredsCo.doc")
names(dd)
#Compte les figures no es guarden
hist(dd[,14])
sink()
names(dd)
