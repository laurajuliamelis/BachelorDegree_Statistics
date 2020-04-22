#####################################################################################
#####                                                                           #####   
#####  PRIMERA SESSIÓ DE PRÀCTIQUES:                                            #####  
#####	 INTRODUCCIÓ AL PAQUET R, RECOLLIDA DE DADES I DEPURACIÓ                  #####
#####                                                                           #####             
#####################################################################################

##### 0. CONEIXEM R? 
#https://cran.r-project.org/

##### 1. IMPORTAR LES DADES

### 1.1. Canviar el directori del arxiu 
# Utilitzant el menú de R: File > Change dir...
setwd("C:/Users/Desktop")

### 1.2. Llegir les dades des de R
# Per la pràctica treballarem amb les dades CREDSCO.CSV
?read.table
dd <- read.table("credsco.csv")
dd <- read.table("credsco.csv",sep=";")
dd <- read.table("credsco.csv",header=T,sep=";")

# Si coneixem l'estructura, podem seleccionar directament el format més adequat
dd <- read.csv2("credsco.csv")

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
##credscoInfo.pdf

### 3. PRIMER CONTROL: MÀXIMS, MÍNIMS, ..... (ERRORS I VALORS ANÒMALS)
#3.1. Fer un resum
summary(dd)

# Respondre les tres preguntes: 
# 1) Quina és la variable principal?
# 2) Quines variables són categòriques i quines són continues?
# 3) Hi han missings?

#3.2. Variable principal 
summary(dd$Dictamen)
hist(dd$Dictamen)

#3.3. Missings
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
summary(dd$Dictamen)
dd$Dictamen <- as.factor(dd$Dictamen)
summary(dd$Dictamen)
mean(dd$Dictamen)
class(dd$Dictamen)

dd[dd$Dictamen==0,] # Individu que té valor missing

colnames(dd)
dd$Vivienda<-factor(dd$Vivienda)
dd$Estado.civil<-factor(dd$Estado.civil)
dd$Registros<-factor(dd$Registros)
dd$Tipo.trabajo<-factor(dd$Tipo.trabajo)

for(i in c(1,3,6,7,8)) dd[,i]<-as.factor(dd[,i])

### 5. MODALITATS
#5.1. Revisar les modalitats
levels(dd$Dictamen)
levels(dd$Vivienda)
levels(dd$Estado.civil)
levels(dd$Registros)
levels(dd$Tipo.trabajo)
sapply(dd[,1:14],levels)
summary(dd)

summary(dd$Dictamen)
barplot(table(dd$Dictamen))
pie(table(dd$Dictamen))
pie(table(dd$Vivienda))

#5.2. Redefinir les modalitats
#Cal redefinir les modalitats quan cal

levels(dd$Dictamen) <- c(NA, "positiu","negatiu")
table(dd$Dictamen)
table(dd$Dictamen, useNA="ifany")

levels(dd$Vivienda) <- c("VivUnkown", "lloguer","escriptura","contr_privat","ignora_cont","pares","altres viv")
pie(table(dd$Vivienda))

levels(dd$Estado.civil) <- c("ECUnknown", "solter","casat","vidu","separat","divorciat")
levels(dd$Registros) <- c("reg_no","reg_si")

#5.3. Ordenar les modalitats
summary(dd$Tipo.trabajo)
#levels(dd$Tipo.trabajo) <- c("WorkingTypeUnknown","fixe","temporal","autonom","altres sit")
barplot(table(dd$Tipo.trabajo),names.arg=c("WorkingTypeUnknown","fixe","temporal","autonom","altres sit"))
dd$Tipo.trabajo<-factor(dd$Tipo.trabajo, levels=c( "1", "2", "3", "4", "0"), labels=c("fixe","temporal","autonom","altres sit", "WorkingTypeUnknown"))
barplot(table(dd$Tipo.trabajo))

#5.4. Reordenar les modalitats
dd$Tipo.trabajo <- factor(dd$Tipo.trabajo, ordered=TRUE,  levels= c("WorkingTypeUnknown","temporal","fixe","autonom","altres sit"))
summary(dd$Tipo.trabajo)
barplot(table(dd$Tipo.trabajo))

#5.5. Recodificacions
table(dd$Tipo.trabajo)
newvalues<-c("Other","Fix","Other","Auto","Other")
dd$Tipo.trabajo <- newvalues[match(dd$Tipo.trabajo,c("WorkingTypeUnknown","fixe","temporal","autonom","altres sit"))]
table(dd$Tipo.trabajo)
frecs<-table(dd$Tipo.trabajo)
barplot(frecs, las=3, cex.names=0.7, main=paste("Barplot of", "Tipo.trabajo"))
summary(dd$Tipo.trabajo)
dd$Tipo.trabajo<-as.factor(dd$Tipo.trabajo)

### 6. GUARDAR LA BASE PROCESSADA
write.table(dd, file = "credscoCategoriques.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

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
