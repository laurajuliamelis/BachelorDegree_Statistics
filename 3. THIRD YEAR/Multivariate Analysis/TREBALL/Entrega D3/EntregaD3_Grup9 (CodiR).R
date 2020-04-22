############################
# DESCRIPTIVES UNIVARIANTS #
############################

bbdd <- read.csv("C:/Users/Victor/Downloads/bdd_revisada.csv",sep=",")
# bbdd <- read.csv("bdd_preprocessed.csv") per a la de la bb de dades preprocessada

##### Anàlisi Numeric Univariant abans del preprocessing #####
### 2. Total.Household.Income:
c(summary(bbdd$Total.Household.Income),Desv.Tip.=sqrt(var(bbdd$Total.Household.Income)) )

plot(bbdd$Total.Household.Income,main="Resum Gràfic ",xlab="Observacions",ylab="Ingressos Totals")
lines(1:5000,rep(median(bbdd$Total.Household.Income),5000),col=c("red"))
lines(1:5000,rep(mean(bbdd$Total.Household.Income),5000),col=c("yellow"))

### 3. Region

table(bbdd$Region)
prop.table(table(bbdd$Region))*100


regbarplot <- barplot(table(bbdd$Region),names.arg="")
text(regbarplot, par("usr")[3], labels = names(table(bbdd$Region)), srt = 35, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)

### 4. Total.Food.Expenditure

c(summary(bbdd$Total.Food.Expenditure),Desv.Tip.=sqrt(var(bbdd$Total.Food.Expenditure)))

hist(bbdd$Total.Food.Expenditure,xlab = "Despesa en Alimentació",ylab="Freqüència",main="Distribució de la Despesa en Alimentació")
lines(rep(median(bbdd$Total.Food.Expenditure),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Total.Food.Expenditure),5000),1:5000,col=c("yellow"))
legend(280000,2500,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Total.Food.Expenditure)

### 5. Main.Source.of.Income

table(bbdd$Main.Source.of.Income)
prop.table(table(bbdd$Main.Source.of.Income))*100


pie(table(bbdd$Main.Source.of.Income))

### 6. Agricultural.Household.indicator

table(bbdd$Agricultural.Household.indicator)
prop.table(table(bbdd$Agricultural.Household.indicator))*100

lbls<- paste(c("NS/NC","SI","NO"),prop.table(table(bbdd$Agricultural.Household.indicator))*100)
lbls <- paste(lbls,"%",sep="")

par(oma=c(1,1,1,1),cex.main=0.75)

pie(table(bbdd$Agricultural.Household.indicator),labels=lbls,col=c("grey","green","blue"))
mtext(outer=T,"GRÀFIC DE SECTORS PER INDICADOR DE LLAR AGRÍCOLA",side=3.8,cex=.85)
### 7. Restaurant.and.hotels.Expenditure


c(summary(bbdd$Restaurant.and.hotels.Expenditure),Desv.Tip.=sqrt(var(bbdd$Restaurant.and.hotels.Expenditure)))

hist(bbdd$Restaurant.and.hotels.Expenditure,xlab = "Despesa en Restaurants i Hotels",ylab="Freqüència",main="Distribució de la Despesa en Restaurants i Hotels")
lines(rep(median(bbdd$Restaurant.and.hotels.Expenditure),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Restaurant.and.hotels.Expenditure),5000),1:5000,col=c("yellow"))
legend(150000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Restaurant.and.hotels.Expenditure)


### 8. Alcohol.and.tobacco.expenditure

c(summary(bbdd$Alcohol.and.tobacco.expenditure),Desv.Tip.=sqrt(var(bbdd$Alcohol.and.tobacco.expenditure)))

hist(bbdd$Alcohol.and.tobacco.expenditure,xlab = "Despesa en Alcohol i tabac",ylab="Freqüència",main="Distribució de la Despesa en Alcohol i tabac")
lines(rep(median(bbdd$Alcohol.and.tobacco.expenditure),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Alcohol.and.tobacco.expenditure),5000),1:5000,col=c("yellow"))
legend(30000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Alcohol.and.tobacco.expenditure)


### 9. Clothing..Footwear.and.Other.Wear.Expenditure


c(summary(bbdd$Clothing..Footwear.and.Other.Wear.Expenditure),Desv.Tip.=sqrt(var(bbdd$Clothing..Footwear.and.Other.Wear.Expenditure)))

hist(bbdd$Clothing..Footwear.and.Other.Wear.Expenditure,xlab = "Despesa en Roba",ylab="Freqüència",main="Distribució de la Despesa en Roba")
lines(rep(median(bbdd$Clothing..Footwear.and.Other.Wear.Expenditure),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Clothing..Footwear.and.Other.Wear.Expenditure),5000),1:5000,col=c("yellow"))
legend(30000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Clothing..Footwear.and.Other.Wear.Expenditure)

### 10. Housing.and.water.Expenditure

c(summary(bbdd$Housing.and.water.Expenditure),Desv.Tip.=sqrt(var(bbdd$Housing.and.water.Expenditure)))

hist(bbdd$Housing.and.water.Expenditure,xlab = "Despesa en llar i aigua",ylab="Freqüència",main="Distribució de la Despesa en llar i aigua")
lines(rep(median(bbdd$Housing.and.water.Expenditure),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Housing.and.water.Expenditure),5000),1:5000,col=c("yellow"))
legend(200000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Housing.and.water.Expenditure)

### 11. Imputed.House.Rental.Value


c(summary(bbdd$Housing.and.water.Expenditure),Desv.Tip.=sqrt(var(bbdd$Housing.and.water.Expenditure)))

hist(bbdd$Housing.and.water.Expenditure,xlab = "Despesa en llar i aigua",ylab="Freqüència",main="Distribució de la Despesa en llar i aigua")
lines(rep(median(bbdd$Housing.and.water.Expenditure),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Housing.and.water.Expenditure),5000),1:5000,col=c("yellow"))
legend(200000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Housing.and.water.Expenditure)

### 12. Medical.education.transport.and.communication.expenditure


c(summary(bbdd$Medical.education.transport.and.communication.expenditure),Desv.Tip.=sqrt(var(bbdd$Medical.education.transport.and.communication.expenditure)))

hist(bbdd$Medical.education.transport.and.communication.expenditure,xlab = "Despesa en Educació, transport i comunicació",ylab="Freqüència",main="Distribució de la Despesa en Educació, transport i comunicació")
lines(rep(median(bbdd$Medical.education.transport.and.communication.expenditure),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Medical.education.transport.and.communication.expenditure),5000),1:5000,col=c("yellow"))
legend(400000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Medical.education.transport.and.communication.expenditure)

### 13. Miscellaneous.good.and.special.occasions.expenditure



c(summary(bbdd$Miscellaneous.good.and.special.occasions.expenditure),Desv.Tip.=sqrt(var(bbdd$Miscellaneous.good.and.special.occasions.expenditure)))

hist(bbdd$Miscellaneous.good.and.special.occasions.expenditure,xlab = "Despesa en béns diversos",ylab="Freqüència",main="Distribució de béns diversos")
lines(rep(median(bbdd$Miscellaneous.good.and.special.occasions.expenditure),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Miscellaneous.good.and.special.occasions.expenditure),5000),1:5000,col=c("yellow"))
legend(400000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Miscellaneous.good.and.special.occasions.expenditure)

### 14. Crop.Farming.and.Gardening.expenses



c(summary(bbdd$Crop.Farming.and.Gardening.expenses),Desv.Tip.=sqrt(var(bbdd$Crop.Farming.and.Gardening.expenses)))

hist(bbdd$Crop.Farming.and.Gardening.expenses,xlab = "Despesa en productes de granja i jardineria",ylab="Freqüència",main="Distribució de productes de granja i jardineria")
lines(rep(median(bbdd$Crop.Farming.and.Gardening.expenses),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Crop.Farming.and.Gardening.expenses),5000),1:5000,col=c("yellow"))
legend(300000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Crop.Farming.and.Gardening.expenses)



### 15. Total.Income.from.Entrepeneurial.Activities

c(summary(bbdd$Total.Income.from.Entrepeneurial.Activities),Desv.Tip.=sqrt(var(bbdd$Total.Income.from.Entrepeneurial.Activities)))

hist(bbdd$Total.Income.from.Entrepeneurial.Activities,xlab = "Ingresos d'activitats laborals",ylab="Freqüència",main="Distribució dels ingresos d'activitats laborals")
lines(rep(median(bbdd$Total.Income.from.Entrepeneurial.Activities),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Total.Income.from.Entrepeneurial.Activities),5000),1:5000,col=c("yellow"))
legend(300000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Total.Income.from.Entrepeneurial.Acitivites)

### 16. Household.Head.Sex
table(bbdd$Household.Head.Sex)

prop.table(table(bbdd$Household.Head.Sex))*100

lbls<- paste(c("Dones","Homes"),prop.table(table(bbdd$Household.Head.Sex))*100)
lbls <- paste(lbls,"%",sep="")
pie(table(bbdd$Household.Head.Sex),labels =lbls)



### 17. Household.Head.Age
c(summary(bbdd$Household.Head.Age),Desv.Tip.=sqrt(var(bbdd$Household.Head.Age)))

hist(bbdd$Household.Head.Age,xlab = "Edat del cap de família",ylab="Freqüència",main="Distribució de les edats dels caps de família")
#lines(rep(median(bbdd$Household.Head.Age),5000),1:5000,col=c("red"))
#lines(rep(mean(bbdd$Household.Head.Age),5000),1:5000,col=c("yellow"))
#legend("topright",,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Household.Head.Age) ## Millor!

# Segueix una Normal , wow.

### 18. Household.Head.Marital.Status
table(bbdd$Household.Head.Marital.Status)

prop.table(table(bbdd$Household.Head.Marital.Status))*100

lbls<- paste(c("Nul","Divorciat/da","Casat/da","Solter/a","Desconegut/da","Vidu/a"),prop.table(table(bbdd$Household.Head.Marital.Status))*100)
lbls <- paste(lbls,"%",sep="")
pie(table(bbdd$Household.Head.Marital.Status),labels =lbls)

barplot18 <- barplot(table(bbdd$Household.Head.Marital.Status), names.arg="")
text(barplot18, par("usr")[3], labels = lbls, srt = 35, adj = c(1.1,1.1), xpd = TRUE, cex=0.9)


### 19. Household.Head.Highest.Grade.Completed
table(bbdd$Household.Head.Highest.Grade.Completed)

prop.table(table(bbdd$Household.Head.Highest.Grade.Completed))*100


pie(table(bbdd$Household.Head.Highest.Grade.Completed))

barplot19 <- barplot(table(bbdd$Household.Head.Highest.Grade.Completed), names.arg="")
text(barplot19, par("usr")[3], labels = names(table(bbdd$Household.Head.Highest.Grade.Completed)), srt = 35, adj = c(1.1,1.1), xpd = TRUE, cex=0.9)



### 20. Household.Head.Job.or.Business.Indicator 
table(bbdd$Household.Head.Job.or.Business.Indicator)

prop.table(table(bbdd$Household.Head.Job.or.Business.Indicator))*100

lbls<- paste(c("Sense treball/empresa","Amb treball/empresa"),prop.table(table(bbdd$Household.Head.Job.or.Business.Indicator))*100)
lbls <- paste(lbls,"%",sep="")
pie(table(bbdd$Household.Head.Job.or.Business.Indicator),labels =lbls)


### 21. Household.Head.Occupation
table(bbdd$Household.Head.Occupation)

prop.table(table(bbdd$Household.Head.Occupation))*100


pie(table(bbdd$Household.Head.Occupation))

barplot21 <- barplot(table(bbdd$Household.Head.Occupation), names.arg="")
text(barplot21, par("usr")[3], labels = names(table(bbdd$Household.Head.Occupation)), srt = 35, adj = c(1.1,1.1), xpd = TRUE, cex=0.9)


### 22. Household.Head.Class.of.Worker
table(bbdd$Household.Head.Class.of.Worker)

prop.table(table(bbdd$Household.Head.Class.of.Worker))*100

lbls<- paste(c("Empleat/da empresa familiar","Autònom/a","Funcionari/a","Empleat/da","Empleat/da de la llar","Assalariat/da en empresa familiar","Voluntari/a en empresa familiar"),round(prop.table(table(bbdd$Household.Head.Class.of.Worker))*100,2))
lbls <- paste(lbls,"%",sep="")
pie(table(bbdd$Household.Head.Class.of.Worker),labels =lbls)

barplot22 <- barplot(table(bbdd$Household.Head.Class.of.Worker), names.arg="")
text(barplot22, par("usr")[3], labels = lbls, srt = 35, adj = c(1.1,1.1), xpd = TRUE, cex=0.9)


### 23. Total.Number.of.Family.members
c(summary(bbdd$Total.Number.of.Family.members),Desv.Tip.=sqrt(var(bbdd$Total.Number.of.Family.members)))

hist(bbdd$Total.Number.of.Family.members,xlab = "Edat del cap de família",ylab="Freqüència",main="Distribució de les edats dels caps de família")
lines(rep(median(bbdd$Total.Number.of.Family.members),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Total.Number.of.Family.members),5000),1:5000,col=c("yellow"))
legend("topright",,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Total.Number.of.Family.members)

### 24. Members.with.age.less.than.5.year.old
table(bbdd$Members.with.age.less.than.5.year.old)

prop.table(table(bbdd$Members.with.age.less.than.5.year.old))*100

lbls<- paste(c("1.","2.","3.","4.","5."),round(prop.table(table(bbdd$Total.Number.of.Family.members))*100,2))
lbls <- paste(lbls,"%",sep="")
barplot24 <- barplot(table(bbdd$Members.with.age.less.than.5.year.old), names.arg="")
text(barplot24, par("usr")[3], labels = lbls, srt = 35, adj = c(1.1,1.1), xpd = TRUE, cex=0.9)

### 25. Members.with.age.5...17.years.old
table(bbdd$Members.with.age.5...17.years.old)

prop.table(table(bbdd$Members.with.age.5...17.years.old))*100

lbls<- paste(c("0.","1.","2.","3.","4.","5.","6.","7.","8."),round(prop.table(table(bbdd$Members.with.age.5...17.years.old))*100,2))
lbls <- paste(lbls,"%",sep="")
barplot25 <- barplot(table(bbdd$Members.with.age.5...17.years.old), names.arg="")
text(barplot25, par("usr")[3], labels = lbls, srt = 35, adj = c(1.1,1.1), xpd = TRUE, cex=0.9)

### 26. Total.number.of.family.members.employed
table(bbdd$Total.number.of.family.members.employed)

prop.table(table(bbdd$Total.number.of.family.members.employed))*100

lbls<- paste(c("0.","1.","2.","3.","4.","5.","6.","7.","8."),round(prop.table(table(bbdd$Total.number.of.family.members.employed))*100,2))
lbls <- paste(lbls,"%",sep="")
barplot26 <- barplot(table(bbdd$Total.number.of.family.members.employed), names.arg="")
text(barplot26, par("usr")[3], labels = lbls, srt = 35, adj = c(1.1,1.1), xpd = TRUE, cex=0.9)

### 27. Type.of.Building.House
table(bbdd$Type.of.Building.House)

prop.table(table(bbdd$Type.of.Building.House))*100

lbls<- paste(c("Edifici Industrial/Agricola","Duplex","Barri d'empresa","Residencia","Casa Unifamiliar"),round(prop.table(table(bbdd$Type.of.Building.House))*100,2))
lbls <- paste(lbls,"%",sep="")
barplot27 <- barplot(table(bbdd$Type.of.Building.House), names.arg="")
text(barplot27, par("usr")[3], labels = lbls, srt = 35, adj = c(1.1,1.1), xpd = TRUE, cex=0.5)

### 28. House.Floor.Area
c(summary(bbdd$House.Floor.Area),Desv.Tip.=sqrt(var(bbdd$House.Floor.Area)))

hist(bbdd$House.Floor.Area,xlab = "Superfície del habitatge",ylab="Freqüència",main="Distribució de la superficie dels habitatges")
lines(rep(median(bbdd$House.Floor.Area),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$House.Floor.Area),5000),1:5000,col=c("yellow"))
legend(300,3000,c("Mediana","Mitjana"),c("red","yellow"))


### 29. House.Age
c(summary(bbdd$House.Age),Desv.Tip.=sqrt(var(bbdd$House.Age)))

hist(bbdd$House.Age,xlab = "Anys de la Casa",ylab="Freqüència",main="Distribució dels anys de les cases")
lines(rep(median(bbdd$House.Age),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$House.Age),5000),1:5000,col=c("yellow"))
legend(60,2000,c("Mediana","Mitjana"),c("red","yellow"))

### 30. Number.of.bedrooms
table(bbdd$Number.of.bedrooms)
prop.table(table(bbdd$Number.of.bedrooms))*100
barplot(table(bbdd$Number.of.bedrooms))



### 31. Electricity
table(bbdd$Electricity)

prop.table(table(bbdd$Electricity))*100

lbls<- paste(c("No Electricitat","Electricitat"),prop.table(table(bbdd$Electricity))*100)
lbls <- paste(lbls,"%",sep="")
pie(table(bbdd$Electricity),labels =lbls)

### 32. Number.of.Car..Jeep..Van
table(bbdd$Number.of.Car..Jeep..Van)
prop.table(table(bbdd$Number.of.Car..Jeep..Van))*100

barplot(table(bbdd$Number.of.Car..Jeep..Van))

### 33. Number.of.Cellular.phone
table(bbdd$Number.of.Cellular.phone)

prop.table(table(bbdd$Number.of.Cellular.phone))*100

barplot(table(bbdd$Number.of.Cellular.phone))

### 34. Number.of.Motorcycle.or.Tricycle
table(bbdd$Number.of.Motorcycle.Tricycle)
prop.table(table(bbdd$Number.of.Motorcycle.Tricycle))*100
barplot(table(bbdd$Number.of.Motorcycle.Tricycle))


##################
# PRE PROCESSING #
##################

### 1. Importació de Dades

bbdd <- read.csv("bdd_final_revisada.csv",sep=",")
View(bbdd)

### 2. Dimensions BDD 

class(bbdd) 
dim(bbdd) 
nrow(bbdd) 
ncol(bbdd) 
colnames(bbdd) 
str(bbdd) 

### 3. Primer Control

summary(bbdd)

# La variable resposta és el Total.Household.Income

summary(bbdd$Total.Household.Income)
hist(bbdd$Total.Household.Income)

# Només les variables Household.Head.Class.of.Worker i Household.Head.Occupation dónen NA's, i ho fan en les mateixes entries. 

### 4. Categoritzar

sapply(bbdd, class)

# Agricultural.Household.indicator surt com a intenger però ha de ser categòrica.
# La resta de variables són ja la class que els correspon.

bbdd$Agricultural.Household.indicator <- as.factor(bbdd$Agricultural.Household.indicator)
bbdd$Electricity <- as.factor(bbdd$Electricity)
levels(bbdd$Agricultural.Household.indicator)

### 5. Modalitats 

# Amb la funció str() s'ha vist que les variables Household.Head.Occupation i Household.Head.Highest.Grade.Completed 
# tenen 243 i 46 levels respectivament. Es procedeix a remodelar aquestes dues variables. 

## Variable Household.Head.Highest.Grade.Completed

for (i in 1:nrow(bbdd)){
  
  if (bbdd$Household.Head.Highest.Grade.Completed[i]== "High School Graduate" || bbdd$Household.Head.Highest.Grade.Completed[i]== "No Grade Completed" 
      || bbdd$Household.Head.Highest.Grade.Completed[i]== "First Year College" || bbdd$Household.Head.Highest.Grade.Completed[i]== "Second Year College" 
      || bbdd$Household.Head.Highest.Grade.Completed[i]== "Third Year College" || bbdd$Household.Head.Highest.Grade.Completed[i]== "Fourth Year College"){
    
    bbdd$Household.Head.Highest.Grade.Completed.new[i]= "High School Studies"
    
    
  }else if (bbdd$Household.Head.Highest.Grade.Completed[i]== "Elementary Graduate" ||  bbdd$Household.Head.Highest.Grade.Completed[i]== "First Year High School" 
            || bbdd$Household.Head.Highest.Grade.Completed[i]== "Second Year High School" 
            || bbdd$Household.Head.Highest.Grade.Completed[i]== "Third Year High School" 
            || bbdd$Household.Head.Highest.Grade.Completed[i]== "Fourth Year High School") {
    
    bbdd$Household.Head.Highest.Grade.Completed.new[i]="Elementary Studies"
    
    
  }else if (bbdd$Household.Head.Highest.Grade.Completed[i]== "Agriculture" || bbdd$Household.Head.Highest.Grade.Completed[i]== "Preschool" || bbdd$Household.Head.Highest.Grade.Completed[i]== "Grade 1" ||  bbdd$Household.Head.Highest.Grade.Completed[i]== "Grade 2" 
            || bbdd$Household.Head.Highest.Grade.Completed[i]== "Grade 3" || bbdd$Household.Head.Highest.Grade.Completed[i]=="Grade 4" 
            || bbdd$Household.Head.Highest.Grade.Completed[i]== "Grade 5" || bbdd$Household.Head.Highest.Grade.Completed[i]== "Grade 6") {
    
    
    bbdd$Household.Head.Highest.Grade.Completed.new[i]="No Studies"
    
  } else {
    
    bbdd$Household.Head.Highest.Grade.Completed.new[i]= "Program Studies"
    
    
  }
  
}

table(bbdd$Household.Head.Highest.Grade.Completed.new)


## Variable Household.Head.Occupation


for (i in 1:nrow(bbdd)){
  
  
  
  if (is.na(bbdd$Household.Head.Occupation[i]) || bbdd$Household.Head.Occupation[i]== " "){
    
    bbdd$Household.Head.Occupation.new[i] <- NA
    
  } else if (bbdd$Household.Head.Occupation[i]== "Rice farmers" || bbdd$Household.Head.Occupation[i]== "Root croops farmers" 
             || bbdd$Household.Head.Occupation[i]== "Forestry laborers" || bbdd$Household.Head.Occupation[i]== "Fishery laborers and helpers" 
             || bbdd$Household.Head.Occupation[i]== "Corn farmers" || bbdd$Household.Head.Occupation[i]== "Coffee and cacao farmers" 
             || bbdd$Household.Head.Occupation[i]== "Fruit" || bbdd$Household.Head.Occupation[i]== "Garbage collectors" 
             || bbdd$Household.Head.Occupation[i]== " Mining and quarrying laborers" 
             || bbdd$Household.Head.Occupation[i]== "Motorized farm and forestry plant operators" 
             || bbdd$Household.Head.Occupation[i]== "Other plant growers" || bbdd$Household.Head.Occupation[i]== "Other livestock farmers" 
             || bbdd$Household.Head.Occupation[i]== "Other field crop farmers" || bbdd$Household.Head.Occupation[i]== "Cattle and dairy farmers" 
             || bbdd$Household.Head.Occupation[i]== "Chicken farmers" || bbdd$Household.Head.Occupation[i]== "Coconut farmers" 
             || bbdd$Household.Head.Occupation[i]== "Cotton and fiber crops farmers" || bbdd$Household.Head.Occupation[i]== "Deep-sea fishermen" 
             || bbdd$Household.Head.Occupation[i]== "Field legumes farmers" || bbdd$Household.Head.Occupation[i]== "Forest tree planters" 
             || bbdd$Household.Head.Occupation[i]== "Fruit tree farmers" || bbdd$Household.Head.Occupation[i]== "Hog raising farmers" 
             || bbdd$Household.Head.Occupation[i]== "Inland and coastal waters fishermen" || bbdd$Household.Head.Occupation[i]== "Miners and quarry workers" 
             || bbdd$Household.Head.Occupation[i]== "Minor forest products gatherers" || bbdd$Household.Head.Occupation[i]== "Ornamental plant growers" 
             || bbdd$Household.Head.Occupation[i]== "Other aqua products cultivators" || bbdd$Household.Head.Occupation[i]== "Other orchard farmers" 
             || bbdd$Household.Head.Occupation[i]== "Other poultry farmers" || bbdd$Household.Head.Occupation[i]== "Production and operations managers in agriculture" 
             || bbdd$Household.Head.Occupation[i]== "Seaweeds cultivators" || bbdd$Household.Head.Occupation[i]== "Sugarcane farmers" 
             || bbdd$Household.Head.Occupation[i]== "Vegetable farmers" || bbdd$Household.Head.Occupation[i]== "Wood processing plant operators"
             || bbdd$Household.Head.Occupation[i]== "Farmhands and laborers" || bbdd$Household.Head.Occupation[i]== "Tree nut farmers"){
    
    bbdd$Household.Head.Occupation.new[i]= "Primary Sector"
    
    
  }else if (bbdd$Household.Head.Occupation[i]== "Brewers and wine and other beverage machine operators" 
            ||  bbdd$Household.Head.Occupation[i]== "Building and related electricians" 
            || bbdd$Household.Head.Occupation[i]== "Building construction laborers" 
            || bbdd$Household.Head.Occupation[i]== "Cement and other mineral products machine operators" 
            || bbdd$Household.Head.Occupation[i]== "Fishery laborers and helpers" 
            || bbdd$Household.Head.Occupation[i]== "Industrial robot operators" || bbdd$Household.Head.Occupation[i]== "Metal finishing" 
            || bbdd$Household.Head.Occupation[i]== "Metal finishing" || bbdd$Household.Head.Occupation[i]== "Wood and related products assemblers" 
            || bbdd$Household.Head.Occupation[i]== "Wood products machine operators" 
            || bbdd$Household.Head.Occupation[i]== "Earth-moving and related plant operators" 
            || bbdd$Household.Head.Occupation[i]== "Freight handlers" || bbdd$Household.Head.Occupation[i]== "Hand launderers and pressers" 
            || bbdd$Household.Head.Occupation[i]== "Marine craft mechanics" || bbdd$Household.Head.Occupation[i]== "Masons and related concrete finishers" 
            || bbdd$Household.Head.Occupation[i]== "Metal" || bbdd$Household.Head.Occupation[i]== "Production and operations managers in construction" 
            || bbdd$Household.Head.Occupation[i]== "Sheet-metal workers" || bbdd$Household.Head.Occupation[i]== "Textile"
            || bbdd$Household.Head.Occupation[i]== "Sewers" || bbdd$Household.Head.Occupation[i]== "Weavers") {
    
    bbdd$Household.Head.Occupation.new[i]="Secondary Sector"
    
    
  }else {
    
    
    bbdd$Household.Head.Occupation.new[i]="Tertiary Sector"
    
    
    
    
    
    
  }
  
}

table(bbdd$Household.Head.Occupation.new)


## A continuació posem els nous valors a les variables corresponents i 
## s'eliminen les variables provisionals que s'havien fet amb anterioritat

bbdd$Household.Head.Highest.Grade.Completed <- as.factor(bbdd$Household.Head.Highest.Grade.Completed.new)
bbdd$Household.Head.Occupation <- as.factor(bbdd$Household.Head.Occupation.new) 
bbdd$Household.Head.Highest.Grade.Completed.new <- NULL
bbdd$Household.Head.Occupation.new <- NULL

str(bbdd)


### 6. Tractament de Missings
bd_NA <-bbdd[NAS,] # ens quedem només amb les files que tenen missings
head(bd_NA)  # A simple vista sembla que totes les files amb missings corresponen a individus que no treballen

# Ho confirmem amb el següent gràfic:
tab<- table(bd_NA$Household.Head.Job.or.Business.Indicator)

par(oma=c(1,1,1,1),cex.main=0.75)
bp<-barplot(tab, main="Distribució NA's")
text(bp, 0, round(tab, 1), cex=1, pos=3)
# Procedim a codificar-les com a un level de més:
# "No Occupation" per a Household.Head.Occupation i "Unemployed" per a Household.Head.Class.of.Worker.

levels(bbdd$Household.Head.Class.of.Worker)<-c(levels(bbdd$Household.Head.Class.of.Worker),"Unemployed")
bbdd$Household.Head.Class.of.Worker[is.na(bbdd$Household.Head.Class.of.Worker)] <- "Unemployed"

levels(bbdd$Household.Head.Occupation)<-c(levels(bbdd$Household.Head.Occupation),c("No Occupation"))
bbdd$Household.Head.Occupation[is.na(bbdd$Household.Head.Occupation)] <- c("No Occupation")

### 7. Escrivim la nova base de dades ja processada

write.csv(bbdd, file= "bdd_preprocessed.csv", row.names = FALSE)


###################
# CODI CLUSTERING #
###################
setwd("C:/Users/laura.julia/Desktop")
dd <- read.csv("bdd_preprocessed.csv")
dim(dd)
summary(dd)
attach(dd)

library(cluster) # CLUSTERING JERÀRQUIC

# Dissimilarity matrix
actives<-c(1:34) # variables que volem utilitzar
n <- 5000 # nombre d'observacions
filtro<- c(1:n) # totes

dissimMatrix <- daisy(dd[filtro,actives], metric = "gower", stand=TRUE) # calculem matriu de distàncies utilitzant mètode de gower
distMatrix<-dissimMatrix^2 # matriu de distàncies nova

# Mètode de ward "ward.D2" important!!!!
h1 <- hclust(distMatrix,method="ward.D2")  # NOTICE THE COST
plot(h1) # Dendograma

k <- 4 #mirar el gràfic per decidir-ho
c2 <- cutree(h1,k)  #cutree fa talls a l'arbre d'hclust i genera una columna
dd[,35]<-c2 #afegim la columna identificadora del cluster a la base de dades

table(c2) #class sizes, podem veure si les classes estan equilibrades

##################
# CODI PROFILING #
##################
dades<-dd #dades contain the dataset
dades[,6]<- as.factor(dades[,6])
dades[,31]<- as.factor(dades[,31])
K<-dim(dades)[2] # nombre de variables
par(ask=TRUE, cex.main=0.75) # per a que en le bucle de després vagi fent les coses poc a poc
P<-dd[,35] # la última variable (creada en el clustering) és la variable de classe, ara P.
nc<-length(levels(factor(P)))
nameP<-"Class"

## 1. Tests per veure la significació de les variables ente clústers.
for(k in 1:K){
  if (sapply(dd,class)[k] == "integer"){ 
    print(paste("Anàlisi per classes de la Variable:", names(dades)[k]))
    o<-oneway.test(dades[,k]~P)
    print(paste("p-valueANOVA:", o$p.value))
    kw<-kruskal.test(dades[,k]~P)
    print(paste("p-value Kruskal-Wallis:", kw$p.value))
  }else{
    #qualitatives
    print(paste("Variable qualitativa", names(dades)[k]))
    
    print("Test Chi quadrat: ")
    print(chisq.test(dades[,k], as.factor(P))$p.value)
  }
}

## 2. Mètodes gràfics per al profiling
for(k in 2:34){
  if (is.numeric(dades[,k])){ 
    print(paste("Anàlisi per classes de la Variable:", names(dades)[k]))
    boxplot(dades[,k]~P, main=paste("Boxplot of", names(dades)[k], "vs", nameP ), horizontal=TRUE)
    
    barplot(tapply(dades[[k]], P, mean),main=paste("Means of", names(dades)[k], "by", nameP ))
    abline(h=mean(dades[[k]]))
    legend(0,mean(dades[[k]]),"global mean",bty="n")
  }else{
    print(paste("Variable", names(dades)[k])) # qualitatives
    table<-table(P,dades[,k])
    rowperc<-prop.table(table,1)
    colperc<-prop.table(table,2)
    dades[,k]<-as.factor(dades[,k])
    
    marg <- table(as.factor(P))/n
    print(append("Categories=",levels(as.factor(dades[,k]))))
    
    # Snake plot
    plot(marg,type="l",ylim=c(0,1),main=paste("Snake plot of",names(dades)[k]))
    paleta<-rainbow(length(levels(dades[,k])))
    for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
    legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
    
    #diagrames de barres apilades                                         
    paleta<-rainbow(length(levels(dades[,k])))
    barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta,main=paste("Means of", names(dades)[k]))
    legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
  }
}
