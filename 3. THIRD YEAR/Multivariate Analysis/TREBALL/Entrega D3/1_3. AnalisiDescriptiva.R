############# Agrupaci√≥ i categoritzaci√≥ de les variables Household.Head.Highest.Grade.Completed i Household.Head.Occupation ############


bbdd <- read.csv("C:/Users/Victor/Downloads/bdd_revisada.csv",sep=",")


##### An‡lisi Numeric Univariant abans del preprocessing #####

### 2. Total.Household.Income:

c(summary(bbdd$Total.Household.Income),Desv.Tip.=sqrt(var(bbdd$Total.Household.Income)) )


plot(bbdd$Total.Household.Income,main="Resum Gr‡fic ",xlab="Observacions",ylab="Ingressos Totals")
lines(1:5000,rep(median(bbdd$Total.Household.Income),5000),col=c("red"))
lines(1:5000,rep(mean(bbdd$Total.Household.Income),5000),col=c("yellow"))



### 3. Region

table(bbdd$Region)
prop.table(table(bbdd$Region))*100


regbarplot <- barplot(table(bbdd$Region),names.arg="")
text(regbarplot, par("usr")[3], labels = names(table(bbdd$Region)), srt = 35, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)

### 4. Total.Food.Expenditure

c(summary(bbdd$Total.Food.Expenditure),Desv.Tip.=sqrt(var(bbdd$Total.Food.Expenditure)))

hist(bbdd$Total.Food.Expenditure,xlab = "Despesa en AlimentaciÛ",ylab="Freq¸Ëncia",main="DistribuciÛ de la Despesa en AlimentaciÛ")
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
mtext(outer=T,"GR¿FIC DE SECTORS PER INDICADOR DE LLAR AGRÕCOLA",side=3.8,cex=.85)
### 7. Restaurant.and.hotels.Expenditure


c(summary(bbdd$Restaurant.and.hotels.Expenditure),Desv.Tip.=sqrt(var(bbdd$Restaurant.and.hotels.Expenditure)))

hist(bbdd$Restaurant.and.hotels.Expenditure,xlab = "Despesa en Restaurants i Hotels",ylab="Freq¸Ëncia",main="DistribuciÛ de la Despesa en Restaurants i Hotels")
lines(rep(median(bbdd$Restaurant.and.hotels.Expenditure),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Restaurant.and.hotels.Expenditure),5000),1:5000,col=c("yellow"))
legend(150000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Restaurant.and.hotels.Expenditure)


### 8. Alcohol.and.tobacco.expenditure

c(summary(bbdd$Alcohol.and.tobacco.expenditure),Desv.Tip.=sqrt(var(bbdd$Alcohol.and.tobacco.expenditure)))

hist(bbdd$Alcohol.and.tobacco.expenditure,xlab = "Despesa en Alcohol i tabac",ylab="Freq¸Ëncia",main="DistribuciÛ de la Despesa en Alcohol i tabac")
lines(rep(median(bbdd$Alcohol.and.tobacco.expenditure),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Alcohol.and.tobacco.expenditure),5000),1:5000,col=c("yellow"))
legend(30000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Alcohol.and.tobacco.expenditure)


### 9. Clothing..Footwear.and.Other.Wear.Expenditure


c(summary(bbdd$Clothing..Footwear.and.Other.Wear.Expenditure),Desv.Tip.=sqrt(var(bbdd$Clothing..Footwear.and.Other.Wear.Expenditure)))

hist(bbdd$Clothing..Footwear.and.Other.Wear.Expenditure,xlab = "Despesa en Roba",ylab="Freq¸Ëncia",main="DistribuciÛ de la Despesa en Roba")
lines(rep(median(bbdd$Clothing..Footwear.and.Other.Wear.Expenditure),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Clothing..Footwear.and.Other.Wear.Expenditure),5000),1:5000,col=c("yellow"))
legend(30000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Clothing..Footwear.and.Other.Wear.Expenditure)

### 10. Housing.and.water.Expenditure

c(summary(bbdd$Housing.and.water.Expenditure),Desv.Tip.=sqrt(var(bbdd$Housing.and.water.Expenditure)))

hist(bbdd$Housing.and.water.Expenditure,xlab = "Despesa en llar i aigua",ylab="Freq¸Ëncia",main="DistribuciÛ de la Despesa en llar i aigua")
lines(rep(median(bbdd$Housing.and.water.Expenditure),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Housing.and.water.Expenditure),5000),1:5000,col=c("yellow"))
legend(200000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Housing.and.water.Expenditure)

### 11. Imputed.House.Rental.Value


c(summary(bbdd$Housing.and.water.Expenditure),Desv.Tip.=sqrt(var(bbdd$Housing.and.water.Expenditure)))

hist(bbdd$Housing.and.water.Expenditure,xlab = "Despesa en llar i aigua",ylab="Freq¸Ëncia",main="DistribuciÛ de la Despesa en llar i aigua")
lines(rep(median(bbdd$Housing.and.water.Expenditure),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Housing.and.water.Expenditure),5000),1:5000,col=c("yellow"))
legend(200000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Housing.and.water.Expenditure)

### 12. Medical.education.transport.and.communication.expenditure


c(summary(bbdd$Medical.education.transport.and.communication.expenditure),Desv.Tip.=sqrt(var(bbdd$Medical.education.transport.and.communication.expenditure)))

hist(bbdd$Medical.education.transport.and.communication.expenditure,xlab = "Despesa en EducaciÛ, transport i comunicaciÛ",ylab="Freq¸Ëncia",main="DistribuciÛ de la Despesa en EducaciÛ, transport i comunicaciÛ")
lines(rep(median(bbdd$Medical.education.transport.and.communication.expenditure),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Medical.education.transport.and.communication.expenditure),5000),1:5000,col=c("yellow"))
legend(400000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Medical.education.transport.and.communication.expenditure)

### 13. Miscellaneous.good.and.special.occasions.expenditure



c(summary(bbdd$Miscellaneous.good.and.special.occasions.expenditure),Desv.Tip.=sqrt(var(bbdd$Miscellaneous.good.and.special.occasions.expenditure)))

hist(bbdd$Miscellaneous.good.and.special.occasions.expenditure,xlab = "Despesa en bÈns diversos",ylab="Freq¸Ëncia",main="DistribuciÛ de bÈns diversos")
lines(rep(median(bbdd$Miscellaneous.good.and.special.occasions.expenditure),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Miscellaneous.good.and.special.occasions.expenditure),5000),1:5000,col=c("yellow"))
legend(400000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Miscellaneous.good.and.special.occasions.expenditure)

### 14. Crop.Farming.and.Gardening.expenses



c(summary(bbdd$Crop.Farming.and.Gardening.expenses),Desv.Tip.=sqrt(var(bbdd$Crop.Farming.and.Gardening.expenses)))

hist(bbdd$Crop.Farming.and.Gardening.expenses,xlab = "Despesa en productes de granja i jardineria",ylab="Freq¸Ëncia",main="DistribuciÛ de productes de granja i jardineria")
lines(rep(median(bbdd$Crop.Farming.and.Gardening.expenses),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$Crop.Farming.and.Gardening.expenses),5000),1:5000,col=c("yellow"))
legend(300000,3000,c("Mediana","Mitjana"),c("red","yellow"))

boxplot(bbdd$Crop.Farming.and.Gardening.expenses)



### 15. Total.Income.from.Entrepeneurial.Activities

c(summary(bbdd$Total.Income.from.Entrepeneurial.Activities),Desv.Tip.=sqrt(var(bbdd$Total.Income.from.Entrepeneurial.Activities)))

hist(bbdd$Total.Income.from.Entrepeneurial.Activities,xlab = "Ingresos d'activitats laborals",ylab="Freq¸Ëncia",main="DistribuciÛ dels ingresos d'activitats laborals")
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

hist(bbdd$Household.Head.Age,xlab = "Edat del cap de famÌlia",ylab="Freq¸Ëncia",main="DistribuciÛ de les edats dels caps de famÌlia")
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

### Seria necessaria l'agrupacio de variables categoriques, hi ha massa nivells
  
  


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

### Seria necessaria l'agrupacio de variables categoriques, hi ha massa nivells


### 22. Household.Head.Class.of.Worker

table(bbdd$Household.Head.Class.of.Worker)

prop.table(table(bbdd$Household.Head.Class.of.Worker))*100

lbls<- paste(c("Empleat/da empresa familiar","AutÚnom/a","Funcionari/a","Empleat/da","Empleat/da de la llar","Assalariat/da en empresa familiar","Voluntari/a en empresa familiar"),round(prop.table(table(bbdd$Household.Head.Class.of.Worker))*100,2))
lbls <- paste(lbls,"%",sep="")
pie(table(bbdd$Household.Head.Class.of.Worker),labels =lbls)

barplot22 <- barplot(table(bbdd$Household.Head.Class.of.Worker), names.arg="")
text(barplot22, par("usr")[3], labels = lbls, srt = 35, adj = c(1.1,1.1), xpd = TRUE, cex=0.9)


### 23. Total.Number.of.Family.members

### Com a categorica:

table(bbdd$Total.Number.of.Family.members)

prop.table(table(bbdd$Total.Number.of.Family.members))*100

lbls<- paste(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),round(prop.table(table(bbdd$Total.Number.of.Family.members))*100,2))
lbls <- paste(lbls,"%",sep="")
pie(table(bbdd$Total.Number.of.Family.members),labels =lbls)

barplot23 <- barplot(table(bbdd$Total.Number.of.Family.members), names.arg="")
text(barplot23, par("usr")[3], labels = lbls, srt = 35, adj = c(1.1,1.1), xpd = TRUE, cex=0.9)

### Com a numËrica:

c(summary(bbdd$Total.Number.of.Family.members),Desv.Tip.=sqrt(var(bbdd$Total.Number.of.Family.members)))

hist(bbdd$Total.Number.of.Family.members,xlab = "Edat del cap de famÌlia",ylab="Freq¸Ëncia",main="DistribuciÛ de les edats dels caps de famÌlia")
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

hist(bbdd$House.Floor.Area,xlab = "SuperfÌcie del habitatge",ylab="Freq¸Ëncia",main="DistribuciÛ de la superficie dels habitatges")
lines(rep(median(bbdd$House.Floor.Area),5000),1:5000,col=c("red"))
lines(rep(mean(bbdd$House.Floor.Area),5000),1:5000,col=c("yellow"))
legend(300,3000,c("Mediana","Mitjana"),c("red","yellow"))


### 29. House.Age

c(summary(bbdd$House.Age),Desv.Tip.=sqrt(var(bbdd$House.Age)))

hist(bbdd$House.Age,xlab = "Anys de la Casa",ylab="Freq¸Ëncia",main="DistribuciÛ dels anys de les cases")
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
