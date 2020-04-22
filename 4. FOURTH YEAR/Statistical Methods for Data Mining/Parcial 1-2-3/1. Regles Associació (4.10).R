#library of Association rules
library(arules)

#some databases that are in form of transactios

data("Epub")
data("Groceries")
data("Adult")


#How to work with associations rules with data matrix

dd2 <- read.table("credscoClean.csv",header=T, sep=";");

#Seleccion de las variables categoricas
#dd22<-data.frame(iris,iris)
dcat2<-dd2[,sapply(dd2, is.factor)]


#transformamos a transacciones

dtrans2<-as(dcat2, "transactions")

#tendra tantas columnas como categorias
length(levels(dcat2$Dictamen))
#[1] 2
length(levels(dcat2$Vivienda))
#[1] 7
length(levels(dcat2$Estado.civil))
#[1] 6
length(levels(dcat2$Registros))
#[1] 2
length(levels(dcat2$Tipo.trabajo))
#[1] 5
 
  
foo<-function(x){length(levels(x))}
sum(sapply(dcat2, foo))

dtrans2
inspect(head(dtrans2,10))
summary(trans)
image(dtrans2)
dim(dtrans2)
image(dtrans2[sample(1:length(dtrans2),20)])

plot(size(dtrans2))
plot(size(Groceries))

transactionInfo(dtrans2[size(dtrans2) > 5])
transactionInfo(dtrans2[1:5])
transactionInfo(Epub[1:10])
transactionInfo(Groceries[1:10])

nt <- which(size(Groceries) > 20)
inspect(Groceries[nt])
inspect(dtrans2[1:10])

itemFrequencyPlot(Groceries, support=0.1, cex.names = 1)
itemFrequencyPlot(Groceries, support=0.05, cex.names = 1)

#Apriori
?apriori

#minlen : minimo numero de items, cuando el target = "rules" only right part

rulesdtrans2 <- apriori(dtrans2, parameter = list(support = 0.1, confidence = 0.5,  minlen=1))
rulesEpub <- apriori(Epub, parameter = list(sup = 0.1, conf = 0.5, target="rules", minlen=1))
rulesGroceries <- apriori(Groceries, parameter = list(sup = 0.01, conf = 0.5,  minlen=2))
rulesGroceries <- apriori(Groceries, parameter = list(sup = 0.01, conf = 0.05,  maxlen=1))
rulesGroceries
summary(rulesGroceries)
inspect(rulesGroceries)


items<-apriori(dtrans2, parameter = list(sup = 0.2,  target="frequent itemsets", minlen=2))
inspect(items)
rules_output <- as(rules,data.frame)

#param ext
rulesdtrans2 <- apriori(dtrans2, parameter = list(support = 0.4, confidence = 0.8,  minlen=2, ext=TRUE))
inspect(rulesdtrans2)

inspect(head(rulesdtrans2,n=10, by="lift"))

data("Groceries")
mbarules<-apriori(Groceries, parameter = list (support=0.01, confidence=0.1, maxlen = 2))
inspect(head(mbarules,n=100, by="lift"))
inspect(head(mbarules,n=10, by="confidence"))
mbaitemsets<-apriori(Groceries, parameter = list (support=0.01, confidence=0.1, maxlen = 2, target="frequent itemsets"))
inspect(head(mbaitemsets,n=10, by="confidence"))

#ECLAT

eclatdtrans2<-eclat(dtrans2)
mbaeclat<-eclat(Groceries)
inspect(mbaeclat)
eclatdtrans2<-eclat(dtrans2, parameter = list(support=0.4, minlen=1, maxlen=10))
inspect(eclatdtrans2)
#eclatdtrans2<-eclat(dtrans2, parameter = list(support=0.4, minlen=2, maxlen=5, target="maximally frequent itemsets"))
#inspect(eclatdtrans2)
eclatTransrules<-ruleInduction(eclatdtrans2,dtrans2,confidence=0.1)
inspect(eclatTransrules)
## Select a subset of rules using partial matching on the items 
## in the right-hand-side and a quality measure


## Mine frequent itemsets with Eclat.
eclatdtrans2 <- eclat(dtrans2, parameter = list(supp = 0.5))

## Display the 5 itemsets with the highest support.
orderedItemsets <- sort(eclatdtrans2)
inspect(orderedItemsets)

top5 <- sort(eclatdtrans2)[1:5]
inspect(top5)

## Get the itemsets as a list
as(items(top5), "list")

## Get the itemsets as a binary matrix
as(items(top5), "matrix")

## Get the itemsets as a sparse matrix, a ngCMatrix from package Matrix.
## Warning: for efficiency reasons, the ngCMatrix you get is transposed 
as(items(top5), "ngCMatrix")

library("arulesViz")
plot(top5)
plot(mbaeclat)
plot(mbaeclat, method = "paracoord")
plot(mbaeclat, method = "graph")

inspect(rulesdtrans2)
plot(rulesdtrans2, measure = c("support", "lift"), shading = "confidence")
#order es el numero de items qe hay
plot(rulesdtrans2, method = "two-key plot")
plot(rulesdtrans2, method = "grouped")
plot(rulesdtrans2, method = "paracoord")
#plot(rulesdtrans2, method = "graph")
