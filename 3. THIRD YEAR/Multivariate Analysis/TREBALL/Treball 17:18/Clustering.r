library(cluster)

#dissimilarity matrix

actives<- seq(1, 30)
actives <- actives[-c(1, 14, 22, 24)]
dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

h1 <- hclust(distMatrix,method="ward.D")  # NOTICE THE COST
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot

plot(h1)

c5 <- cutree(h1,5)
c4 <- cutree(h1,4)
c3 <- cutree(h1,3)
c2 <- cutree(h1,2)

#class sizes 
table(c4)

#comparing with other partitions (la millor és 5)
table(c4, c5)
table(c3,c4)

## El vector c5 conté per a cada individu el seu clúster.

## DESCRIPTIVA DELS CLÚSTERS

boxplot(dd[,7]~c5, horizontal=TRUE)
boxplot(dd[,8]~c5, horizontal=TRUE)
boxplot(dd[,9]~c5, horizontal=TRUE)


# falta fer mes descrittiva dels cluster, plotmeans y boxplots de moltes variables
