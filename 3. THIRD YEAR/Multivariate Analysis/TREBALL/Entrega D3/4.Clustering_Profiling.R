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