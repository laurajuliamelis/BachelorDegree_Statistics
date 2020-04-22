#1.1 FILTRATGE DADES
listing.melbourne <- read.csv("~/Desktop/TREBALL MULTI/listing melbourne.csv")
View(listing.melbourne)
nrow(listing.melbourne)
set.seed(2019)
p<-sample(x=nrow(listing.melbourne),size = 5000)
noudataframe <- listing.melbourne[p,]
summary(noudataframe)
write.csv(noudataframe, file="datanueva.csv")


#1.3 ESTRUCTURA DE LES DADES
d<-read.csv("~/Desktop/TREBALL MULTI/datanueva.csv",header=TRUE)

summary(d)
str(d)
dim(d)

#borrem variable X, que ?s un index per la mostra
d<-d[,-1]
length(d)


#missings

sum(is.na(d))
colSums(is.na(d))
colSums(is.na(d))/5000
n_var_miss(d)


library(naniar)
vis_miss(d)
