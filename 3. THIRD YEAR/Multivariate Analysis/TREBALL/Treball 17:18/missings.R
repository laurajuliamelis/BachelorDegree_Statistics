

### Variables numèriques

Comencem construint un data.frame que reculli totes les variables numèriques i testem, amb el test de Little, la aleatorietat dels missing values.

```{r, warnings = F, comment = ""}
require(BaylorEdPsych)
nums <- subset_colclasses(sampledata, "numeric")

## Test de Little
LittleMCAR(as.data.frame(nums))$p.value
```

El p-valor és força petit, això indica que pot existir una certa sistematització en el repartiment dels NA. Aquest resultat és llògic, ja que la majoria dels nostres valors missings són conseqüència de buits en els registres de latituds i longuituds. És natural pensar que aquestes mesures no estan disponibles per a una zona en particular del mapa i no repartides aleatòriament entre totes les localitzacions geogràfiques que tenim. 

Fem un filtratge de les observacions que tinguin NA en variables numèriques (tenim les mateixes 23 per a totes les variables):
  
  ```{r}
imputed <- filter(sampledata, is.na(Hotel_lat))
summary(imputed)
```

Dividim la matriu de variables numèriques en les observacions que tenen missings i les que no en tenen, respecte a la variable que volem fer la imputació. Fem servir el mètode knn per a diverses variables:
  
  ```{r}
require(class)

## indexos per a les variables que necessiten imputació

uncompleteVars<-c(5:9)

fullVariables <- c(13, 15, 18, 20, 23, 25, 26, 27, 28, 29)
aux<-sampledata[,fullVariables]
dim(aux)
names(aux)

for (k in uncompleteVars){
  aux1 <- aux[!is.na(sampledata[,k]),]
  dim(aux1) 
  aux2 <- aux[is.na(sampledata[,k]),]
  dim(aux2)
  
  RefValues<- sampledata[!is.na(sampledata[,k]),k]
  #Find nns for aux2
  knn.values = knn(aux1,aux2,RefValues)   
  
  #CARE: neither aux1 nor aux2 can contain NAs
  
  
  #CARE: knn.ing is generated as a factor. 
  #Be sure to retrieve the correct values
  
  sampledata[is.na(sampledata[,k]),k] = as.numeric(as.character(knn.values))
  fullVariables<-c(fullVariables, k)
  aux<-sampledata[,fullVariables]
}

dim(sampledata)
summary(sampledata)
```


## Guardem la base de dades processada

```{r}
write.table(sampledata, file = "Booking_processed.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
```
