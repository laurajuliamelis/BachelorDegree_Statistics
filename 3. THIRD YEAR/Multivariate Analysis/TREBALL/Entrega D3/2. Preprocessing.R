##### PRE PROCESSING #####

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