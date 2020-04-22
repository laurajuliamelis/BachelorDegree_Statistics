## Variables

# - Class: Classe en que viatjaba el passatger (1st, 2nd, 3rd, Crew)
# - Sex: Male, Female
# - Age: Child, Adult
# - Survived: No, Yes --> variable resposta
# - Freq: frequencia de cada categoria

#Volem saber quines característiques fan  que la probabilitat de sobreviviure sigui més alta
#(hi ha diferència entre home i dona, nen i adult?)

#Y|X segueix Bern(pi)
#g(pi)=beta0 
#g(pi)=beta0 + beta1 I{c=2} + beta2 I{c=3} + beta3 I{c=crew} 
#g(pi)=beta0 + beta1 I{S=F} 
#g(pi)=beta0 + beta1 I{E=C} 
#E:Edat C:Child

# 1. Carrega les dades titanic.RData
load('titanic.RData')

# 2. Comprova la independencia de sobreviure o no segons l'edat. Calcula l'estadístic de Pearson.
xtabs(Freq~Survived+Age, df.titanic)
xtabs(Freq~Age+Survived, df.titanic)
apply(xtabs(Freq~Age+Survived, df.titanic),1,sum)
apply(xtabs(Freq~Age+Survived, df.titanic),2,sum)

#Fent regressió logística, el model NUL és:

#A mà
pSurvive_Child <- 57/109
pSurvive_Adult <- 654/2092
pSurvive <- (57+654)/(1490+711)
p <- pSurvive
beta0 <- log(p/(1-p)) #y=-0.739859

#Funció glm R
tab<-data.frame(Age=c("Child", "Adult"), Yes=c(57,654), No=c(52,1438))
glm(cbind(Yes,No)~1, tab, family=binomial(link = "logit"))

#Model #g(pi)=beta0 + beta1 I{E=C} 
  #Contrast de tipus treatment
contrast(tab$Age) #la categoria de referencia es adult
pchild = 57/(57+52)
padult = 654/(654+1438) #=beta0


# 3. Calcula el p-valor  



# 4. Empra la funció chisq.test per comprovar que ho has fet correctament


# 5. Ajusta un model amb la funció link logit i un amb la funció link probit i interpreta'ls


# 6. Repeteix el procés (pasos 2 a 5) per la independencia de sobreviure segons la clase que té 3 categories.

