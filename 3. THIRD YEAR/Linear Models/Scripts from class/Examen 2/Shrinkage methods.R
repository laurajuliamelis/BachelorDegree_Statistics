#SHRINKAGE METHODS

#moltes variables regressores, pero per la rao que sigui no podem seleccionar variables
#quan tenim moltes variables regressores �s probable que tinguem un problema de muliticonienalitat i en consecuancia
# una perdua d'eficiencia en l'estimaci�.
#Es tracta d'utlitzar la informacio de les variables regressores en el seu conjunt sense seleccionar-les.
#analisis de components principals

#hem de reduir la infornamcio de dos varuiabels a una, haig de buscar una direcci� que repsesenti b� la informacio del nuvol de punts
#escollim la direcci� de maxima variabilitat de les dades: primera component principal
#segona component ha de ser perpendicular a la primera

library(faraway)
data(fat, package = "faraway")
head(fat)

help(fat)
cor(fat)

cfat <- fat[9:18]
cor(cfat)
lmoda <- lm(fat$brozek ~ ., data=cfat)
vif(lmoda)

# buscar que son els VIF's
pca <- prcomp(cfat)
summary(pca)
# si nomes utilitzo les dos primeres components pricnpals, la informacio que yo tnia amb 10 variables quedara representada per un 92.3%
# 0.867 + 0.05605 = 0.92305

pca$rotation #components principals: son els coeficients que proporcionen la component principal
#0.65808293...abdom �s la que m�s participa en la component principal
#la primera component principal �s considera que �s el tamany. 
#El tamany no es pot mesurar...quan una persona sera gran en tamany, �s a dir, grosa? Per aix� abdom contribueix tant en el tamany per dir que una persona �s 'faty'
#la segona component principal �s la forma. Contraposici� de positius i negatius

plot(pca$x[ ,1:2])
#el indiviu m�s gran que hi ha de tamany esta a la banda negativa de pc2, �s a dir, t� chest i abdom molt petit i les demes grans

# S'utlitza nom�s PC1 i PC2 per fer la regressi�
