#SHRINKAGE METHODS

#moltes variables regressores, pero per la rao que sigui no podem seleccionar variables
#quan tenim moltes variables regressores és probable que tinguem un problema de muliticonienalitat i en consecuancia
# una perdua d'eficiencia en l'estimació.
#Es tracta d'utlitzar la informacio de les variables regressores en el seu conjunt sense seleccionar-les.
#analisis de components principals

#hem de reduir la infornamcio de dos varuiabels a una, haig de buscar una direcció que repsesenti bé la informacio del nuvol de punts
#escollim la direcció de maxima variabilitat de les dades: primera component principal
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
#0.65808293...abdom és la que més participa en la component principal
#la primera component principal és considera que és el tamany. 
#El tamany no es pot mesurar...quan una persona sera gran en tamany, és a dir, grosa? Per això abdom contribueix tant en el tamany per dir que una persona és 'faty'
#la segona component principal és la forma. Contraposició de positius i negatius

plot(pca$x[ ,1:2])
#el indiviu més gran que hi ha de tamany esta a la banda negativa de pc2, és a dir, té chest i abdom molt petit i les demes grans

# S'utlitza només PC1 i PC2 per fer la regressió
