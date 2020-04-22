### PROBLEMA 3 ###

### MÈTODES DE SELECCIÓ DE VARIABLES ###

#1. Model de regressio OLS

    g <- lm(vardependent ~ ., data = dades)
    summary(g)
    
#2. Model seleccinat per AIC
    
    g_AIC <- step(g, direction = "both")
      #la variable que surt en positiu la treiem, i ens quedem amb
      #les restants.
    summary(g_AIC)
    
#3. Model seleccionat per Cp de Mallows ?????????
    
    # install.packages('leaps')
    # library(leaps)
    X <- as.matrix(dades[,-5]) #totes les vars - la dependent
    y <- dades[,5] #variable dependent
    
    
    g_Cp <- leaps(X, y, method = "Cp", names = c('names de totes les vars excepte la dependent'))
    
    # Si P = 3 (2 regressores més intercept)
    g_Cp$which[g_Cp$Cp==min(g_Cp$Cp[g_Cp$size==3])]
    # agafem les vars que surt TRUE
    
    
    # Si P = 4 (3 regressores més intercept)
    g_Cp$which[g_Cp$Cp==min(g_Cp$Cp[g_Cp$size==4])]
    # agafem les vars que surt TRUE
    
    # Gràficament
    plot(g_Cp$size,g_Cp$Cp, ylim = c(0,6),xlab="P",ylab="C_P")
    abline(0, 1)
    
    # El model que més s'apropa a Cp=P és el millor model
    # amb P = 3 (2 regressores més l'intercept)
    # Per tant, escolliriem sex + income
    
    g_Mallows <- lm(gamble ~ suma de les variables escollides, data = teengamb)
    
#4. Model seleccionat per FORWARD STEPWISE
    
    # Fórmula del model complet
    gc.formula <- formula(g)
    # Model simple
    g0 <- lm(vardependent ~ 1, data = dades)
    # Forward stepwise
    model <- g0
    add1(model, scope = gc.formula, test="F")
    # Haig d'afegir aquella var amb la F més gran 
    model <- update(g0, ~ . + varFmésgran1)
    # Al model g0 li sumo varFmésgran1
    add1(model, scope = gc.formula, test="F")
    # Haig d'afegir aquella var amb la F més gran 
    model <- update(model, ~ . + varFmésgran2)
    # Al model "model" li sumo varFmésgran2
    add1(model, scope = gc.formula, test="F")
    # Ja no tinc que afegir-ne cap
    # gamble ~ varFmésgran1 + varFmésgran2
    
    
### MÈTODES D'AJUST D'UN MODEL (MODELS ROBUSTOS) ###
    
#1. MINIMS QUADRATS OLS (sense algun punt)
    
    g_ols <- lm(vardependent ~ ., data = dades[-24,])
    summary(g_ols)
    
#2. MÈTODE DE HUBER
    
    g_huber <- rlm(vardependent ~ ., data = dades)
    summary(g_huber) #Mirem possibles canvis
    plot(g_huber) #surten varios grafics
    
#3. LEAST TRIMMED SQUARES - LTS
    
    g_lts <- ltsreg(vardependnet ~ ., data = dades, nsamp = "exact")
    coef(g_lts)
    # Posem 'nsamp = "exact"' ja que així es demana una cerca més
    # exhaustiva, tot i que per a grans conjunts de dades el temps
    # de computació és alt.
    
    # mirem possibles canvis en els coeficients
    
   plot(fitted(g_lts),residuals(g_lts),xlab="Fitted",ylab="Residuals")
  #  ????
