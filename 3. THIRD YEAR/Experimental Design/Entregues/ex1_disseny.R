ex1 <- read.table("Book1.csv", header=TRUE, sep=";", dec=",")

PERDUA_FRED <- ((ex1$FRED_INICIAL - ex1$FRED_FINAL)/ex1$FRED_INICIAL)*100
PERDUA_FRED

PERDUA_NOFRED <- ((ex1$NOFRED_INICIAL - ex1$NOFRED_FINAL)/ex1$NOFRED_INICIAL)*100
PERDUA_FRED

ex1a <- data.frame(PERDUA_FRED, PERDUA_NOFRED)
ex1a

CONJUNT <- c(PERDUA_FRED, PERDUA_NOFRED)

##############
## APARTAT A #
##############

# 1. Resultats descriptius:
mean(ex1a$PERDUA_NOFRED)
mean(ex1a$PERDUA_FRED)
sd(ex1a$PERDUA_NOFRED)
sd(ex1a$PERDUA_FRED)


# 2. Estimaci? puntual:
mean(ex1a$PERDUA_FRED)                # 44.87580028
mean(ex1a$PERDUA_NOFRED)            # 24.28072
sd(ex1a$PERDUA_FRED) /sqrt(20)     # 0.388274
sd(ex1a$PERDUA_NOFRED) /sqrt(20) # 0.55921586
sd(CONJUNT) /sqrt(40)           # 1.6825323
  
# 3. Plantejament del test:
  # Ho: mu_FRED  = mu_NOFRED
  # H1: mu_FRED != mu_NOFRED

# 4. C?lcul estad?stic de contrast:

# 5. Distribuci? de l'estad?stic de contrast:

# 6. Regi? d'acceptaci? de l'estad?stic de contrast:

# 7. Test significatiu o no significatiu?
t.test(PERDUA_FRED, PERDUA_NOFRED, alternative = "two.sided", var.equal=T)

# 8. Quin seria el p-valor si l'estad?stic de contrast fos 1.71?:
t  <- 1.71
alpha <- 0.05

pval <- 2* pt (t1, df=38)
pval


##############
## APARTAT B #
##############

# 1. Resultats descriptius:
mean(ex1$FRED_INICIAL)
mean(ex1$FRED_FINAL)
sd(ex1$FRED_INICIAL)
sd(ex1$FRED_FINAL)


# 2. Plantejament hip?tesis i c?lculs:
alpha <- 0.05

# Hip?tesis:
  # Ho: mu_INICIAL = mu_FINAL
  # H1: mu_INICIAL != mu_FINAL

t.test(ex1$FRED_INICIAL, ex1$FRED_FINAL, paired=T)


##############
## APARTAT C #
##############

# 1. Resultats descriptius:
mean(ex1$FRED_INICIAL)
mean(ex1$NOFRED_INICIAL)
sd(ex1$FRED_INICIAL)
sd(ex1$NOFRED_INICIAL)

# Amb fred INICAL      N = 20  mitjana = 48.26   S = 2.154164
# Sense fred INICIAL   N = 20  mitjana = 48.865  S = 1.881286

# 2. Plantejament hip?tesi i c?lculs:
alpha <- 0.05

# Hip?tesis:
  # Ho: mu_FRED_INICIAL  = mu_NOFRED_INICIAL 
  # H1: mu_FRED_INICIAL != mu_NOFRED_INICIAL 

t.test(ex1$FRED_INICIAL, ex1$NOFRED_INICIAL, alternative = "two.sided", var.equal=T)












