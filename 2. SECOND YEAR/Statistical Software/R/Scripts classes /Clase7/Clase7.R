#########################################################
###  Software Estadístic, 13.10.2016                  ###
###  Descriptive analysis with one and two variables  ###
#########################################################
load("ClasseR_Oct13.RData")
ls()
str(states)

### A. Univariate descriptive analysis
### ==================================
## 1. Numeric variables
## --------------------
### See functions mean, median, sd, range, summary etc.

# Another important function: quantile
quantile(states$inco)
quantile(states$inco, c(0.3, 0.9))
quantile(states$inco, 1:4/5)     # same as: quantile(states$inco, c(.2, .4, .6, .8))


## 2. Categorical variables
## ------------------------
# A very simple frequency table
table(states$reg)
summary(states$reg)    # Only meaningful if the variable is a factor!

# Maybe a bit nicer and more informative
# install.packages("Hmisc")
library(Hmisc)
describe(states$reg)

## Three alternatives for frequency tables
# (a) Function ctab (package catspec)
# install.packages("catspec")
library(catspec)
ctab(states$reg)

# (b) Function stat.table (package Epi)
# install.packages("Epi")
library(Epi)
stat.table(list(reg), data = states)
stat.table(list(Region = reg), list(N = count(), "%" = percent(reg)),
           data = states, margins = T)
stat.table(list(Region = reg), list(N = count(), "%" = percent(reg),
           "Av. income" = mean(inco)), data = states, margins = T)

# (c) Function freq (package descr)
# install.packages("descr")
library(descr)
freq(states$reg)
freq(states$reg, plot = F)

# The 3 functions represent NAs in different manners
reg <- states$reg
reg[49:50] <- NA
summary(reg)
ctab(reg, addmargins = T)
stat.table(list(Region = reg), list(N = count(), "%" = percent(reg)),
           margins = T)
freq(reg, plot = F)


## How to create an ordinal variable from a numeric one?
## -----------------------------------------------------
# Using function cut2 of the Hmisc package
cut2(states$inco, c(4000, 4500, 5000))
states$inco2 <- cut2(states$inco, c(4000, 4500, 5000))
head(states)
summary(states)
# Let's change the labels of categories 1 and 4
levels(states$inco2)
levels(states$inco2)[c(1, 4)] <- c("< 4000", ">= 5000")
levels(states$inco2)
label(states$inco2) <- "Income"
units(states$inco2) <- "Dollar"
describe(states)

# See, also, functions cut, ifelse, or recode (paquete memisc).

## Little exercise:
## Create vector pop2 dividing variable pop into four categories
## of (nearly) the same size
## -------------------------------------------------------------
pop2 <- cut2(states$pop, quantile(states$pop, 1:3/4))
summary(pop2)
# Alternative using argument g
cut2(states$pop, g = 4)
summary(cut2(states$pop, g = 4))


### B. Bivariate descriptive analysis
### =================================
## 1. Both variables are numeric: Correlation
## ------------------------------------------
with(states, cor(murd, lifex))
with(states, cor(murd, lifex, method = "spearman"))
cor(states[1:8])
round(cor(states[1:8]), 3)
# If interested in a confidence interval
with(states, cor.test(inco, illi))

## Little exercise:
## Which pair of variables shows the highest correlation (in absolute values)?
## ---------------------------------------------------------------------------
cormat <- round(cor(states[1:8]), 3)
# Highest correlation (in absolute value)
max(abs(cormat)[cormat < 1])
# Position of the highest correlation
which(abs(cormat) == max(abs(cormat)[cormat < 1]), arr.ind = T)
rownames(which(abs(cormat) == max(abs(cormat)[cormat < 1]), arr.ind = T))

## 2. Both variables are categorical: contingency tables
## -----------------------------------------------------
# Very simple contingency tables
with(states, table(reg, inco2))
tab <- with(states, table(reg, inco2))
tab
# Relative (conditional) frequencies
prop.table(tab)
round(prop.table(tab, 1)*100, 1)

# Somewhat nicer and more informative
# (a) Function ctab (package catspec)
with(states, ctab(reg, inco2))
with(states, ctab(reg, inco2, type = c("n", "r"), addmargins = T))

# (b) Function stat.table (package Epi)
stat.table(list(Region = reg, Income = inco2), list(N = count(),
           "%" = percent(inco2)), data = states)
stat.table(list(Region = reg, Income = inco2), list(N = count(),
           "%" = percent(inco2)), data = states, margins = T)

# (c) Function CrossTable (package descr)
with(states, CrossTable(reg, inco2))
with(states, CrossTable(reg, inco2, prop.t = F, prop.c = F,
                        prop.chisq = F, format = "SPSS",
                        dnn = c("Region", "Income")))

# Note that a table can be stored as an object
tabla <- with(states, CrossTable(reg, inco2, prop.t = F, prop.c = F,
                                 prop.chisq = F, format = "SPSS",
                                 dnn = c("Region", "Income")))
str(tabla)
tabla$prop.row


## 3. Numeric variables vs. a categorical one
## ------------------------------------------
# Function tapply
with(states, tapply(lifex, reg, mean))
with(states, tapply(lifex, inco2, summary))

# Function by
with(states, by(lifex, reg, mean))
with(states, by(lifex, inco2, summary))
with(states, by(states, reg, summary))

# Function summaryBy(doBy)
# install.packages("doBy")
library(doBy)
summaryBy(lifex ~ reg, states, FUN = mean)
summaryBy(lifex ~ reg, states, FUN = c(mean, sd))
summaryBy(illi ~ reg + inco2, states, FUN = summary)
summaryBy(lifex + murd~reg, states, FUN = c(mean, sd))

# A nice table created with function summaryBy
sumby <- summaryBy(lifex ~ reg, states, FUN = c(mean, sd, median, min, max))
sumby
names(sumby) <- c("Region", "Mean", "St.Dev.", "Median", "Min.", "Max.")
print(sumby, digits = 3)


# But, what if there is a NA in any of the variables?
# Assume we have two missing data in variable lifex
states$lifex[c(1, 50)] <- NA
with(states, tapply(lifex, reg, median))            # returns two missing values
?tapply
with(states, tapply(lifex, reg, median, na.rm=T))
