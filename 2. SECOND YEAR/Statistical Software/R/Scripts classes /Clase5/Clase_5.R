#################################################################
###  Software Estadístic, 4.10.2016                           ###
###  Lecture 6: Creation of and working with data frames (II) ###
#################################################################

## Reading data from external files
## ================================
dfram1 <- read.table("Data1.txt", header=T)
dfram2 <- read.table("Data2.txt", header = T, skip = 3,
                      na.strings = c("--", "*"), dec = ",")
dfram1; dfram2


## Function fix()
## --------------
fix(dfram1)
dfram1


## Working with data frames
## ========================
## Extraction of columns and changing their order
## ----------------------------------------------
dfram1[, c(1, 3, 4)]
# Little exercise: Change the order of the columns to
# "Name" "Sex" "Age" "Height" "Weight"
dfram1 <- dfram1[, c(1, 5, 2:4)]
# Alternative that always works
dfram1 <- dfram1[, c("Name", "Sex", "Age", "Height", "Weight")]
dfram1

# Random order of columns
dfram1[, sample(1:5)]

## Some useful functions to deal with missing values
## -------------------------------------------------
is.na(dfram2)
which(is.na(dfram2), arr.ind = T)
# Missings per variable
colSums(is.na(dfram2))
# ¿Qué variable tiene más missings (sabiendo que es una sola variable)?
which.max(colSums(is.na(dfram2)))
# ¿Cuántos son?
max(colSums(is.na(dfram2)))

# Data frame containing only complete observations
na.omit(dfram2)


## Function with
## -------------
with(dfram1, Height)                # same a dfram1$Height or dfram1[, "Height"]
with(dfram1, sum(Sex == "M" & Age > 22))
with(dfram1, mean(Weight/(Height/100)^2))


## Adding new variables to a data frame
## ------------------------------------
dfram1$BMI <- with(dfram1, round(Weight/(Height/100)^2, 2))
dfram1$car <- rep(c("Yes", "No"), c(3, 3))
dfram1
# Delete variable BMI
dfram1$BMI <- NULL
# Alternative:
subset(dfram1, select = -BMI)

# Deleting more than 1 variable
dfram1$BMI <- with(dfram1, round(Weight/(Height/100)^2, 2))
dfr1 <- dfram1
dfram1$BMI <- dfram1$car <- NULL
# Alternative:
subset(dfram1, select = -c(BMI, car))


## Subsetting data frames
## ----------------------
dfram23 <- subset(dfram1, Age == 23)
dfram23
subset(dfram1, Sex == "M" & Age > 22)
subset(dfram1, Sex == "F" | Height < 175)
# Function transform: "2 in 1"
transform(dfram1, Height = Height/100, BMI = round(Weight/(Height/100)^2, 2))
rm(dfram23)


# Which is the mean age of the women (of dfram1)?
mean(subset(dfram1, Sex == "F")$Age)
with(subset(dfram1, Sex == "F"), mean(Age))


## Merging data frames
## -------------------
file.show("Data3.txt")
dfram3 <- read.table("Data3.txt", header=T)
dfram3

# Which variables are in both data frames?
intersect(names(dfram1), names(dfram3))

# Which individuals are in both data frames?
intersect(dfram1$Name, dfram3$Name)

# Four different ways to merge two data frames
merge(dfram1, dfram3)
merge(dfram1, dfram3, all = T)
merge(dfram1, dfram3, all.x = T)
merge(dfram1, dfram3, all.y = T)

(dfram3 <- merge(dfram1, dfram3, all=T))
dfram3[7, "Sex"] <- "F"
dfram3[8, "Sex"] <- "M"
dfram3$BMI <- with(dfram3, round(Weight/(Height/100)^2, 2))
summary(dfram3)


## How to sort a data frame?
## -------------------------
# Using function order
dfram3[order(Sex, Age), ]
dfram3[order(dfram3$Sex, dfram3$Age), ]
# Alternative
with(dfram3, dfram3[order(Sex, Age), ])

# Using function orderBy of package doBy
# install.packages("doBy")       # Only necessary if not yet installed
library(doBy)
search()
dfram3 <- orderBy(~Sex + Age, dfram3)
dfram3


## Visualization of large data frames
## ----------------------------------
?datasets
library(help = datasets)
?state
state.x77
View(state.x77)
summary(state.x77)
head(state.x77)
head(state.x77, 12)
tail(state.x77)
str(dfram3)
str(state.x77)
state.x77df <- data.frame(state.x77)
str(state.x77df)
