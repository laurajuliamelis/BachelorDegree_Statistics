#################################################################
###  Software Estadístic, 29.09.2016                          ###
###  Lecture 5: Creation of and working with data frames (I)  ###
#################################################################

## Creation of data frames
## =======================
## 1. Creation of data frames "by hand"
## ------------------------------------
data.frame(c("Joan", "Rosa", "Laura", "Miguel"), c(32, 30, 5, 2))
dfram <- data.frame(Names = c("Joan", "Rosa", "Laura", "Miguel"),
                     Ages=c(32, 30, 5, 2))
dfram

# Data frames have the same structure as matrices ...
dim(dfram)
ncol(dfram)
dfram[, 2]
dfram[1:2, 2]
summary(dfram)

# ... but there are several differences:
is.matrix(dfram)
is.data.frame(dfram)
class(dfram)
length(dfram)


# There are different ways to work with the variables
# For example, the mean of variable Ages is
# (a)
mean(dfram[, 2])
# (b)
mean(dfram[, "Ages"])
# (c)
mean(dfram[["Ages"]])
# (d)
mean(dfram$Ages)
# The following works, too:
mean(dfram$A)
# The following does not work:
mean(dfram[, "A"])
mean(dfram[["A"]])

# IMPORTANT
dfram[, "Ages"]     # Vector
dfram["Ages"]       # Data frame


# 2. Creation of a new data frame using an existing matrix
# --------------------------------------------------------
(A <- matrix(sample(1:15), nr = 5))
data.frame(A)


# 3. Creation of a new data frame using the editor
# ------------------------------------------------
datfram <- edit(data.frame())
datfram

# The objects created before won't be used any longer
rm(list=ls(all=TRUE))


## Reading data from external files
## ================================
## 1. See ?scan for numeric vectors or matrices
## --------------------------------------------

## 2. Reading data from a text file: Function read.table
## -----------------------------------------------------
# File Data1.txt
file.show("Data1.txt")
dfram1 <- read.table("Data1.txt", header=T)
dfram1
class(dfram1)
class(dfram1$Sex)
dim(dfram1)
colnames(dfram1)
names(dfram1)
rownames(dfram1)

# File Data1b.txt
file.show("Data1b.txt")
dfram1b <- read.table("Data1b.txt")
dfram1b
dim(dfram1b)
names(dfram1b)
# Here, the rownames are the names of the 7 persons
rownames(dfram1b)

# The data of Josep:
# using dfram1
dfram1[which(dfram1$N == "Josep"), ]
# using dfram1b
dfram1b["Josep", ]


# A little exercise: Read the data from Data2.txt into a data frame
file.show("Data2.txt")
dfram2 <- read.table("Data2.txt", header = T, skip = 3,
                      na.strings = c("--", "*"), dec = ",")
dfram2
