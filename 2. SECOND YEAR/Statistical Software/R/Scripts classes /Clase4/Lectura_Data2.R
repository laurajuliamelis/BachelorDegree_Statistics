#########################################
###  Software Estadístic, 29.09.2016  ###
###  Lectura del fitxer Data2.txt     ###
#########################################

## Note: The following solution does only work if Data2.txt
##       is part of the working directory
dir()       # Data2.txt should appear in the object list

## First attempt
## -------------
read.table("Data2.txt")                # :-(
read.table("Data2.txt", header = T)    # Still :-(


## Second attempt: We have to skip the first two lines
## Blank lines are skipped by default
## ---------------------------------------------------
dfram2 <- read.table("Data2.txt", header = T, skip = 2)
dfram2

# Looks better, but:
dfram2[, "Height"]
mean(dfram2[, "Height"])
mean(dfram2[, "Weight"])
# That is, variables Height and Weight are string variables.


## Third attempt: Let's do it correctly!
## -------------------------------------
dfram2 <- read.table("Data2.txt", header = T, skip = 2, dec = ",",
                      na.strings = c("*", "--"))
dfram2
summary(dfram2)                # Yeah! :-)
# Now, the mean of height and weight can be computed
dfram2[, "Height"]
mean(dfram2[, "Height"])
mean(dfram2[, "Height"], na.rm = T)
mean(dfram2[, "Weight"], na.rm = T)
