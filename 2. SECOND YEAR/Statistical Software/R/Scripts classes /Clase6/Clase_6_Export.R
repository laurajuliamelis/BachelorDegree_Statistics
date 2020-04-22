###################################################
###  Software Estadístic, 11.10.2016            ###
###  Writing data and R output into text files  ###
###################################################
load("ClasseR_Oct11.RData")
ls()

# install.packages('Hmisc')     # Only if not installed yet
library(Hmisc)                  # To use function describe
head(states)
str(states)
Label(states)


## Printing a data frame into an external txt file
## -----------------------------------------------
## Example 1
dfram3
write.table(dfram3, "DataFrame3.txt")
file.show("DataFrame3.txt")
write.table(dfram3, "DataFrame3.txt", quote = F, sep = "\t", row.names = F)
file.show("DataFrame3.txt")


## Printing R output into an external (text) file: function sink
## -------------------------------------------------------------
sink("ROutput.txt")
summary(dfram3)
summary(states)
sink()
file.show("ROutput.txt")
summary(dfram3)             # From here, output appears again on the screen

# Use of functions print and cat may improve the output
sink("ROutput.txt")
print("Summary of data frame dfram3")
summary(dfram3)
sink()
file.show("ROutput.txt")        # But, what about the former file ROutput.txt?

# Option append
sink("ROutput.txt", append = T)
cat("Summary of data frame dfram3\n")            # \n forces a line break
summary(dfram3)
cat("\nSummary of data frame states", fill = T)  # fill = T does the same as \n
describe(states)                                 # from package Hmisc
sink()
file.show("ROutput.txt")

# Watch out: things that may happen
sink("AnotherROutput.txt")
summary(dfram3
sink()

describe(states)            # Where did this go to?
sink()
describe(states)            # Vale, vale, I got it
