#############################################################
###  Software Estad?stic, 18.10.2016                      ###
###  Graphs 1: Scatterplots, histograms, boxplots et al.  ###
#############################################################
# install.packages("Hmisc")
library(Hmisc)
load("ClasseR_Oct18.RData")
str(states)
head(states)


## 1. Scatterplots
## ===============
plot(inco~pop, states, main = "A simple figure")

# Instead of argument data = states, we may also use function with
with(states, plot(inco~pop, xlab = "Population", ylab = "Income"))
title("A simple scatterplot")

# The same is obtained by:
with(states, plot(pop, inco, xlab = "Population", ylab = "Income"))
title("A simple scatterplot")

# Close the active graph window
dev.off()


## If a data frame has labels and units, these can be used:
Label(states)
plot(inco~pop, states, xlab = label(states$pop), ylab = units(states$inco))
title("A simple scatterplot")
# The same graph
with(states, plot(inco~pop, xlab = label(pop), ylab = units(inco)))
title("A simple scatterplot")


## Two graphs in one graph window
windows(width = 10, height = 6)          # in Linux: x11(); Mac: quartz()
par(mfrow = c(1, 2))
# Plot 1
plot(inco~illi, states, xlab= "Illiteracy", ylab = "Dollars")
title("Average income vs. Illiteracy rate")
# Plot 2
plot(lifex~murd, states, xlab = "Murder rate (per 100000)",ylab = "Years", pch = 19, col = "blue")
title("Life expectancy vs. Murder rate")

# Now substitute par(mfrow = c(1, 2)) by
par(mfrow = c(1, 2), font = 2, font.lab = 4, font.axis = 3, las = 1)

# Which changes do you observe?


## Scatterplots matrices can be obtained as follows
windows(width = 10, height = 8)
plot(states[1:8])
pairs(states[1:8])

# Close all graph devices
graphics.off()


## 2. Histograms
## =============
windows()
par(font = 2, font.lab = 2, font.axis = 4, las = 1)
hist(states$inco)

# Let's improve that figure a little bit
hist(states$inco, xlab = "Dollars", breaks = 15, col = 2, main="Distribution of average income in the USA (1977)")

# Changing the y-axis, a smoothed curve may be overlaid
hist(states$inco, xlab = "Dollars", breaks = 15, col = "steelblue", freq = F, main="Distribution of average income in the USA (1977)")
lines(density(states$inco), lwd = 3)

# Would like some more colours? Choose among...
colours()

# Or, what about the rainbow colours?
hist(states$inco, xlab = "Dollars", breaks = 15, col = rainbow(20), main = "Distribution of average income in the USA (1977)")

## How to save a graph?
# (I) Use of function savePlot
savePlot("IncoHisto", type = "pdf")
savePlot("IncoHisto", type = "png")
dir()

## (II) Function pdf can be used to create a pdf file
##      that contains one or more graphs
pdf("histograma.pdf", width=8)
par(font = 2, font.lab = 2, font.axis = 4, las = 1)
hist(states$inco, xlab = "Dollars", breaks = 15, col = 2,main = "Distribution of average income in the USA (1977)")

hist(states$inco, xlab = "Dollars", breaks = 15, col = "steelblue", freq = F, main = "Distribution of average income in the USA (1977)")
lines(density(states$inco), lwd = 3)

hist(states$inco, xlab = "Dollars", breaks = 15, col = rainbow(20), main = "Distribution of average income in the USA (1977)")
dev.off()
# Similar functions exist for other formats: png(), tiff(), etc.


## Multiple histograms can be drawn with function hist(Hmisc)
hist(states, nclass = 15)

# Close all graph devices
graphics.off()


## 3. Boxplots
## ===========
## We may save some values for function par, if we use them very often
myPar <- list(font = 2, font.lab = 2, font.axis = 4, las = 1)

windows()
par(myPar)
boxplot(states$inco)
boxplot(inco~reg, data = states, col=3, pch = 16)
boxplot(inco~reg, states, col = 2:5, pch = 16, main = "Average income")

## Maybe add a beeswarm?
# install.packages("beeswarm")
library(beeswarm)
beeswarm(inco~reg, states, col = 2:5, pch = 16, main = "Average income")
# Put the bees in a box
boxplot(inco~reg, states, col = 2:5, pch = 16, main = "Average income")
beeswarm(inco~reg, states, pch = 16, add = T)

## A little exercise:
## Try to reproduce figure BoxplotWithoutSouth.png
## -----------------------------------------------



## 4. Pie charts ("Gr?ficos de pastel")
## ====================================
pie(states$inco2)         # :-(
pie(table(states$inco2))
pie(table(states$inco2), col = 1:4, main = "Average income")

# Nice pie charts (from help(pie)):
windows(width = 10)
par(mfrow = c(1, 2))
pie(rep(1, 300), labels = "", col = rainbow(300), border = NA)
pie(rep(1, 300), labels = "", col = rainbow(300))


## 5. Barplots: for some better examples, see help(barplot)
## ========================================================
barplot(table(states$inco2))
with(states, barplot(tapply(inco, reg, median), col = 1:4,main = "Median income per state"))
with(states, barplot(table(inco2, reg), col = 1:4, ylab = "Number of states", legend = T))
with(states, barplot(table(inco2, reg), beside = T, col = 1:4, ylab = "Number of states", legend = T))
dev.off()


## 6. Instead of a barplot, better use a mosaicplot
## ================================================
windows(width=8)
par(myPar)
mosaicplot(reg~inco2, states, col = 1:4, xlab = "Region", ylab = "Dollars", main = "Income per Region", cex.axis = 1)

## Which is the interpretation of this graph?
dev.off()
rm(myPar)
