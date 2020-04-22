####################################################
###  Software Estadístic, 20.10.2016             ###
###  Graphs 2: Some more sophisticated examples  ###
####################################################
# install.packages("Hmisc")
library(Hmisc)
load("ClasseR_Oct20.RData")
ls()

## Scatterplots with subgroups (Figure1.png)
## ========================================
windows(width = 8)
par(las = 1, font.axis = 2, font.lab = 4)
plot(illi ~ inco, states, xlab = "Income (Dollars)", ylab = "Illiteracy",
     pch = 16, col = as.numeric(reg), cex = 1.5)
title("Illiteracy rate vs. Income")
legend("topright", levels(states$reg), col = 1:4, pch = 16)

# Alternatives to place the legend
legend(5500, 2, levels(states$reg), col = 1:4, pch = 16)
legend(locator(1), levels(states$reg), col = 1:4, pch = 16)
# Points can be identified using function identify
# (Use Esc to stop identifying points)
with(states, identify(inco, illi))
# The following instruction permits identifying the states,
# BUT R might crash. :-((
with(states, identify(inco, illi, labels = rownames))



# A bit more tedious, but more flexible
# -------------------------------------
# Ranges of both axes
xlims <- range(states$inco)
ylims <- range(states$illi)

windows(width = 8)
par(las = 1, font.axis = 2, font.lab = 4)
with(subset(states, reg == "Northeast"),
     plot(illi ~ inco, xlab = "Income (Dollars)", ylab = "Illiteracy",
          pch = 16, col = "tomato", cex = 1.5, xlim = xlims, ylim = ylims))
with(subset(states, reg == "South"),
     points(illi ~ inco, pch = 17, col = "steelblue", cex = 1.5))
with(subset(states, reg == "North Central"),
     points(illi ~ inco, pch = 18, col = "maroon", cex = 1.5))
with(subset(states, reg == "West"),
     points(illi ~ inco, pch = 19, col = "springgreen", cex = 1.5))
legend("topright", levels(states$reg), pch = 16:19,
       col = c("tomato", "steelblue", "maroon", "springgreen"))
rm(xlims, ylims)

# To include text in a graph use function text
text(6100, 1.5, "Alaska")
text(4000, 2.5, "Louisiana")
# An arrow might be helpful here...
arrows(3775, 2.55, 3610, 2.75, lwd = 3)

# For different types of points, see
example(pch)


## A global title for various graphs (Figure2.png)
## ===============================================
windows(width = 8, height = 6)
par(mfrow = c(2, 2), lwd = 3, font.lab = 2, font.axis = 2, las = 1,
    oma = c(0, 0, 1, 0))
curve(dnorm(x, mean = 0, sd = 1), from = -4, to = 4, ylab = "", col = 2,
      main = "Standard normal distribution")
curve(dchisq(x, 3), from = 0, to = 10, main = "Chi-square distribution: n = 3",
      ylab = "", col = "springgreen4")
curve(dnorm(x, mean = 0, sd = 1.5), from = -4, to = 4, ylab = "",
      col = "slateblue2")
title(expression(bold(paste("Normal distribution: ", mu, "  =  0,  ", sigma, "  =  1.5"))))
curve(dchisq(x, 4), from = 0, to = 10, ylab = "", col = "steelblue",
      main = "Chi-square distribution: n = 4")
title("Some density functions", cex.main = 1.5, outer = T)

# PD: expression() enables to write greek letters
# For help, see ?plotmath


## A boxplot with some text (Figure3.png)
## ======================================
(means <- with(states, tapply(inco, reg, mean)))
(txt <- paste("Mean:", round(means, 1)))

windows(width = 8, height = 6)
par(font = 2, font.lab = 4, font.axis = 2, las = 1)
boxplot(inco ~ reg, states, col = rainbow(4), ylim = c(2600, max(states$inco)),
        xlab = "Region", main = "Average income", pch = 16)
text(1:4, 2600, txt, font = 2)
# Modification of the y-axis
axis(2, at = c(2600, 3500, 4500, 5500, 6200))

graphics.off()
rm(txt, means)
