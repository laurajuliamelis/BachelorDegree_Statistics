dades <- data.frame("Grup1"=c(589.0, 578.6, 593.5, 578.2, 588.9, 586.3),"Grup2"=c(547.9, 557.2, 551.9, 549.9, 545.1, 555.3),
                     "Grup3"=c(573.9, 561.5, 572.9, 577.6, 573.7, 565.4), "Grup4"=c(530.8, 539.5, 543.8 ,540.5, 535.4, 545.3),
                     "Grup5"=c(528.7, 554.0, 556.3, 547.2, 548.1, 550.6))
y <- c(dades$Grup1,dades$Grup2,dades$Grup3,dades$Grup4,dades$Grup5)
grup <- rep(1:5, each=6)
grup <- factor(grup, labels=c("Grup1","Grup2","Grup3","Grup4","Grup5"))



# Pregunta 2
round(mitjgrup1 <- mean(dades$Grup1),1)
round(mitjgrup2 <- mean(dades$Grup2),1)
round(mitjgrup3 <- mean(dades$Grup3),1)
round(mitjgrup4 <- mean(dades$Grup4),1)
round(mitjgrup5 <- mean(dades$Grup5),1)

round(varpob <- var(y),2)

# Pregunta 3
IC1 <- c(qnorm(p=0.025, mean=mitjgrup1, sd=sd(dades$Grup1), lower.tail=TRUE),qnorm(p=0.025, mean=mitjgrup1, sd=sd(dades$Grup1), lower.tail=FALSE))
IC2 <- c(qnorm(p=0.025, mean=mitjgrup2, sd=sd(dades$Grup2), lower.tail=TRUE),qnorm(p=0.025, mean=mitjgrup2, sd=sd(dades$Grup2), lower.tail=FALSE))
IC3 <- c(qnorm(p=0.025, mean=mitjgrup3, sd=sd(dades$Grup3), lower.tail=TRUE),qnorm(p=0.025, mean=mitjgrup3, sd=sd(dades$Grup3), lower.tail=FALSE))
IC4 <- c(qnorm(p=0.025, mean=mitjgrup4, sd=sd(dades$Grup4), lower.tail=TRUE),qnorm(p=0.025, mean=mitjgrup4, sd=sd(dades$Grup4), lower.tail=FALSE))
IC5 <- c(qnorm(p=0.025, mean=mitjgrup5, sd=sd(dades$Grup5), lower.tail=TRUE),qnorm(p=0.025, mean=mitjgrup5, sd=sd(dades$Grup5), lower.tail=FALSE))

round(c(IC1,IC2,IC3,IC4,IC5), 1)

# Pregunta 4
model <- aov(y~grup)
summary(model) # valor de F i pvalor en el problema
pf(1.7, df1= 4, df2= 25, lower.tail = F)  # p-valor si F=1,7


# Preguntes 5 i 6
library(agricolae)
HSD.test(model, "grup", console=TRUE) # q = Critical Value of Studentized Range /A=Minimun Significant Difference


# Preguntes 7 i 8
HSD.test(model, "grup", console=TRUE, alpha = 0.01)



