require(mlbench)
data(BostonHousing)
# data(BostonHousing2)

bh.lm <- lm(rm ~ age, data = BostonHousing)
plot(bh.lm)

plot(BostonHousing$age, BostonHousing$rm)
abline(coef = coef(bh.lm))

bh.sm <- sm.regression(BostonHousing$age, BostonHousing$rm)
