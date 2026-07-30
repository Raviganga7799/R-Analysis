# Vanessa Petrarca
# Research methods in management
# Regression analysis

library(readxl)
library(alr4)

setwd("~/Master's degree/research methods")
real_estate <- read_excel("Real estate valuation data set.xlsx")
View(real_estate)
str(real_estate)
summary(real_estate)
real_estate$No = NULL # setting it as null for simplicity, because it was showing 
# no. of observations
View(real_estate)
real_estate$`transaction date`=NULL # i removed it because useless for the analysis
anyNA(real_estate) # no NA values
number.of.convenience.stores <- as.factor(real_estate$number.of.convenience.stores)
pairs(real_estate) # no correlation detected among the regressors



m0 <- lm(house.price.of.unit.area~ ., data = real_estate)
residualPlots(m0)
ncvTest(m0)
ncvTest(m0, ~distance.to.the.nearest.MRT.station)
par(mfrow=c(2,2))
plot(m0) #normality assumption not perfectly met
summary(m0) #longitude not significant

outlierTest(m0)
influenceIndexPlot(m0)
max(cooks.distance(m0))
vif(m0)

RSE <- sqrt(deviance(m0)/df.residual(m0))
RSE
mean(m0$fitted.values)

RSE/mean(m0$fitted.values) #error 0.23



nmcv=as.factor(real_estate$number.of.convenience.stores)
# add nmcv to your data instead of number.of.cv.stores
