##########################
library(alr4)
head(UN11)
# predictor1: real GDP per person in log scale (log(ppgdp))
# predictor2: female life expectancy (lifeExpF)
# response: average fertility rate per woman in log scale (log(fertility))
##########################

# fitting a multiple linear regression model
m0<-lm(log(fertility)~log(ppgdp)+lifeExpF, data=UN11)
summary(m0)
log(1.25)*-0.06544
# If we randomly select 2 countries and one of them will have 25% higher real GDP per person
# than another one, on average its fertility rate per woman will be 1.46% lower, given all the other 
# factors are the same.
# Given the coefficient of determination value (0.6926), the regression model describes around 70%
# variation of log(fertility)