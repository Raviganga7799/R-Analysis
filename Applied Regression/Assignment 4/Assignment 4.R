# ASSIGNMENT-4
# PROBLEM-2.7
#2.7.1
library(alr4)
# Added a variable 'u1' by using the formula and the values given in the question.
Forbes$u1 <- 1/(255.37 +(5/9)*Forbes$bp)
# Plotting the pres vs u1
with(Forbes, plot(pres~u1,pch=19))
with(Forbes, abline(lm(pres~u1)))
# (The 16 observations in the data is close to the staright line apart from 
# the 12th observation which is far from the staright line).
# (The slpoe in the fig 1.4(a) is positive and here the slope is negative because
# of the inverse equation of bpkelvin)

#2.7.2
# Given equation is 'E(pres/bp)=β0+β1u1' lets summarize the results
m1 <-lm(pres~u1,Forbes)
summary(m1)
# (The result that we got is in the scientific form beacuse the data ot u1 is
# in decimal form and the coef of determination is 99% which shows residuals 
# are near to mean line other than 12th observation)

#2.7.3
# Now we have 2 plots predictor as bp and as u1
m2 <- lm(pres~bp,Forbes)
plot(predict(m1)~predict(m2))
abline(0,1)
summary(m2)
# The predictions of the two lines is equal(Y=X) and fit.
#Clarifying through our prediction model
plot(pres~bp,Forbes)
predict(m2, newdata=data.frame(bp=c(195)),interval="prediction")
predict(m1, newdata=data.frame(u1=c(0.00275)),interval="prediction")
# The prediction of the fit values are almost equal with very minimal deviation

#2.7.4
# lets load the data collected by the botanist Joseph D. Hooker
# Added a variable 'u1' by using the formula same as before.
Hooker$u1 <- 1/(255.37 +(5/9)*Hooker$bp)
with(Hooker, plot(pres~u1,pch=19))
with(Hooker, abline(lm(pres~u1)))
m3 <- lm(pres~u1,Hooker)
summary(m3)
# (This mean function is also looks alike Forbes and the mean is fit.according to
# the summary multiple R-sqrd is 98% where residuals are near fit.
# test of hypothesis
-1.045e+05/2.011e+03
qt(1-0.1/2 , 29)

#problem-2.8
#(Here given that α =β0+β1xbar where the value of α depends upon the value of mean
# which means that the value of mean doesn't change with each observation of x(predictor))

#Problem-2.21
#2.21.1
with(wm1, plot(CSpd~RSpd,pch=20))
with(wm1, abline(lm(CSpd~RSpd),col="red",lwd=2))
#(According to the plot the response CSpd and the predictor RSpd are correlated 
# and it has constant variance.So, the linearity is pausible for this data)

#2.21.2
m4<-lm(CSpd~RSpd,wm1)
summary(m4)
#(Summary shows that intercept=3.14123,slope=0.75573,multiple R sqrd shows that 
# 57% of variability in CSpd is explained by RSpd.Hence the regression may not be 
# be good explaining the relationship of variables)

#2.21.3
# Lets obtain a 95% prediction interval for CSpd at a time when RSpd = 7.4285.
predict(m4, newdata=data.frame(RSpd=c(7.4285)),interval="prediction", level=0.95)
#2.215
predict(m4, newdata=data.frame(RSpd=c(7.4285)),interval="confidence", level=0.95)

# Problem-4.13
# According to the formula log(perCapitaUse) = log(10^6muniUse/muniPop) we add a variable
# because we have to show it as a response variable
MinnWater$perCapitaUse <- with(MinnWater,10^68*muniUse/muniPop)
m5<-lm(log(perCapitaUse)~year+muniPrecip,MinnWater)

require(rgl)
with(MinnWater, plot3d(log(perCapitaUse)~year+muniPrecip, type = "s", col = "red", size = 1))

plot(log(perCapitaUse)~muniPrecip,MinnWater)

abline(lm(log(perCapitaUse)~muniPrecip,MinnWater))
summary(m5)
#after hypothesis test
# we don't reject h0 means that year has no effect of the response variable with respect to the other variables.