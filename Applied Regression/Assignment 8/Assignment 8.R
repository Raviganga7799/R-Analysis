#########Assignment-8######
# 5.8 #
library(alr4)
#A)
data(cakes)
m1 <-lm(Y~X1+X2+I(X1^2)+I(X2^2)+X1:X2, data=cakes)
summary(m1)
# According to the summary given absolute t-value is more than 2.so,our model is 
# significant also the p values are less than 0.05% so our model is definately significant.
# B) here i need to add the regressor block to the given regrssion function
m2<-lm(Y~X1+X2+I(X1^2)+I(X2^2)+X1:X2+block, data=cakes)
summary(m2)
#according to the new summary the regressor block has the absolute t-value 0.47 less than 2
# which is not significant also p value is 65% which shows our model is not significant.
# but in the both cases i cannot explain the significance surely because the sample size
# of the data is too low.


#problem 6.9
# 1)
h0 <-lm(Y~X1+X2+I(X1^2)+I(X2^2), data=cakes)
summary(h0)
# HA is
summary(m1)
qf(1-0.05,1,8)
# the f quantile value is 5.3. f value is 4.8 which is less than quantile so we dont reject 
# null hypothesis also for the m1(HA) summary we can study the p values of coefficeints β5(X1:X2)
# also for β2 is significant because of the p value which is nearly zero less than 0.05.
# so every coefficent is signicant so there is no evidence to say β5 and β2 are zero so we use the interaction model.
h2<-lm(Y~X2+I(X2^2),cakes)
summary(h2)
qf(1-0.05,3,8)
#2)new H0
Hzero <-lm(Y~X2++I(X2^2), data=cakes)
summary(Hzero)
qf(1-0.05,3,8)
# the value of quantile is 8.98 and the f value is 3.7 which is less than quantile so we reject the 
# null hypothesis but according to the p value the values are in no man's land but according to 
# f test our test is significant mean we don't consider h0 as beta values zero.

(sum(h2$res^2)-sum(m1$res^2))/3
sum(m1$res^2)/8


#problem 5.20.1
head(domedata)
pairs(~Cond+Velocity+Angle+BallWt+BallDia+Dist,data=domedata)
m3=lm(Dist~Cond+Velocity+Angle+BallWt+BallDia,data=domedata)
summary(m3)

# The fan does affect the ball distance because in summary the absolute t-value is
# more than 2 for factors-condition, velocity and balldiameter also the p value is 
# approximately less than α=0.05 value for same regressors which means that these regressors
# are significant by the given model.so,The distance can be changed by the condition, velocity and balldiameter.

## 5.20.2

head(domedata1)
scatterplotMatrix(~Date+Cond+Velocity+Angle+BallWt+BallDia+Dist,data=domedata1)
m4=lm(Dist~Date+Cond+Velocity+Angle+BallWt+BallDia,data=domedata1)
summary(m4)

# when we take into the account of the date in the summary for the factors of
# velocity and angle has the significane as though our 2 experiments results are different.
# The date does affect the result of distance which means the way to manipulate fans can affect the distance.


