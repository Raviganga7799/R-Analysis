#Problem 2.1
#2.1.1) given predictor- height and response-weight.
library(alr4)
with(Htwt, plot(wt~ht,pch=19))
with(Htwt, abline(lm(wt~ht),col="red"))
#(it is very hard to say about linearity using 10 observations
# but it may be possible because of the straightline)
m1 <-lm(wt~ht,Htwt)
summary(m1)
#problem-2.5
data=ftcollinssnow
m1<-lm(Late~Early,ftcollinssnow)
summary(m1)

#problem 2.6
library(alr4)
data= ftcollinstemp
#2.6.1
#plotting the average fall temperature and the average winter temperature
with(ftcollinstemp, plot(winter~fall,pch=19))
with(ftcollinstemp, abline(lm(winter~fall),col="red",lwd=2))
m2<-lm(winter~fall,ftcollinstemp)
summary(m2)
#there is no linearity in the model and the variance is also not
# constant multiple r2 value is also too low so there is no relation.
#2.6.2
#Test of hypothesis for slope
0.3132/0.1528 
qt(1-0.05/2 , 109)
# we will reject the null hypothesis because the t value is more
# the quantile value.
#2.6.3
# In the summary the multiplke R^2 value is 0.0371 that implies that
# 3.7% of the variability of average winter temp is expained by average fall.
#2.6.4
#dividing the data into 2 time periods
# from 1900 to 1989
m00<-data[1:90,]
m3 <-lm(winter~fall,m00)
summary(m3)
# from 1990 to 2010.
m90<-data[90:length(data$year),]
m4<-lm(winter~fall,m90)
summary(m4)
# on the two time lines approximately the values of summary are same.

#problem 3.3
# draw scatterplot of selected variables
m5<-BGSgirls[,c(2,4,1,3,6,11)]
pairs(m5)
# finding coorelation
cor(m5)
# both the scaterplot and the values shows that the variables are positively correlated
#3.3.3
# for the given mean function
m6<-lm(BMI18~HT2+WT2+HT9+WT9+ST9,BGSgirls)
summary(m6)
# the value of sigma^ and multiple R^2 are 2.14 and 0.4431
#hypothesis test
qt(1-0.1/2 , 64)
#WT9 and ST9 have the t value more than quantilelevel which rejects 
# null hypothesis.
# H0 - variable doesnt related to the reponse given the effect of other variable.