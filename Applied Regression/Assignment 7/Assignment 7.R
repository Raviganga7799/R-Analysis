library(alr4)
#problem-5.4
data(MinnLand)
# 5.4.1
boxplot(log(acrePrice)~year,MinnLand)
# housing sales prices in the United States were generally increasing from
# about 2002–2006 and began to fall in the beginning of 2007 but 
# its not the same case in our pattern, prices were increasing after 2007
# in minnland farm sales because the median of prices were increasing.

#5.4.2
m1<-as.factor(MinnLand$year)
m2<-lm(log(acrePrice)~m1,MinnLand)
summary(m2)
# The intercept is positive and the value of the intercept is 7.27175
# the other coefficients shows the value with respect to the predictor(year)
# to the log(acreprice) with considering the other years.
qt(1-0.05/2,18690)
# by using t value and quartile, for the year 2003 we donot reject the
# null hypothesis and its not significance and remaining all years we reject 
# the null hypothesis and all significant.
#in average there is no difference between the years 2002 and 2004.---
# problem-5.14
#5.14.1
scatterplot(HT18~HT9|Sex,data = BGSall)

# Given scatter plot shows the information of better fit regression lines for 
# males and females the effect is same in both the lines it looks like parallel.
#5.14.2
with(BGSall, plot(HT18~Sex+HT9, col=ifelse(Sex=="0", "blue", "red"), pch=20))
m2 <- with(BGSall, lm(HT18~Sex+HT9))
m2$coef
abline(m2$coef[1],m2$coef[3],col="blue")
abline(m2$coef[1]+m2$coef[2],m2$coef[3],col="red")
# the given parallel regression model describes the relation between HT18
# and sex using HT9. for the regression the effect is constant(fixed) also the slope
# the difference of height at the age of 18 between males and females at a given height(age-9)
# is 11.6958406. 1 unit increase in HT9 then result in HT18 by 0.9600564 cm.

#5.14.3
summary(m2)
#obtaining 95% Confidence Interval
# alpha = 0.05
# quantile of the t distribution
qt(1-0.05/2, 133)

# Upper limit
-11.69584 + qt(1-0.05/2, 133)* 0.59036
# Lower limit
-11.69584 - qt(1-0.05/2, 133)* 0.59036

#problem 5.16
data(cathedral)
with(cathedral, plot(Length~Height, col=ifelse(Type=="Romanesque", "blue", "red"), pch=20))
with(cathedral[cathedral$Type=="Gothic",], abline(lm(Length~Height),col="red",lwd=2))
with(cathedral[cathedral$Type=="Romanesque",], abline(lm(Length~Height),col="blue",lwd=2))

m3 <- with(cathedral, lm(Length~Type+Height))
summary(m3)
# we dont see a redsidual pattern for the fit for the romanesque model
# so we get the quadratic plot for the plot and for the for the red points then the residuls 
# spread and we get the fit which we do t test and analysis we reject the quadratic part, but in the 
# beginning we test t test for quardatic part and later for linear part.
# no full answer

#PROBLEM 5.17
data(salary)
#5.17.1
pairs(salary,pch=20)
# we can see some regression trend on the variables salary,ysdeg and year.
#5.17.2
m5<-with(salary,lm(salary~sex))
summary(m5)
qt(1-0.05/2,50)
# the null hypothesis is the mean salary for men and women is the same
# the alternative hypothesis is the mean salary of men and women is not same and
# we test the hypothesis then the t test result in not rejecting the null hypothesis.
#5.17.3
#confidence interval
# Upper limit
-3340 + qt(1-0.05/2, 50)* 1808
# Lower limit
-3340 - qt(1-0.05/2, 50)* 1808

#5.17.4
m6<-lm(salary~.-rank,data = salary)
summary(m6)
