########################################################################
######################  Exam-Stats #####################################
########################################################################
# Adding requried libraries
library(alr4)
library(readr)
library(boot)
# Importing dataset
setwd("~/Masters(GLEB)/Statistics/Exam")
m0<-read.csv("BS_day.csv")
View(m0)
str(m0)
# According to do our analysis i think removing the variables like:-instant,dteday,
# yr,mnth,weekday and even causal and registerd because sum of the both is the count 
# i think its better to do the analysis removing this variables. 
# Lets visualize the data 
pairs(count~season+holiday+workingday+weathersit+atemp+temp+hum+windspeed,m0)
# according to the marginal plots i see that season, holiday,workingday and weathersit are 
# categorical variables.atemp and temps are linearly related to count and i can also see 
# there is a strong correlation between atemp and temp. and humidity is may be linearly related but 
# that linearlty is depends on the leverage point(i have to check whether its influential or not)
# also its left assymetric and likewise windspeed also have some influential points.
weather<-as.factor(m0$weathersit)
sea<-as.factor(m0$season)
holi<-as.factor(m0$holiday)
working<-as.factor(m0$workingday)


m1<-lm(count~sea+holi+working+weather+temp+hum+windspeed,m0)


dim(m0)
vif(m1)
summary(m1)
residualPlots(m1)

m3<-lm(count~sea+holi+working+weather+log(temp)+hum+I(hum^2)+windspeed,m0)

residualPlots(m3)
powerTransform(count~sea+holi+working+weather+I(temp*windspeed)+hum+I(hum^2),m0)

ncvTest(m3)
ncvTest(m3,~sea)
ncvTest(m3,~holi)
ncvTest(m3,~working)
ncvTest(m3,~weather)
ncvTest(m3,~log(temp))
ncvTest(m3,~hum)
ncvTest(m3,~I(hum^2))
ncvTest(m3,~windspeed)

pt1<-powerTransform(count~sea+holi+working+weather+I(temp*windspeed)+hum+I(hum^2),m0)
summary(pt1)
m4<-lm(I(count^0.6592546)~sea+holi+working+weather+log(temp)+hum+I(hum^2)+windspeed,m0)

residualPlots(m4)
ncvTest(m4)
ncvTest(m4,~sea)
ncvTest(m4,~holi)
ncvTest(m4,~working)
ncvTest(m4,~weather)
ncvTest(m4,~log(temp))
ncvTest(m4,~hum)
ncvTest(m4,~I(hum^2))
ncvTest(m4,~windspeed)

B1<- Boot(m4,R=999)
summary(B1)
confint(B1)
hist(B1)
outlierTest(m4)
plot(m4)
influenceIndexPlot(m4)
max(cooks.distance(m4))
pairs(I(count^0.6592546 )~sea+holi+working+weather+log(temp)+hum+I(hum^2)+windspeed,m0)

summary(m4)

boxplot(count~holiday,m0)
     