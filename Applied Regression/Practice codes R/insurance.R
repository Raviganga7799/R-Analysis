# Adding requried libraries
library(alr4)
library(readr)
library(boot)
library(readxl)

m0 <- read_excel("C:/Users/ganga/OneDrive/Desktop/Insurance_Claim.xlsx")
View(m0)
CLAIM =na.omit(m0)  
str(CLAIM)

pairs(claim~age +gender+bmi+bloodpressure+diabetic+children+smoker+region,CLAIM )
# i have got non numeric argument it suggests that i have categorical variables in my dataset
str(CLAIM)
# so i used str to know what are the categorical variables
Gender=as.factor(CLAIM$gender)
Region=as.factor(CLAIM$region)
Smoker=as.factor(CLAIM$smoker)
Diabetic=as.factor(CLAIM$diabetic)

pairs(claim~age+Gender+bloodpressure+Diabetic+children+Smoker+Region,CLAIM)

M1=lm(claim~age+bmi+Gender+bloodpressure+Diabetic+children+Smoker+Region,CLAIM)

residualPlots(M1)

pt1<-powerTransform(claim~age+bmi+Gender+bloodpressure+Diabetic+children+Smoker+Region,CLAIM)
summary(pt1)

M2=lm(I(claim^-0.265)~age+bmi+Gender+bloodpressure+I(bloodpressure^2)+Diabetic+children+I(children^2)+Smoker+Region,CLAIM)
residualPlots(M2)
pairs(claim~age+bloodpressure+bmi+children,CLAIM)
cooks.distance(m2)
outlierTest(m2) 
which.max(cooks.distance(m2))
summary(m2)
