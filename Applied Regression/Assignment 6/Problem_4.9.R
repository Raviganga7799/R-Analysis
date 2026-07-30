##########################
library(alr4)
head(salary)
# predictor1: sex of the college worker (Sex)
# predictor2: years of experience (Year)
# response: annual salary of the worker (Salary)
##########################

# drawing a scatterplot of Salary on the vertical axis versus Sex on the horizontal axis
plot(salary~sex, data=salary)

# fitting a simple linear regression model
m0<-lm(salary~sex, data=salary)
summary(m0)
# The estimated average salary for male college workers is represented by the intercept ($24,697). 
# The estimated average salary for female college workers is $3340 less.

# fitting a multiple linear regression model
m1<-lm(salary~sex+year, data=salary)
summary(m1)
# If we take 2 workers (Male or Female), one of which will have 1 year more of work experience,
# his/her Salary will be on average higher by 759.01$
# Estimated value of the R-squared≈0.49 tells us that variable Year and Sex explains just around 
# half of the variation in Salary.

# plotting the data in 2 colors
with(salary, plot(salary~sex+year, col=ifelse(sex=="Male", "blue", "red"), pch=20))
abline(v=0)
# visualize the obtained mean function on the plot Salary vs Year (2 parallel lines)
abline(m1$coef[1],m1$coef[3],col="blue")
abline(m1$coef[1]+m1$coef[2],m1$coef[3],col="red")
# Given that the regressors are negatively correlated this gives the sign changing between 
# multiple and simple linear regression. But it's not sufficient because it depends on the 
# strength of this negative correlation.

