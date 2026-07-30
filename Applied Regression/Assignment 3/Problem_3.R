# Regression with categorical variables
require(alr4)
View(salary)

# plotting the data in 2 colors
with(salary, plot(salary~sex+year, col=ifelse(sex=="Male", "blue", "red"), pch=20))
abline(v=0)
# plotting variance characteristics of the variables year and sex which contain median (solid middle 
# line inside the box), 1st and 3rd quartiles which is upper line and bottom line of the box.

# building main effect regression model using as regressors Sex and Year to predict Salary
m2 <- with(salary, lm(salary~sex+year))
m2$coef
# The main model effect describes the difference between Salary and Sex based on the variable Year 
# which effect is fixed (kept constant) regardless of the Year variation. In this model 
# the difference in the Salary between Female and Male at any given Year equals 201.47$

# If we take 2 workers (Male or Female), one of which will have 1 year more of work experience,
# his/her Salary will be on average higher by 759.01$

summary(m2)
# Estimated value of the R-squared≈0.49 tells us that variable Year explains just around 
# half of the variation in Salary. The rest includes the factors not taken
# into account in constructed linear model

# visualize the obtained mean function on the plot Salary vs Year (2 parallel lines)
abline(m2$coef[1],m2$coef[3],col="blue")
abline(m2$coef[1]+m2$coef[2],m2$coef[3],col="red")
# 

# obtaining 95% Confidence Interval for β1^
# alpha = 0.05
# quantile of the t distribution
qt(1-0.05/2, 49)

# quantile of the normal(0,1) distribution
qnorm(1-0.05/2)

# Upper limit
759.0 + qt(1-0.05/2, 49)* 118.3
# Lower limit
759.0 - qt(1-0.05/2, 49)* 118.3