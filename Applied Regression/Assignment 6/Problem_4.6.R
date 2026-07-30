##########################
library(alr4)
head(UN11)
# predictor: urban population (% of total population) (pctUrban)
# response: average fertility rate per woman in log scale (log(fertility))
##########################

# drawing a scatterplot of log(fertility) on the vertical axis versus pctUrban on the horizontal axis
plot(log(fertility)~pctUrban, data=UN11)

# fitting a  simple linear regression model
m0<-lm(log(fertility)~pctUrban, data=UN11)
abline(m0, col="red")
summary(m0)
# The variance appears to be constant and relationship between the studied variables seems linear.
# If we randomly select 2 countries and one of them will have 1% more urban population than another,
# on average its fertility rate per woman in log scale will be 0.01 lower.
# Given the coefficient of determination value (0.2864), the regression model describes less than 1/3
# variation of log(fertility) which is far from perfect.

# the effect in original scale
exp(-0.010163)-1
# If we randomly select 2 countries and one of them will have 1% more urban population than another,
# on average its fertility rate per woman in original scale will be 1% lower.