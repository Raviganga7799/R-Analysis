##########################
setwd("E:/UNICAS/3rd semester/Applied Statistics/0_Working directory/A challenging task!")
nym<-read.table("nym.txt", header=TRUE)
library(alr4)
# predictor1: a soil productivity score (P)
# predictor2: either 1981 or 1982 (Year)
# predictor3: names for four counties in Minnesota (County)
# response: per capita expenditures (Expenditure)
##########################

# fitting a multiple linear regression model (initial state)
m0<-lm(Expenditure~Pop+Wealth+PInter+Density+Income+GrowthRate, data=nym)
summary(m0)
# The model is far from perfect because the 3 parameters (Pop, PInter and Density) 
# are not statistically significant at α=0.05. The model has to be improved.

# power transformations of regressors
pt1<-with(nym, powerTransform(cbind(Pop, Income)))
summary(pt1)

# fitting a multiple linear regression model (modified)
m1<-lm(Expenditure~I(Pop^-0.1431)+Wealth+PInter+Density+I(Income^-0.3232)+GrowthRate, data=nym)
summary(m1)
# The predictors (Pop^-0.1431), Wealth, Density and (Income^-0.3232) have significant positive
# effect on Expenditure (at α=0.05). Among them the greatest magnitude have:
# (Income^-0.3232): by increasing it on 1 unit the Expenditure rises by 4328$;
# (Pop^-0.1431): by increasing it on 1 unit the Expenditure rises by 1395$.
# The predictors PInter and GrowthRate have significant negative effect on the response variable.

# creating a scatterplot to see a graphical representation of the acquired results
scatterplotMatrix(~Expenditure+I(Pop^-0.1431)+Wealth+PInter+Density+I(Income^-0.3232)+GrowthRate, data=nym)
# According to the scatterplot matrix, the relationships between predictors (Pop^-0.1431), 
# Wealth, PInter, Density and (Income^-0.3232) and the response variable Expenditure appears 
# to be linear. I can't say the variance is constant for any of the variables though.

# The effect of the the predictor GrowthRate on the response is not linear. On this plot 
# a 2 outliers could be noticed situated far to the right from the rest of the points.
# The scatterplot confirms that the mostly correlated regressors are (Income^-0.3232) and (Pop^-0.1431).

# There is intercorrelation between the predictors. Many of them aren't independent from the others.

# The density of most of the variables (besides Pop^-0.1431 and Income^-0.3232) 
# appears to be shifted to the left from the center.

# drawing residual plots and fitted values plot
residualPlots(m1, smooth=T)
# By transforming the regressors Pop and Income the graphs are now looking closer to the null plots
# which serves as an evidence that the fitted model is adequate.

############

# FITTED VALUES CONFIDENCE INTERVAL (CONFIDENCE INTERVAL FOR A CONDITIONAL MEAN) -- Warwick 2005
predict(m1, newdata=data.frame(Pop=c(20442), Wealth=c(85000),PInter=c(24.7),Density=c(214), Income=c(19500), GrowthRate=c(35)), interval="confidence", level=0.95)

# SINGLE VALUE CONFIDENCE INTERVAL -- Warwick 2005
predict(m1, newdata=data.frame(Pop=c(20442), Wealth=c(85000),PInter=c(24.7),Density=c(214), Income=c(19500), GrowthRate=c(35)),interval="prediction", level=0.95)

# FITTED VALUES CONFIDENCE INTERVAL (CONFIDENCE INTERVAL FOR A CONDITIONAL MEAN) -- Warwick 2025
predict(m1, newdata=data.frame(Pop=c(31033), Wealth=c(89000),PInter=c(26),Density=c(325), Income=c(20000), GrowthRate=c(40)), interval="confidence", level=0.95)

# SINGLE VALUE CONFIDENCE INTERVAL -- Warwick 2025
predict(m1, newdata=data.frame(Pop=c(31033), Wealth=c(89000),PInter=c(26),Density=c(325), Income=c(20000), GrowthRate=c(40)), interval="prediction", level=0.95)

############

# FITTED VALUES CONFIDENCE INTERVAL (CONFIDENCE INTERVAL FOR A CONDITIONAL MEAN) -- Manroe 2005
predict(m1, newdata=data.frame(Pop=c(10496), Wealth=c(58000),PInter=c(8.8),Density=c(695), Income=c(17100), GrowthRate=c(35)), interval="confidence", level=0.95)

# SINGLE VALUE CONFIDENCE INTERVAL -- Manroe 2005
predict(m1, newdata=data.frame(Pop=c(10496), Wealth=c(58000),PInter=c(8.8),Density=c(695), Income=c(17100), GrowthRate=c(35)), interval="prediction", level=0.95)

# FITTED VALUES CONFIDENCE INTERVAL (CONFIDENCE INTERVAL FOR A CONDITIONAL MEAN) -- Manroe 2025
predict(m1, newdata=data.frame(Pop=c(13913), Wealth=c(60000),PInter=c(10.1),Density=c(959), Income=c(18100), GrowthRate=c(35)), interval="confidence", level=0.95)

# SINGLE VALUE CONFIDENCE INTERVAL -- Manroe 2025
predict(m1, newdata=data.frame(Pop=c(13913), Wealth=c(60000),PInter=c(10.1),Density=c(959), Income=c(18100), GrowthRate=c(35)), interval="prediction", level=0.95)

############

# FITTED VALUES CONFIDENCE INTERVAL (CONFIDENCE INTERVAL FOR A CONDITIONAL MEAN) -- Tuxedo 2005
predict(m1, newdata=data.frame(Pop=c(10685), Wealth=c(116000),PInter=c(6.1),Density=c(249), Income=c(28300), GrowthRate=c(300)), interval="confidence", level=0.95)

# SINGLE VALUE CONFIDENCE INTERVAL -- Tuxedo 2005
predict(m1, newdata=data.frame(Pop=c(10685), Wealth=c(116000),PInter=c(6.1),Density=c(249), Income=c(28300), GrowthRate=c(300)), interval="prediction", level=0.95)

# FITTED VALUES CONFIDENCE INTERVAL (CONFIDENCE INTERVAL FOR A CONDITIONAL MEAN) -- Tuxedo 2025
predict(m1, newdata=data.frame(Pop=c(29246), Wealth=c(115000),PInter=c(7),Density=c(656), Income=c(25000), GrowthRate=c(100)), interval="confidence", level=0.95)

# SINGLE VALUE CONFIDENCE INTERVAL -- Tuxedo 2025
predict(m1, newdata=data.frame(Pop=c(29246), Wealth=c(115000),PInter=c(7),Density=c(656), Income=c(25000), GrowthRate=c(100)), interval="prediction", level=0.95)

############

tab <- as.table(rbind(c(230.0062, 222.6666), c(225.8093, 226.362), c(166.2972, 300.1394)))
dimnames(tab) <- list(Town = c("Warwick", "Manroe", "Tuxedo"),
                      Expenditure = c("2005", "2025")) 
tab
