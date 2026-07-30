### Special Assignment ###
#Importing the text file into R
setwd("~/Masters(GLEB)/Statistics")
library(alr4)
m1 <- read.table("nym.txt",header = TRUE)

# Regression model for the variables in the data.
pairs(Expenditure~Wealth+Pop+PInter+Density+Income+GrowthRate,data = m1)

# as we know that there is no linear relation from OBS,country and state, with 
# expenditures so i removed the variables and plotted.By analyzing the plot i see some
# relation with wealth,income.so, lets summarize the results 

#fitting the regression model by expenditure as response 
m2<-lm(Expenditure~Wealth+Pop+PInter+Density+Income+GrowthRate,data = m1)
summary(m2)

qt(1-0.05/2,907)

#According to the summary,
# the given t-value for the wealth,income and growthRate(36,6.6,4.2) is more than the significant level of quantile 
# α=0.05 which is 1.96.so these regressors are statistically significant also the p-values of these regressors are 
# nearly zero.The given data provides evidence against the null hypothesis. so, we reject the null hypothesis
# which states that there is some effect between expenditure and predictors- wealth,income and growthRate.

residualPlots(m2, smooth=T)

# By analyzing the residual plots i see that the residuals for wealth, population,density are nearly 
# zero but we are interested in the fitted (tukey test) of residuals where the p value is 0.46
# which is not significant. so, there is no evidence to reject the null hypothesis. According to our
# assumption our fitted model has less residual values. Which helps us to predict the model for the 
# years 2005 and 2025.

# By using the table from the question i predict the fitted expenditure for the given years 
# FITTED VALUES CONFIDENCE INTERVAL (CONFIDENCE INTERVAL FOR A CONDITIONAL MEAN) -- Warwick 2005
predict(m2, newdata=data.frame(Pop=c(20442), Wealth=c(85000),PInter=c(24.7),Density=c(214), Income=c(19500), GrowthRate=c(35)), interval="confidence", level=0.95)

# SINGLE VALUE CONFIDENCE INTERVAL -- Warwick 2005
predict(m2, newdata=data.frame(Pop=c(20442), Wealth=c(85000),PInter=c(24.7),Density=c(214), Income=c(19500), GrowthRate=c(35)),interval="prediction", level=0.95)

# FITTED VALUES CONFIDENCE INTERVAL (CONFIDENCE INTERVAL FOR A CONDITIONAL MEAN) -- Warwick 2025
predict(m2, newdata=data.frame(Pop=c(31033), Wealth=c(89000),PInter=c(26),Density=c(325), Income=c(20000), GrowthRate=c(40)), interval="confidence", level=0.95)

# SINGLE VALUE CONFIDENCE INTERVAL -- Warwick 2025
predict(m2, newdata=data.frame(Pop=c(31033), Wealth=c(89000),PInter=c(26),Density=c(325), Income=c(20000), GrowthRate=c(40)), interval="prediction", level=0.95)

############
# FITTED VALUES CONFIDENCE INTERVAL (CONFIDENCE INTERVAL FOR A CONDITIONAL MEAN) -- Manroe 2005
predict(m2, newdata=data.frame(Pop=c(10496), Wealth=c(58000),PInter=c(8.8),Density=c(695), Income=c(17100), GrowthRate=c(35)), interval="confidence", level=0.95)

# SINGLE VALUE CONFIDENCE INTERVAL -- Manroe 2005
predict(m2, newdata=data.frame(Pop=c(10496), Wealth=c(58000),PInter=c(8.8),Density=c(695), Income=c(17100), GrowthRate=c(35)), interval="prediction", level=0.95)

# FITTED VALUES CONFIDENCE INTERVAL (CONFIDENCE INTERVAL FOR A CONDITIONAL MEAN) -- Manroe 2025
predict(m2, newdata=data.frame(Pop=c(13913), Wealth=c(60000),PInter=c(10.1),Density=c(959), Income=c(18100), GrowthRate=c(35)), interval="confidence", level=0.95)

# SINGLE VALUE CONFIDENCE INTERVAL -- Manroe 2025
predict(m2, newdata=data.frame(Pop=c(13913), Wealth=c(60000),PInter=c(10.1),Density=c(959), Income=c(18100), GrowthRate=c(35)), interval="prediction", level=0.95)

############

# FITTED VALUES CONFIDENCE INTERVAL (CONFIDENCE INTERVAL FOR A CONDITIONAL MEAN) -- Tuxedo 2005
predict(m2, newdata=data.frame(Pop=c(10685), Wealth=c(116000),PInter=c(6.1),Density=c(249), Income=c(28300), GrowthRate=c(300)), interval="confidence", level=0.95)

# SINGLE VALUE CONFIDENCE INTERVAL -- Tuxedo 2005
predict(m2, newdata=data.frame(Pop=c(10685), Wealth=c(116000),PInter=c(6.1),Density=c(249), Income=c(28300), GrowthRate=c(300)), interval="prediction", level=0.95)

# FITTED VALUES CONFIDENCE INTERVAL (CONFIDENCE INTERVAL FOR A CONDITIONAL MEAN) -- Tuxedo 2025
predict(m2, newdata=data.frame(Pop=c(29246), Wealth=c(115000),PInter=c(7),Density=c(656), Income=c(25000), GrowthRate=c(100)), interval="confidence", level=0.95)

# SINGLE VALUE CONFIDENCE INTERVAL -- Tuxedo 2025
predict(m2, newdata=data.frame(Pop=c(29246), Wealth=c(115000),PInter=c(7),Density=c(656), Income=c(25000), GrowthRate=c(100)), interval="prediction", level=0.95)


FinalE <- as.table(rbind(c(351.0252, 362.8376), c(274.014, 274.8319 ), c(371.533, 409.1213)))
dimnames(FinalE) <- list(Town = c("Warwick", "Manroe", "Tuxedo"),
                      Expenditure = c("2005", "2025")) 
# According to my prediction for constructing the new houses by the municipality has to spend the expenditure per capita
# amount in the specific years as follows:
FinalE





# the best model we have from the class discussion 
CLass <-lm(Expenditure~Wealth+Pop+PInter*Income+Density+Income+I((GrowthRate+55)^0.327787),data = m1)

residualPlots(CLass,smomoth=T)

summary(CLass)

# i think this is a good model for the prediction so we may be remove the pop and density variables 
# because these two variables dosent give any effect to the expenditure. so its good to remove them 
# and make the predicion of the given projected values of the year 2005 and 2025.  . 
