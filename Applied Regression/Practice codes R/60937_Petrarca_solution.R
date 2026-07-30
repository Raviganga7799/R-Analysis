###### 60937_Petrarca_solution ######
###### Vanessa Petrarca ######
###### matricola 60937 ######
###### Statistical Learning and Data Mining ######
###### Regression Challenge ######

############
library(alr4)
library(caret)
############

setwd("~/Master's degree/statistical learning/regression challenge")

# IMPORTING THE DATASETS 
traindata <- read.csv("train_ch.csv"); View(traindata)
testdata <- read.csv("test_ch.csv"); View(testdata)

# PRELIMINARY DATA ANALYSIS
traindata$X <- NULL
testdata$X <- NULL
anyNA(traindata)

# Checking for correlated variables
pairs(traindata)
cor(traindata) # some variables have 99% of correlation. This is a problem!
# Eliminating the highly correlated variables from my train set
traindata$v1 <- NULL
traindata$v5 <- NULL
attach(traindata)

# LINEAR REGRESSION
lm1 <- lm(Y ~ ., data = traindata)
residualPlots(lm1)

lm2 <- lm(Y ~ v2+ v3 + I(v3^2) + v4 + v6 + v7 +v8 + v9, data=traindata)
residualPlots(lm2)
ncvTest(lm2)
par(mfrow=c(2, 2))
plot(lm2)

traindata$v7 <- NULL
lm3 <- lm(Y ~ v2+ v3 + I(v3^2) + v4 + v6 +v8 + v9, data=traindata)
residualPlots(lm3)
ncvTest(lm3)
plot(lm3) # from the qq-plot I can see that the normality assumption of a linear 
# regression, is violated, thus, it is better to include the variable v7 in the 
# final model. 

lm4 <- lm(Y ~ v3 + I(v3^2), data = traindata)
residualPlots(lm4)
ncvTest(lm4)
plot(lm4) # from the qq-plot I can see that even here, the normality assumption 
# is violated, however for n large enough, the model does not need further.
# The chosen final model is between lm2 and lm4

summary(lm2)
outlierTest(lm2)
influenceIndexPlot(lm2)
max(cooks.distance(lm2))
vif(lm2)

summary(lm4)
outlierTest(lm4)
influenceIndexPlot(lm4)
max(cooks.distance(lm4))
vif(lm4)

# with lm4 we reach r-squared of 100%, and checking the r-squared of all the 
# possible models, only by including input variable v3 and its square, we get 
# 100% R-squared.
# However, I want to underline that also variables v2, v6 and v7, in lm2 were
# statistically significant for the analysis.

# Predictions
pred_lm <- predict(lm4, newdata = testdata)

detach(traindata)

# traindata <- read.csv("train_ch.csv"); View(traindata)
# traindata$X <- NULL
# traindata$v1 <- NULL
# traindata$v5 <- NULL

# KEY-NEAREST-NEIGHBORS 
# I tried to implement the function contained in the FNN library, with no success
# because the system was repeatedly ending up in an error.
attach(traindata)
set.seed(42) 
knn.1 <- train(Y ~ . , data = traindata, method = 'knn')
knn.1
plot(knn.1)
varImp(knn.1) #v3 is the most important variable

knn.model <- train(Y ~ v3, data= traindata, method= "knn")
plot(knn.model)
knn.model

# Predictions
pred_knn <- predict(knn.model, newdata = testdata)

# SAVING THE RESULTS
results <- cbind(pred_lm, pred_knn)
View(results)
write.csv(results, "60937_Petrarca_pred.csv")
