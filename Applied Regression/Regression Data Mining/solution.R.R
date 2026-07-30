#######################################
####### REGRESSION CHALLENGE ##########
#######################################

# required libraries
library(alr4)
library(caret)
library(utils)
setwd("~/Masters(GLEB)/SLDM/regression challenge")

# Importing datasets
traindata=read.csv("train_ch.csv")
testdata=read.csv("test_ch.csv")

# checking for Na's
traindata=na.omit(traindata)
testdata=na.omit(testdata)

# Using lm function
m1=lm(Y~.,traindata)

# Checking Colinearity
vif(m1)

# visualizing the colinearity
pairs(Y~.,traindata)

# Removing the variables with high colinearity
m2=lm(Y~v2+v3+v4+v6+v8+v9,traindata)
pairs(Y~v2+v3+v4+v6+v8+v9,traindata)

# test of curvature
residualPlots(m2,smooth=T)

# transforming the model 
m3=lm(Y~v2+v3+I(v3^2)+v4+v6+I(v4*v6)+v8+v9,traindata)
residualPlots(m3)

# checking non constant variance test
ncvTest(m3)
# checking normality
plot(m3)
# checking fore outliers
outlierTest(m3)
max(cooks.distance(m3))
# prediction 
pred_lm <- predict(m3, newdata = testdata)

# i have tried to use FNN but i couldnt finish in time.
# using knn
knn.1 <- train(Y ~ . , data = traindata, method = 'knn')
knn.1
# prediction using KNN results
pred_knn=predict(knn.1,newdata = testdata)

# comparing results ok lm and knn predictions
comp <- cbind(pred_lm, pred_knn)
View(comp)

write.csv(comp, "54875.csv")

