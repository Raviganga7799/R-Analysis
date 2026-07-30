# Assignment-10 #
# 7.6.1
library(alr4)
data(stopping)
# visualizing the given 'stopping' data
# the data consists of two variables:- distance as response and speed as predictor
plot(Distance~Speed,stopping)
abline(lm(Distance~Speed,stopping))
# according to the plot, the plot is linear and the variance is not constant
# which implies that when the speed is less then the observations are near to 
# line with less variance and if speed is high then there is more variance
# so, our assumption is wrong as a linear model.by analyzing the plot i see
# that the distribution of data is in 'U' curvy path which shows there maybe 
# quadratic path.
scatterplot(Distance~Speed,stopping)
# the dashed line shows that our assumption of quadratic path is correct.so,
# lets fit function using quadratic method.
X<-lm(Distance~Speed,stopping)
residualPlots(X)
# Residual analysis also suggests that the given model has the p-value nearly
# zero. so, we reject null hypothesis and the plot shows quadratic function.

#lets fit function using quadratic method.
# 7.6.2
m0<-lm(Distance~Speed+I(Speed^2),stopping)
# lets do the variance test
#(A) Variance depending on the mean.
ncvTest(m0)
# According to the non constant variance test for the fitted values of the 
# given model has highest value of test statistic(22.97013) and p-value is less than
# 0.05 of alpha which is p = 1.6454e-06(closer to zero). so, we reject the
# null hypothesis which means that the variance in the direction of mean is not constant.

#(B) testing variance depends on Speed.
ncvTest(m0,~Speed)
# same as before the test statistic is 23.4 and p value is approx.zero. we 
# reject the null hypothesis which means that the variance in the direction 
# of variable speed is not constant.

#(C) Test on variance depending on Speed and Speed2.
ncvTest(m0,~Speed+I(Speed^2))
# The test of non-constant variance shows the test statistic=23.46559, p-value
# as approx zero less than quantile alpha value(0.05). so, here we reject the
# null hypothesis as before which means that the variance in the direction of the 
# quadratic function is also not constant. so, even though we see a quadratic
# path in the distribution of the scatterplot its not having the constant variance 
# which is bad for the prediction("Adding quadratic term is not helpful").

# Problem -9.9
data(lakes)
View(lakes)
# lets visualize the data
pairs(Species~.,lakes)
# According the visualization i saw more pattern in most of the plots
# but when comparing with species i see a linear pattern with Dist,Nlakes,Photo
# and Area but there are some limitations like leverage points in Dist plots etc.
# note: I think its better removing the predictors latitude, longitude and cond because we cannot transform them.
# lets confirm the model is the better fit or not
Y<-lm(Species~MaxDepth+MeanDepth+Dist+NLakes+Photo+Area,lakes)
residualPlots(Y,smooth=T)
# by analyzing the residual plots we see that for the tukey test and area plots 
# the quadratic line is not near zero we see a pattern.the test statistic values are high
#(-3.3,-3.4) and the p-value is nearly zero less than alpha value. so we reject the null hypothesis.
# which results in Powertransformation.

pt0<-powerTransform(cbind(lakes$Species,lakes$MaxDepth,lakes$MeanDepth,lakes$Dist,lakes$NLakes,lakes$Photo,lakes$Area))
summary(pt0) 
     

# the new transformed values are  (0.50 ,-0.9,-0.18,-0.50,0.00,0.33,0.00). where we get two 
# predictor transformation as logs. according to the hypothesis tests we see that
# we reject null hypothesis for both lambda as (0000) and (1111) using p value< alpha.

# lets build the model using the new transformations
m1<-lm(I(Species^0.50)~I(MaxDepth^ -0.9)+I(MeanDepth^-0.18)+I(Dist^-0.50)+log(NLakes)+I(Photo^0.33)+log(Area),lakes)
residualPlots(m1)
# According to the residual analysis we see the t value is less than quantile value
# and p value is more than  the alpha value which shows the variables for the residual
# test is not significant. i see a small significance but its not much harmful
# the new model is very good comparing to the old one, which means we do not reject 
# the null hypothesis so all the plots has no pattern, but we are not 100% sure for prediction
# I can only say that this model is good compared to the previous ones.

# problem 10.6
# Given data is galapagos
data(galapagos)
# Visualizing the given data
pairs(NS~Area+Anear+Dist+DistSC+Elevation,galapagos)
# According to the plots i see a strong linear relation with elevation 
# remaining all are also linear but they have some issues like leverage,outliners
# also they are asymmetric.
# lets summarize the results
md=lm(NS+ES~Area+Anear+Dist+DistSC+Elevation,galapagos)
summary(md)
qt(1-0.05/2,17)
# its better to do the analysis removing one NS and do analysis for ES and remove ES and do analysis for NS.
# According to the summary, the estimated T-value of the predictors Anear and
# Elevation has higher values(-3.759,5.054) than the significance level of quantile α=0.05
# which is 2.109816.Also the p-values of these regressors are 0.00157 and 9.78e-05 which is 
# less than the significance quantile value of α. so, these variables are significant
# means we reject null hypothesis. so these regressors have some effect on the diversity of
# the two species from the variables NS+ES.

# According to the question we have four possibilities
#  (1) find the elevations; 
#  (2) delete these six islands from the data;
#  (3) ignore elevation as a predictor of diversity,
#  (4) substitute a plausible value for the missing data. 

# my answer is selecting (2) and (3) is not a good idea because we know that
# elevation strongly effects the response and we know that these missing values 
# are less than 200 which means that these values are not an outliners or leverage
# points(influential points) it really effects the model.so i personally
# think prediction is the best thing i can do here so i choose (1).

residualPlots(md)
# for the prediction i used the residual analysis to check whether is it
# a good model or not and the result shows that the area and dist has a significant effect 
# in residual analysis and also the plots shows any pattern.so, its time to transformation.

# using powertransformation
pt2<-powerTransform(cbind(galapagos$NS,galapagos$ES,galapagos$Area,galapagos$Anear,galapagos$Dist,galapagos$Elevation))
summary(pt2)
# these are the results of my power transformation.
#Y1           Y2           Y3           Y4           Y5            Y6 
#0.183204742  0.217506231 -0.004287417 -0.085591249  0.007498920   0.052529105  
# lets build a model using the new values of our power transformation
mt=lm(log(Elevation)~log(NS)+log(ES)+log(Area)+log(Anear)+log(Dist),galapagos)

residualPlots(mt)
# i genuinely tried the transformation for the variable distSC but i couldn't transform it
# but according to the new residual analysis we see that the there is no significance of 
# quadratic term(for dist slight significance but not an issue). we get the result using the p-values
# which are greater than the quantile value.so, we do not reject the null hypothesis.
# According to the plots i don't see any pattern.so, its better model when compared with 
# the previous but im not 100% sure but its a better model.

# lets do the prediction of the elevation
# FITTED AND SINGLE VALUES CONFIDENCE INTERVAL FOR BALTRA  
predict(mt, newdata=data.frame(NS=c(58),ES=c(23),Area=c(25.09),Anear=c(1.84),Dist=c(0.6)),interval="confidence", level=0.95)
predict(mt, newdata=data.frame(NS=c(58),ES=c(23),Area=c(25.09),Anear=c(1.84),Dist=c(0.6)),interval="prediction", level=0.95)

# FITTED AND SINGLE VALUES CONFIDENCE INTERVAL FOR COAMANO  
predict(mt, newdata=data.frame(NS=c(2),ES=c(1),Area=c(0.05),Anear=c(903.82),Dist=c(1.9)),interval="confidence", level=0.95)
predict(mt, newdata=data.frame(NS=c(2),ES=c(1),Area=c(0.05),Anear=c(903.82),Dist=c(1.9)),interval="prediction", level=0.95)

# FITTED AND SINGLE VALUES CONFIDENCE INTERVAL FOR Daphne_Major  
predict(mt, newdata=data.frame(NS=c(18),ES=c(11),Area=c(0.34),Anear=c(1.84),Dist=c(8.0)),interval="confidence", level=0.95)
predict(mt, newdata=data.frame(NS=c(18),ES=c(11),Area=c(0.34),Anear=c(1.84),Dist=c(8.0)),interval="prediction", level=0.95)

# FITTED AND SINGLE VALUES CONFIDENCE INTERVAL FOR Eden 
predict(mt, newdata=data.frame(NS=c(8),ES=c(4),Area=c(0.03),Anear=c(17.95),Dist=c(0.4)),interval="confidence", level=0.95)
predict(mt, newdata=data.frame(NS=c(8),ES=c(4),Area=c(0.03),Anear=c(17.95),Dist=c(0.4)),interval="prediction", level=0.95)

# FITTED AND SINGLE VALUES CONFIDENCE INTERVAL FOR Las_Plazas
predict(mt, newdata=data.frame(NS=c(12),ES=c(9),Area=c(0.23),Anear=c(25.09),Dist=c(0.5)),interval="confidence", level=0.95)
predict(mt, newdata=data.frame(NS=c(12),ES=c(9),Area=c(0.23),Anear=c(25.09),Dist=c(0.5)),interval="prediction", level=0.95)

# FITTED AND SINGLE VALUES CONFIDENCE INTERVAL FOR Seymour
predict(mt, newdata=data.frame(NS=c(44),ES=c(16),Area=c(1.84),Anear=c(25.09),Dist=c(0.6)),interval="confidence", level=0.95)
predict(mt, newdata=data.frame(NS=c(44),ES=c(16),Area=c(1.84),Anear=c(25.09),Dist=c(0.6)),interval="prediction", level=0.95)

FinalE <- as.table(rbind(c(1.367452), c(1.197976), c(1.268801),c(1.197568),c(1.26625),c( 1.291008)))
dimnames(FinalE) <- list( c("baltra", "coamano", "Daphne_Major","Eden","Las_Plazas","Seymour"),
                         mt$Elevation^0.0525291)

FinalE
# Thanking you#
