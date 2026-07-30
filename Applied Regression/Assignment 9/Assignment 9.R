### Assignment-9 ####

# 6.19
library(alr4)
data("prodscore")
force(prodscore)
# visualizing given data 
pairs(~Value+P+County+Year,data=prodscore)
# From the visualization of the given data I conclude that I see a relation
# between the variables P and the value. we can compare the average assessed value 
# and soil productivity for the given four countries by analyzing the country,value and P plots. 
# The plot year and Value shows that the average assessed value of lands increased from
# the year 1981-1982.the plot p and year shows soil productivity from 1 to 100(according to the question)
# when comparing the both years the productivity of the soil doesn't change and I
# see no difference in the projection of observations so it may be a null plot.

scatterplotMatrix(~Value+P+County+Year,data=prodscore)
hist(prodscore$Value)
# our assumptions from the visualization are all correct, but for the plot
# value-p i see a good regression fit with constant variance, the distribution
# of the data is almost symmetric. so lets dive into the summarizing the data.

# fitting the regression model
m1 <-lm(Value~P+County+Year,prodscore)
summary(m1)

# According to the summary, the most interested variable "P" has the positive
# effect on Value, if P increases by 1 unit which resulting increase of value
# by 5.378e+00 by taking into account of other regressors.almost all regressors 
# are significant other than some countries.but we want to decide whether the 
# variable p is a good regressor or not.
qt(1-0.05/2,114)
# our t value is 5.356 which is more than the significant level of quantile α=0.05 
# which is 1.980992.The p-value is also approximately zero which means the variable
# p is significant means it rejects the null hypothesis.so, p has a strong effect on
# value, it is a good predictor.
# the decision makers can make decisions of taxes on soil productivity land by using the the 
# average assessed value.


# 8.3
data(water)
# 8.3.1-visualizing the data
pairs(~APMAM+APSAB+APSLAKE+OPBPC+OPRC+OPSLAKE+BSAAM+Year,data = water)
# By analyzing the plots, the variables A's have linear models and also have 
# constant variances also the same thing with 'O' variables but the variable BSAAM
# has strong linear relation with 'O' variables. the most common thing is
# all the variable plots are right asymmetric. from variable year i can only say that
# it has a outlier point but according to the question we should remove it.

# 8.3.2- lets do the power-transformation of box-cox
pt1 <- powerTransform(cbind(water$APMAM,water$APSAB,water$APSLAKE,water$OPBPC,water$OPRC,water$OPSLAKE))
summary(pt1)
qt(1-0.05/2,6)
# the estimated lambda values for the transformed model is given in the summary
# we also have the confidence intervals of the given all lambdas. according to 
# the hypothesis test(H0=lambda=0) the p value is not significant which means 
# we don't reject null hypothesis.so, log model is better which the lamda=0 model
# is better but im not sure because the t value is more than quantileα=0.05 where 
# the significant value of quantile is 2.446912.

# lets check the linearity of the predictors using transformed model using residual plots
m2<-lm(water$BSAAM~I(water$APMAM^0.0982)+I(water$APSAB^0.3450)+I(water$APSLAKE^0.0818)+I(water$OPBPC^0.0982)+I(water$OPRC^0.2536)+I(water$OPSLAKE^0.2534))
residualPlots(m2,smooth=TRUE)
# by using the function residual plots we can state that for all the A's predictors
# we get the best linear model which so close to zero which means the observations are
# symmetric in these models and the transformation works for A's variables. But for
# the O's and tukey test plots the power transformation doesn't work which means 
# there is quadratic and non parametric effect in the model.so, for these models
# our plot is not near null.so, its better to find another transformation models.

# 8.3.3,8.3.4- by analyzing the summary of the power transformation we know that 
# log transformation is better than previous model using hypothesis test. so lets 
# do the fit the log model and check
m3<-lm(log(BSAAM)~log(APMAM)+log(APSAB)+log(APSLAKE)+log(OPBPC)+log(OPRC)+log(OPSLAKE),water)
pairs(log(BSAAM)~log(APMAM)+log(APSAB)+log(APSLAKE)+log(OPBPC)+log(OPRC)+log(OPSLAKE),water)
# The log transformation for the response log(BSAAM) and all the predictors because
# i get the strong relation from all the plots associated with the response. so, the transformation works.

summary(m3)
# in the given summary of the given regression model the negative value coefficients 
# are for the log(APMAM)and log(APSAB) and those are -0.02033 and -0.10303. i really 
# cannot interpret why i got the negative coefficient only i can say is i see some leverage
# points in the associated plots.so maybe because of these points i get negative values.

# because of the high correlation between the predictors the coeffecient are negative.
#8.3.5
H0 <- lm(log(BSAAM)~I(log(OPBPC)+log(OPRC)+log(OPSLAKE)),water)
summary(H0)
qf(1-0.05,5,36)
# f-test
(sum(H0$res^2)-sum(m3$res^2))/5
sum(m3$res^2)/36
0.02772201/0.01034542
# 9.8 the residual function of the given log model
residualPlots(m3,smooth=T)
# according the residual plot we get the plots which all almost null plots so i dont
# see any effect log model for all the variables also for the fitted residual plot
# but i see the some curvature for the fitted values but most of the valuesd are 
# in the middle so due one or two outliners we get some significance. if we 
# remove them then our model is well fitted model which shows no evidence of the
# quadratic curve.


# 9.1 
data(Rpdata)
pairs(y~x1+x2+x3+x4+x5+x6,Rpdata)
m4<-lm(y~x1+x2+x3+x4+x5+x6,Rpdata)
summary(m4)       
residualPlots(m4,smooth=T)

#9.16

