##########################
library(alr4)
head(cathedral)
# predictor: total length of medieval English cathedral, feet (Length)
# response: nave height of medieval English cathedral, feet (Height)
##########################

# dividing the data into groups for separate plotting
Romanesque=subset(cathedral,Type=='Romanesque')
Gothic=subset(cathedral,Type=='Gothic')

# fitting simple linear regression model for Romanesque style cathedrals
m1=lm(Height~Length,data=Romanesque)

# drawing a scatterplot of Length on the vertical axis versus Height on the horizontal axis
# for Romanesque cathedral type
plot(Romanesque$Height~Romanesque$Length, xlim=c(150,650), ylim=c(40,110))
abline(m1, col="red", lwd=2)
summary(m1)
mean(Romanesque$Height)
var(Romanesque$Height)
var(Gothic$Height)
# The total Length of Romanesque style cathedral has little to no effect on its nave Height,
# the Height's variance is relatively low and is close to 74 feet regardless of the Length.
# In this model the effect of Length isn't statistically significant at α=0.05
# given the estimated t value (0.74<2.365), which means the data doesn't provide enough 
# evidence against H0 that Height is independent of Length.
qt(1-0.05/2, 7)

# fitting a quadratic regression model for Romanesque style cathedrals
Romanesque$Length2<-Romanesque$Length^2
m2<-lm(Height~Length+Length2, data=Romanesque)
summary(m2)
# Still, given the estimated t value for the regressors (<2.446912) the data doesn't
# provide enough evidence to reject H0 that the Romanesque style cathedral's Height 
# doesn't depend on its Length
qt(1-0.05/2, 6)

# computing the maximum of the quadratic mean function
xM<--8.379e-01/(2*-8.940e-04)
xM
# According to the model, observed cathedral's Height is increasing with increased cathedral's 
# Length reaches 468.62 feet while after this point the relationship between variables
# becomes negative and then cathedral's Height starts to decrease

# plotting a quadratic regression model
LengthValues <- seq(150, 650, 1)
LengthPredictRomanesque<-predict(m2,list(Length=LengthValues, Length2=LengthValues^2))
lines(LengthValues, LengthPredictRomanesque, col='green', lwd=2)

##########################
##########################

# fitting simple linear regression model for Gothic style cathedrals
m3=lm(Height~Length,data=Gothic)

# drawing a scatterplot of Length on the vertical axis versus Height on the horizontal axis
# for Gothic cathedral type
plot(Gothic$Height~Gothic$Length, xlim=c(150,650), ylim=c(40,110))
abline(m3, col="blue", lwd=2)
summary(m3)
# The total Length of Gothic style cathedral has significant positive effect on its nave 
# Height. If we take 2 Gothic style cathedrals one of which is 1 feet Longer than another,
# on average its Height will be 0.114 feet higher.
# In this model the effect of Length is statistically significant at α=0.05
# given the estimated t value (4.207>2.144787), which means the data provides us 
# with enough evidence against H0 that Height is independent of Length.
qt(1-0.05/2, 14)

# fitting a quadratic regression model for Romanesque style cathedrals
Gothic$Length2<-Gothic$Length^2
m4<-lm(Height~Length+Length2, data=Gothic)
summary(m4)
# Comparing m4 with m3 I can conclude that the quadratic model is abundant for describing 
# the correlation between the variables given the values of the estimated t values (<2.16)
# that suggest that the effect of the regressors is statistically insignificant at α=0.05
qt(1-0.05/2, 13)

# computing the maximum of the quadratic mean function
xM<--0.0843467/(2*0.0002540)
xM
# According to the model, observed cathedral's Height is increasing with increased cathedral's 
# Length reaches 468.62 feet while after this point the relationship between variables
# becomes negative and then cathedral's Height starts to decrease

# plotting a quadratic regression model
LengthPredictGothic<-predict(m4,list(Length=LengthValues, Length2=LengthValues^2))
lines(LengthValues, LengthPredictGothic, col='purple', lwd=2)