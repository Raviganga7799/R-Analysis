########### Practice ##########
# required packages
library(alr4)
library(readr)

# Lets import our data set
m0 <- read_csv("Spotify.csv")
# lets see our data variables
View(Spotify)
# lets visualize the data set
pairs(popularity ~ danceability+key+X+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature, data = Spotify)
# According to the marginal plots i see some linear relationship with some of the variables 
# and it also contains some of influential points it may be a concern later also the plots are assymetric.

# lets build the regression model 
m1<-lm( popularity ~ danceability+key+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature, data = Spotify)
# Lets check the correlation between the predictors
vif(m1)
# i used variable inflation factor to check the correlation between the predictors but 
# i see slight correlation. so it is acceptable.
# if we see any correlation then we can do this: find mean and add it to the model example
X<-(Spotify$energy+Spotify$loudness/2)
# lets see summary if we find any information. i amazed by seeing non significant values
summary(m1)
# using residual
residualPlots(m1,smooth=T)
# i can say that mostly my residual plots done well in the test of curvature which means
# all the predictors don't have enough evidence to reject the p-value where there are not significant
# almost other than slight significance in the fitted and key so lets go for the transformation.
m2<-lm( popularity ~ danceability+key+I(key^2)+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature, data = Spotify)
residualPlots(m2,smooth=T)
m3<-lm( popularity ~ danceability+key+I(key^2)+energy+loudness+mode+acousticness+instrumentalness+liveness+I(valence*speechiness)+tempo+duration_ms+time_signature, data = Spotify)
residualPlots(m3,smooth=T)
# finally without doing the transformations i got the equation.
ncvTest(m3,~instrumentalness)
# here our constant variance assumption not met so i tred to do the weighted least square 
# but it doesnt happen
sd(m0$popularity+m0$danceability+m0$key+I(m0$key^2)+m0$loudness+m0$mode+m0$acousticness+m0$instrumentalness+m0$liveness+I(m0$valence*m0$speechiness)+m0$tempo+m0$duration_ms+m0$time_signature)

# the alternative is to use the transformation
pt <- powerTransform(cbind(m0$popularity,m0$danceability,m0$energy,m0$speechiness,m0$acousticness,m0$liveness,m0$valence,m0$tempo,m0$duration_ms,m0$time_signature))
summary(pt)
# this is the new transformed results but i removed some of the variables which is not need for me
# to give a summary regarding the question
m5<-lm(I(m0$popularity^7.0615)~I(m0$danceability^ 2.3597)+I(m0$energy^ 1.0393)+I(m0$speechiness^-0.4068)+I(m0$acousticness^0.4964 )+I(m0$liveness^-0.4997)+I(m0$valence^0.8253)+I(m0$tempo^0.0478)+I(m0$duration_ms^ 0.1369))
residualPlots(m5,smooth=T)
# our new transformed model is good
ncvTest(m5)
ncvTest(m5,~I(m0$danceability^ 2.3597),level=0.05)
ncvTest(m5,~I(m0$energy^ 1.0393))
ncvTest(m5,~I(m0$speechiness^-0.4068))
ncvTest(m5,~I(m0$acousticness^0.4964 ))
ncvTest(m5,~I(m0$valence^0.8253))
ncvTest(m5,~I(m0$tempo^0.0478))
ncvTest(m5,~I(m0$duration_ms^ 0.1369))
ncvTest(m5,I(m0$time_signature^86.9072))

# for the time signature we can do nothing because it the data is recorded like that
plot(m5)

outlierTest(m5)

#no outliers detected because our bonferroni p-value is high so we dont reject the null hypothesis

influenceIndexPlot(m1)
max(cooks.distance(m1))

B0<-Boot(m5,R=999)
summary(B0)
confint(B0)
hist(B0)
summary(m5)

