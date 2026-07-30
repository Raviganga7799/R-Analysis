#########################
## ASSIGNMENT_2 #########
#########################

library (alr4)

############# ##############

#### Population + samples: coefficients

# sample size: change this value (start with 5, then substitute 5 with a number of your choice)
# the sample size i took here is 10

size <- 10

rep <- 500

coefficients <- matrix (NA, ncol = 2, nrow = rep)

for (i in 1: rep)
  
{index <- sample (x = seq (1: dim (Heights) [1]), size = size, replace = FALSE)
  
  coefficients [i,] <- with (Heights [index,], lm (dheight ~ mheight) $ coef)}

# Visualizing sampling distributions: many lines on the population plot

with (Heights, plot (dheight ~ mheight, pch = 19, main = "Many lines for many samples"))

with (Heights, abline (lm (dheight ~ mheight), lty = "solid", lwd = 2, col = "red"))

for (i in 1: rep) {abline (coefficients [i,], lty = "dashed")}

  
  ###SAMPLED DISTRUBUTION OF HAT(BETAS)##########
  
  
  hist(coefficients[,1],xlab="Intercept - beta_0", main="Sampling distr. of hat(beta)_0")
  with(Heights, points(0~lm(dheight~mheight)$coef[1],col="red",pch=19))
  
  hist(coefficients[,2],xlab="Slope - beta_1", main="Sampling distr. of hat(beta)_1")
  with(Heights, points(0~lm(dheight~mheight)$coef[2],col="red",pch=19))
  

## Joint sampling distribution of both hat(BETAS)##

plot(coefficients[,2]~coefficients[,1],xlab="Slope - beta_0", ylab="Intercept - beta_1", main="Joint sampling distribution")
with(Heights, points(lm(dheight~mheight)$coef[2]~lm(dheight~mheight)$coef[1],pch=19,col="red"))

##COMPARING CONFIDENCE INTERVALS OF HAT(BETA1) IN 95% and 40% ###

coefficients <- matrix(NA,ncol=2,nrow=rep)
confinterval.beta1.95  <- matrix(NA,ncol=2,nrow=rep)
confinterval.beta1.40  <- matrix(NA,ncol=2,nrow=rep)



for (i in 1:rep)
{
  index <- sample(x=seq(1:dim(Heights)[1]),size=size,replace=FALSE)
  coefficients[i,] <- with(Heights[index,], lm(dheight~mheight)$coef)
  confinterval.beta1.95[i,] <- with(Heights[index,], confint(lm(dheight~mheight),level=0.95)[2,])
  confinterval.beta1.40[i,] <- with(Heights[index,], confint(lm(dheight~mheight),level=0.40)[2,])
}

##### 95% of confidence interval ####
#Sampling distribution of slope
hist(coefficients[,2],xlab="Slope - beta_1", main="95% CI's for beta_1", xlim=c(min(confinterval.beta1.95),max(confinterval.beta1.95)))
with(Heights, points(0~lm(dheight~mheight)$coef[2],col="red",pch=19))

#visualizing 
for (i in 1: c(rep/4))
{
  LL <- confinterval.beta1.95[i,1]
  UL <- confinterval.beta1.95[i,2]
  # interval
  segments(LL, y0=i, x1 = UL, y1 = i, col = "red", lty = par("lty"), lwd = 1)
  # center of the interval:
  points(c(i)~coefficients[i,2],col="blue",pch=19)
  # true value
  with(Heights, points(c(i)~lm(dheight~mheight)$coef[2],col="red",pch=19))
}

####FOR 40% Confidence Interval ######


hist(coefficients[,2],xlab="Slope - beta_1", main="40% CI's for beta_1", xlim=c(min(confinterval.beta1.95),max(confinterval.beta1.95)))
with(Heights, points(0~lm(dheight~mheight)$coef[2],col="red",pch=19))

# Visualizing many confidence intervals 

for (i in 1: c(rep/4))
{
  LL <- confinterval.beta1.40[i,1]
  UL <- confinterval.beta1.40[i,2]
  # interval
  segments(LL, y0=i, x1 = UL, y1 = i, col = "red", lty = par("lty"), lwd = 1)
  # center of the interval:
  points(c(i)~coefficients[i,2],col="blue",pch=19)
  # true value
  with(Heights, points(c(i)~lm(dheight~mheight)$coef[2],col="red",pch=19))
}


########
# PREDICTION
########

with(Heights, plot(dheight~mheight))
with(Heights, abline(lm(dheight~mheight), lwd=2))

m0 <- lm(dheight~mheight,data=Heights)
predict(m0)


# FITTED VALUES CONFIDENCE INTERVAL (CONFIDENCE INTERVAL FOR A CONDITIONAL MEAN)
predict(m0, newdata=data.frame(mheight=c(63)), interval="confidence", level=0.95)

# SINGLE VALUE CONFIDENCE INTERVAL
predict(m0, newdata=data.frame(mheight=c(63)),interval="prediction")
predict(m0, newdata=data.frame(mheight=c(63)),interval="prediction", level=0.95)



#### Plotting confidence intervals at x*=63

estimate <- predict(m0, newdata=data.frame(mheight=c(63)),interval="prediction")[1]

lowerpred <- predict(m0, newdata=data.frame(mheight=c(63)),interval="prediction")[2]
upperpred <- predict(m0, newdata=data.frame(mheight=c(63)),interval="prediction")[3]

upperfitted <- predict(m0, newdata=data.frame(mheight=c(63)),interval="confidence")[3]
lowerfitted <- predict(m0, newdata=data.frame(mheight=c(63)),interval="confidence")[2]

points(estimate~c(63),col="red",pch=19)
points(upperpred~c(63),col="blue",pch=19)
points(lowerpred~c(63),col="blue",pch=19)
points(upperfitted~c(63),col="purple",pch=19)
points(lowerfitted~c(63),col="purple",pch=19)



############
#PROBLEMS###
############

#problem 2.2

names(UBSprices)
with(UBSprices, plot(rice2009~rice2003))

with(UBSprices, abline(lm(rice2009~rice2003), lwd=2))


#FOR ANSWERS PLEASE REFER TO THE PDF

#problem 2.13(A)
names(Heights)
with(Heights, plot(dheight~mheight))
with(Heights, abline(lm(dheight~mheight)))
with(Heights, summary(lm(dheight~mheight)))
var(Heights)

#(B) 
# alpha = 0.01
# 99% Confidence Interval
# Upper limit
0.54175 + qt(1-0.01/2, 1373)* 0.02596   
# Lower limit
0.54175 - qt(1-0.01/2, 1373)* 0.02596   


#(C)99% of Peediction Interval
predict(m0, newdata=data.frame(mheight=c(64)),interval="prediction", level=0.99)



#Problem 2.15(A)
library(alr4)
names(wblake)
plot(Length~Age,wblake)
m1<-lm(Length~Age,wblake)
predict(m1, newdata=data.frame(Age=c(2,4,6)),interval="confidence", level=0.99)

#(B)
predict(m1, newdata=data.frame(Age=c(9)),interval="confidence", level=0.95)


#Question 2.20(A)
plot(Interval~Duration,oldfaith)
abline(lm(Interval~Duration,oldfaith))
m2 <-lm(Interval~Duration,oldfaith)
predict(m2, newdata=data.frame(Duration=c(190)),interval="prediction")
upperfitted <-predict(m2, newdata=data.frame(Duration=c(190)),interval="prediction")[2]
lowerfitted <-predict(m2, newdata=data.frame(Duration=c(190)),interval="prediction")[3] 
points(upperfitted~c(190),col="purple",pch=19)
points(lowerfitted~c(190),col="purple",pch=19)


#(B)
predict(m2, newdata=data.frame(Duration=c(250)),interval="prediction", level = 0.95)
upperfitted <-predict(m2, newdata=data.frame(Duration=c(250)),interval="prediction",level = 0.95)[2]
lowerfitted <-predict(m2, newdata=data.frame(Duration=c(250)),interval="prediction",level = 0.95)[3]
points(upperfitted~c(250),col="purple",pch=19)
points(lowerfitted~c(250),col="purple",pch=19)


#(C) quantile of 90% the normal(0,1) distribution 
qnorm(1-0.10/2)
sumamry(m2)
predict(m2, newdata=data.frame(Duration=c(250)),interval="prediction", level = 0.90)
answer:78.20354+1.64*6.004