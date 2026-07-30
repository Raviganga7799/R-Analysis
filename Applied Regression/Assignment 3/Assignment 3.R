##############################
### ASSIGNMENT-3 ############
#############################
#PROBLEM-2.16
#A)
library(alr4)
scatterplot((fertility)~(ppgdp),UN11)
?scatterplot
#B)
m1 <- lm(log(fertility) ~ log(ppgdp),UN11)
plot(log(fertility) ~ log(ppgdp),UN11)
abline(m1)
#C)
summary(m1)
#Coefficient of determination is 0.526, which shows us that 52% of variability of values of Y explained on X

#D)
predict <-predict(m1, newdata=data.frame(ppgdp=c(1000)),interval="prediction", level=0.95)
exp(predict)


#E)
win.graph() 

with(UN11,plot(log(fertility)~log(ppgdp)))

with(UN11, abline(lm(log(fertility)~log(ppgdp)), lwd=2))
#Plotting in Graph needed points
identify(log(UN11$ppgdp),log(UN11$fertility),tolerance=1,n=10)


#MAX and MIN Values
print(UN11[UN11$fertility ==  max(UN11$fertility),][1])
print(UN11[UN11$fertility ==  min(UN11$fertility),][1])
#Largest Residual value
print(sort(residuals(m1))[199])
print(sort(residuals(m1))[1])
#prof Answer
#2.16.5
predict(m1, newdata=data.frame(ppgdp=c(1000)),interval="prediction", level=0.95)
points(c(0.6258791)~log(1000),col="blue",pch=19 , cex=1.5)
points(c(1.843256)~log(1000),col="blue",pch=19 , cex=1.5)

#Exp
win.graph()
with(UN11,plot(fertility~ppgdp))
points(c(exp(0.6258791))~c(1000),col="blue",pch=19 , cex=1.5)
points(c(exp(1.843256))~c(1000),col="blue",pch=19 , cex=1.5)


#PROBLEM-2


names(salary)
with(salary, boxplot(year~sex))


with(salary, plot(salary~year, pch=19))
with(salary, abline(lm(salary~year),lwd=2))
with(salary, plot(salary~year, col=ifelse(sex=="Male", "blue", "red"),pch=20))
data("salary")
with(salary[salary$sex=="Female",], abline(lm(salary~year),col="red",lwd=2))
with(salary[salary$sex=="Male",], abline(lm(salary~year),col="blue",lwd=2))

#Confidence interval of 95%
m2 <- lm(salary~sex+year+sex*year,salary)

confint(m2,level = 0.95)