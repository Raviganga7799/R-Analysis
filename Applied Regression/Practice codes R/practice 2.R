library(alr4)
m0<- read.csv("I_Capital.txt", sep="")
str(data)
pairs(m0[,3:12])
pairs(ATO~HumanC + RelationalC + InnovationC + LogSales + Leverage + 
        ProcessC,m0)
m1<-lm(ATO~HumanC + RelationalC + InnovationC + LogSales + Leverage + 
         ProcessC,m0)
summary(m1)
residualPlots(m1)
pt1<-powerTransform(cbind(m1$))
