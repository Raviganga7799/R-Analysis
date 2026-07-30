##########################################
#### Analysis of DN/LTS using SPlit ######
##########################################


# Required Libraries
library(ggplot2)
library(datanugget)
library(dplyr)
library(plotly)
library(SPlit)
library(robustbase)
library(AppliedPredictiveModeling)


# Importing dataset
data("iris")
iris <- iris[, -5]
iris <- iris[, -4]
IRIS <- iris[, -3]

# creating data nugget centers
M1<-create.DN(IRIS,
              RS.num = 50,
              DN.num1 =25,
              DN.num2 = 10,
              dist.metric = "euclidean",
              seed = 25000,
              no.cores = (detectCores() - 1),
              make.pbs = TRUE)
M1

M2=refine.DN(IRIS,
             M1,
             scale.tol = .9,
             shape.tol = .9,
             min.nugget.size = 3,
             max.nuggets = 15,
             scale.max.splits = 10,
             shape.max.splits = 10,
             no.cores = (detectCores() - 1),
             make.pbs = TRUE)
M2

M3=as.data.frame(M2[["Data Nuggets"]])
# Spiltting Datanuggets (Standard SPlit 70-30%)
SPlitIndices = SPlit(M3,splitRatio = 0.3)
M3Test = M3[SPlitIndices, ]
M3Train = M3[-SPlitIndices, ]
SPlitIndices

# LTS on the Trainset
M4=ltsReg(Center2~Center1,M3Train)
# Plotting LTS on Training DN set
par(mfrow = c(1, 2))
plot(Center2~Center1,M3Train)
abline(M4,col="red",lwd=3)

# Comparing DN Train LTS and LTS on Main(IRIS) dataset
M5=ltsReg(Sepal.Width~Sepal.Length,IRIS)
abline(M5,col="blue",lwd=3)

# Comparing Coefficients of both models
M4$coefficients
M5$coefficients

# Split (80-20%)
SPlitIndices = SPlit(M3,splitRatio = 0.2)
M3Test1 = M3[SPlitIndices, ]
M3Train1 = M3[-SPlitIndices, ]
SPlitIndices


# LTS on the Trainset1
M6=ltsReg(Center2~Center1,M3Train1)

# Plotting LTS on Training1 DN set
plot(Center2~Center1,M3Train1)
abline(M6,col="red",lwd=3)

# Comparing DN Train1 LTS and LTS on Main(IRIS) dataset
abline(M5,col="blue",lwd=3)




################################################################################
data(abalone)
m0=abalone[,c(4,9)]
m0=na.omit(m0)

# creating data nugget centers
m1<-create.DN(m0,
              RS.num = 1000,
              DN.num1 =500,
              DN.num2 = 40,
              dist.metric = "euclidean",
              seed = 25000,
              no.cores = (detectCores() - 1),
              make.pbs = TRUE)
m1


m2=refine.DN(m0,
             m1,
             scale.tol = .9,
             shape.tol = .9,
             min.nugget.size = 25,
             max.nuggets = 50,
             scale.max.splits = 10,
             shape.max.splits = 10,
             no.cores = (detectCores() - 1),
             make.pbs = TRUE)
m2

m3=as.data.frame(m2[["Data Nuggets"]])

# Spiltting Datanuggets (Standard SPlit 70-30%)
SPlitIndices = SPlit(m3,splitRatio = 0.3)
m3Test = m3[SPlitIndices, ]
m3Train = m3[-SPlitIndices, ]
SPlitIndices

# LTS on the Trainset
m4=ltsReg(Center2~Center1,m3Train)

# Plotting LTS on Training DN set
par(mfrow = c(1, 3))
plot(Center2~Center1,m3Train)
abline(m4,col="red",lwd=3)

# Comparing DN Train LTS and LTS on Main(Abalone) dataset
m5=ltsReg(Rings~Height,m0)
abline(m5,col="blue",lwd=3)

# Comparing Coefficients of both models
m4$coefficients
m5$coefficients


# Split (80-20%)
SPlitIndices = SPlit(m3,splitRatio = 0.2)
m3Test1 = m3[SPlitIndices, ]
m3Train1 = m3[-SPlitIndices, ]
SPlitIndices


# LTS on the Trainset1
m6=ltsReg(Center2~Center1,m3Train1)

# Plotting LTS on Training1 DN set
plot(Center2~Center1,m3Train1)
abline(m6,col="red",lwd=3)

# Comparing DN Train1 LTS and LTS on Main(Abalone) dataset
abline(m5,col="blue",lwd=3)
# comparing Coefficients
m6$coefficients
m5$coefficients
m4$coefficients

#############

# Split (90-10%)
SPlitIndices = SPlit(m3,splitRatio = 0.1)
m3Test2 = m3[SPlitIndices, ]
m3Train2 = m3[-SPlitIndices, ]
SPlitIndices


# LTS on the Trainset1
m7=ltsReg(Center2~Center1,m3Train2)

# Plotting LTS on Training1 DN set
plot(Center2~Center1,m3Train2)
abline(m7,col="red",lwd=3)

# Comparing DN Train1 LTS and LTS on Main(Abalone) dataset
abline(m5,col="blue",lwd=3)
m7$coefficients

