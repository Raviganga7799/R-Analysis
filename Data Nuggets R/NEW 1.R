# Libraries Needed
library(AppliedPredictiveModeling)
library(datanugget)
library(ggplot2)
library(dplyr)
library(plotly)
library(SPlit)

#Importing data set
data(abalone)
m0=abalone[,c(4,9)]
m0=na.omit(m0)

# using create function using Data nuggets
x=create.DN(m0,
          RS.num = 2000,
          DN.num1 =200,
          DN.num2 =25,
          dist.metric = "euclidean",
          seed = 25000,
          no.cores = (detectCores() - 1),
          make.pbs = TRUE)

# refining the initial create function
X = refine.DN(m0,
                     x,
                     scale.tol = .9,
                     shape.tol = .9,
                     min.nugget.size = 2,
                     max.nuggets = 30,
                     scale.max.splits = 10,
                     shape.max.splits = 10,
                     no.cores = (detectCores() - 1),
                     make.pbs = TRUE)
# creating a dataframes
Index.dn=as.data.frame(X[[2]])
index.dn=as.data.frame(X[[1]])

#
n1=ceiling(index.dn$Weight/dim(m0)[[1]]*80)

sel1=sample(seq(1:index.dn$Weight[1]),n1[1],replace=F)
m=list(30)

for (i in 1:30){m[[i]]=sample(seq(1:index.dn$Weight[1]),n1[i],replace=F)}

d1=m0[Index.dn==1,]

k=list()

for (i in vector) {
  
}