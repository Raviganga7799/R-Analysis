# Required Libraries
library(ggplot2)
library(datanugget)
library(dplyr)
library(plotly)
library(SPlit)

# Importing dataset
data("iris")
iris <- iris[, -5]
iris <- iris[, -4]
IRIS <- iris[, -3]

# split using the "split" function

SPlitIndices = SPlit(IRIS,splitRatio = 0.05)
irisTest = IRIS[SPlitIndices, ]
irisTrain = IRIS[-SPlitIndices, ]

par(mfrow = c(1, 2))

plot(IRIS, main = "SPlit testing set")
points(irisTest, col = 'red', cex = 2)
points(irisTrain,col='blue')
mx=SPlitIndices
mx

# Randomized split the same data.
# randomly split the dataset into 70% training data and 30% test data
train_idx <- sample(nrow(IRIS), round(0.95 * nrow(IRIS)), replace = FALSE)
train_data <- IRIS[train_idx, ]
test_data <- IRIS[-train_idx, ]

plot(IRIS, main = "Randomized testing set")
points(test_data, col = 'red', cex = 2)
points(train_data,col='blue')

# creating data nugget centers
xc<-create.DN(IRIS,
            RS.num = 50,
            DN.num1 =25,
            DN.num2 = 10,
            dist.metric = "euclidean",
            seed = 25000,
            no.cores = (detectCores() - 1),
            make.pbs = TRUE)
xc


xr=refine.DN(IRIS,
          xc,
          scale.tol = .9,
          shape.tol = .9,
          min.nugget.size = 3,
          max.nuggets = 15,
          scale.max.splits = 10,
          shape.max.splits = 10,
          no.cores = (detectCores() - 1),
          make.pbs = TRUE)
xr

xn=as.data.frame(xr[["Data Nuggets"]])

plot(IRIS, main = "refining data nuggets",col="blue")
points(xn$Center1,xn$Center2,col="red",cex=2)



# i would say that SPlit and refining dataset has almost same data nugget points but 1 more observaions in refining
# data nuggets matters a lot.
