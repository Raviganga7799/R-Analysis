# Required Libraries
library(ggplot2)
library(datanugget)
library(dplyr)
library(plotly)
library(SPlit)
SPlitIndices = SPlit(IRIS,splitRatio = 0.05)
irisTest = IRIS[SPlitIndices, ]
irisTrain = IRIS[-SPlitIndices, ]
plot(IRIS, main = "SPlit testing set")
points(irisTest, col = 'green', cex = 2)
points(irisTrain,col='red')
mx=SPlitIndices
mx
# dataset and removing unwanted columns.
data("iris")
iris <- iris[, -5]
iris <- iris[, -4]
IRIS <- iris[, -3]

# creating linear model(population) 
plot(Sepal.Width~Sepal.Length,iris)
m0=lm(Sepal.Width~Sepal.Length,iris)
abline(m0)
m0$coefficients


# creating a for loop to identify the different 
# combinations of the data nuggets to create(using Create.DN)

x <- list()
for (i in 3:18) {x[[i]]<-
  create.DN(IRIS,
            RS.num = 50,
            DN.num1 =25,
            DN.num2 = i,
            dist.metric = "euclidean",
            seed = 25000,
            no.cores = (detectCores() - 1),
            make.pbs = TRUE)

}
print(x)

# creating a vector with the length of previous x vector and
# creating weighted least squares regression using the data nuggets from x.
y=list()

for (z in 3:18){
  mi=as.data.frame(x[[z]]$`Data Nuggets`)
  y[[z]]<-lm(Center2~Center1,data=mi,weights = Weight/Scale)
}
print(y)

pred_list <- list()
for (i in 3:18) {
  data_df <- as.data.frame(x[[i]]$`Data Nuggets`)
  pred_list[[i]] <- predict(y[[i]], data_df)

}

ols_pred <- predict(m0, newdata = IRIS)


mse_list <- list()
for (i in 3:18) {
  true_vals <- x[[i]][["Data Nuggets"]][["Center2"]]
  pred_vals <- pred_list[[i]]
  mse <- mean((true_vals - pred_vals)^2)
  mse_list[[i]] <- mse
}
mse_list

wlsMSE <- mean((wls_pred -x13$Center2)^2)
olsMSE <- mean((ols_pred - IRIS$Sepal.Width)^2)

wlsMSE
olsMSE
plot(unlist(mse_list[-c(1:2)]))


m5=list()
for (i in 3:18) {
 
  m5[i]= refine.DN(IRIS,
                x[[i]],
                scale.tol = .9,
                shape.tol = .9,
                min.nugget.size = 3,
                max.nuggets = 25,
                scale.max.splits = 10,
                shape.max.splits = 10,
                no.cores = (detectCores() - 1),
                make.pbs = TRUE)
  
}
m5
m6=list()
for (i in 3){

m6[[i]] <- predict(y[[i]], newdata = data.frame(Center1))
}
m6



