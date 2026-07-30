    ######################################
        #PROJECT K MEANS clustering#
    ######################################


#Required packages to Install

pkg <- c("tidyverse","ggpolt2")
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) {install.packages(new.pkg, dependencies = TRUE)}
sapply(pkg, require, character.only = TRUE)


#Link to Data set-https://archive.ics.uci.edu/ml/datasets/Iris

# Using R to Perform a K-Means Analysis
#import the data set

iris= read.csv("Iris.data", header= FALSE)
head(iris)
dim(iris)
str(iris)


#Assigning headings for columns

colnames(iris) <- c("sepal.length","sepal.width","petal.length","petal.width","species")


summary(iris)

ggplot(iris, aes(petal.length, petal.width))   + geom_point()

#Clustering process

irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster



table(irisCluster$cluster, iris$species)


#plotting
irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(petal.length, petal.width, color = irisCluster$cluster)) + geom_point()


#Thanking you