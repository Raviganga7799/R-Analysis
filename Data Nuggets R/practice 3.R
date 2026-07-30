# Required Libraries
library(ggplot2)
library(datanugget)
library(dplyr)
library(plotly)

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
for (i in 3:18) {x[i]<-
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


# creating a vector with length 20 and comparing the coefficients of population model
# and data nugget model centers of every combination. 
  
m1=numeric()
for(i in 3:18){
  m1[i]=sum(m0$coefficients-y[[i]][["coefficients"]])^2
}
print(m1)
plot(m1[-c(1:2)]~c(3:18))

# by using the result of the beta's sum of squares i have plotted graph using population 
# betas,DN(12),DN(13).
ggplot() +
  # plot other_data as points
  geom_point(data = IRIS, aes(x = Sepal.Length, y = Sepal.Width), color = "blue") +
  # plot new_data as points
  geom_point(data = x[[3]], aes(x = Center1, y = Center2), color = "red") +
  geom_point(data = x[[13]], aes(x = Center1, y = Center2), color = "green") +
  
  # add fit line for other_data
  geom_smooth(data = IRIS, aes(x = Sepal.Length, y = Sepal.Width), method = "lm", color = "blue") +
  # add fit line for new_data
  geom_smooth(data = x[[3]], aes(x = Center1, y = Center2), method = "lm", color = "red") +
  geom_smooth(data = x[[13]], aes(x = Center1, y = Center2), method = "lm", color = "green") +
  
  # add a title and axis labels
  ggtitle("Comparing population and DN centers") +
  xlab("SepalLength") +
  ylab("SepalWidth")








