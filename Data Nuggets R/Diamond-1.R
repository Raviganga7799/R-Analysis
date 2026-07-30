library(ggplot2)
library(datanugget)
library(dplyr)
library(plotly)
data(cars)

weights=(1/cars$speed^2)

plot(cars$speed,cars$dist)
# Create a scatter plot of the data points
ggplot(data = cars, aes(x = ~Center1, y = ~Center2)) +
  geom_point() +
  
  # Add a line representing the regression equation
  stat_smooth(method = "lm", formula =y~x, se = FALSE, aes(weight = Weight)) +
  
  # Add a title and axis labels
  labs(title = "Weighted Least Squares Regression", x = "speed", y = "dist")




# Create a scatter plot of the data points
ggplot(data = X[["Data Nuggets"]], aes(x = speed, y = dist)) +
  geom_point() +
  
  # Add a line representing the regression equation
  stat_smooth(method = "lm", formula =y~x, se = FALSE, aes(weight = weights)) +
  
  # Add a title and axis labels
  labs(title = "Weighted Least Squares Regression", x = "speed", y = "dist")


X=create.DN(cars,
          
          RS.num = 50,
          DN.num1 =150,
          DN.num2 = 5,
          dist.metric = "euclidean",
          seed = 25000,
          no.cores = (detectCores() - 1),
          make.pbs = TRUE)
# this is the best i can do even i can make the data nugget circles for this but i can do for only the data centers from the 
# create.dn but i couldnt add the initial observations to make the comparision.
plot_ly(data = X$`Data Nuggets`, x = ~Center1, y = ~Center2, text = ~`Data Nugget`, type = "scatter", mode = "lines+markers")


library(ggplot2)

# Create a scatter plot of the cars dataset
ggplot(data = cars, aes(x = speed, y = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE, aes(weight = weights)) +
  labs(title = "Weighted Least Squares Regression", x = "speed", y = "dist")
df <- data.frame(
  "Data Nugget" = c(1, 2, 3),
  "Center1" = c(12.84375, 19.70588, 24),
  "Center2" = c(27.15625, 68.23529, 120),
  "Weight" = c(32, 17, 1),
  "Scale" = c(80.94254, 109.51838, 0)
)

plot_df <- ggplot(data = df, aes(x = Center1, y = Center2)) +
  geom_point(aes(size = Weight, color = Scale)) +
  labs(title = "Original Data", x = "Center 1", y = "Center 2")

plot_dn <- plot_df +
  geom_point(data = X$`Data Nuggets`, aes(x = Center1, y = Center2), shape = 4, size = 4, color = "black") +
  labs(title = "Data Nugget", x = "Center 1", y = "Center 2")

plot_dn

library(plotly)

fig=plot_ly(data = X[["Data Nuggets"]], x = ~Center1, y = ~Center2, text = ~Weight, type = "scatter",  mode = 'markers', size = ~Scale,color = ~`Data Nugget`, colors = 'Paired',
            marker = list(opacity = 0.5, sizemode = 'diameter'))
fig <- fig %>% layout(title = 'Gender Gap in Earnings per University',
                      xaxis = list(showgrid = FALSE),
                      yaxis = list(showgrid = FALSE))
fig

data("diamonds")
diamonds_subset <- diamonds %>% 
  sample_n(2000)



# creating a vector of the previous create and make it as a length. 
x <- vector("list", length = length(create))
# using for loop to create a linear model with the specified data nugget centers
for (k in 3:8) {
  if (is.data.frame(create[[k]])) {
    x[[k]] <- lm(Center2 ~ Center1, data = create[[k]])
  } else {
    warning(paste0("Skipping element ", k, " - not a data frame"))
  }
}

print(x)


for (r in 10:15) {rm[r]=deviance(x[[r]])

}


print(rm)

