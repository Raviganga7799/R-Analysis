library(ggplot2)
library(datanugget)
library(dplyr)
library(plotly)
data(cars)
# creating a subset of weights
weights=(1/cars$speed^2)
# ploting the given data WLS
plot(cars$speed,cars$dist)
m0=lm(dist~speed,cars,weights = weights)
residual
abline(m0)


# Creating data nuggets for the given data
X=create.DN(cars,
            
            RS.num = 50,
            DN.num1 =15,
            DN.num2 = 15,
            dist.metric = "euclidean",
            seed = 25000,
            no.cores = (detectCores() - 1),
            make.pbs = TRUE)

X$`Data Nuggets`
X$`Data Nugget Assignments`
# Creating a data frame using the result of create.dn function.
XX=data.frame(X[["Data Nuggets"]])

# Creating Data centers from a Random Sample
Y=create.DNcenters(cars,
                 DN.num=6,
                 dist.metric='euclidean',
                 make.pb = TRUE)
Y

# Using the refine.dn function
Z= refine.DN(cars,
             X,
             scale.tol = .9,
             shape.tol = .9,
             min.nugget.size = 6,
             max.nuggets = 20,
             scale.max.splits =15,
             shape.max.splits = 15,
             seed = 250000,
             no.cores = (detectCores() - 1),
             make.pbs = TRUE)
Z
# Creating a data frame using the result of refine.dn function.
ZZ=data.frame(Z[["Data Nuggets"]])

# plotting the intial data nuggets before refining
Cplot=plot_ly(data = XX) %>% 
  add_markers(x = ~Center1, 
              y = ~Center2,
              text = ~Weight,
              type = "scatter",
              mode="markers",
              
              size = XX$Scale,
              color =c(XX$Weight) ,
              colors = 'Paired',
        marker = list(opacity = 1.0, sizemode = 'diameter'))
Cplot

# plotting for comparision of Cplot and Y(data centers from the random sample) points
C1plot <- Cplot %>% add_trace(x = c(Y$speed), y = c(Y$dist), 
                                     type = 'scatter', 
                                     mode = 'markers',
                                     marker = list(size=20,color="red",opacity = 1, sizemode = 'diameter'))
C1plot

# Creating a data set which has information combining the X and cars to visualize
X1=data.frame(X[["Data Nugget Assignments"]])
X2=cbind(cars,X1)
colnames(X2)=c('speed','dist','XAssignments')
X3=X2 %>% mutate(weights=case_when(
  XAssignments==1~2,
  XAssignments==2~4,
  XAssignments==3~6,
  XAssignments==4~1,
  XAssignments==5~1,
  XAssignments==6~2,
  XAssignments==7~3,
  XAssignments==8~8,
  XAssignments==9~5,
  XAssignments==10~5,
  XAssignments==11~2,
  XAssignments==12~3,
  XAssignments==13~3,
  XAssignments==14~1,
  XAssignments==15~4
))
# adding all cars observations on the plot
C2plot<- Cplot %>%  add_trace(x = c(X3$speed), y = c(X3$dist), 
                                                              type = 'scatter', 
                                                              mode = 'markers', 
                                                              color = c(X3$weights),
                                                              colors = "paired",
                                                              marker = list(size = 10, 
                                                                            line = list(color = "black", width = 1)))
C2plot


#Ploting after refining
Rplot=plot_ly(data = ZZ) %>% 
  add_markers(x = ~Center1, 
              y = ~Center2,
              text = ~Weight,
              type = "scatter",
              mode="markers",
              size = ZZ$Scale,
              color =c(ZZ$Weight) ,
              colors = 'Paired',
              marker = list(opacity = 1.0, sizemode = 'diameter'))
Rplot
# plotting for comparision of Rplot and Y(data centers from the random sample) points
R1plot <- Rplot %>% add_trace(x = c(Y$speed), y = c(Y$dist), 
                              type = 'scatter', 
                              mode = 'markers',
                              marker = list(size=20,color="red",opacity = 1, sizemode = 'diameter'))
R1plot

# Creating a data set which has information combining the Z and cars to visualize
X4=data.frame(Z[["Data Nugget Assignments"]])
X5=cbind(cars,X4)
colnames(X5)=c('speed','dist','ZAssignments')
X6=X5 %>% mutate(weights=case_when(
  ZAssignments==1~19,
  ZAssignments==2~10,
  ZAssignments==3~4,
  ZAssignments==4~1,
  ZAssignments==5~3,
  ZAssignments==6~7,
  ZAssignments==7~6
))
# adding all cars observations on the plot
R2plot<- R1plot %>%  add_trace(x = c(X6$speed), y = c(X6$dist), 
                               type = 'scatter', 
                               mode = 'markers', 
                               color = c(X6$weights),
                               colors = c("brown","green","yellow", "orange","red"),
                               marker = list(size = 10, 
                                             line = list(color = "black", width = 1)))
R2plot
plots=subplot(C2plot,R2plot)
plots


# creating a for loop to identify the different combinations of the data nuggets to create(using Create.DN)
create <- vector("list", length = length(X))
for (i in 5:15) {create[i]<-
  create.DN(cars,
           RS.num = 50,
              DN.num1 =15,
              DN.num2 = i,
              dist.metric = "euclidean",
              seed = 25000,
              no.cores = (detectCores() - 1),
              make.pbs = TRUE)
  
}
print(create)

# creating a vector of the previous create and make it as a length also 
# using for loop to create a linear model with the specified data nugget centers
x <- vector("list", length = length(create))
for (k in 5:10) {
  if (is.data.frame(create[[k]])) {
    x[[k]] <- lm(Center2 ~ Center1, data = create[[k]])
  } else {
    warning(paste0("Skipping element ", k, " - not a data frame"))
  }
}

print(x)

# finding the Residual sum of squares of lm models 
rs <- rep(NA, 6) # create an empty vector of length 6
for (r in c(5:10)) {
  model <- lm(Center2 ~ Center1, data = create[[r]])
  rs[r] <- deviance(model) # assign deviance to the appropriate element of the vector
}


print(rs)

# as we know there are no 1 and 2 data nuggets in model we counte from 3 so i get 8 rss values and ploting 
# with the no of data nuggets
plot(create[[8]][["Data Nugget"]],rs)
   
create[[r]]$Scale


for (m in (10:15)) ki[m]=as.data.frame(create[m])

     