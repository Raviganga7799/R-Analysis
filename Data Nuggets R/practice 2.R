library(dplyr)
library(plotly)
library(datanugget)
mo=read.csv("abalone.data",header = FALSE)
colnames(mo)=c("Sex","Length","Diameter","Height","Whole weight","Shucked weight","Viscera weight","shell weight","Rings")
mo
summary(mo)
# selected only the requried variables
m0=mo[,-c(1,3,5,6,7,8,9)]
m0=na.rm(m0)
M0 <- m0 %>%
  sample_n(1000)
# creating a subset of weights
weight=(1/m0$Length^2)
# ploting the given data WLS
plot(m0$Length,m0$Height)
m1=lm(Height~Length,m0,weights = weight)
abline(m0,lwd=3,col="red")
# Creating data nuggets for the given data
X=create.DN(M0,
            
            RS.num = 1000,
            DN.num1 =500,
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
Y=create.DNcenters(M0,
                   DN.num=15,
                   dist.metric='euclidean',
                   make.pb = TRUE)
Y
# Using the refine.dn function
Z= refine.DN(M0,
             X,
             scale.tol = .9,
             shape.tol = .9,
             min.nugget.size = 8,
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
C1plot <- Cplot %>% add_trace(x = c(Y$Length), y = c(Y$Height), 
                              type = 'scatter', 
                              mode = 'markers',
                              marker = list(size=20,color="red",opacity = 1, sizemode = 'diameter'))
C1plot

