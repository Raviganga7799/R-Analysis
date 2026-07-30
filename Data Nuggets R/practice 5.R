library(AppliedPredictiveModeling)
library(datanugget)
library(ggplot2)
library(dplyr)
library(plotly)
library(SPlit)
data(abalone)
m0=abalone[,c(4,9)]
m0=na.omit(m0)
plot(m0)
m1=lm(Rings~Height,m0)
abline(m1,lwd=3,col="red")
m1$coefficients

x <- list()
for (i in 5:15) {x[[i]]<-
  create.DN(m0,
            RS.num = 2000,
            DN.num1 =200,
            DN.num2 = i,
            dist.metric = "euclidean",
            seed = 25000,
            no.cores = (detectCores() - 1),
            make.pbs = TRUE)

}
print(x)


y=list()

for (z in 5:15){
  mi=as.data.frame(x[[z]]$` Data Nuggets`)
  y[[z]]<-lm(Center2~Center1,data=mi,weights = Weight/Scale)
}
print(y)

pred_list <- list()
for (i in 3:18) {
  data_df <- as.data.frame(x[[i]]$`Data Nuggets`)
  pred_list[[i]] <- predict(y[[i]], data_df)
  
}



