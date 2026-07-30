library(factoextra)
library(cluster)
# DATA
x=matrix(rnorm(10000,mean=0,sd=1),ncol=5)
x=as.data.frame(x)
# using PAM function to check the k value
m1=pam(x, 4,metric = "euclidean")


wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) { kmn <- kmeans(x, centers = i, nstart = 20)
# Saving the total within sum of squares to wss variable
wss[i] <- kmn$tot.withinss}

# Plot total within sum of squares vs. number of clusters to find k
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")


k <- 3


m1$clusinfo
# visualizing the WSS with no of clusters
fviz_nbclust(x, pam, method = "wss")

fviz_nbclust(x, kmeans, method = "wss")

  

# selecting the number of clusters from elbow
X1=pam(x,k=4)

# visualizing the cluster
fviz_cluster(X1,data=x)

sum(m1$clusinfo[,1]*m1$clusinfo[,3])

dim(x)
dist(x)
m1$medoids
m1$id.med
