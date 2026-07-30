library(alr4)
library(datanugget)

x = Heights
n = 100

trial <- function(x,n){
  DN = create.DN(x,
                 RS.num = 1000,
                 DN.num1 = 500,
                 DN.num2 = 20,
                 dist.metric = "euclidean",
                 seed = 291102,
                 no.cores = (detectCores() - 1),
                 make.pbs = TRUE) 
  DN.ref <- refine.DN(x,
                      DN,
                      scale.tol = .9,
                      shape.tol = .9,
                      min.nugget.size = 2,
                      max.nuggets = 100,
                      scale.max.splits = 5,
                      shape.max.splits = 5,
                      seed = 291102,
                      no.cores = (detectCores() - 1),
                      make.pbs = TRUE)
  dn.out <- data.frame(DN.ref$`Data Nuggets`)

  #stratified sampling
  #nk number of observations to draw from each nugget
  nk = ceiling((dn.out$Weight/dim(x)[1])*n)
  #index.dn tells for each observation to which nugget it belongs
  index.dn <- data.frame(DN.ref[[2]])
  #m indeces of observations to be drawn from the DN
  m=list()
  for(i in 1:dim(dn.out)[1]){
    m[[i]] <- sample(seq(1:dn.out$Weight[i]), nk[i], replace=F)}
  #Create a list d, with the elements from my dataset. it contains all the 
  # original observations  divided by nugget
  d=list()
  for(i in 1:dim(dn.out)[1]){
    d[[i]] <- x[index.dn==i, ]}
  #s contains the observations drawn nugget by nugget
  s=list()
  for(i in 1:dim(dn.out)[1]){
    s[[i]] <- d[[i]][m[[i]],]}
  strat.s <- do.call(rbind, s)
  #print(dim(strat.s)[1])
  #random sampling
  # simple random sampling from the original dataset with size equal to the size
  # of the stratified sample
  selection <-  sample(seq(1:dim(x)[1]), size=dim(strat.s)[1], replace=F, set.seed(1))
  random.s <- x[selection,]
  
  return(list("random.s" = random.s, "strat.s" = strat.s))
  
  }
output <- trial(x, n)

output

#visualizing the plots
plot(output$strat.s, col="red")
points(output$random.s)




library(factoextra)
library(cluster)
# DATA

# using PAM function to check the k value
pam(x, 10,metric = "euclidean",stand = FALSE)

# visualizing the WSS with no of clusters
fviz_nbclust(x, pam, method = "wss")

# selecting the number of clusters from elbow
X1=pam(x,k=3)
X1
summary(X1)
# visualizing the cluster
fviz_cluster(X1,data=x)

plot(X1)
