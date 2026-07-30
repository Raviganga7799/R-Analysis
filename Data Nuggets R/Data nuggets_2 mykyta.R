# uploading the datanugget-package
library(datanugget)
library(plotly)
library(dplyr)
library(psych)
iris
iris <- iris[, -5]
iris <- iris[, -4]
iris <- iris[, -3]
##################################

# testing "create.DN" function (small example)
suppressMessages({
  my.DN1 = create.DN(x = iris,
                    RS.num = 150,
                    DN.num1 = 75,
                    DN.num2 = 10,
                    dist.metric = "euclidean",
                    seed = 291102,
                    no.cores = (detectCores() - 1),
                    make.pbs = TRUE)
})

my.DN1$`Data Nuggets`
my.DN1$`Data Nugget Assignments`

df001 <- data.frame(my.DN1$`Data Nugget Assignments`)
iris01 <- cbind(iris, df001)
colnames(iris01) <- c('Sepal.Length','Sepal.Width','DN1assignments')
iris1 <- iris01 %>%
  mutate(Weights = case_when(DN1assignments == 1 ~ 6,
                             DN1assignments == 2 ~ 32,
                             DN1assignments == 3 ~ 21,
                             DN1assignments == 4 ~ 4,
                             DN1assignments == 5 ~ 31,
                             DN1assignments == 6 ~ 12,
                             DN1assignments == 7 ~ 20,
                             DN1assignments == 8 ~ 12,
                             DN1assignments == 9 ~ 2,
                             DN1assignments == 10 ~ 10))
##################################


# Discovering manually the "scale" parameter used my DN package
iris1
DN04 <- data.frame(iris1[iris1$Weights == '4',])
DN04
DN4 <- DN04[c(1,2)]
DN4
cov(DN4)
tr(cov(DN4))/2

var(c(7.6, 7.2, 7.2, 7.7))
##################################

# testing "create.DNcenters" function
suppressMessages({
  DNcen1 <- create.DNcenters(iris,
                            DN.num = 10,
                            dist.metric = "euclidean",
                            make.pb = TRUE)
})
suppressMessages({
  DNcen2 <- create.DNcenters(iris,
                             DN.num = 11,
                             dist.metric = "euclidean",
                             make.pb = TRUE)
})

DNcen1
my.DN1$`Data Nuggets`

diffC1 <- (DNcen1$Sepal.Length - my.DN1$`Data Nuggets`$Center1)
diffC1
sum(abs(diffC1)) 
diffC2 <- DNcen1$Sepal.Width - my.DN1$`Data Nuggets`$Center2
diffC2
sum(abs(diffC2)) 
# The DNcen and CreateDN functions gave different results when identifying the centers of Data Nuggets. 
# I think DNcen simply chooses the most appropriate points from the random sample to make them Data Nuggets 
# centers while CreateDN chooses the best appropriate points (even previously non-existent) to make them
# Data Nuggets centers which I assume should be more accurate (since we are not limited by the RS points only).
##################################

# testing "refine.DN" function (small example)
suppressMessages({
  my.DN2 = refine.DN(iris,
                   my.DN1,
                   scale.tol = .9,
                   shape.tol = .9,
                   min.nugget.size = 2,
                   max.nuggets = 15,
                   scale.max.splits = 10,
                   shape.max.splits = 10,
                   no.cores = (detectCores() - 1),
                   make.pbs = TRUE)
})

my.DN2$`Data Nuggets`
my.DN1$`Data Nuggets`
my.DN2$`Data Nugget Assignments`
# Looks like the results of "my.DN2$`Data Nuggets" and "my.DN$`Data Nuggets" are identical, 
# no significant changes are observed. Maybe it is because the data is created artificially
# and perfectly normally distributed.

df002 <- data.frame(my.DN2$`Data Nugget Assignments`)
iris02 <- cbind(iris, df002)
colnames(iris02) <- c('Sepal.Length','Sepal.Width','DN2assignments')
iris2 <- iris02 %>%
  mutate(Weights = case_when(DN2assignments == 1 ~ 6,
                             DN2assignments == 2 ~ 32,
                             DN2assignments == 3 ~ 21,
                             DN2assignments == 4 ~ 4,
                             DN2assignments == 5 ~ 12,
                             DN2assignments == 6 ~ 20,
                             DN2assignments == 7 ~ 12,
                             DN2assignments == 8 ~ 2,
                             DN2assignments == 9 ~ 10,
                             DN2assignments == 10 ~ 16,
                             DN2assignments == 11 ~ 15))
##################################

# trying to visualize Data Nuggets BEFORE refining
df01 <- data.frame(my.DN1$`Data Nuggets`)
df1 <- df01[order(df01$Weight,decreasing=FALSE),]
p1 <- plot_ly(data = df1) %>% 
  add_markers(x = ~Center1, 
              y = ~Center2,
              text = ~Weight,
              hoverinfo = 'text',
              hovertext = ~paste('Data Nugget includes:', Weight),
              mode = "markers", 
              size = I(100000*df1$Scale), 
              color = c(df1$Weight), 
              colors = c("white","green","Yellow", "orange","red"),
              marker = list(
                line = list(color = "black",
                            width = 2)))

# visually confirming mismatch between DNcen and CreateDN functions
DNcen1.CreateDN1 <- p1 %>% add_trace(x = c(DNcen1$Sepal.Length), y = c(DNcen1$Sepal.Width), 
                                   type = 'scatter', 
                                   mode = 'markers', 
                                   marker = list(size = 25, 
                                                 color = "purple"))
DNcen1.CreateDN1

# adding all iris observations on the plot
DNcen1.CreateDN1.Allpoints <- DNcen1.CreateDN1 %>%  add_trace(x = c(iris1$Sepal.Length), y = c(iris1$Sepal.Width), 
                                                          type = 'scatter', 
                                                          mode = 'markers', 
                                                          color = c(iris1$Weights),
                                                          colors = c("white","green","Yellow", "orange","red"),
                                                          marker = list(size = 10, 
                                                                        line = list(color = "black", width = 1)))
DNcen1.CreateDN1.Allpoints
##################################

# trying to visualize Data Nuggets AFTER refining
df02 <- data.frame(my.DN2$`Data Nuggets`)
df2 <- df02[order(df02$Weight,decreasing=FALSE),]
p2 <- plot_ly(data = df2) %>% 
  add_markers(x = ~Center1, 
              y = ~Center2,
              text = ~Weight,
              hoverinfo = 'text',
              hovertext = ~paste('Data Nugget includes:', Weight),
              mode = "markers", 
              size = I(100000*df2$Scale), 
              color = c(df2$Weight), 
              colors = c("white","green","Yellow", "orange","red"),
              marker = list(
                line = list(color = "black",
                            width = 2)))

# visually confirming mismatch between DNcen and CreateDN functions
DNcen2.CreateDN2 <- p2 %>% add_trace(x = c(DNcen2$Sepal.Length), y = c(DNcen2$Sepal.Width), 
                                    type = 'scatter', 
                                    mode = 'markers', 
                                    marker = list(size = 25, 
                                                  color = "blue"))
DNcen2.CreateDN2

# adding all iris observations on the plot
DNcen2.CreateDN2.Allpoints <- DNcen2.CreateDN2 %>%  add_trace(x = c(iris2$Sepal.Length), y = c(iris2$Sepal.Width), 
                                                              type = 'scatter', 
                                                              mode = 'markers', 
                                                              color = c(iris2$Weights),
                                                              colors = c("white","green","Yellow", "orange","red"),
                                                              marker = list(size = 10, 
                                                                            line = list(color = "black", width = 1)))
DNcen2.CreateDN2.Allpoints
##################################

# comparing the scatterplots BEFORE and AFTER Data Nuggets refining (10DN vs 11DN)
subplot(p1, p2)
subplot(DNcen1.CreateDN1.Allpoints, DNcen2.CreateDN2.Allpoints)
# On the graphs we can see that 2nd largest Data Nugget was split in 2 after refining:
# it contained 31 observations and now there are 2 Data Nuggets each containing 16
# and 15 observations correspondingly

















# non-working code
##################################
##################################
p1
iris001 <- data.frame(iris1)
ptest <- plot_ly (iris1, x = iris001$Sepal.Length , y = iris001$Sepal.Width,
              type = "scatter", 
              mode="markers")

#build it up
for(i in levels(factor(iris001$DN1assignments))){
  #subset data
  dataFilt1 <- iris001[iris001$DN1assignments==i,]
  #add it
  p01 <- add_trace(ptest, x=dataFilt1$Sepal.Length, y=dataFilt1$Sepal.Width,mode="lines", color = c(dataFilt1$DN1assignments),
                   colors = c("white","green","Yellow", "orange","red"))
}

p01
##################################
##################################
##################################
X=c(1,2,1,3,4,6,3)
Y=c(3,5,4,1,5,3,4)
seq=c(1,1,1,2,2,3,3)
dataX <- data.frame(X,Y,seq)

NO <- "this won't work"
plot_ly (dataX, x = dataX$X , y = dataX$Y,
                 type = "scatter", 
                 mode="markers") %>%
  add_trace(NO, mode="lines")

#make the plot
p <- plot_ly (dataX, x = dataX$X , y = dataX$Y,
              type = "scatter", 
              mode="markers")

#build it up
for(i in levels(factor(dataX$seq))){
  #subset data
  dataFilt <- dataX[dataX$seq==i,]
  #add it
  p <- add_trace(p, x=dataFilt$X, y=dataFilt$Y,mode="lines",color ='yellow')
}
p








p1
points(df1$Center1, df1$Center2, col='black', pch=19)
add_trace(x = c(5, 8), y = c(4, 4), type = "scatter", mode = "lines", line = list(color = 'grey')
          , showlegend = FALSE)
























# trying to visualize Data Nuggets before refining
df01 <- data.frame(my.DN$`Data Nuggets`)
df1 <- df01[order(df$Weight,decreasing=FALSE),]
plot_ly(type = 'scatter', x = df1$Center1, y = df1$Center2, 
        mode = "markers", 
        size = I(10000*df2$Scale), 
        color = c(df2$Weight), 
        colors = c("white","green","Yellow", "orange","red"),
        add_markers = list (width = 2))
#################

# trying to visualize Data Nuggets before refining
df01 <- data.frame(my.DN$`Data Nuggets`)
df1 <- df01[order(df$Weight,decreasing=FALSE),]
plot_ly(type = 'scatter', x = df1$Center1, y = df1$Center2,
        
        mode = "markers",
        text = ~df2$Weight
        size = I(10000*df2$Scale), 
        color = c(df2$Weight), 
        colors = c("white","green","Yellow", "orange","red"))
#################

# trying to visualize Data Nuggets after refining
df02 <- data.frame(my.DN2$`Data Nuggets`)
df2 <- df02[order(df$Weight,decreasing=FALSE),]
plot_ly(type = 'scatter', x = df2$Center1, y = df2$Center2, 
        mode = "markers", 
        size = I(10000*df2$Scale), 
        color = c(df2$Weight), 
        colors = c("white","green","Yellow", "orange","red"))
#################

plot_ly(type = 'scatter', x = df2$Center1, y = df2$Center2, mode = "markers", size = I(10000*df2$Scale), color_discrete_sequence=["red"])
plot_ly(type = 'scatter', x = df2$Center1, y = df2$Center2, mode = "markers", size = I(10000*df2$Scale), color = df2$Weight, colors = c("white","green","blue","red"))
# need to write an explanation



##############################################################################z
##############################################################################
##############################################################################