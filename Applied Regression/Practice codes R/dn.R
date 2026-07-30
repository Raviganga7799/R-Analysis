# Load required packages
install.packages("fpc")
install.packages("cluster")
library(fpc)
library(cluster)

# Set a seed for reproducibility
set.seed(123)

# Number of data points in each cluster
num_points <- 50

# Mean and standard deviation for each cluster
mean_cluster1 <- c(10, 15)
mean_cluster2 <- c(20, 25)
mean_cluster3 <- c(30, 35)
sd_cluster <- 2

# Generate data for each cluster
cluster1 <- matrix(rnorm(2 * num_points, mean = mean_cluster1, sd = sd_cluster), ncol = 2)
cluster2 <- matrix(rnorm(2 * num_points, mean = mean_cluster2, sd = sd_cluster), ncol = 2)
cluster3 <- matrix(rnorm(2 * num_points, mean = mean_cluster3, sd = sd_cluster), ncol = 2)

# Combine the data from all clusters
data_matrix <- rbind(cluster1, cluster2, cluster3)

# Function to compute WSS for a given clustering result
compute_wss <- function(cluster_result, data_matrix) {
  wss <- 0
  for (i in 1:length(cluster_result$medoids)) {
    cluster_points <- data_matrix[cluster_result$clustering == i, ]
    medoid_index <- cluster_result$medoids[i]
    wss <- wss + sum((cluster_points - data_matrix[medoid_index, ])^2)
  }
  return(wss)
}

# Compute the dissimilarity matrix using Euclidean distance
diss_matrix <- dist(data_matrix, method = "euclidean")

# Define a range of values for K (number of clusters)
k_values <- 2:6

# Perform k-medoids clustering for each value of K and compute WSS
wss_values <- vector()
for (k in k_values) {
  cluster_result <- pam(diss_matrix, k)
  wss_values <- c(wss_values, compute_wss(cluster_result, data_matrix))
}

# Print the WSS values for each K
cat("K values:", k_values, "\n")
cat("Within-Cluster Sum of Squares (WSS) for each K:", wss_values, "\n")



fviz_nbclust(elbow_plot_data, kmeans(), method = "wss")

elbow_plot_data <- data.frame(K = k_values, WSS = wss_values)
ggplot(elbow_plot_data, aes(x = K, y = WSS)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Plot: Within-Cluster Sum of Squares (WSS) vs. Number of Clusters (K)",
       x = "Number of Clusters (K)",
       y = "Within-Cluster Sum of Squares (WSS)")
