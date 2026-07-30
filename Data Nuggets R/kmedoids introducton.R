# Create a data frame
data <- data.frame(x = rnorm(100), y = rnorm(100)),

# Set the sample size
sample_size <- 10

# Perform uniform random sampling
sampled_data <- data[sample(nrow(data), size = sample_si,mze, replace = FALSE), ]
