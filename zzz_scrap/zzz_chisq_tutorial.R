rm(list=ls())

# Create a contingency table
data <- data.frame(X = c("A", "B", "C", "A", "B", "C"),
                   Y = c(1, 0, 1, 0, 1, 0))

# Create a contingency table
contingency_table <- table(data$X, data$Y)

# Perform the chi-squared test
result <- chisq.test(contingency_table)

# Print the results
print(result)




# install.packages("mosaic")

# Load the mosaic package
library(mosaic)

# Create a data frame
data <- data.frame(X = c("A", "B", "C", "A", "B", "C"),
                   Y = factor(c(1, 0, 1, 0, 1, 0)))

# Perform the chi-squared test

mosaic::chisq(data$X, data$Y)



