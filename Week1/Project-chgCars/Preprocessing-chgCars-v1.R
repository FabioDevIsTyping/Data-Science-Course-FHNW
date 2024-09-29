# Load necessary libraries
library(VIM)  # For kNN imputation

# Load the dataset
data <- read.csv("cgh_cars.csv", header = TRUE)
show(data)

# Inspect the data structure
head(data)
str(data)
summary(data)

# Check for missing values in the dataset
colSums(is.na(data))

# Checking for outliers in the 'mpg' column
boxplot(data$mpg, main="MPG Plot", ylab="Miles per Gallon")

# Checking for outliers in the 'price' column
boxplot(data$price, main="Price Plot", ylab="Price")

# Impute missing values in 'mpg' and 'price' using the median
data$mpg[is.na(data$mpg)] <- median(data$mpg, na.rm = TRUE)
data$price[is.na(data$price)] <- median(data$price, na.rm = TRUE)

# Convert negative 'cyl' values to NA for imputation
data$cyl[data$cyl < 0] <- NA

# Impute remaining missing values using kNN
data <- kNN(data, variable = c("cyl", "mpg", "price"), k = 5)

# Remove columns automatically added by kNN
data <- subset(data, select = -c(ends_with("_imp")))

# Double-check for any remaining missing values
colSums(is.na(data))

# Checking for duplicated rows
duplicates <- data[duplicated(data), ]
print(duplicates)

# Exploring categorical columns with bar plots
barplot(table(data$brand), main="Car Brands", col="blue", ylab="Frequency", las=2)
barplot(table(data$type), main="Car Type", col="purple", ylab="Frequency", las=2)
barplot(table(data$style), main="Car Style", col="orange", ylab="Frequency", las=2)

# Checking numerical columns for negative or zero values where not allowed
columns_to_check <- c("mpg", "cyl", "disp", "wt", "gear", "carb", "price")

# Function to check for negative values
check_neg_values <- function(column) {
  negative_values <- data[[column]] < 0
  sum(negative_values)
}

# Function to check for zero values
check_zero_values <- function(column) {
  zero_values <- data[[column]] == 0
  sum(zero_values)
}

# Check for negative and zero values in each column
for (column in columns_to_check) {
  cat("Number of negative values in", column, ":", check_neg_values(column), "\n")
  cat("Number of zero values in", column, ":", check_zero_values(column), "\n")
}

# Corrected numerical columns
num_columns <- c("mpg", "disp", "wt", "price")

# Standardization of numerical columns
data_standardized <- data
data_standardized[num_columns] <- scale(data[num_columns])

# Check the standardized data
summary(data_standardized)

# Final check for missing values in the standardized data
colSums(is.na(data_standardized))

# Show the final standardized dataset
head(data_standardized)
