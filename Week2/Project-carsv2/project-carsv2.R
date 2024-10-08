# Importing libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(VIM)

# Load the dataset from the current working directory
car_data <- read.csv("cgh_cars.csv")

# Check if the data is loaded correctly
head(car_data)
summary(car_data)

# Checking for outliers in numerical variables
boxplot(car_data$mpg, main="MPG Plot", ylab="Miles per Gallon", col="lightblue")
boxplot(car_data$price, main="Price Plot", ylab="Price", col="lightgreen")
boxplot(car_data$cyl, main="Cylinders Plot", ylab="Cylinders", col="lightcoral")
boxplot(car_data$disp, main="Displacement Plot", ylab="Displacement", col="lightyellow")
boxplot(car_data$wt, main="Weight Plot", ylab="Weight", col="lightcyan")
boxplot(car_data$gear, main="Gear Plot", ylab="Gear", col="lightpink")
boxplot(car_data$carb, main="Carburetors Plot", ylab="Carburetors", col="lightgray")

# Histograms in numerical variables
hist(car_data$mpg, main="Histogram of MPG", xlab="Miles per Gallon (MPG)", col="lightblue", border="black")
hist(car_data$price, main="Histogram of Price", xlab="Price", col="lightgreen", border="black")
hist(car_data$cyl, main="Histogram of Cylinders", xlab="Cylinders", col="lightcoral", border="black")
hist(car_data$disp, main="Histogram of Displacement", xlab="Displacement", col="lightyellow", border="black")
hist(car_data$wt, main="Histogram of Weight", xlab="Weight", col="lightcyan", border="black")
hist(car_data$gear, main="Histogram of Gear", xlab="Gear", col="lightpink", border="black")
hist(car_data$carb, main="Histogram of Carburetors", xlab="Carburetors", col="lightgray", border="black")


# Bar plots for categorical variables
barplot(table(car_data$brand), main="Car Brands", col="lightblue", ylab="Frequency", las=2)
barplot(table(car_data$type), main="Car Type", col="lightgreen", ylab="Frequency", las=2)
barplot(table(car_data$style), main="Car Style", col="lightcoral", ylab="Frequency", las=2)

# Correlation Matrix
numeric_data <- car_data %>% 
  select(mpg, cyl, disp, wt, gear, carb, price)  # Select numeric columns
# Calculate correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")
# Reshape the correlation matrix for ggplot
cor_melt <- melt(cor_matrix)
# Plot heatmap
ggplot(cor_melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Start of data-cleaning
summary(car_data)
columns_to_check <- c("mpg", "cyl", "disp", "wt", "price")
# Replace negative and zero values with NA
for (col in columns_to_check) {
  car_data[[col]][car_data[[col]] <= 0] <- NA
}
# Check 
summary(car_data)

car_data_imputed <- kNN(car_data, variable = c("mpg", "cyl", "disp", "wt", "price"), k = 5)
# Remove the extra columns added by kNN (ending in '_imp')
car_data_imputed <- car_data_imputed %>%
  select(-ends_with("_imp"))

# Check if missing values are imputed
summary(car_data_imputed)

# List of numerical columns to standardize
numeric_cols <- c("mpg", "cyl", "disp", "wt", "price")

# Standardization (mean = 0, standard deviation = 1)
car_data_standardized <- car_data_imputed
car_data_standardized[numeric_cols] <- scale(car_data_imputed[numeric_cols])

# Check summary to confirm standardization
summary(car_data_standardized)

# Function to cap outliers at 95th percentile
cap_outliers <- function(df, column) {
  upper_bound <- quantile(df[[column]], 0.95, na.rm = TRUE)
  df[[column]][df[[column]] > upper_bound] <- upper_bound
  return(df)
}

# Cap outliers for numeric columns
for (col in numeric_cols) {
  car_data_standardized <- cap_outliers(car_data_standardized, col)
}

# Check dataset after capping outliers
summary(car_data_standardized)

# One-Hot Encoding for categorical variables
car_data_encoded <- car_data_standardized %>%
  mutate(across(c(brand, type, style), as.factor)) %>%
  model.matrix(~ brand + type + style - 1, .) %>%
  as.data.frame()

# Combine one-hot encoded variables with the standardized numeric data
car_data_final <- cbind(car_data_standardized, car_data_encoded)

# Check the final dataset
head(car_data_final)

