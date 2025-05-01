# Load necessary libraries
library(tidyverse)
if (!requireNamespace("e1071", quietly = TRUE)) install.packages("e1071")
library(e1071)

# Load the dataset
data <- read.csv("C:/Users/AZMINUR RAHMAN/OneDrive - American International University-Bangladesh/2024-2025, Fall/INTRODUCTION TO DATA SCIENCE [C]/Final/Lab/Final_Lab Task 1/hajj_umrah_crowd_management_dataset.csv", 
                 header = TRUE, sep = ',')
# Univariate Analysis ---------------------------------------------------
# 1. Calculate Mean, Median, and Mode of Satisfaction Rating
mean_satisfaction <- mean(data$Satisfaction_Rating, na.rm = TRUE)
median_satisfaction <- median(data$Satisfaction_Rating, na.rm = TRUE)
mode_satisfaction <- as.numeric(names(sort(table(data$Satisfaction_Rating), decreasing = TRUE))[1]) # Mode

# 2. Histogram with Density and Skewness
hist(data$Satisfaction_Rating, prob = TRUE, main = paste("Histogram with Density (Skewness =", round(skewness_value, 2), ")"),
     xlab = "Satisfaction Rating", col = "skyblue", border = "black")
lines(density(data$Satisfaction_Rating, na.rm = TRUE), col = "red", lwd = 2)

# 3. Add Lines for Mean, Median, and Mode
abline(v = mean_satisfaction, col = "green", lwd = 2, lty = 2)
abline(v = median_satisfaction, col = "red", lwd = 2, lty = 2)
abline(v = mode_satisfaction, col = "blue", lwd = 2, lty = 2)  
legend("topright", legend = c("Mean", "Median", "Mode"), 
       col = c("green","red","blue"), lty = 2, lwd = 2)

# 4. Skewness Calculation and Print
skewness_value <- skewness(data$Satisfaction_Rating, na.rm = TRUE)
print(paste("Skewness of Satisfaction Rating:", round(skewness_value, 2)))

# 1. Histogram of Satisfaction Rating
ggplot(data, aes(x = Temperature)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Temperature", x = "Temperature", y = "Frequency") +
  theme_minimal()


# 2. Line Histogram to understand distribution
ggplot(data, aes(x = Temperature)) + geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) + 
  geom_freqpoly(binwidth = 1, color = "blue", size = 1.2) +
  labs(title = "Line Histogram of Temperature", x = "Temperature", y = "Frequency") +
  theme_minimal()

# 3. Skewness graph and value
skewness_value <- skewness(data$Temperature, na.rm = TRUE)
ggplot(data, aes(x = Temperature)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1.2) +
  labs(title = paste("Temperature Distribution (Skewness =", round(skewness_value, 2), ")"),
       x = "Temperature", y = "Density") +
  theme_minimal()

# Print Skewness Value
print(paste("Skewness of Temperature:", round(Temperature, 2)))

# 4. Box plot for Time Spent
ggplot(data, aes(y = Time_Spent_at_Location_minutes)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Time Spent at Location", y = "Time Spent (minutes)") +
  theme_minimal()

# Multivariate Analysis --------------------------------------------------

# 5. Scatter plot of Distance Between People vs Satisfaction Rating
ggplot(data, aes(x = Distance_Between_People_m, y = Satisfaction_Rating, color = Crowd_Density)) +
  geom_point(alpha = 0.7) +
  labs(title = "Scatter Plot: Distance Between People vs Satisfaction Rating",
       x = "Distance Between People (meters)", y = "Satisfaction Rating") +
  theme_minimal()

# 6. Violin plot of Satisfaction Rating by Crowd Density
ggplot(data, aes(x = Crowd_Density, y = Satisfaction_Rating, fill = Crowd_Density)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(title = "Violin Plot: Satisfaction Rating by Crowd Density", x = "Crowd Density", y = "Satisfaction Rating") +
  theme_minimal()

# 7. Line graph of Satisfaction Rating over Time
ggplot(data, aes(x = as.POSIXct(Timestamp), y = Satisfaction_Rating, group = 1)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Line Graph of Satisfaction Rating over Time", x = "Time", y = "Satisfaction Rating") +
  theme_minimal()
