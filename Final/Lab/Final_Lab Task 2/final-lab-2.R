# Load required libraries
library(ggplot2)
library(dplyr)
library(moments)

# Load the dataset
data <- read.csv("C:/Users/AZMINUR RAHMAN/OneDrive - American International University-Bangladesh/2024-2025, Fall/INTRODUCTION TO DATA SCIENCE [C]/Final/Lab/Final_Lab Task 1/hajj_umrah_crowd_management_dataset.csv", 
                 header = TRUE, sep = ',')

# Univariate Analysis
# Column of interest for univariate analysis
univariate_col <- "Temperature"  # Replace with desired column

# 1. Histogram
ggplot(data, aes(x = .data[[univariate_col]])) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Temperature", x = "Temperature (°C)", y = "Frequency")

# 2. Line Histogram (Density Plot)
ggplot(data, aes(x = .data[[univariate_col]])) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  labs(title = "Density Plot of Temperature", x = "Temperature (°C)", y = "Density")

# 3. Skewness Graph and Value
skew_value <- skewness(data[[univariate_col]], na.rm = TRUE)
cat("Skewness value for", univariate_col, ":", skew_value, "\n")

# Skewness Graph
ggplot(data, aes(x = .data[[univariate_col]])) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(.data[[univariate_col]], na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = paste("Skewness Graph of", univariate_col), 
       x = "Temperature (°C)", y = "Frequency")

# Calculate mean, median, and mode
mean_val <- mean(data[[univariate_col]], na.rm = TRUE)
median_val <- median(data[[univariate_col]], na.rm = TRUE)
mode_val <- as.numeric(names(sort(table(data[[univariate_col]]), decreasing = TRUE)[1]))

# Skewness Graph with Mean, Median, and Mode
ggplot(data, aes(x = .data[[univariate_col]])) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mean_val, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_val, color = "green", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mode_val, color = "purple", linetype = "dashed", size = 1) +
  annotate("text", x = mean_val, y = 10, label = paste("Mean:", round(mean_val, 2)), color = "red", hjust = -0.1) +
  annotate("text", x = median_val, y = 8, label = paste("Median:", round(median_val, 2)), color = "green", hjust = -0.1) +
  annotate("text", x = mode_val, y = 6, label = paste("Mode:", round(mode_val, 2)), color = "purple", hjust = -0.1) +
  labs(title = paste("Skewness Graph of", univariate_col), 
       x = univariate_col, y = "Frequency")



# 4. Box Plot
ggplot(data, aes(y = .data[[univariate_col]])) +
  geom_boxplot(fill = "green", color = "black") +
  labs(title = "Box Plot of Temperature", y = "Temperature (°C)")

# Multivariate Analysis
# Columns for multivariate analysis
x_col <- "Movement_Speed"       # Numeric column for scatter plot x-axis
y_col <- "Sound_Level_dB"       # Numeric column for scatter plot y-axis
category_col <- "Crowd_Density" # Categorical column for color/violin plot

# 5. Scatter Plot
ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]], color = .data[[category_col]])) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "Scatter Plot of Movement Speed vs Sound Level", 
       x = "Movement Speed (m/s)", y = "Sound Level (dB)", color = "Crowd Density")

# 6. Violin Plot
ggplot(data, aes(x = .data[[category_col]], y = .data[[y_col]])) +
  geom_violin(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Violin Plot of Sound Level by Crowd Density", 
       x = "Crowd Density", y = "Sound Level (dB)")

# 7. Line Graph
time_col <- "Time_Spent_at_Location_minutes"  # Adjust for time-like variable
ggplot(data, aes(x = .data[[time_col]], y = .data[[y_col]], group = .data[[category_col]], color = .data[[category_col]])) +
  geom_line(size = 1) +
  labs(title = "Line Graph of Sound Level over Time by Crowd Density", 
       x = "Time Spent at Location (minutes)", y = "Sound Level (dB)", color = "Crowd Density")
