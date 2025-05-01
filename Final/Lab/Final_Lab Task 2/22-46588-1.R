library(tidyverse)
library(e1071)


data <- read.csv("C:/Users/AZMINUR RAHMAN/OneDrive - American International University-Bangladesh/2024-2025, Fall/INTRODUCTION TO DATA SCIENCE [C]/Final/Lab/Final_Lab Task 1/hajj_umrah_crowd_management_dataset.csv", 
                 header = TRUE, sep = ',')


ggplot(data, aes(x = Temperature)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Temperature", x = "Temperature(°C)", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = Temperature)) + geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) + 
  geom_freqpoly(binwidth = 1, color = "blue", size = 1.2) +
  labs(title = "Line Histogram of Temperature", x = "Temperature(°C)", y = "Frequency") +
  theme_minimal()

skewness_value_Temperature <- skewness(data$Temperature, na.rm = TRUE)

ggplot(data, aes(x = Temperature)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1.2) +
  labs(title = paste("Temperature Distribution (Skewness =", round(skewness_value_Temperature, 2), ")"),
       x = "Temperature(°C)", y = "Density") +
  theme_minimal()

mean_Temperature <- mean(data$Temperature, na.rm = TRUE)
median_Temperature <- median(data$Temperature, na.rm = TRUE)
mode_Temperature <- as.numeric(names(sort(table(data$Temperature), decreasing = TRUE))[1])

hist(data$Temperature, prob = TRUE, main = paste("Histogram with Density (Skewness =", round(skewness_value_Temperature, 2), ")"),
     xlab = "Temperature(°C)", col = "skyblue", border = "black")
lines(density(data$Temperature, na.rm = TRUE), col = "red", lwd = 2)

abline(v = mean_Temperature, col = "green", lwd = 2, lty = 2)
abline(v = median_Temperature, col = "red", lwd = 2, lty = 2)
abline(v = mode_Temperature, col = "blue", lwd = 2, lty = 2)  
legend("topright", legend = c("Mean", "Median", "Mode"), 
       col = c("green","red","blue"), lty = 2, lwd = 2)

print(paste("Skewness of Temperature:", round(skewness_value_Temperature, 2)))

mean_satisfaction <- mean(data$Satisfaction_Rating, na.rm = TRUE)
median_satisfaction <- median(data$Satisfaction_Rating, na.rm = TRUE)
mode_satisfaction <- as.numeric(names(sort(table(data$Satisfaction_Rating), decreasing = TRUE))[1])
skewness_value_satisfaction <- skewness(data$Satisfaction_Rating, na.rm = TRUE)

hist(data$Satisfaction_Rating, prob = TRUE, main = paste("Histogram with Density (Skewness =", round(skewness_value_satisfaction, 2), ")"),
     xlab = "Satisfaction Rating", col = "skyblue", border = "black")
lines(density(data$Satisfaction_Rating, na.rm = TRUE), col = "red", lwd = 2)

abline(v = mean_satisfaction, col = "green", lwd = 2, lty = 2)
abline(v = median_satisfaction, col = "red", lwd = 2, lty = 2)
abline(v = mode_satisfaction, col = "blue", lwd = 2, lty = 2)  
legend("topright", legend = c("Mean", "Median", "Mode"), 
       col = c("green","red","blue"), lty = 2, lwd = 2)

print(paste("Skewness of Satisfaction Rating:", round(skewness_value_satisfaction, 2)))

ggplot(data, aes(y = Time_Spent_at_Location_minutes)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Time Spent at Location", y = "Time Spent (minutes)") +
  theme_minimal()


ggplot(data, aes(x = Distance_Between_People_m, y = Satisfaction_Rating, color = Crowd_Density)) +
  geom_point(alpha = 0.7) +
  labs(title = "Scatter Plot: Distance Between People vs Satisfaction Rating",
       x = "Distance Between People (meters)", y = "Satisfaction Rating") +
  theme_minimal()

ggplot(data, aes(x = Crowd_Density, y = Satisfaction_Rating, fill = Crowd_Density)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(title = "Violin Plot: Satisfaction Rating by Crowd Density", x = "Crowd Density", y = "Satisfaction Rating") +
  theme_minimal()

ggplot(data, aes(x = as.POSIXct(Timestamp), y = Satisfaction_Rating, group = 1)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Line Graph of Satisfaction Rating over Time", x = "Time", y = "Satisfaction Rating") +
  theme_minimal()
