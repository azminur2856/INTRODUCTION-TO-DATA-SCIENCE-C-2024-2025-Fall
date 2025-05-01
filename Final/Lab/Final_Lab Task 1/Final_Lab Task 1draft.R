# Load necessary libraries
library(dplyr)
library(infotheo)
library(ggplot2)

# Load the dataset
data <- read.csv("C:/Users/AZMINUR RAHMAN/OneDrive - American International University-Bangladesh/2024-2025, Fall/INTRODUCTION TO DATA SCIENCE [C]/Final/Lab/Final_Lab Task 1/hajj_umrah_crowd_management_dataset.csv", 
                 header = TRUE, sep = ',')

# Check for missing values
colSums(is.na(data))

# 1. Numerical Input and Numerical Output (Pearson's Correlation)
correlation_pearson <- cor(data$Time_Spent_at_Location_minutes, data$Sound_Level_dB, method = "pearson", use = "complete.obs")
cat("Pearson's Correlation (Time_Spent_at_Location_minutes & Sound_Level_dB):", correlation_pearson, "\n")
if (correlation_pearson > 0) {
  cat("Type of Correlation: Positive Linear Relationship\n\n")
} else if (correlation_pearson < 0) {
  cat("Type of Correlation: Negative Linear Relationship\n\n")
} else {
  cat("Type of Correlation: Zero Linear Relationship\n\n")
}

# Visualization for Pearson's Correlation
ggplot(data, aes(x = Time_Spent_at_Location_minutes, y = Sound_Level_dB)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  ggtitle("Scatter Plot: Time Spent at Location vs Sound Level") +
  xlab("Time Spent at Location (minutes)") +
  ylab("Sound Level (dB)")

# 2. Numerical Input and Numerical Output (Spearman's Correlation)
correlation_spearman <- cor(data$Time_Spent_at_Location_minutes, data$Sound_Level_dB, method = "spearman", use = "complete.obs")
cat("Spearman's Correlation (Time_Spent_at_Location_minutes & Sound_Level_dB):", correlation_spearman, "\n")
if (correlation_spearman > 0) {
  cat("Type of Correlation: Positive Rank-based Relationship\n\n")
} else if (correlation_spearman < 0) {
  cat("Type of Correlation: Negative Rank-based Relationship\n\n")
} else {
  cat("Type of Correlation: Zero Rank-based Relationship\n\n")
}

# Visualization for Spearman's Correlation
ggplot(data, aes(x = Time_Spent_at_Location_minutes, y = Sound_Level_dB)) +
  geom_point() +
  geom_smooth(method = "loess", color = "green") +
  ggtitle("Scatter Plot: Time Spent at Location vs Sound Level (Spearman)") +
  xlab("Time Spent at Location (minutes)") +
  ylab("Sound Level (dB)")

# 3. Numerical Input and Categorical Output (ANOVA)
anova_result <- aov(Movement_Speed ~ Activity_Type, data = data)
anova_summary <- summary(anova_result)
cat("ANOVA Results:\n")
print(anova_summary)
cat("Type of Analysis: Variation of Numerical Data across Categories\n\n")

# Visualization for ANOVA
ggplot(data, aes(x = Activity_Type, y = Movement_Speed)) +
  geom_boxplot() +
  ggtitle("Boxplot: Movement Speed by Activity Type") +
  xlab("Activity Type") +
  ylab("Movement Speed")

# 4. Categorical Input and Numerical Output (Kendall's Tau)
data$Pilgrim_Experience_Numeric <- as.numeric(factor(data$Pilgrim_Experience))
kendall_result <- cor(data$Pilgrim_Experience_Numeric, data$Satisfaction_Rating, method = "kendall", use = "pairwise.complete.obs")
cat("Kendall's Tau Correlation (Pilgrim_Experience vs Satisfaction_Rating):", kendall_result, "\n")
if (kendall_result > 0) {
  cat("Type of Correlation: Positive Ordinal Relationship\n\n")
} else if (kendall_result < 0) {
  cat("Type of Correlation: Negative Ordinal Relationship\n\n")
} else {
  cat("Type of Correlation: Zero Ordinal Relationship\n\n")
}

# Visualization for Kendall's Tau Correlation
ggplot(data, aes(x = Pilgrim_Experience, y = Satisfaction_Rating)) +
  geom_boxplot() +
  ggtitle("Boxplot: Satisfaction Rating by Pilgrim Experience") +
  xlab("Pilgrim Experience") +
  ylab("Satisfaction Rating")

# 5. Categorical Input and Categorical Output (Chi-Squared Test)
chi_square_result <- chisq.test(table(data$Activity_Type, data$Pilgrim_Experience))
cat("Chi-Squared Test Results:\n")
print(chi_square_result)
cat("Type of Test: Association between Categorical Variables\n\n")

# Visualization for Chi-Squared Test
ggplot(data, aes(x = Activity_Type, fill = Pilgrim_Experience)) +
  geom_bar(position = "dodge") +
  ggtitle("Bar Plot: Pilgrim Experience by Activity Type") +
  xlab("Activity Type") +
  ylab("Count")

# 6. Categorical Input and Categorical Output (Mutual Information)
mutual_info <- mutinformation(as.factor(data$Activity_Type), as.factor(data$Pilgrim_Experience))
cat("Mutual Information (Activity_Type and Pilgrim_Experience):", mutual_info, "\n")
if (mutual_info > 0) {
  cat("Type of Analysis: Positive Information Gain between Two Categorical Variables\n")
} else {
  cat("Type of Analysis: Zero Information Gain between Two Categorical Variables\n")
}

# Visualization for Mutual Information
ggplot(data, aes(x = Activity_Type, fill = Pilgrim_Experience)) +
  geom_bar(position = "fill") +
  ggtitle("Proportional Bar Plot: Pilgrim Experience by Activity Type") +
  xlab("Activity Type") +
  ylab("Proportion")
