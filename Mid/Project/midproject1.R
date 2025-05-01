library(readxl)
library(dplyr)

mydata <- read_excel("C:/Users/AZMINUR RAHMAN/OneDrive - American International University-Bangladesh/2024-2025, Fall/INTRODUCTION TO DATA SCIENCE [C]/Mid/Lab/Project/Midterm_Dataset_Section(C).xlsx", sheet = "Sheet1")

View(mydata)
str(mydata)
summary(mydata)

num_instances <- nrow(mydata)
num_attributes <- ncol(mydata)

print(paste("Number of instances (rows):", num_instances))
print(paste("Number of columns:", num_attributes))

missing_values_indices <- lapply(mydata, function(x) {
  if (is.numeric(x) || is.character(x)) {
    return(which(is.na(x) | x == ""))
  } else {
    return(NULL)
  }
})
print(missing_values_indices)

na_counts <- colSums(is.na(mydata))
print(na_counts)

barplot(na_counts, names.arg = names(na_counts),
        ylab = "Number of Missing Values", col = "red", cex.names = 0.9,
        main = "Missing Values per Attribute", las = 2)




age_median <- round(median(mydata$person_age, na.rm = TRUE))
mydata$person_age[is.na(mydata$person_age)] <- age_median
print(mydata$person_age)
print(age_median)
boxplot(mydata$person_age, main = "Age")

Q1 <- quantile(mydata$person_age, 0.25, na.rm = TRUE)
Q3 <- quantile(mydata$person_age, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

threshold <- 1.5
outlier_condition <- (mydata$person_age < (Q1 - threshold * IQR_value)) | 
  (mydata$person_age > (Q3 + threshold * IQR_value))

mydata <- mydata %>%
  filter(!outlier_condition) %>%
  arrange(row_number())

View(mydata)

boxplot(mydata$person_age, main = "Age After Outlier Removal")



unique_values_gender <- unique(mydata$person_gender)
print(unique_values_gender)

mydata$person_gender <- tolower(mydata$person_gender)
mydata$person_gender <- factor(mydata$person_gender,
                               levels = c("male", "female"),
                               labels = c(1, 2))
View(mydata)

mode_gender <- as.numeric(names(sort(table(mydata$person_gender), decreasing = TRUE)[1]))
mydata$person_gender[is.na(mydata$person_gender)] <- mode_gender

View(mydata)



unique_education <- unique(mydata$person_education)
print(unique_education)

mydata$person_education <- tolower(mydata$person_education)
mydata$person_education <- factor(mydata$person_education,
                                  levels = c("master", "high school", "bachelor", "associate", "doctorate"),
                                  labels = c(1, 2, 3, 4, 5))

View(mydata)

mode_education <- names(which.max(table(mydata$person_education)))
mydata$person_education[is.na(mydata$person_education)] <- mode_education

View(mydata)



income_median <- median(mydata$person_income, na.rm = TRUE)
mydata$person_income[is.na(mydata$person_income)] <- income_median

View(mydata)



missing_values_emp_exp <- sum(is.na(mydata$person_emp_exp)) 
print(missing_values_emp_exp)

boxplot(mydata$person_emp_exp, main = "Person Emp_exp")

Q1 <- quantile(mydata$person_emp_exp, 0.25, na.rm = TRUE)
Q3 <- quantile(mydata$person_emp_exp, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
threshold <- 1.5
outlier_condition <- (mydata$person_emp_exp < (Q1 - threshold * IQR_value)) | 
  (mydata$person_emp_exp > (Q3 + threshold * IQR_value))

mydata <- mydata %>%
  filter(!outlier_condition) %>%
  arrange(row_number())

View(mydata)

boxplot(mydata$person_emp_exp, main = "Person Emp_exp After Outlier Removal")



unique_home_ownership <- unique(mydata$person_home_ownership)
print(unique_home_ownership)

mydata$person_home_ownership <- tolower(mydata$person_home_ownership)
mydata$person_home_ownership <- ifelse(mydata$person_home_ownership == "rentt", "rent", mydata$person_home_ownership)
mydata$person_home_ownership <- ifelse(mydata$person_home_ownership == "oown", "own", mydata$person_home_ownership)

mydata$person_home_ownership <- factor(mydata$person_home_ownership,
                                       levels = c("rent", "own", "mortgage", "other"),
                                       labels = c(1, 2, 3, 4))

View(mydata)



missing_values_loan_amnt <- sum(is.na(mydata$loan_amnt)) 
print(missing_values_loan_amnt)



unique_loan_intent <- unique(mydata$loan_intent)
print(unique_loan_intent)

mydata$loan_intent <- tolower(mydata$loan_intent)
mydata$loan_intent <- factor(mydata$loan_intent,
                             levels = c("personal", "education", "medical", "venture", "homeimprovement", "debtconsolidation"),
                             labels = c(1, 2, 3, 4, 5, 6))

View(mydata)



missing_values_loan_int_rate <- sum(is.na(mydata$loan_int_rate)) 
print(missing_values_loan_int_rate)



loan_percent_income_median <- median(mydata$loan_percent_income, na.rm = TRUE)
mydata$loan_percent_income[is.na(mydata$loan_percent_income)] <- loan_percent_income_median

View(mydata)



missing_values_cb_person_cred_hist_length <- sum(is.na(mydata$cb_person_cred_hist_length)) 
print(missing_values_cb_person_cred_hist_length)



missing_values_credit_score <- sum(is.na(mydata$credit_score)) 
print(missing_values_credit_score)



unique_previous_loan_defaults_on_file <- unique(mydata$previous_loan_defaults_on_file)
print(unique_previous_loan_defaults_on_file)

mydata$previous_loan_defaults_on_file <- tolower(mydata$previous_loan_defaults_on_file)
mydata$previous_loan_defaults_on_file <- factor(mydata$previous_loan_defaults_on_file,
                                                levels = c("yes", "no"),
                                                labels = c(1, 2))

View(mydata)



missing_values_loan_status <- sum(is.na(mydata$loan_status)) 
print(missing_values_loan_status)

mode_loan_status <- as.numeric(names(sort(table(mydata$loan_status), decreasing = TRUE)[1]))
mydata$loan_status[is.na(mydata$loan_status)] <- mode_loan_status

View(mydata)



duplicate_rows <- mydata[duplicated(mydata), ]
print(paste("Number of duplicate rows:", nrow(duplicate_rows)))

mydata <- mydata[!duplicated(mydata), ]

View(mydata)



numeric_columns <- sapply(mydata, is.numeric)
mydata[numeric_columns] <- lapply(mydata[numeric_columns], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})

View(mydata)


summary(mydata)

descriptive_stats <- function(column) {
  mean_value <- mean(column, na.rm = TRUE)
  median_value <- median(column, na.rm = TRUE)
  mode_value <- as.numeric(names(sort(table(column), decreasing = TRUE)[1]))
  return(c(Mean = mean_value, Median = median_value, Mode = mode_value))
}

descriptive_summary <- lapply(mydata[numeric_columns], descriptive_stats)
descriptive_summary <- do.call(rbind, descriptive_summary)

print("Descriptive Statistics for Numeric Columns:")
print(descriptive_summary)

barplot(t(descriptive_summary), beside = TRUE, col = c("white", "green", "blue"),
        legend.text = rownames(descriptive_summary), args.legend = list(x = "topright", bty = "n"),
        main = "Mean, Median, and Mode for Numeric Columns", ylab = "Values", xlab = "Columns")


write.csv(mydata, "C:/Users/AZMINUR RAHMAN/OneDrive - American International University-Bangladesh/2024-2025, Fall/INTRODUCTION TO DATA SCIENCE [C]/Mid/Lab/Project/Midterm_Dataset_Section(C).csv", row.names = FALSE)
View(mydata)