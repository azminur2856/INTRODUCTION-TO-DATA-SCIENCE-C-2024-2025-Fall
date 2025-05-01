#Descriptive Statistics

library(datasets)
data<-iris
head(data)

summary(data)

# Get the names of numeric columns from the iris dataset
numeric_columns <- names(iris)[sapply(iris, is.numeric)]
print(numeric_columns)
summary(data[numeric_columns])

#change catagorical to numaric
data$Species <- factor(data$Species, c("setosa", "versicolor", "virginica"),labels = c(1,2,3))
print(data)

#stracture of data set
str(data)

#stnder daviation
library(dplyr)
data%>%summarise_if(is.numeric,sd)


#decting null value
# Create a copy of the iris dataset to preserve the original
iris_with_na <- iris

# Randomly assign NA to some values in the dataset
set.seed(123)  # For reproducibility
rows_to_null <- sample(1:nrow(iris_with_na), 10)  # Randomly select 10 rows
cols_to_null <- sample(1:4, 2)  # Randomly select 2 numeric columns (1 to 4 are numeric columns in iris)

# Replace selected rows and columns with NA
for (col in cols_to_null) {
  iris_with_na[rows_to_null, col] <- NA
}

#true means missing
is.na(iris_with_na)

#counting numbers of null values in each column
colSums(is.na(iris_with_na))

#finding thw specific row number of null values
which(is.na(iris_with_na$Sepal.Length))
which(is.na(iris_with_na$Petal.Length))

#remove na instance
remove <- na.omit(iris_with_na)
which(is.na(remove$Sepal.Length))
