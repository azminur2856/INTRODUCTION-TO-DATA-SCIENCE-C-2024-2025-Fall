#install.packages(dplyr)
library(dplyr) #load library
stats <- data.frame(player = c('A','B','C','D','A','A'),
                    run =c(100,200,408,19,56,100),
                    wickets = c(17,20,NA,5,2,17))

# Fetch players who scored more than 100 runs
filter(stats, run > 100)

#use of distinct(dataFrame) remove duplicate row only
distinct(stats)

#remove duplicates based on a column
distinct(stats, player, .keep_all = TRUE)

#use of arrange()
arrange(stats, wickets)
arrange(stats, -wickets)

#use of select()
select(stats, player,wickets)

#use of rename(dataFrame, newName = oldName, ..........)
rename(stats, run_scored = run, wicket = wickets)

#use of mutate(dataFrame, newVariable = formula)
mutate(stats, avg = run / 6)
#use of transmute(dataFrame, newVariable = formula)
transmute(stats, avg = run/6)

#use of summari





#NORMALIZED
data(iris)

min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

iris_normalized <- iris
iris_normalized[1:4] <- lapply(iris[1:4], min_max_normalize)

head(iris_normalized)

