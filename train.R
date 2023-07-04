library("readr")
train <- read.csv("train.csv") 
summary(train) 


cat("\nMissing Values:\n")
print(colSums(is.na(train)))

# Explore Unique values in categorical values

cat("\nUnique Departments:")
print(unique(train$Dept))

cat("\nUnique Holidays:")
print(unique(train$IsHoliday))

