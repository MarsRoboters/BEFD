library("readr")
train <- read.csv("train.csv") 
summary(train) 


cat("\nMissing Values:\n")
print(colSums(is.na(train)))
