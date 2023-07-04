library("readr")
test <- read.csv("test.csv") 
summary(test)

cat("\nMissing Values:\n")
print(colSums(is.na(test)))
