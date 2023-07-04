library("readr")
features <- read.csv("features.csv") 
summary(features)

cat("\nMissing Values:\n")
print(colSums(is.na(features)))
