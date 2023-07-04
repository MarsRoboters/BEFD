library("readr")
stores <- read.csv("stores.csv") 
summary(stores)

cat("\nMissing Values:\n")
print(colSums(is.na(stores)))

