library("readr")
stores <- read.csv("stores.csv") 
summary(stores)

cat("\nMissing Values:\n")
print(colSums(is.na(stores)))


# Explore Unique values in categorical values

cat("\nUnique Store Types:")
print(unique(stores$Type))
