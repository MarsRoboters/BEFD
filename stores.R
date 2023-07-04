# ---------------------------------------------------
# Packages
# ---------------------------------------------------

library("readr")

# ---------------------------------------------------
# Data Exploration
# ---------------------------------------------------


stores <- read.csv("stores.csv") 
summary(stores)

cat("\nMissing Values:\n")
print(colSums(is.na(stores)))


# Explore Unique values in categorical values

cat("\nUnique Store Types:")
print(unique(stores$Type))

# ---------------------------------------------------
# Feature Engineering 
# ---------------------------------------------------

# Encode categorical variable 
# Example One-Hot encoding for store types
unique(stores$Type)

stores$Type

# Perform one-hot encoding for store types
store_type_dummies <- model.matrix(~ Type - 1, data = stores)

store_type_dummies

store_type_dummies <- as.data.frame(store_type_dummies)
#colnames(store_type_dummies) <- paste("Store_Type", colnames(store_type_dummies), sep = "_")

store_type_dummies

# Concatenate the encoded columns to the stores dataframe
stores <- cbind(stores, store_type_dummies)

head(stores)