# ---------------------------------------------------
# Packages
# ---------------------------------------------------
library("readr")
library("data.table")

# ---------------------------------------------------
# Data Exploration
# ---------------------------------------------------


test <- read.csv("test.csv") 
summary(test)

cat("\nMissing Values:\n")
print(colSums(is.na(test)))

# ---------------------------------------------------
# Feature Engineering
# ---------------------------------------------------

# Check if 'train$Store_Dept' column exists and drop it if necessary
if ("Store_dept" %in% colnames(test)) {
  test <- test[, !colnames(test) == "Store_dept"]
}
colnames(test)

# Combine store and department information
test$Store_dept <- paste(test$Store, test$Dept, sep = '_')
colnames(test)

# Convert dataframes to data.table format
setDT(test)

# Extract month and year from the Date column in test dataframe
test[, c("Month", "Year") := .(month(as.Date(Date)), year(as.Date(Date)))]
test$Month <- as.integer(test$Month)
test$Year  <- as.integer(test$Year)

head(test)

# Merge stores dataframe to train and test dataframes based on 'Store' column
test <- merge(test, stores, by = "Store", all.x = TRUE)

# Merge features dataframe to train and test dataframes based on 'Store' and 'Date' columns
test <- merge(test, features, by = c("Store", "Date"), all.x = TRUE)

head(test)
