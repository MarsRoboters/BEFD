# ---------------------------------------------------
# Packages
# ---------------------------------------------------
library("readr")
library("data.table")
library("dplyr")
library("zoo")

# ---------------------------------------------------
# Data Exploration
# ---------------------------------------------------

train <- read.csv("train.csv") 
summary(train) 


cat("\nMissing Values:\n")
print(colSums(is.na(train)))

# # Explore Unique values in categorical values

cat("\nUnique Departments:")
print(unique(train$Dept))

cat("\nUnique Holidays:")
print(unique(train$IsHoliday))

# ---------------------------------------------------
# Feature Engineering
# ---------------------------------------------------

# Check if 'train$Store_Dept' column exists and drop it if necessary
if ("Store_dept" %in% colnames(train)) {
  train <- train[, !colnames(train) == "Store_dept"]
}


# Combine store and department information

train$Store_dept <- paste(train$Store, train$Dept, sep = '_')
#train$Store_dept

# Convert dataframes to data.table format
setDT(train)

# Extract month and year from the Date column in train Dataframe
train[, c("Month", "Year") := .(month(as.Date(Date)), year(as.Date(Date)))]

train$Month <- as.integer(train$Month)
train$Year  <- as.integer(train$Year)


# Merge stores dataframe to train and test dataframes based on 'Store' column
train <- merge(train, stores, by = "Store", all.x = TRUE)

# Merge features dataframe to train and test dataframes based on 'Store' and 'Date' columns
train <- merge(train, features, by = c("Store", "Date"), all.x = TRUE)

head(train)

# Create lag features for weekly_sales in train dataframe
train <- train %>% mutate(Weekly_Sales_Lag1 = lag(Weekly_Sales, n = 1),
                          Weekly_Sales_lag2 = lag(Weekly_Sales, n = 2))


# Create rolling mean and standard deviation for weekly sales in train dataframe 
train$Rolling_Mean <- rollmean(train$Weekly_Sales, k= 4, fill = NA)
train$Rolling_Std <- rollapply(train$Weekly_Sales, width = 4, FUN = sd, fill = NA)

# Convert 'Date' column to datetime type
train$Date <- as.Date(train$Date)

# Convert seasonal features
train$Month <- format(train$Date, '%m')
train$Quarter <- as.integer(train$Date, '%q')
train$WeekOfYear <- format(train$Date, '%V')

# ---------------------------------------------------
# Data Transformation
# ---------------------------------------------------

# Convert Date column into DateTime format
train$Date <- as.Date(train$Date)

# Sort of Datasets by 'Date'
train <- train %>% arrange(Date)

# Set 'Date' as the index 
train <- train[order(train$Date), ]
rownames(train) <- NULL

# Filling missing values
train[is.na(train)] <- 0

#Reset index
train <- data.frame(Date = train$Date, train)

head(train)
