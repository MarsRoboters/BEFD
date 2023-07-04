# ---------------------------------------------------
# Packages
# ---------------------------------------------------
library("readr")
library("data.table")

# ---------------------------------------------------
# Data Exploration
# ---------------------------------------------------

features <- read.csv("features.csv") 
summary(features)

cat("\nMissing Values:\n")
print(colSums(is.na(features)))

# ---------------------------------------------------
# Feature Engineering
# ---------------------------------------------------


# Convert dataframes to data.table format
setDT(features)

# Extract month and year from the Date column in features dataframe
features[, c("Month", "Year") := .(month(as.Date(Date)), year(as.Date(Date)))]
features$Month <- as.integer(features$Month)
features$Year <- as.integer(features$Year)

head(features)

# Calculate the total markdown amount
features$Total_MarkDown <- rowSums(features[, c('MarkDown1', 'MarkDown2'
                                                  , 'MarkDown3', 'MarkDown4', 'MarkDown5')], na.rm = TRUE)
head(features)



# ---------------------------------------------------
# Data Transformation
# ---------------------------------------------------

# Convert Date column into DateTime format
features$Date <- as.Date(features$Date)

# Sort the datasets by 'Date'
features <- features %>% arrange(Date)


# Set 'Date' as the index 
features <- features[order(features$Date), ]
rownames(features) <- NULL

# Filling missing values
features[is.na(features)] <- 0

#Reset index
features <- data.frame(Date = features$Date, features)

