# ---------------------------------------------------
# Packages
# ---------------------------------------------------
library("readr")
library("data.table")
library("dplyr")
library("zoo")
library("corrplot")
library("plotly")
library("forecast")
library("stats")

# ---------------------------------------------------
# Data Exploration
# ---------------------------------------------------

train <- read.csv("train.csv") 
summary(train) 


cat("\nMissing Values:\n")
print(colSums(is.na(train)))

print(distinct_rows <- length(unique(train$Store)))
# 45 Stores
print(distinct_rows <- length(unique(train$Dept)))
# 81 Depts 
print(distinct_rows <- length(unique(train$Weekly_Sales)))
# 359464

# # Explore Unique values in categorical values

cat("\nUnique Departments:")
print(unique(train$Dept))

cat("\nUnique Holidays:")
print(unique(train$IsHoliday))

# Checks
unique(train$Date)

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
# check type of month adn year column

train$Month <-  as.Date(train$Month)
train$Year  <-  as.Date(train$Year)

# Merge stores dataframe to train and test data frames based on 'Store' column
train       <-  merge(train, stores, by = "Store", all.x = TRUE)
#features$Date_c <-  as.character(features$Date)
train$Date  <-  as.Date(train$Date)
features$Date <- as.Date(features$Date)


# Merge features dataframe to train and test dataframes based on 'Store' and 'Date' columns
train       <-  merge(train, features, by = c("Store", "Date"), all.x = TRUE)


#head(train)
#print(unique(train$Temperature))

cat("\nMissing Values:\n")
print(colSums(is.na(train)))

#print(!is.na(train$Temperature))

# Create lag features for weekly_sales in train dataframe
train <- train %>% mutate(Weekly_Sales_Lag1 = lag(Weekly_Sales, n = 1),
                          Weekly_Sales_lag2 = lag(Weekly_Sales, n = 2))

# Convert 'Date' column to datetime type
train$Date <- as.Date(train$Date)

# Convert seasonal features
train$Month       <- format(train$Date, '%m')
train$Quarter     <- format(train$Date, '%q')
train$WeekOfYear  <- format(train$Date, '%V')

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

# Had to change from data.table to data.frame because I had to assign 0 to NA values it was not letting me in data.table 
train <- as.data.frame(train)
date_columns <- sapply(train, function(x) inherits(x, "Date"))
date_columns[date_columns][is.na(train[date_columns])]
#date_columns[which(date_columns)]
#train[date_columns][is.na(train[date_columns])] <- as.Date(0, origin = "1970-01-01")

#min(!is.na(train$Date))
#max(train$Date)
#head(train$Date)
#unique(train$Date)

#train$Date  <-  as.double(train$Date)
#train$Date  <-  as.double(train$Date.1)

# Filling missing values
#train[is.na(train)] <- 0
train[is.na(train)] <- as.Date("1970-01-01")

#length(train[unique(is.na(train))]) 

length(train$Date == "1970-01-01")
# 421570

unique(train$Date)

head(train)

#Reset index
train <- data.frame(Date = train$Date, train)

head(train)

# ---------------------------------------------------
# EDA (Exploratory Data Analysis)
# ---------------------------------------------------

cat('\ntrain.csv columns:')
print(colnames(train))

#
# Countplot of store types
ggplot(stores, aes(x = Type, fill = Type)) +
  geom_bar() +
  labs(title = "Count of Store Types",
       x = "Store Type",
       y = "Count") + 
      theme_minimal()

head(stores$Type)
#head(stores$Size)

# Distribution of store sizes
ggplot(stores, aes(x = Size, color = "blue")) +
  geom_histogram(binwidth = 30000) +
  labs(title = " Distribution of Store Sizes",
       x = "Store Size",
       y = "Count") +
  scale_fill_manual(values = 'green') +
    theme_minimal() 

# Boxplot of store sizes by store type
ggplot(stores, aes(x = Type, y = Size, fill = "green")) +
  geom_boxplot() +
  labs(title = "Store Sizes by Store Type",
       x = "Store Type",
       y = "Store Size") +
  theme_minimal()

# Correlation Matrix

# Create a boolean vector indicating numeric columns - Select only numeric columns
numeric_columns <- sapply(stores, is.numeric)
numeric_data <- stores[,numeric_columns]

# Compute the Correlation Matrix
correlation_matrix <- cor(numeric_data)

# Create the correlation matrix heatmap using corrplot
corrplot(correlation_matrix, method = "color",
        order = "hclust")
# Set the plot title 
title("Correlation Matrix - Stores Dataset")

#corrplot(correlation_matrix, method = "color", type = "upper",
#        order = "hclust", col = colorRampPalette(c("cool", "white", "warm"))(200),
#        tl.cex = 0.8, tl.col = "black", addCoef.col = "black", number.cex = 0.7,
#        diag = FALSE)



# Distribution of Weekly sales
ggplot(train, aes(x = Weekly_Sales)) +
      geom_histogram(color = "orange", fill = "lightblue", bins = 70) +
      geom_density(alpha = 0.5, fill = "blue") +
      labs(title = "Distribution of Weekly Sales",
           x = "Weekly Sales",
           y = "Count")

# Boxplot of weekly sales by store

# Convert Store column to a factor - WHY?
train$Store <- as.factor(train$Store)

ggplot(train, aes(x = Store, y = Weekly_Sales)) + 
      geom_boxplot(fill = "lightblue", color = "black") +
      labs(title = " Weekly Sales by store",
           x = "Stores",
           y = "Weekly Sales")


# Boxplot of weekly sales by department

# Convert Store column to a factor - WHY?
train$Dept <- as.factor(train$Dept)


ggplot(train, aes(x = Dept, y = Weekly_Sales)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Weekly sales by department",
       x = "Department",
       y = "Weekly Sales")

## Line Plot of Weekly Sales over time 
# Convert Date column to date/time format
# sort the data by date first and then plot

#class(train$Date)
#min(train$Date)
#max(train$Date)
#unique(train$Date)

#train$Date <- as.Date(train$Date, format = "%Y-%m-%d")
#train <- train[order(train$Date),]

ggplot(train, aes(x = Date, y = Weekly_Sales)) +
         geom_line() +
          labs(title = "Weekly Sales over Time"
               , x = "Date"
               , y = "Weekly Sales") +
        scale_x_date(date_labels = "%Y-%m-%d") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------ DONE


# Scatter plot of weekly sales vs. Temperature 

#train_temp <- train[complete.cases(train),]
#head(train_temp)

print(length(unique(!is.na(train$Temperature))))


#train_temp <- train

#train_temp[is.na(train_temp$Temperature)] <- 0

#testing_Temperature_x <- ifelse(is.na(train$Temperature), mean(train$Temperature, na.rm = TRUE), train$Temperature)


#train_temp_cleansed <- train_temp[complete.cases(train_temp$Temperature, train_temp$Weekly_Sales), ]

ggplot(train, aes(x = Temperature, y = Weekly_Sales)) + 
        geom_point(color = "blue", fill = "white") +
        labs(title = "Weekly sales vs. Temperature"
             , x = "Temperature"
             , y = "Weekly Sales")

#train_temp_cleansed$Temperature

#train_temp <- train_temp %>% mutate(Temperature = na_if(Temperature, 0))

colnames(train)


# Correlation Matrix

#Compute the Correlation Matrix
correlation_matrix <- cor(train)


# Create the correlation plot
corrplot(correlation_matrix, method = "color", type = "full", tl.cex = 0.8)

# Set the title
title("Correlation Matrix - train Dataset")

# Adjust the plot margins if needed
par(mar = c(4,4,2,1))

# Create a heatmap plot using plotly
plot <- plot_ly(
  x = colnames(correlation_matrix),
  y = colnames(correlation_matrix),
  z = correlation_matrix,
  type = "heatmap",
  colorscale = "viridis"
)

# Set plot title and axis labels 
plot <- plot %>% 
        layout(
          title = "Correlation Matrix - Train Dataset",
          xaxis = list(title = "Features"),
          yaxis = list(title = "Features")
                )
#Display the plot
print(plot)



# 
# Features
# 



library(plotly)

# Define numerical columns
numerical_cols <- c("Temperature", "Fuel_Price", "MarkDown1", "MarkDown2", "MarkDown3", "MarkDown4", "MarkDown5", "CPI", "Unemployment")

# Create a list to store the individual histogram plots
plots <- lapply(numerical_cols, function(col) {
  plot_ly(features, x = ~get(col), type = "histogram", nbinsx = 30, histnorm = "count") %>%
    layout(showlegend = FALSE, title = col)
})

# Create a subplot with the list of plots
fig <- subplot(plots, nrows = length(numerical_cols), shareX = TRUE, shareY = FALSE)

# Update subplot layout
fig <- fig %>%
  layout(height = 600, width = 900, title = "Distribution of Numerical Features")

# Display the plot
print(fig)


#--------------------------------------------------------------------------

# Define numerical columns
numerical_cols <- c("Temperature", "Fuel_Price", "MarkDown1", "MarkDown2", "MarkDown3", "MarkDown4", "MarkDown5", "CPI", "Unemployment")

# Iterate over numerical columns
for (col in numerical_cols) {
  # Create individual histogram plot for each column
  plot <- plot_ly(features, x = ~get(col), type = "histogram", nbinsx = 30, histnorm = "count") %>%
    layout(showlegend = FALSE, title = col)
  
  # Display the plot
  print(plot)
}

#--------------------------------------------------------------------------


# Plotting the relationship between features
pairs(features[,numerical_cols])


# ---------------------------------------------------
# Sales Forcasting
# ---------------------------------------------------


store <- 1
department <- 1

# Filter the sales data for the specified store and department
sales_data <- train[train$Store == store & train$Dept == department, "Weekly_Sales"]

head(sales_data)
colnames(sales_data)
length(sales_data)

model <- arima(sales_data, order = c(1,1,1))

# Fit Arima model to the Sales Data
model_fit <- forecast::Arima(sales_data, order = c(1,1,1))

# Forcast future sales
forecast_steps <- 10 # Specify the number of steps in forecast
forecast <- forecast::forecast(model, h = forecast_steps)

# Print the forecast sales
cat("Forecasted sales for store", store, "and Department", department, ":\n")
print(forecast)

# Plot the Forcasted Sales

# Create the sales_df data frame
sales_date <- index(sales_data)
sales_values <- coredata(sales_data)
sales_df <- data.frame(Date = sales_date, Sales = sales_values)

# Create a Dataframe for Forcasted sales
forecast_data <- index(forecast)
forecast_values <- as.numeric(forecast$mean)
forecast_df <- data.frame(Date = forecast_data, Sales = forecast_values)

# Plot the forecasted sales
ggplot() +
  geom_line(data = sales_df, aes(x = Date, y = Sales), color = "blue",
            linetype = "solid", size = 1) +
  geom_line(data = forecast_df, aes(x = Date, y = Sales), color = "red",
            linetype = "solid", size = 1) +
  labs(title = paste("Sales Forecast for Store", store, "and Department", department),
       x = "Date", y = "Sales") +
  theme_minimal() +
  theme(legend.position = "none")


# ------- Another way-----

num_stores <- 40
num_department <- 40

selected_stores <- 1:num_stores
selected_departments <- 1:num_department 

# Filter the sales data for the specified store and department
sales_data <- train[train$Store %in% selected_stores & train$Dept %in% selected_departments, "Weekly_Sales"]

model <- arima(sales_data, order = c(1,1,1))
model_fit <- forecast::Arima(sales_data, order = c(1,1,1))
# --------------------------

# 