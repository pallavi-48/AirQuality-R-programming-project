# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
data <- read.csv("data/aqi_data.csv")

# Basic structure
print("Original Data:")
str(data)

# Remove missing values
data <- na.omit(data)

# Convert Date column (if exists)
if("Date" %in% colnames(data)){
  data$Date <- as.Date(data$Date)
}

# Standardize column names
colnames(data) <- make.names(colnames(data))

# Create AQI Categories
data$AQI_Category <- cut(data$AQI,
                         breaks = c(-Inf, 50, 100, 200, 300, Inf),
                         labels = c("Good", "Moderate", "Unhealthy",
                                    "Very Unhealthy", "Hazardous"))

# Normalize numeric columns
num_cols <- sapply(data, is.numeric)
data[num_cols] <- scale(data[num_cols])

# Save cleaned dataset
write.csv(data, "data/cleaned_data.csv", row.names = FALSE)

print("✅ Data Cleaning Completed")