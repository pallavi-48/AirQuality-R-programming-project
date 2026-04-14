library(tidyverse)
library(lubridate)
library(corrplot)
library(caret)
library(randomForest)
library(forecast)

# Load dataset
data <- read.csv("data/air_quality.csv")

# Clean data
data <- na.omit(data)
data$Date <- as.Date(data$Date)
data$Year <- year(data$Date)

# Plot AQI trend
ggplot(data, aes(Date, AQI)) +
  geom_line()

# Train model
set.seed(123)
trainIndex <- createDataPartition(data$AQI, p=0.8, list=FALSE)
train <- data[trainIndex,]
test <- data[-trainIndex,]

model <- randomForest(AQI ~ PM2.5 + PM10 + NO2 + SO2 + CO, data=train)

pred <- predict(model, test)

# RMSE
rmse <- sqrt(mean((test$AQI - pred)^2))
print(rmse)

# -----------------------------
# SAVE PLOT
# -----------------------------

png("outputs/aqi_trend.png")

ggplot(data, aes(x = Date, y = AQI)) +
  geom_line(color = "blue") +
  ggtitle("AQI Trend Over Time")

dev.off()
