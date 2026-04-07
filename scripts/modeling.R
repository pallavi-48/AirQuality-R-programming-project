# Load libraries
library(caret)
library(rpart)
library(class)

# Load cleaned data
data <- read.csv("data/cleaned_data.csv")

# Split dataset
set.seed(123)
trainIndex <- createDataPartition(data$AQI, p = 0.7, list = FALSE)

train <- data[trainIndex, ]
test  <- data[-trainIndex, ]

# -----------------------------
# 1. Linear Regression
# -----------------------------
model_lm <- lm(AQI ~ PM2.5 + PM10 + NO2 + SO2 + CO + O3, data = train)
summary(model_lm)

# Predictions
pred_lm <- predict(model_lm, test)

# RMSE
rmse <- sqrt(mean((pred_lm - test$AQI)^2))
print(paste("RMSE:", rmse))

# -----------------------------
# 2. Decision Tree
# -----------------------------
tree_model <- rpart(AQI ~ ., data = train, method = "anova")

plot(tree_model)
text(tree_model)

# -----------------------------
# 3. KNN (Classification)
# -----------------------------
# Convert AQI to category if not exists
if(!"AQI_Category" %in% colnames(data)){
  data$AQI_Category <- cut(data$AQI,
                           breaks = c(-Inf, 50, 100, 200, 300, Inf),
                           labels = c("Good", "Moderate", "Unhealthy",
                                      "Very Unhealthy", "Hazardous"))
}

# Prepare data
num_cols <- sapply(data, is.numeric)

train_x <- train[, num_cols]
test_x  <- test[, num_cols]

train_y <- train$AQI_Category
test_y  <- test$AQI_Category

knn_pred <- knn(train = train_x, test = test_x, cl = train_y, k = 5)

# Confusion Matrix
print(confusionMatrix(knn_pred, test_y))

# -----------------------------
# 4. T-Test
# -----------------------------
if(length(unique(data$AQI_Category)) >= 2){
  print(t.test(AQI ~ AQI_Category, data = data))
}

# -----------------------------
# 5. ANOVA
# -----------------------------
anova_model <- aov(AQI ~ AQI_Category, data = data)
summary(anova_model)

print("✅ Modeling Completed")