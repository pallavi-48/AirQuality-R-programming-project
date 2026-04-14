# ============================================================
# modeling.R — Predictive Modeling
# BreatheEasy: Air Quality Analysis Project
# ============================================================

library(caret)
library(rpart)
library(rpart.plot)
library(class)

# Load normalized data for modeling
data <- read.csv("data/cleaned_data.csv", stringsAsFactors = FALSE)

# FIX: cats must match exactly what 01_preprocessing.R creates
# "Unhealthy for Sensitive Groups" was missing in the old version
cats <- c("Good", "Moderate", "Unhealthy for Sensitive Groups",
          "Unhealthy", "Very Unhealthy", "Hazardous")

data$AQI_Category <- factor(data$AQI_Category, levels = cats)

# Check exact column names — critical for lm() formula below
cat("Column names in dataset:\n")
print(colnames(data))

# -------------------------------------------------------
# Train / Test Split (70% train, 30% test)
# -------------------------------------------------------
set.seed(123)
trainIndex <- createDataPartition(data$AQI, p = 0.7, list = FALSE)
train <- data[trainIndex, ]
test  <- data[-trainIndex, ]

# -------------------------------------------------------
# 1. Linear Regression
# FIX: column is named "PM25" after renaming in preprocessing
# (not "PM2.5" — that would cause an error)
# -------------------------------------------------------
model_lm <- lm(AQI ~ PM25 + PM10 + NO2 + SO2 + CO + O3, data = train)
summary(model_lm)

pred_lm <- predict(model_lm, newdata = test)
rmse_lm <- sqrt(mean((pred_lm - test$AQI)^2, na.rm = TRUE))
print(paste("Linear Regression RMSE:", round(rmse_lm, 4)))

# -------------------------------------------------------
# 2. Decision Tree
# -------------------------------------------------------
tree_model <- rpart(AQI ~ ., data = train, method = "anova")
rpart.plot(tree_model, main = "Decision Tree for AQI Prediction")

pred_tree <- predict(tree_model, newdata = test)
rmse_tree <- sqrt(mean((pred_tree - test$AQI)^2, na.rm = TRUE))
print(paste("Decision Tree RMSE:", round(rmse_tree, 4)))

# -------------------------------------------------------
# 3. KNN Classification
# FIX: both train_y and test_y must be factors with
# identical levels to avoid confusionMatrix() crash
# -------------------------------------------------------
num_cols <- sapply(data, is.numeric)
train_x  <- train[, num_cols]
test_x   <- test[, num_cols]

train_y <- factor(train$AQI_Category, levels = cats)
test_y  <- factor(test$AQI_Category,  levels = cats)

knn_pred <- knn(train = train_x, test = test_x, cl = train_y, k = 5)
knn_pred <- factor(knn_pred, levels = cats)

cat("KNN Confusion Matrix:\n")
print(confusionMatrix(knn_pred, test_y))

# -------------------------------------------------------
# 4. T-Test — 2 groups only
# -------------------------------------------------------
two_groups <- data[data$AQI_Category %in% c("Good", "Unhealthy"), ]
two_groups$AQI_Category <- droplevels(two_groups$AQI_Category)

cat("T-Test: Good vs Unhealthy\n")
print(t.test(AQI ~ AQI_Category, data = two_groups))

# -------------------------------------------------------
# 5. ANOVA
# -------------------------------------------------------
anova_model <- aov(AQI ~ AQI_Category, data = data)
cat("ANOVA Summary:\n")
print(summary(anova_model))

cat("Tukey Post-Hoc:\n")
print(TukeyHSD(anova_model))

# -------------------------------------------------------
# Model Comparison
# -------------------------------------------------------
cat("\n======= Model RMSE Comparison =======\n")
cat("Linear Regression RMSE:", round(rmse_lm,   4), "\n")
cat("Decision Tree RMSE    :", round(rmse_tree, 4), "\n")
cat("======================================\n")

print("✅ Modeling Completed")