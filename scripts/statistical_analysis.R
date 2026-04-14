# ============================================================
# statistical_analysis.R — Statistical Tests
# BreatheEasy: Air Quality Analysis Project
# ============================================================

library(tidyverse)

# Uses raw (unnormalized) data so correlation values are
# based on real pollutant concentrations, not z-scores
data <- read.csv("data/cleaned_data_raw.csv", stringsAsFactors = FALSE)

# FIX: cats must match exactly what 01_preprocessing.R creates
# and match modeling.R — all 3 files must use the same list
cats <- c("Good", "Moderate", "Unhealthy for Sensitive Groups",
          "Unhealthy", "Very Unhealthy", "Hazardous")

data$AQI_Category <- factor(data$AQI_Category, levels = cats)

# -------------------------------------------------------
# 1. Correlation & Covariance
# Only include columns that actually exist in the dataset
# -------------------------------------------------------
pollutants <- c("PM25", "PM10", "NO2", "SO2", "CO", "O3")
pollutants <- pollutants[pollutants %in% colnames(data)]

cat("=== Correlation Matrix ===\n")
print(round(cor(data[, pollutants], use = "complete.obs"), 2))

cat("\n=== Covariance Matrix ===\n")
print(round(cov(data[, pollutants], use = "complete.obs"), 2))

# -------------------------------------------------------
# 2. T-Test — Good vs Unhealthy only
# t.test() requires exactly 2 groups
# -------------------------------------------------------
two_groups <- data[data$AQI_Category %in% c("Good", "Unhealthy"), ]
two_groups$AQI_Category <- droplevels(two_groups$AQI_Category)

cat("\n=== T-Test: Good vs Unhealthy ===\n")
print(t.test(AQI ~ AQI_Category, data = two_groups))

# -------------------------------------------------------
# 3. ANOVA — all categories
# -------------------------------------------------------
cat("\n=== One-Way ANOVA ===\n")
anova_model <- aov(AQI ~ AQI_Category, data = data)
print(summary(anova_model))

# Tukey shows exactly which category pairs are significantly different
cat("\n=== Tukey Post-Hoc ===\n")
print(TukeyHSD(anova_model))

cat("\n✅ Statistical Analysis Completed\n")