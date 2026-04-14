# ============================================================
# 01_preprocessing.R — Data Cleaning & Preprocessing
# BreatheEasy: Air Quality Analysis Project
# Run this FIRST before any other script
# ============================================================

library(tidyverse)
library(lubridate)

# -------------------------------------------------------
# STEP 1: Load the correct dataset
# -------------------------------------------------------
data <- read.csv("data/c4_epa_air_quality.csv", stringsAsFactors = FALSE)

cat("Rows loaded:", nrow(data), "| Columns:", ncol(data), "\n")

# -------------------------------------------------------
# STEP 2: Standardize column names
# Converts "PM2.5" -> "PM2.5", "Date Local" -> "Date.Local" etc.
# -------------------------------------------------------
colnames(data) <- make.names(colnames(data))
cat("Column names after make.names():\n")
print(colnames(data))

# -------------------------------------------------------
# STEP 3: Remove missing values
# -------------------------------------------------------
cat("Rows before NA removal:", nrow(data), "\n")
data <- na.omit(data)
cat("Rows after NA removal: ", nrow(data), "\n")

# -------------------------------------------------------
# STEP 4: Rename key columns
# Check the printed colnames above and adjust left-hand
# side values here if your CSV uses different names
# -------------------------------------------------------
data <- data %>%
  rename(
    AQI       = AQI.Value,
    PM25      = PM2.5,
    Date      = Date.Local,
    State     = State.Name,
    County    = County.Name,
    City      = City.Name,
    Latitude  = Site.Latitude,
    Longitude = Site.Longitude
  )

# -------------------------------------------------------
# STEP 5: Convert date column
# -------------------------------------------------------
if ("Date" %in% colnames(data)) {
  data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
  cat("Date range:", format(min(data$Date, na.rm = TRUE)),
      "to", format(max(data$Date, na.rm = TRUE)), "\n")
}

# -------------------------------------------------------
# STEP 6: Create AQI categories (EPA 6-level standard)
# These exact labels are used by ALL other scripts —
# do not change them
# -------------------------------------------------------
data$AQI_Category <- cut(
  data$AQI,
  breaks = c(-Inf, 50, 100, 150, 200, 300, Inf),
  labels = c("Good",
             "Moderate",
             "Unhealthy for Sensitive Groups",
             "Unhealthy",
             "Very Unhealthy",
             "Hazardous"),
  right = TRUE
)

cat("\nAQI Category counts:\n")
print(table(data$AQI_Category))

# -------------------------------------------------------
# STEP 7: Save RAW cleaned copy (for EDA plots)
# Uses real AQI values — readable axes on all plots
# -------------------------------------------------------
write.csv(data, "data/cleaned_data_raw.csv", row.names = FALSE)
cat("Saved: data/cleaned_data_raw.csv\n")

# -------------------------------------------------------
# STEP 8: Normalize numeric pollutant columns
# scale() → mean=0, sd=1 for each column
# Only pollutants are normalized — NOT Lat/Long
# -------------------------------------------------------
cols_to_normalize <- c("AQI", "PM25", "PM10", "NO2", "SO2", "CO", "O3")
cols_to_normalize <- cols_to_normalize[cols_to_normalize %in% colnames(data)]

data[cols_to_normalize] <- scale(data[cols_to_normalize])
cat("Normalized:", paste(cols_to_normalize, collapse = ", "), "\n")

# -------------------------------------------------------
# STEP 9: Save normalized copy (for modeling)
# -------------------------------------------------------
write.csv(data, "data/cleaned_data.csv", row.names = FALSE)
cat("Saved: data/cleaned_data.csv\n")

cat("\n===== Summary =====\n")
cat("Final rows   :", nrow(data), "\n")
cat("Final cols   :", ncol(data), "\n")
cat("Missing values:", sum(is.na(data)), "\n")
cat("===================\n")

print("✅ Preprocessing Completed")