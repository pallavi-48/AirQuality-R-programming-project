# Load libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(viridis)
library(maps)

# Load cleaned data
data <- read.csv("data/cleaned_data.csv")

# -----------------------------
# 1. Histogram of AQI
# -----------------------------
ggplot(data, aes(x = AQI)) +
  geom_histogram(fill = "skyblue", bins = 30) +
  theme_minimal()

# -----------------------------
# 2. Violin Plot
# -----------------------------
ggplot(data, aes(x = AQI_Category, y = AQI)) +
  geom_violin(fill = "purple") +
  theme_minimal()

# -----------------------------
# 3. Correlation Matrix
# -----------------------------
num_cols <- sapply(data, is.numeric)
cor_matrix <- cor(data[, num_cols])

corrplot(cor_matrix, method = "color", type = "upper")

# -----------------------------
# 4. Boxplot (Outliers)
# -----------------------------
ggplot(data, aes(y = AQI)) +
  geom_boxplot(fill = "orange") +
  theme_minimal()

# -----------------------------
# 5. AQI Trend (if Date exists)
# -----------------------------
if("Date" %in% colnames(data)){
  ggplot(data, aes(x = Date, y = AQI)) +
    geom_line(color = "blue") +
    theme_minimal()
}

# -----------------------------
# 6. Geographic Heatmap (if coordinates exist)
# -----------------------------
if(all(c("Longitude", "Latitude") %in% colnames(data))){
  
  world_map <- map_data("world")
  
  ggplot() +
    geom_map(data = world_map, map = world_map,
             aes(long, lat, map_id = region),
             fill = "white", color = "black") +
    geom_point(data = data,
               aes(x = Longitude, y = Latitude, color = AQI),
               size = 2) +
    scale_color_viridis() +
    theme_minimal()
}

print("✅ EDA Completed")