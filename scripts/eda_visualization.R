# ============================================================
# eda_visualization.R — Exploratory Data Analysis
# BreatheEasy: Air Quality Analysis Project
# ============================================================

library(tidyverse)
library(ggplot2)
library(corrplot)
library(viridis)
library(maps)

# -------------------------------------------------------
# Load BOTH datasets
# data_raw → real AQI values → used for all visual plots
# data_norm → normalized values → used for correlation only
# -------------------------------------------------------
data_raw  <- read.csv("data/cleaned_data_raw.csv", stringsAsFactors = FALSE)
data_norm <- read.csv("data/cleaned_data.csv",     stringsAsFactors = FALSE)

# Ordered factor for correct left-to-right category ordering on plots
cat_levels <- c("Good", "Moderate", "Unhealthy for Sensitive Groups",
                "Unhealthy", "Very Unhealthy", "Hazardous")

data_raw$AQI_Category <- factor(data_raw$AQI_Category,
                                 levels = cat_levels, ordered = TRUE)

# Create outputs folder if missing
dir.create("outputs", showWarnings = FALSE)

# -------------------------------------------------------
# 1. Histogram — AQI distribution with EPA boundaries
# Uses data_raw so axes show real AQI numbers (0-300+)
# -------------------------------------------------------
p1 <- ggplot(data_raw, aes(x = AQI)) +
  geom_histogram(fill = "skyblue", bins = 30, color = "white") +
  geom_vline(xintercept = c(50, 100, 150, 200, 300),
             linetype = "dashed", color = "gray40", linewidth = 0.4) +
  labs(title = "AQI Distribution",
       subtitle = "Dashed lines = EPA category boundaries",
       x = "AQI Value", y = "Count") +
  theme_minimal()

print(p1)
ggsave("outputs/01_histogram_aqi.png", p1, width = 9, height = 5, dpi = 150)

# -------------------------------------------------------
# 2. Violin Plot — PM2.5 spread per AQI category
# Uses data_raw so y-axis shows real concentration values
# -------------------------------------------------------
p2 <- ggplot(data_raw, aes(x = AQI_Category, y = PM25, fill = AQI_Category)) +
  geom_violin(trim = FALSE, alpha = 0.8) +
  geom_boxplot(width = 0.08, fill = "white", outlier.shape = NA) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  labs(title = "PM2.5 Concentration by AQI Category",
       x = "AQI Category", y = "PM2.5 Concentration") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8))

print(p2)
ggsave("outputs/02_violin_pm25.png", p2, width = 10, height = 5, dpi = 150)

# -------------------------------------------------------
# 3. Correlation Matrix
# Uses data_norm so scale differences don't distort results
# -------------------------------------------------------
pollutant_cols <- c("AQI", "PM25", "PM10", "NO2", "SO2", "CO", "O3")
pollutant_cols <- pollutant_cols[pollutant_cols %in% colnames(data_norm)]

cor_matrix <- cor(data_norm[, pollutant_cols], use = "complete.obs")
rownames(cor_matrix) <- colnames(cor_matrix) <-
  gsub("PM25", "PM2.5", colnames(cor_matrix))

corrplot(cor_matrix,
         method      = "color",
         type        = "upper",
         order       = "hclust",
         addCoef.col = "black",
         number.cex  = 0.75,
         tl.col      = "black",
         tl.srt      = 45,
         title       = "Pollutant Correlation Matrix",
         mar         = c(0, 0, 2, 0))

png("outputs/03_correlation_matrix.png", width = 700, height = 650, res = 120)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", number.cex = 0.75, tl.col = "black",
         tl.srt = 45, title = "Pollutant Correlation Matrix",
         mar = c(0, 0, 2, 0))
dev.off()

# -------------------------------------------------------
# 4. Boxplot — AQI outliers by category
# Uses data_raw so y-axis shows real AQI values
# -------------------------------------------------------
p4 <- ggplot(data_raw, aes(x = AQI_Category, y = AQI, fill = AQI_Category)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5,
               outlier.alpha = 0.5, outlier.fill = "red",
               width = 0.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  labs(title = "AQI Distribution by Category",
       subtitle = "Red dots = outliers",
       x = "AQI Category", y = "AQI Value") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8))

print(p4)
ggsave("outputs/04_boxplot_aqi.png", p4, width = 10, height = 5, dpi = 150)

# -------------------------------------------------------
# 5. AQI Trend over time (runs only if Date column exists)
# -------------------------------------------------------
if ("Date" %in% colnames(data_raw)) {
  data_raw$Date <- as.Date(data_raw$Date)

  trend_data <- data_raw %>%
    group_by(Date) %>%
    summarise(avg_AQI = mean(AQI, na.rm = TRUE), .groups = "drop")

  p5 <- ggplot(trend_data, aes(x = Date, y = avg_AQI)) +
    geom_line(color = "blue", linewidth = 0.5, alpha = 0.7) +
    geom_smooth(method = "loess", se = TRUE,
                color = "red", linewidth = 0.8) +
    labs(title = "Average AQI Trend Over Time",
         x = "Date", y = "Average AQI") +
    theme_minimal()

  print(p5)
  ggsave("outputs/05_aqi_trend.png", p5, width = 11, height = 5, dpi = 150)
}

# -------------------------------------------------------
# 6. Geographic Heatmap (runs only if coordinates exist)
# -------------------------------------------------------
if (all(c("Longitude", "Latitude") %in% colnames(data_raw))) {

  world_map <- map_data("world")

  p6 <- ggplot() +
    geom_map(data = world_map, map = world_map,
             aes(long, lat, map_id = region),
             fill = "white", color = "black", linewidth = 0.3) +
    geom_point(data = data_raw,
               aes(x = Longitude, y = Latitude, color = AQI),
               size = 1.5, alpha = 0.6) +
    scale_color_viridis(option = "C", name = "AQI") +
    labs(title = "Geographic Distribution of AQI",
         x = NULL, y = NULL) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          panel.grid = element_blank())

  print(p6)
  ggsave("outputs/06_geo_heatmap.png", p6, width = 11, height = 6, dpi = 150)
}

print("✅ EDA Completed")