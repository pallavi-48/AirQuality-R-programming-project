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
# Use raw (unnormalized) for all visual plots → readable axes
# Use normalized for correlation matrix → fair comparison
# -------------------------------------------------------
data_raw  <- read.csv("data/cleaned_data_raw.csv", stringsAsFactors = FALSE)
data_norm <- read.csv("data/cleaned_data.csv",     stringsAsFactors = FALSE)

# Ensure AQI_Category is an ordered factor for correct plot ordering
category_levels <- c("Good", "Moderate",
                      "Unhealthy for Sensitive Groups",
                      "Unhealthy", "Very Unhealthy", "Hazardous")

data_raw$AQI_Category <- factor(data_raw$AQI_Category,
                                 levels = category_levels, ordered = TRUE)

# Create outputs folder if it doesn't exist
dir.create("outputs", showWarnings = FALSE)


# ============================================================
# PLOT 1: Histogram — AQI Distribution
# ============================================================
p1 <- ggplot(data_raw, aes(x = AQI)) +
  geom_histogram(
    aes(fill = after_stat(count)),
    bins    = 40,
    color   = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(option = "C", name = "Count") +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 300)) +
  geom_vline(xintercept = c(50, 100, 150, 200, 300),
             linetype = "dashed", color = "gray40", linewidth = 0.4) +
  annotate("text", x = 25,  y = Inf, label = "Good",
           vjust = 2, size = 2.8, color = "gray30") +
  annotate("text", x = 75,  y = Inf, label = "Moderate",
           vjust = 2, size = 2.8, color = "gray30") +
  annotate("text", x = 125, y = Inf, label = "Unhealthy\n(Sensitive)",
           vjust = 2, size = 2.8, color = "gray30") +
  annotate("text", x = 175, y = Inf, label = "Unhealthy",
           vjust = 2, size = 2.8, color = "gray30") +
  labs(
    title    = "Distribution of Air Quality Index (AQI)",
    subtitle = "Dashed lines show EPA category boundaries",
    x        = "AQI Value",
    y        = "Number of Readings"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p1)
ggsave("outputs/01_histogram_aqi.png", p1, width = 9, height = 5, dpi = 150)
cat("Saved: outputs/01_histogram_aqi.png\n")


# ============================================================
# PLOT 2: Boxplot — AQI by Category (with outliers highlighted)
# ============================================================
p2 <- ggplot(data_raw, aes(x = AQI_Category, y = AQI, fill = AQI_Category)) +
  geom_boxplot(
    outlier.shape  = 21,
    outlier.size   = 1.5,
    outlier.alpha  = 0.4,
    outlier.fill   = "red",
    width          = 0.5,
    linewidth      = 0.4
  ) +
  scale_fill_manual(values = c(
    "Good"                           = "#00C853",
    "Moderate"                       = "#FFD600",
    "Unhealthy for Sensitive Groups" = "#FF6D00",
    "Unhealthy"                      = "#D50000",
    "Very Unhealthy"                 = "#6A1B9A",
    "Hazardous"                      = "#37101A"
  )) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  labs(
    title    = "AQI Distribution by EPA Category",
    subtitle = "Red dots = outliers | Box = 25th–75th percentile",
    x        = "AQI Category",
    y        = "AQI Value"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold"),
    axis.text.x     = element_text(size = 9)
  )

print(p2)
ggsave("outputs/02_boxplot_aqi_category.png", p2, width = 10, height = 5, dpi = 150)
cat("Saved: outputs/02_boxplot_aqi_category.png\n")


# ============================================================
# PLOT 3: Violin Plot — Pollutant spread by AQI Category
# Shows distribution shape, not just the summary stats
# ============================================================
p3 <- ggplot(data_raw, aes(x = AQI_Category, y = PM25, fill = AQI_Category)) +
  geom_violin(trim = FALSE, alpha = 0.8, linewidth = 0.3) +
  geom_boxplot(width = 0.08, fill = "white",
               outlier.shape = NA, linewidth = 0.4) +
  scale_fill_manual(values = c(
    "Good"                           = "#00C853",
    "Moderate"                       = "#FFD600",
    "Unhealthy for Sensitive Groups" = "#FF6D00",
    "Unhealthy"                      = "#D50000",
    "Very Unhealthy"                 = "#6A1B9A",
    "Hazardous"                      = "#37101A"
  )) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  labs(
    title    = "PM2.5 Concentration by AQI Category",
    subtitle = "Violin shape = full distribution | Inner box = IQR",
    x        = "AQI Category",
    y        = "PM2.5 Concentration"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold"),
    axis.text.x     = element_text(size = 9)
  )

print(p3)
ggsave("outputs/03_violin_pm25_category.png", p3, width = 10, height = 5, dpi = 150)
cat("Saved: outputs/03_violin_pm25_category.png\n")


# ============================================================
# PLOT 4: Correlation Matrix — Pollutant relationships
# Uses normalized data so scale differences don't distort results
# ============================================================

# Select only numeric pollutant columns that exist in the dataset
pollutant_cols <- c("AQI", "PM25", "PM10", "NO2", "SO2", "CO", "O3")
pollutant_cols <- pollutant_cols[pollutant_cols %in% colnames(data_norm)]

cor_matrix <- cor(data_norm[, pollutant_cols], use = "complete.obs")

# Rename for cleaner labels on the plot
rownames(cor_matrix) <- colnames(cor_matrix) <-
  gsub("PM25", "PM2.5", colnames(cor_matrix))

png("outputs/04_correlation_matrix.png", width = 700, height = 650, res = 120)
corrplot(
  cor_matrix,
  method      = "color",
  type        = "upper",
  order       = "hclust",          # groups correlated variables together
  addCoef.col = "black",
  number.cex  = 0.75,
  tl.col      = "black",
  tl.srt      = 45,
  tl.cex      = 0.9,
  col         = COL2("RdYlGn"),    # red = negative, green = positive
  title       = "Pollutant Correlation Matrix",
  mar         = c(0, 0, 2, 0)
)
dev.off()

# Also display it in the plot panel
corrplot(
  cor_matrix,
  method      = "color",
  type        = "upper",
  order       = "hclust",
  addCoef.col = "black",
  number.cex  = 0.75,
  tl.col      = "black",
  tl.srt      = 45,
  col         = COL2("RdYlGn"),
  title       = "Pollutant Correlation Matrix",
  mar         = c(0, 0, 2, 0)
)
cat("Saved: outputs/04_correlation_matrix.png\n")


# ============================================================
# BONUS PLOT 5: AQI Trend over time (if Date column exists)
# ============================================================
if ("Date" %in% colnames(data_raw)) {
  data_raw$Date <- as.Date(data_raw$Date)

  trend_data <- data_raw %>%
    group_by(Date) %>%
    summarise(avg_AQI = mean(AQI, na.rm = TRUE), .groups = "drop")

  p5 <- ggplot(trend_data, aes(x = Date, y = avg_AQI)) +
    geom_line(color = "#1565C0", linewidth = 0.5, alpha = 0.7) +
    geom_smooth(method = "loess", se = TRUE,
                color = "#D32F2F", fill = "#EF9A9A",
                linewidth = 0.8, alpha = 0.3) +
    labs(
      title    = "Average AQI Trend Over Time",
      subtitle = "Blue = daily average | Red = smoothed trend (LOESS)",
      x        = "Date",
      y        = "Average AQI"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))

  print(p5)
  ggsave("outputs/05_aqi_trend_time.png", p5, width = 11, height = 5, dpi = 150)
  cat("Saved: outputs/05_aqi_trend_time.png\n")
}


# ============================================================
# BONUS PLOT 6: Geographic heatmap (if coordinates exist)
# ============================================================
if (all(c("Longitude", "Latitude") %in% colnames(data_raw))) {

  usa_map <- map_data("state")

  p6 <- ggplot() +
    geom_map(
      data = usa_map, map = usa_map,
      aes(long, lat, map_id = region),
      fill = "#ECEFF1", color = "#90A4AE", linewidth = 0.3
    ) +
    geom_point(
      data = data_raw,
      aes(x = Longitude, y = Latitude, color = AQI),
      size = 0.8, alpha = 0.6
    ) +
    scale_color_viridis_c(
      option = "C",
      name   = "AQI",
      limits = c(0, 300),
      oob    = scales::squish
    ) +
    coord_fixed(1.3, xlim = c(-125, -65), ylim = c(24, 50)) +
    labs(
      title    = "Geographic Distribution of AQI Across the USA",
      subtitle = "Each point = one monitoring station reading",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title      = element_text(face = "bold"),
      axis.text       = element_blank(),
      panel.grid      = element_blank()
    )

  print(p6)
  ggsave("outputs/06_geo_heatmap.png", p6, width = 11, height = 6, dpi = 150)
  cat("Saved: outputs/06_geo_heatmap.png\n")
}


cat("\n✅ EDA Completed — check your outputs/ folder for all plots\n")