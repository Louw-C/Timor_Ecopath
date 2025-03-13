# Chlorophyll a Data Analysis: North vs. South Coast and Seasonal Changes
# This script imports, explores, and visualizes chlorophyll a data from Jan 2019 to Dec 2024
# Data downloaded from Giovanni - MODIS-Aqua MODISA_L3m_CHL vR2022.0

# Load necessary packages
library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For handling dates
library(viridis)    # For color palettes
library(gridExtra)  # For arranging multiple plots
library(readxl)     # For reading Excel files

# 1. Import Data

# Assuming the file is named "chla_data.xlsx" and contains columns for date, location (north/south), and chla values
# Adjust the file path as needed
chla_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Chl a_Timor-Leste_2019-2024_Monthly Averages_North+South.csv")
# If your data is in a specific sheet, you can specify it:
# chla_data <- read_excel("chla_data.xlsx", sheet = "Sheet1")
names(chla_data)

# 2. Data Cleaning and Preparation
# First, let's check the column names to identify the date column
print("Column names in the dataset:")
print(names(chla_data))

#Look at the first few rows of data
print("First few rows of data:")
print(head(chla_data))

# This looks like an already properly formatted datetime in ISO format: "2019-01-01 00:00:00"
# Let's examine the class to see if R already recognizes it as a datetime
print("Class of the first column:")
print(class(chla_data[[1]]))

# The first column is already a proper datetime object (POSIXct/POSIXt)
# Let's simplify by directly using it
chla_data <- chla_data %>%
  mutate(
    # Just to be safe, use the actual column name rather than positional index
    # Modify this if your date column has a different name
    date_clean = chla_data[[1]],  # Use the actual column name here if needed
    year = year(date_clean),
    month = month(date_clean)
  )

# Add season as a separate operation
chla_data <- chla_data %>%
  mutate(
    season = case_when(
      month %in% c(12, 1, 2) ~ "Summer",
      month %in% c(3, 4, 5) ~ "Fall",
      month %in% c(6, 7, 8) ~ "Winter",
      month %in% c(9, 10, 11) ~ "Spring",
      TRUE ~ NA_character_
    )
  )

# Ensure seasons are properly ordered
chla_data$season <- factor(chla_data$season, levels = c("Winter", "Spring", "Summer", "Fall"))


# 3. Data Exploration
print("Overall Chlorophyll a Summary:")
print(summary_overall)

# Summary statistics by location
summary_by_location <- chla_data %>%
  group_by(location) %>%
  summarize(
    mean_chla = mean(chla, na.rm = TRUE),
    median_chla = median(chla, na.rm = TRUE),
    min_chla = min(chla, na.rm = TRUE),
    max_chla = max(chla, na.rm = TRUE),
    sd_chla = sd(chla, na.rm = TRUE)
  )
print("Chlorophyll a Summary by Location:")
print(summary_by_location)

# Summary statistics by zone and season
summary_by_zone_season <- chla_data %>%
  group_by(season,location) %>%
  summarize(
    mean_chla = mean(chla, na.rm = TRUE),
    median_chla = median(chla, na.rm = TRUE),
    min_chla = min(chla, na.rm = TRUE),
    max_chla = max(chla, na.rm = TRUE),
    sd_chla = sd(chla, na.rm = TRUE)
  )
print("Chlorophyll a Summary by Season:")
print(summary_by_zone_season)

# 4. Visualization

# 4.1 North vs. South Coast Comparison

# Boxplot comparing North and South Coast
p1 <- ggplot(chla_data, aes(x = location, y = chla, fill = location)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.7) +
  labs(
    title = "Chlorophyll a Concentrations: North vs. South Coast",
    x = "Location",
    y = "Chlorophyll a (mg/m3)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
p1

# Time series comparison of North and South coast
p2 <- ggplot(chla_data, aes(x = date_clean, y = chla, color = location)) +
  geom_line() +
  geom_point(alpha = 0.6) +
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.7) +
  labs(
    title = "Monthly Averaged Chlorophyll a Time Series: North vs. South Coast",
    x = "Date",
    y = "Chlorophyll a (mg/m3)",
    color = "Location"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
p2

# Display the plots
grid.arrange(p1, p2, ncol = 1)

# 4.2 Seasonal Changes Across Years

# Update all references to date throughout the script
# First, let's add this line to make referencing easier in the script
# After you identify the correct date column name, update the rest of the script

# Modify references to the chlorophyll column name throughout the script
# Change from "chla" to the actual column name "Chl a_mg-m³"

# Boxplot of seasonal patterns
p3 <- ggplot(chla_data, aes(x = season, y = chla, fill = season)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "Seasonal Patterns in Chlorophyll a Concentrations",
    x = "Season",
    y = "Chlorophyll a (mg/m3)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
p3

# Seasonal patterns by Zone
p4 <- ggplot(chla_data, aes(x = season, y = chla, fill = location)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.7) +
  labs(
    title = "Seasonal Chlorophyll a by Location",
    x = "Season",
    y = "Chlorophyll a (mg/m3)",
    fill = "Location"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
p4

# Display the plots
grid.arrange(p3, p4, ncol = 1)

# 4.3 Year by Year Analysis

# Calculate annual averages by Zone
annual_averages <- chla_data %>%
  group_by(year, location) %>%
  summarize(mean_chla = mean(chla, na.rm = TRUE))

# Annual patterns
p5 <- ggplot(annual_averages, aes(x = factor(year), y = mean_chla, fill = location)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.7) +
  labs(
    title = "Annual Average Chlorophyll a by Location",
    x = "Year",
    y = "Mean Chlorophyll a (mg/m3)",
    fill = "Location"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
p5

# Calculate seasonal averages by year
seasonal_yearly_averages <- chla_data %>%
  group_by(year, season, location) %>%
  summarize(mean_chla = mean(chla, na.rm = TRUE))

# Seasonal patterns across years
p6 <- ggplot(seasonal_yearly_averages, aes(x = factor(year), y = mean_chla, color = season)) +
  geom_line(aes(group = season), linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~location) +
  scale_color_viridis(discrete = TRUE) +
  labs(
    title = "Seasonal Chlorophyll a Trends by Year and Location",
    x = "Year",
    y = "Mean Chlorophyll a (mg/m3)",
    color = "Season"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
p6

# Display the plots
grid.arrange(p5, p6, ncol = 1)

# 4.4 Heatmap of monthly values across years
# Prepare data for heatmap
monthly_data <- chla_data %>%
  group_by(year, month, location) %>%
  summarize(mean_chla = mean(chla, na.rm = TRUE)) %>%
  ungroup()

# Create month names for better labeling
monthly_data$month_name <- month.abb[monthly_data$month]
monthly_data$month_name <- factor(monthly_data$month_name, levels = month.abb)

# North coast heatmap
north_data <- filter(monthly_data, location == "North")
p7 <- ggplot(north_data, aes(x = factor(year), y = month_name, fill = mean_chla)) +
  geom_tile() +
  scale_fill_viridis() +
  labs(
    title = "Monthly Chlorophyll a Concentrations - North Coast",
    x = "Year",
    y = "Month",
    fill = "Chl a (mg/m3)"
  ) +
  theme_minimal()
p7

# South coast heatmap
south_data <- filter(monthly_data, location == "South")
p8 <- ggplot(south_data, aes(x = factor(year), y = month_name, fill = mean_chla)) +
  geom_tile() +
  scale_fill_viridis() +
  labs(
    title = "Monthly Chlorophyll a Concentrations - South Coast",
    x = "Year",
    y = "Month",
    fill = "Chl a (mg/m3)"
  ) +
  theme_minimal()
p8

# Display the heatmaps
grid.arrange(p7, p8, ncol = 1)

# 5. Statistical Tests

# 5.1 Compare North vs. South overall
t_test_result <- t.test(chla ~ location, data = chla_data)
print("T-test comparing North and South Coast Chlorophyll a:")
print(t_test_result)

# 5.2 ANOVA for seasonal differences
anova_season <- aov(chla ~ season, data = chla_data)
print("ANOVA for seasonal differences in Chlorophyll a:")
print(summary(anova_season))
print(TukeyHSD(anova_season))

# 5.3 Two-way ANOVA for interaction between Zone and season
anova_interaction <- aov(chla ~ location * season, data = chla_data)
print("Two-way ANOVA for location and season interaction:")
print(summary(anova_interaction))

# 6. Check for long-term trends (time series analysis)
# Create a simple linear model to check for trends over time
north_data <- filter(chla_data, location == "North")
south_data <- filter(chla_data, location == "South")

# Convert date to numeric for regression
north_data$date_numeric <- as.numeric(north_data$date_clean)
south_data$date_numeric <- as.numeric(south_data$date_clean)

# Linear models
north_trend <- lm(chla ~ date_numeric, data = north_data)
south_trend <- lm(chla ~ date_numeric, data = south_data)

print("North Coast Long-term Trend Analysis:")
print(summary(north_trend))

print("South Coast Long-term Trend Analysis:")
print(summary(south_trend))

# Plot the trends
p9 <- ggplot(chla_data, aes(x = date_clean, y = chla, color = location)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.7) +
  labs(
    title = "Long-term Chlorophyll a Trends",
    x = "Date",
    y = "Chlorophyll a (μg/L)",
    color = "Location"
  ) +
  theme_minimal()

print(p9)

# Save all plots for future reference
ggsave("chla_boxplot_comparison.png", p1, width = 10, height = 6)
ggsave("chla_timeseries.png", p2, width = 10, height = 6)
ggsave("chla_seasonal_patterns.png", p3, width = 10, height = 6)
ggsave("chla_seasonal_by_location.png", p4, width = 10, height = 6)
ggsave("chla_annual_averages.png", p5, width = 10, height = 6)
ggsave("chla_seasonal_trends.png", p6, width = 10, height = 8)
ggsave("chla_north_heatmap.png", p7, width = 10, height = 6)
ggsave("chla_south_heatmap.png", p8, width = 10, height = 6)
ggsave("chla_longterm_trends.png", p9, width = 10, height = 6)

# 7. Summary of findings
cat("\n\n======== SUMMARY OF FINDINGS ========\n")
cat("1. North vs. South Comparison:\n")
cat("   - Mean Chl a in North:", round(filter(summary_by_zone, zone == "North")$mean_chla, 2), "μg/L\n")
cat("   - Mean Chl a in South:", round(filter(summary_by_zone, zone == "South")$mean_chla, 2), "μg/L\n")
cat("   - T-test p-value:", round(t_test_result$p.value, 4), "\n\n")

cat("2. Seasonal Patterns:\n")
for(s in levels(chla_data$season)) {
  cat("   - Mean Chl a in", s, ":", 
      round(filter(summary_by_season, season == s)$mean_chla, 2), "μg/L\n")
}
cat("\n")

cat("3. Long-term Trends:\n")
cat("   - North Coast slope:", round(coef(north_trend)[2], 6), 
    "(p-value:", round(summary(north_trend)$coefficients[2,4], 4), ")\n")
cat("   - South Coast slope:", round(coef(south_trend)[2], 6),
    "(p-value:", round(summary(south_trend)$coefficients[2,4], 4), ")\n\n")