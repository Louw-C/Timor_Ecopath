# R Script for Friedland Index Analysis
# The Friedland Index relates fisheries productivity to primary productivity (chlorophyll-a)

# Load required libraries
library(tidyverse)
library(lubridate)
library(zoo)

# 1. Data Import and Preparation
# ==============================

# Import catch data
catch_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Fisheries indices/South Coast_Catch Data_Test 1.csv", stringsAsFactors = FALSE)

# Import chlorophyll data
chla_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Fisheries indices/MODISA_L3m_CHL_Monthly_Jan19-Oct23_SOUTH.csv", stringsAsFactors = FALSE)

# Display data structure
str(catch_data)
str(chla_data)


# 2. Data Cleaning and Transformation
# ==================================

# Convert dates in catch data (format: DD/MM/YY)
catch_data$date <- dmy(catch_data$date)
catch_data$year_month <- floor_date(catch_data$date, "month")

# Convert dates in chlorophyll data (format: DD/MM/YY HH:MM)
chla_data$date <- dmy_hm(chla_data$date)
chla_data$year_month <- floor_date(chla_data$date, "month")

# Aggregate catch data by month (sum across all groups)
monthly_catch <- catch_data %>%
  group_by(year_month) %>%
  summarize(total_catch_kg = sum(catch_kg, na.rm = TRUE))

# Aggregate chlorophyll data by month (mean)
monthly_chla <- chla_data %>%
  group_by(year_month) %>%
  summarize(mean_chla = mean(chla, na.rm = TRUE))

# 3. Merge the datasets
# ====================
merged_data <- full_join(monthly_catch, monthly_chla, by = "year_month") %>%
  arrange(year_month) %>%
  na.omit()  # Remove rows with missing data

# 4. Calculate Friedland Index
# ===========================
# The Friedland Index relates fish catch to primary productivity (chlorophyll)
# FI = ln(Catch) / ln(Chla)

# Unit conversion and aggregation for proper Friedland Index calculation
# =====================================================================

# 1. Convert units and aggregate by year
# For chlorophyll-a: convert from mg/m³ to mg/km² (multiply by 1,000,000)
# For catch: convert from kg to tonnes (divide by 1000)

# First, extract year from dates
catch_data$year <- year(catch_data$date)
chla_data$year <- year(chla_data$date)

# Aggregate catch data by year (sum across all months)
yearly_catch <- catch_data %>%
  group_by(year) %>%
  summarize(total_catch_kg = sum(catch_kg, na.rm = TRUE)) %>%
  # Convert kg to tonnes (t)
  mutate(total_catch_tonnes = total_catch_kg / 1000)

# Aggregate chlorophyll data by year (average across all months)
yearly_chla <- chla_data %>%
  group_by(year) %>%
  summarize(mean_chla_mg_m3 = mean(chla, na.rm = TRUE)) %>%
  # Convert mg/m³ to mg/km² (1 km² = 1,000,000 m²)
  mutate(mean_chla_mg_km2 = mean_chla_mg_m3 * 1000000)

# Merge yearly data
yearly_merged <- full_join(yearly_catch, yearly_chla, by = "year") %>%
  arrange(year) %>%
  na.omit()

# 2. Calculate the correct Friedland Index
# For chlorophyll values in mg/m³ and catch in tonnes, the resulting ratios will be very small

yearly_merged <- yearly_merged %>%
  mutate(
    # Catch per unit area (t/km²/yr)
    catch_per_km2 = total_catch_tonnes / fishing_area_km2,
    
    # Calculate the proper Friedland Index (ratio of catch to chlorophyll)
    friedland_ratio = catch_per_km2 / mean_chla_mg_km2,
    
    # Calculate a scaled version that's easier to interpret (multiply by 1,000,000)
    friedland_ratio_scaled = friedland_ratio * 1000000
  )

# Print summary of the properly calculated Friedland Index
cat("\nProperly calculated Friedland Index (t/mg):\n")
print(summary(yearly_merged$friedland_ratio))
cat("\nScaled Friedland Index (t/mg × 10⁶):\n")
print(summary(yearly_merged$friedland_ratio_scaled))

# Plot the proper Friedland Index over time (scientific notation)
proper_ratio_plot <- ggplot(yearly_merged, aes(x = year, y = friedland_ratio)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Annual Friedland Index (Catch-to-Chlorophyll Ratio)",
       subtitle = paste("Based on estimated fishing area of", fishing_area_km2, "km²"),
       x = "Year", 
       y = "Catch (t/km²/yr) / Chlorophyll (mg/km²/yr)",
       caption = "Note: Values are shown in scientific notation") +
  theme_minimal() +
  # Add data labels in scientific notation (which is more readable for these values)
  geom_text(aes(label = formatC(friedland_ratio, format = "e", digits = 2)), 
            vjust = -1, size = 3.5)
proper_ratio_plot

# Save the plot
ggsave("annual_friedland_index_plot.png", proper_ratio_plot, width = 10, height = 6)

# Add note about area estimation
cat("\nNOTE: The Friedland Index calculation uses an estimated fishing area of", 
    fishing_area_km2, "km². Please update this value to match your actual study area.\n")

# Also calculate without the area (directly comparable index)
yearly_merged <- yearly_merged %>%
  mutate(
    # Direct ratio of tonnes to mg/km² (eliminates need for area estimation)
    direct_ratio = total_catch_tonnes / mean_chla_mg_km2
  )

# Plot this alternative calculation
alt_ratio_plot <- ggplot(yearly_merged, aes(x = year, y = direct_ratio)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "green", size = 3) +
  labs(title = "Alternative Annual Friedland Index",
       subtitle = "Direct ratio of total catch (t) to total chlorophyll (mg/km²)",
       x = "Year", 
       y = "Catch (t) / Chlorophyll (mg/km²)",
       caption = "No area normalization - for trend analysis only") +
  theme_minimal() +
  # Add data labels
  geom_text(aes(label = format(direct_ratio, scientific = TRUE, digits = 3)), 
            vjust = -1, size = 3.5)
alt_ratio_plot

# Save the alternative plot
ggsave("alternative_annual_index_plot.png", alt_ratio_plot, width = 10, height = 6)

# 5. Calculate lag relationships
# =============================
# Chlorophyll changes often precede changes in catch by several months
# Let's calculate lagged relationships (3, 6, and 12 months)

merged_data <- merged_data %>%
  mutate(
    chla_lag3 = lag(mean_chla, 3),
    chla_lag6 = lag(mean_chla, 6),
    chla_lag12 = lag(mean_chla, 12),
    fi_lag3 = ln_catch / log(chla_lag3),
    fi_lag6 = ln_catch / log(chla_lag6),
    fi_lag12 = ln_catch / log(chla_lag12)
  )

# 6. Visualization
# ===============

# Plot 1: Time series of catch and chlorophyll
plot1 <- ggplot(merged_data) +
  geom_line(aes(x = year_month, y = total_catch_kg, color = "Catch (kg)")) +
  geom_line(aes(x = year_month, y = mean_chla * max(total_catch_kg) / max(mean_chla), 
                color = "Chlorophyll-a (scaled)")) +
  scale_y_continuous(
    name = "Catch (kg)",
    sec.axis = sec_axis(~. * max(merged_data$mean_chla) / max(merged_data$total_catch_kg), 
                        name = "Chlorophyll-a")
  ) +
  scale_color_manual(values = c("Catch (kg)" = "blue", "Chlorophyll-a (scaled)" = "green")) +
  labs(title = "Time Series of Fish Catch and Chlorophyll-a",
       x = "Time", y = "Catch (kg)", color = "Metric") +
  theme_minimal()
plot1

# Plot 2: Friedland Index over time - comparing both calculations
plot2 <- ggplot(merged_data) +
  # Original Friedland Index
  geom_line(aes(x = year_month, y = friedland_index, color = "Original Index")) +
  # Alternative Friedland Index with scaled chlorophyll
  geom_line(aes(x = year_month, y = friedland_index_alt, color = "Scaled Chla Index")) +
  scale_color_manual(values = c("Original Index" = "red", "Scaled Chla Index" = "blue")) +
  labs(title = "Friedland Index Over Time",
       subtitle = "Comparing original calculation with scaled chlorophyll approach",
       x = "Time", 
       y = "Friedland Index",
       caption = "Original: ln(Catch) / ln(Chla)  |  Scaled: ln(Catch) / ln(Chla×100)",
       color = "Calculation Method") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Create a third calculation method based on ratio
merged_data <- merged_data %>%
  mutate(friedland_index_ratio = log(total_catch_kg / mean_chla))

# Plot comparing all three methods
plot2b <- ggplot(merged_data) +
  geom_line(aes(x = year_month, y = friedland_index, color = "Original")) +
  geom_line(aes(x = year_month, y = friedland_index_alt, color = "Scaled Chla")) +
  geom_line(aes(x = year_month, y = friedland_index_ratio, color = "ln(Catch/Chla)")) +
  scale_color_manual(values = c("Original" = "red", "Scaled Chla" = "blue", "ln(Catch/Chla)" = "green")) +
  labs(title = "Comparison of Different Friedland Index Calculations",
       x = "Time", 
       y = "Index Value",
       color = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 3: Scatter plot of ln(Catch) vs ln(Chla)
plot3 <- ggplot(merged_data, aes(x = ln_chla, y = ln_catch)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Relationship Between ln(Catch) and ln(Chlorophyll-a)",
       x = "ln(Chlorophyll-a)", y = "ln(Catch)") +
  theme_minimal()

# Plot 4: Friedland Index with different lags
plot4 <- merged_data %>%
  pivot_longer(cols = c(friedland_index, fi_lag3, fi_lag6, fi_lag12),
               names_to = "lag_type", values_to = "fi_value") %>%
  mutate(lag_type = factor(lag_type,
                           levels = c("friedland_index", "fi_lag3", "fi_lag6", "fi_lag12"),
                           labels = c("No lag", "3-month lag", "6-month lag", "12-month lag"))) %>%
  filter(!is.na(fi_value)) %>%
  ggplot(aes(x = year_month, y = fi_value, color = lag_type)) +
  geom_line() +
  labs(title = "Friedland Index with Different Time Lags",
       x = "Time", y = "Friedland Index", color = "Lag Period") +
  theme_minimal()

# 7. Statistical Analysis
# ======================

# Correlation between catch and chlorophyll (with different lags)
cor_no_lag <- cor.test(merged_data$ln_catch, merged_data$ln_chla, method = "pearson")
cor_lag3 <- cor.test(merged_data$ln_catch, log(merged_data$chla_lag3), method = "pearson", use = "complete.obs")
cor_lag6 <- cor.test(merged_data$ln_catch, log(merged_data$chla_lag6), method = "pearson", use = "complete.obs")
cor_lag12 <- cor.test(merged_data$ln_catch, log(merged_data$chla_lag12), method = "pearson", use = "complete.obs")

# Linear regression models
model_no_lag <- lm(ln_catch ~ ln_chla, data = merged_data)
model_lag3 <- lm(ln_catch ~ log(chla_lag3), data = merged_data)
model_lag6 <- lm(ln_catch ~ log(chla_lag6), data = merged_data)
model_lag12 <- lm(ln_catch ~ log(chla_lag12), data = merged_data)

# 8. Display results
# =================

# Print correlation results
print("Correlation between ln(Catch) and ln(Chlorophyll-a):")
print(paste("No lag:", round(cor_no_lag$estimate, 3), "p-value:", round(cor_no_lag$p.value, 4)))
print(paste("3-month lag:", round(cor_lag3$estimate, 3), "p-value:", round(cor_lag3$p.value, 4)))
print(paste("6-month lag:", round(cor_lag6$estimate, 3), "p-value:", round(cor_lag6$p.value, 4)))
print(paste("12-month lag:", round(cor_lag12$estimate, 3), "p-value:", round(cor_lag12$p.value, 4)))

# Print model summaries
summary(model_no_lag)
summary(model_lag3)
summary(model_lag6)
summary(model_lag12)

# Calculate average Friedland Index
mean_fi <- mean(merged_data$friedland_index, na.rm = TRUE)
median_fi <- median(merged_data$friedland_index, na.rm = TRUE)
sd_fi <- sd(merged_data$friedland_index, na.rm = TRUE)

print(paste("Mean Friedland Index:", round(mean_fi, 2)))
print(paste("Median Friedland Index:", round(median_fi, 2)))
print(paste("Standard Deviation of Friedland Index:", round(sd_fi, 2)))

# 9. Export results
# ================

# Export processed data
write.csv(merged_data, "friedland_index_results.csv", row.names = FALSE)

# Save plots
ggsave("time_series_plot.png", plot1, width = 10, height = 6)
ggsave("friedland_index_plot.png", plot2, width = 10, height = 6)
ggsave("scatter_plot.png", plot3, width = 8, height = 6)
ggsave("lag_comparison_plot.png", plot4, width = 10, height = 6)

# 10. Group-specific analysis for Snapper/seaperch
# ====================================

# Analysis function for specific fish groups
group_analysis <- function(group_name) {
  group_catch <- catch_data %>%
    filter(group == group_name) %>%
    group_by(year_month) %>%
    summarize(total_catch_kg = sum(catch_kg, na.rm = TRUE))
  
  group_merged <- full_join(group_catch, monthly_chla, by = "year_month") %>%
    arrange(year_month) %>%
    na.omit() %>%
    mutate(
      ln_catch = log(total_catch_kg),
      ln_chla = log(mean_chla),
      # Original Friedland Index
      friedland_index = ln_catch / ln_chla,
      # Alternative calculation with scaled chlorophyll
      friedland_index_alt = ln_catch / log(mean_chla * 100),
      # Ratio-based calculation
      friedland_index_ratio = log(total_catch_kg / mean_chla)
    )
  
  return(group_merged)
}

# Run analysis for Snapper/seaperch
snapper_data <- group_analysis("Snapper/seaperch")

# Check the results
print("Summary of Snapper/seaperch data:")
summary(snapper_data)

# Visualize Friedland Index for Snapper/seaperch
snapper_plot <- ggplot(snapper_data) +
  geom_line(aes(x = year_month, y = friedland_index_alt, color = "Snapper/seaperch")) +
  labs(title = "Friedland Index for Snapper/seaperch Over Time",
       subtitle = "Using scaled chlorophyll calculation",
       x = "Time", 
       y = "Friedland Index",
       color = "Fish Group") +
  theme_minimal()

# Comparison with overall Friedland Index
comparison_plot <- ggplot() +
  geom_line(data = merged_data, aes(x = year_month, y = friedland_index_alt, color = "All Species")) +
  geom_line(data = snapper_data, aes(x = year_month, y = friedland_index_alt, color = "Snapper/seaperch")) +
  scale_color_manual(values = c("All Species" = "blue", "Snapper/seaperch" = "red")) +
  labs(title = "Comparison of Friedland Index: All Species vs. Snapper/seaperch",
       x = "Time", 
       y = "Friedland Index (scaled)",
       color = "Group") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save the plots
ggsave("snapper_friedland_index.png", snapper_plot, width = 10, height = 6)
ggsave("species_comparison_plot.png", comparison_plot, width = 10, height = 6)

# Correlation analysis for Snapper/seaperch
snapper_cor <- cor.test(snapper_data$ln_catch, snapper_data$ln_chla, method = "pearson")
print(paste("Correlation between ln(Catch) and ln(Chla) for Snapper/seaperch:", 
            round(snapper_cor$estimate, 3), 
            "p-value:", round(snapper_cor$p.value, 4)))

# Test lagged relationships for Snapper/seaperch
snapper_lagged <- snapper_data %>%
  mutate(
    chla_lag3 = lag(mean_chla, 3),
    chla_lag6 = lag(mean_chla, 6),
    chla_lag12 = lag(mean_chla, 12),
    fi_lag3 = ln_catch / log(chla_lag3 * 100),
    fi_lag6 = ln_catch / log(chla_lag6 * 100),
    fi_lag12 = ln_catch / log(chla_lag12 * 100)
  )

# Plot lagged relationships for Snapper/seaperch
snapper_lag_plot <- snapper_lagged %>%
  pivot_longer(cols = c(friedland_index_alt, fi_lag3, fi_lag6, fi_lag12),
               names_to = "lag_type", values_to = "fi_value") %>%
  mutate(lag_type = factor(lag_type,
                           levels = c("friedland_index_alt", "fi_lag3", "fi_lag6", "fi_lag12"),
                           labels = c("No lag", "3-month lag", "6-month lag", "12-month lag"))) %>%
  filter(!is.na(fi_value)) %>%
  ggplot(aes(x = year_month, y = fi_value, color = lag_type)) +
  geom_line() +
  labs(title = "Friedland Index with Different Time Lags for Snapper/seaperch",
       x = "Time", y = "Friedland Index", color = "Lag Period") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("snapper_lag_analysis.png", snapper_lag_plot, width = 10, height = 6)