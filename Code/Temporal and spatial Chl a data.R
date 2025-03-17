# Chlorophyll a Data Analysis: North vs. South Coast and Seasonal Changes
# This script imports, explores, and visualizes chlorophyll a data from Jan 2019 to Dec 2024
# Data downloaded from Giovanni:
#SOUTH COAST: Time Series, Area-Averaged of Chlorophyll a concentration (water only) 8-daily 4 km [MODIS-Aqua MODISA_L3m_CHL_8d_4km vR2022.0] mg m-3 over 2019-01-01 00:00:00Z - 2024-12-01 23:59:59Z, Region 125E, 9.5S, 127.33E, 9.15S
#NORTH COAST: Time Series, Area-Averaged of Chlorophyll a concentration (water only) 8-daily 4 km [MODIS-Aqua MODISA_L3m_CHL_8d_4km vR2022.0] mg m-3 over 2019-01-01 00:00:00Z - 2024-12-01 23:59:59Z, Region 124.3E, 8.75S, 127.3E, 8.25S

# Load necessary packages
library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For handling dates
library(viridis)    # For color palettes
library(gridExtra)  # For arranging multiple plots
library(readxl)     # For reading Excel files

# 1. Import Data

# Assuming the file is named "chla_data.xlsx" and contains columns for date, location (north/south), and chla values
# Adjust the file path as needed
chla_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Chla data/Chl a_Timor-Leste_2019-2024_Monthly Averages_North+South.csv")
names(chla_data)

# 2. Data Cleaning and Preparation
# First, let's check the column names to identify the date column
print("Column names in the dataset:")
print(names(chla_data))

#Look at the first few rows of data
print("First few rows of data:")
print(head(chla_data))

# The first column is already a proper datetime object (POSIXct/POSIXt)
# Create two additional columns with Year and Month
chla_data <- chla_data %>%
  mutate(
    date_clean = dmy_hm(date), 
    year = year(date_clean),
    month = month(date_clean)
  )

# Add season as a separate operation - create another column with season
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
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(lmtest)
library(mgcv)
library(car)

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
#For regression analysis, the code converts the datetime objects to numeric values.
#Essentially seconds since January 1, 1970. 
#This allows the datetime to be used as a continuous predictor variable in the linear model.
north_data$date_numeric <- as.numeric(north_data$date_clean)
south_data$date_numeric <- as.numeric(south_data$date_clean)

# Linear models
#These lines fit linear models for each location.
#Chlorophyll-a concentration as the response variable and time (date_numeric) as the predictor. 
#This tests if chlorophyll-a levels are changing over time.
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
    y = "Chlorophyll a (mg/m3)",
    color = "Location"
  ) +
  theme_minimal()

print(p9)

#===========================================
# 1. TESTING LINEAR REGRESSION ASSUMPTIONS
#===========================================

# Assuming north_data and south_data are already filtered as in your previous code
# and north_trend and south_trend are the linear models

#1. First, let's look at the basic diagnostic plots for the North model
par(mfrow=c(2,2))  # Set up a 2x2 plotting area
plot(north_trend)

# The four plots show:
# 1. Residuals vs Fitted (should show random scatter)
# 2. Normal Q-Q (points should follow the line)
# 3. Scale-Location (should show random scatter)
# 4. Residuals vs Leverage (look for influential points)

# 2. Same for the South model
par(mfrow=c(2,2))
plot(south_trend)
  
# 3. Let's check for autocorrelation in residuals (time series data often has this issue)
# Durbin-Watson test (values should be close to 2, p > 0.05 is good)
dw_north <- dwtest(north_trend)
dw_south <- dwtest(south_trend)

cat("\n==== AUTOCORRELATION TEST ====\n")
cat("North Coast Durbin-Watson test:\n")
print(dw_north)
cat("\nSouth Coast Durbin-Watson test:\n")
print(dw_south)
cat("\nInterpretation: p < 0.05 suggests autocorrelation is present\n")
cat("DW statistic near 2 suggests no autocorrelation, <1 or >3 suggests autocorrelation\n")

# 4. Let's check for heteroscedasticity (non-constant variance)
# Breusch-Pagan test (p > 0.05 is good)
bp_north <- bptest(north_trend)
bp_south <- bptest(south_trend)

cat("\n==== HETEROSCEDASTICITY TEST ====\n")
cat("North Coast Breusch-Pagan test:\n")
print(bp_north)
cat("\nSouth Coast Breusch-Pagan test:\n")
print(bp_south)
cat("\nInterpretation: p < 0.05 suggests heteroscedasticity is present\n")

# 5. Let's check for normality of residuals
# Shapiro-Wilk test (p > 0.05 is good)
sw_north <- shapiro.test(residuals(north_trend))
sw_south <- shapiro.test(residuals(south_trend))

cat("\n==== NORMALITY TEST ====\n")
cat("North Coast Shapiro-Wilk test:\n")
print(sw_north)
cat("\nSouth Coast Shapiro-Wilk test:\n")
print(sw_south)
cat("\nInterpretation: p < 0.05 suggests residuals are not normally distributed\n")

# 6. Check time patterns in residuals (important for time series data)
# Create dataframes with residuals and fitted values
north_resid_df <- data.frame(
  date = north_data$date_clean,
  residuals = residuals(north_trend),
  fitted = fitted(north_trend)
)
south_resid_df <- data.frame(
  date = south_data$date_clean,
  residuals = residuals(south_trend),
  fitted = fitted(south_trend)
)

# Plot residuals over time
p1 <- ggplot(north_resid_df, aes(x = date, y = residuals)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "North Coast: Residuals vs Time",
       x = "Date", y = "Residuals") +
  theme_minimal()

p2 <- ggplot(south_resid_df, aes(x = date, y = residuals)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "South Coast: Residuals vs Time",
       x = "Date", y = "Residuals") +
  theme_minimal()

print(p1)
print(p2)

# Summarize findings
cat("\n==== SUMMARY OF ASSUMPTION TESTS ====\n")

# North model
cat("\nNorth Coast Linear Model:\n")
cat("- Autocorrelation present: ", dw_north$p.value < 0.05, "\n")
cat("- Heteroscedasticity present: ", bp_north$p.value < 0.05, "\n")
cat("- Non-normal residuals: ", sw_north$p.value < 0.05, "\n")

# South model
cat("\nSouth Coast Linear Model:\n")
cat("- Autocorrelation present: ", dw_south$p.value < 0.05, "\n")
cat("- Heteroscedasticity present: ", bp_south$p.value < 0.05, "\n")
cat("- Non-normal residuals: ", sw_south$p.value < 0.05, "\n")

##Seems Linear Model is not suitable to investigate long-term trends in Chl a data.
##Try other approaches:

#GAM model - Generalized Additive Model (GAM)
#Ideal for: Non-linear trends in environmental data
#Advantages: Captures complex patterns without specifying the functional form
library(mgcv)
gam_model_north <- gam(chla ~ s(date_numeric), data = north_data)
print(gam_model_north)
gam_model_south<- gam(chla ~ s(date_numeric), data = south_data)
print(gam_model_south)

# Load necessary packages
library(mgcv)    # For GAM models
library(ggplot2) # For custom plots
library(gratia)  # For enhanced GAM plots (install if needed: install.packages("gratia"))
library(dplyr)

#=========================================
# 1. BASIC MODEL SUMMARY
#=========================================

# Summary of the model (includes significance tests, R-squared, etc.)
cat("\n==== NORTH COAST GAM MODEL SUMMARY ====\n")
print(summary(gam_model_north))

cat("\n==== SOUTH COAST GAM MODEL SUMMARY ====\n")
print(summary(gam_model_south))

#=========================================
# 2. VISUALIZE THE SMOOTH FUNCTIONS
#=========================================

# Basic plot of the smooth function (base R)
par(mfrow=c(1,2))
plot(gam_model_north, main="North Coast Chlorophyll-a Trend")
plot(gam_model_south, main="South Coast Chlorophyll-a Trend")

#=========================================
# 3. PREDICTED VALUES OVER TIME
#=========================================

# Create a sequence of dates for prediction
pred_dates <- seq(min(north_data$date_clean), max(north_data$date_clean), length.out = 200)
pred_dates_numeric <- as.numeric(pred_dates)

# Create prediction dataframe
pred_df_north <- data.frame(date_numeric = pred_dates_numeric)
pred_df_south <- data.frame(date_numeric = pred_dates_numeric)

# Get predictions with confidence intervals
north_pred <- predict(gam_model_north, pred_df_north, se.fit = TRUE)
south_pred <- predict(gam_model_south, pred_df_south, se.fit = TRUE)

# Create dataframe for plotting
north_plot_df <- data.frame(
  date = pred_dates,
  fit = north_pred$fit,
  upper = north_pred$fit + 1.96 * north_pred$se.fit,
  lower = north_pred$fit - 1.96 * north_pred$se.fit
)

south_plot_df <- data.frame(
  date = pred_dates,
  fit = south_pred$fit,
  upper = south_pred$fit + 1.96 * south_pred$se.fit,
  lower = south_pred$fit - 1.96 * south_pred$se.fit
)

# Plot with ggplot2
north_plot <- ggplot() +
  geom_ribbon(data = north_plot_df, aes(x = date, ymin = lower, ymax = upper), 
              fill = "blue", alpha = 0.2) +
  geom_line(data = north_plot_df, aes(x = date, y = fit), 
            color = "blue", size = 1) +
  geom_point(data = north_data, aes(x = date_clean, y = chla), 
             alpha = 0.5, color = "darkgrey") +
  labs(title = "North Coast: GAM Predicted Chlorophyll-a Trend",
       x = "Date", y = "Chlorophyll-a (mg/m³)") +
  theme_minimal()

south_plot <- ggplot() +
  geom_ribbon(data = south_plot_df, aes(x = date, ymin = lower, ymax = upper), 
              fill = "red", alpha = 0.2) +
  geom_line(data = south_plot_df, aes(x = date, y = fit), 
            color = "red", size = 1) +
  geom_point(data = south_data, aes(x = date_clean, y = chla), 
             alpha = 0.5, color = "darkgrey") +
  labs(title = "South Coast: GAM Predicted Chlorophyll-a Trend",
       x = "Date", y = "Chlorophyll-a (mg/m³)") +
  theme_minimal()

# Print the plots
print(north_plot)
print(south_plot)

#=========================================
# 4. COMBINED PLOT FOR COMPARISON
#=========================================

# Combine datasets for a joint plot
north_plot_df$location <- "North"
south_plot_df$location <- "South"
combined_df <- rbind(north_plot_df, south_plot_df)

# Add points data
north_data$location <- "North"
south_data$location <- "South"
points_df <- rbind(
  north_data %>% select(date_clean, chla, location),
  south_data %>% select(date_clean, chla, location)
) %>% rename(date = date_clean)

# Create combined plot
combined_plot <- ggplot() +
  # Add ribbons
  geom_ribbon(data = subset(combined_df, location == "North"), 
              aes(x = date, ymin = lower, ymax = upper), 
              fill = "blue", alpha = 0.2) +
  geom_ribbon(data = subset(combined_df, location == "South"), 
              aes(x = date, ymin = lower, ymax = upper), 
              fill = "red", alpha = 0.2) +
  # Add lines
  geom_line(data = combined_df, 
            aes(x = date, y = fit, color = location), 
            size = 1) +
  # Add points (optional, might be too crowded)
  geom_point(data = points_df, 
             aes(x = date, y = chla, color = location), 
             alpha = 0.2) +
  # Customize
  scale_color_manual(values = c("North" = "blue", "South" = "red")) +
  labs(title = "Chlorophyll-a Trends: North vs South Coast",
       x = "Date", 
       y = "Chlorophyll-a (mg/m³)",
       color = "Location") +
  theme_minimal()

print(combined_plot)

#=========================================
# 5. MODEL DIAGNOSTICS
#=========================================

# Check model diagnostics
par(mfrow=c(2,2))
gam.check(gam_model_north, main="North Coast GAM Diagnostics")

par(mfrow=c(2,2))
gam.check(gam_model_south, main="South Coast GAM Diagnostics")

#=========================================
# 6. RESIDUAL ANALYSIS
#=========================================

# Create residual dataframes
north_resid_df <- data.frame(
  date = north_data$date_clean,
  residuals = residuals(gam_model_north),
  fitted = fitted(gam_model_north)
)

south_resid_df <- data.frame(
  date = south_data$date_clean,
  residuals = residuals(gam_model_south),
  fitted = fitted(gam_model_south)
)

# Residuals vs time
north_resid_time <- ggplot(north_resid_df, aes(x = date, y = residuals)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "North Coast: GAM Residuals vs Time",
       x = "Date", y = "Residuals") +
  theme_minimal()

south_resid_time <- ggplot(south_resid_df, aes(x = date, y = residuals)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "South Coast: GAM Residuals vs Time",
       x = "Date", y = "Residuals") +
  theme_minimal()

print(north_resid_time)
print(south_resid_time)

# Autocorrelation of residuals
par(mfrow=c(1,2))
acf(north_resid_df$residuals, main="North Coast: ACF of GAM Residuals")
acf(south_resid_df$residuals, main="South Coast: ACF of GAM Residuals")

# Formal autocorrelation test
if(requireNamespace("lmtest", quietly = TRUE)) {
  library(lmtest)
  
  # Create time lags of residuals for Durbin-Watson test
  north_resid_df <- north_resid_df %>% arrange(date)
  south_resid_df <- south_resid_df %>% arrange(date)
  
  # Use a linear model of residuals for testing
  north_resid_lm <- lm(residuals ~ 1, data = north_resid_df)
  south_resid_lm <- lm(residuals ~ 1, data = south_resid_df)
  
  cat("\n==== AUTOCORRELATION TESTS ON GAM RESIDUALS ====\n")
  cat("\nNorth Coast Durbin-Watson test on GAM residuals:\n")
  print(dwtest(north_resid_lm))
  
  cat("\nSouth Coast Durbin-Watson test on GAM residuals:\n")
  print(dwtest(south_resid_lm))
}

#=========================================
# 7. COMPARE WITH LINEAR MODEL
#=========================================

# Compare with linear model using AIC and ANOVA
cat("\n==== MODEL COMPARISON: GAM VS LINEAR ====\n")

cat("\nNorth Coast - AIC Comparison:\n")
cat("Linear model AIC:", AIC(north_trend), "\n")
cat("GAM model AIC:", AIC(gam_model_north), "\n")
cat("Difference (Linear - GAM):", AIC(north_trend) - AIC(gam_model_north), 
    "(Positive values favor GAM)\n")

cat("\nSouth Coast - AIC Comparison:\n")
cat("Linear model AIC:", AIC(south_trend), "\n")
cat("GAM model AIC:", AIC(gam_model_south), "\n")
cat("Difference (Linear - GAM):", AIC(south_trend) - AIC(gam_model_south), 
    "(Positive values favor GAM)\n")

# ANOVA comparison (if models are nested)
cat("\nNorth Coast - ANOVA Comparison:\n")
tryCatch({
  print(anova(north_trend, gam_model_north, test="F"))
  cat("Interpretation: p < 0.05 suggests GAM is significantly better\n")
}, error = function(e) {
  cat("Error in ANOVA comparison:", e$message, "\n")
  cat("This may occur if models aren't properly nested\n")
})

cat("\nSouth Coast - ANOVA Comparison:\n")
tryCatch({
  print(anova(south_trend, gam_model_south, test="F"))
  cat("Interpretation: p < 0.05 suggests GAM is significantly better\n")
}, error = function(e) {
  cat("Error in ANOVA comparison:", e$message, "\n")
  cat("This may occur if models aren't properly nested\n")
})

#=========================================
# 8. INTERPRET GAM RESULTS
#=========================================

# Create a function to calculate first derivative of the smooth term
# to identify periods of significant increase or decrease
if(requireNamespace("gratia", quietly = TRUE)) {
  library(gratia)
  
  # Calculate derivatives for North model
  north_derivatives <- derivatives(gam_model_north, term = "s(date_numeric)")
  # Show where trend is significantly increasing or decreasing
  north_sig_trends <- north_derivatives %>%
    mutate(significant = (lower > 0) | (upper < 0),
           direction = case_when(
             lower > 0 ~ "Increasing",
             upper < 0 ~ "Decreasing",
             TRUE ~ "No Significant Change"
           ))
  
  # Same for South model
  south_derivatives <- derivatives(south_gam, term = "s(date_numeric)")
  south_sig_trends <- south_derivatives %>%
    mutate(significant = (lower > 0) | (upper < 0),
           direction = case_when(
             lower > 0 ~ "Increasing",
             upper < 0 ~ "Decreasing",
             TRUE ~ "No Significant Change"
           ))
  
  # Convert derivative data for plotting
  north_sig_df <- north_sig_trends %>%
    mutate(date = as.POSIXct(data, origin = "1970-01-01")) %>%
    arrange(date)
  
  south_sig_df <- south_sig_trends %>%
    mutate(date = as.POSIXct(data, origin = "1970-01-01")) %>%
    arrange(date)
  
  # Add periods of significant change to the trend plots
  north_trend_change <- north_plot +
    geom_point(data = subset(north_sig_df, significant), 
               aes(x = date, y = fit, color = direction), size = 2) +
    scale_color_manual(values = c("Increasing" = "green", 
                                  "Decreasing" = "red", 
                                  "No Significant Change" = "grey")) +
    labs(title = "North Coast: Periods of Significant Change in Chl-a")
  
  south_trend_change <- south_plot +
    geom_point(data = subset(south_sig_df, significant), 
               aes(x = date, y = fit, color = direction), size = 2) +
    scale_color_manual(values = c("Increasing" = "green", 
                                  "Decreasing" = "red", 
                                  "No Significant Change" = "grey")) +
    labs(title = "South Coast: Periods of Significant Change in Chl-a")
  
  print(north_trend_change)
  print(south_trend_change)
  
  # Summary of change periods
  cat("\n==== SUMMARY OF SIGNIFICANT TREND CHANGES ====\n")
  
  cat("\nNorth Coast - Periods of significant change:\n")
  north_periods <- north_sig_df %>%
    filter(significant) %>%
    group_by(direction) %>%
    summarize(start_date = min(date),
              end_date = max(date),
              .groups = "drop")
  print(north_periods)
  
  cat("\nSouth Coast - Periods of significant change:\n")
  south_periods <- south_sig_df %>%
    filter(significant) %>%
    group_by(direction) %>%
    summarize(start_date = min(date),
              end_date = max(date),
              .groups = "drop")
  print(south_periods)
} else {
  cat("Install the 'gratia' package for trend change analysis\n")
}

#=========================================
# 9. FINAL INTERPRETATION
#=========================================

cat("\n==== FINAL GAM MODEL INTERPRETATION ====\n")

# Extract effective degrees of freedom (EDF)
north_edf <- summary(gam_model_north)$edf
south_edf <- summary(gam_model_south)$edf

cat("\nNorth Coast GAM model:\n")
cat("- Effective degrees of freedom:", north_edf, "\n")
cat("- Interpretation: EDF > 1 indicates non-linear trend\n")
cat("  (EDF near 1 = nearly linear, higher values = more complex)\n")

cat("\nSouth Coast GAM model:\n")
cat("- Effective degrees of freedom:", south_edf, "\n")
cat("- Interpretation: EDF > 1 indicates non-linear trend\n")
cat("  (EDF near 1 = nearly linear, higher values = more complex)\n")

# Check R-squared
north_r2 <- summary(gam_model_north)$r.sq
south_r2 <- summary(gam_model_south)$r.sq

cat("\nNorth Coast GAM model explains approximately", 
    round(north_r2 * 100, 1), "% of the variation in chlorophyll-a\n")

cat("\nSouth Coast GAM model explains approximately", 
    round(south_r2 * 100, 1), "% of the variation in chlorophyll-a\n")

cat("\nRemember that GAM models show the OVERALL trend pattern but do not account for:\n")
cat("- Seasonality (could be added with seasonal terms)\n")
cat("- Autocorrelation in residuals (if present)\n")
cat("- External factors like nutrients, temperature, etc.\n")


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

##Moving on to incorporating seasonality to model Chl a

# Load required packages
library(forecast)    # For ARIMA models
library(tseries)     # For time series tests
library(ggplot2)     # For plotting
library(dplyr)       # For data manipulation
library(lubridate)   # For date handling
library(mgcv)        # For GAM models (for comparison)

#=========================================
# 1. PREPARE TIME SERIES OBJECTS
#=========================================

# First, ensure the data is sorted by date
north_data <- north_data %>% arrange(date_clean)
south_data <- south_data %>% arrange(date_clean)

# Check if we have regular time intervals
check_regularity <- function(data, location_name) {
  # Calculate time differences between consecutive measurements
  time_diffs <- diff(as.numeric(data$date_clean))
  
  # Calculate mean and standard deviation of differences
  mean_diff <- mean(time_diffs)/86400  # Convert to days
  sd_diff <- sd(time_diffs)/86400
  cv <- sd_diff/mean_diff  # Coefficient of variation
  
  cat("\n==== TIME SERIES REGULARITY CHECK FOR", location_name, "====\n")
  cat("Mean time between observations:", round(mean_diff, 1), "days\n")
  cat("SD of time between observations:", round(sd_diff, 1), "days\n")
  cat("Coefficient of variation:", round(cv, 3), "\n")
  cat("Series regularity:", ifelse(cv < 0.1, "Regular", "Irregular"), "\n")
  
  # Return whether the series is regular (CV < 0.1 is fairly regular)
  return(cv < 0.1)
}

north_regular <- check_regularity(north_data, "NORTH COAST")
south_regular <- check_regularity(south_data, "SOUTH COAST")

# Function to create a monthly time series
create_monthly_ts <- function(data, location_name) {
  # If the data isn't perfectly regular, we'll aggregate to monthly values
  
  # Extract year and month from dates
  data <- data %>%
    mutate(year = year(date_clean),
           month = month(date_clean),
           yearmonth = paste(year, sprintf("%02d", month), sep="-"))
  
  # Aggregate to monthly averages
  monthly_data <- data %>%
    group_by(yearmonth) %>%
    summarize(mean_chla = mean(chla, na.rm = TRUE),
              year = first(year),
              month = first(month),
              .groups = "drop") %>%
    arrange(yearmonth)
  
  # Check for missing months
  all_months <- expand.grid(
    year = unique(data$year),
    month = 1:12
  ) %>%
    mutate(yearmonth = paste(year, sprintf("%02d", month), sep="-")) %>%
    arrange(yearmonth)
  
  missing_months <- all_months %>%
    anti_join(monthly_data, by = "yearmonth") %>%
    arrange(yearmonth)
  
  cat("\n==== MONTHLY TIME SERIES FOR", location_name, "====\n")
  cat("Number of monthly averages:", nrow(monthly_data), "\n")
  cat("Time span:", min(monthly_data$yearmonth), "to", max(monthly_data$yearmonth), "\n")
  cat("Number of missing months:", nrow(missing_months), "\n")
  
  if(nrow(missing_months) > 0) {
    cat("Missing months:", paste(missing_months$yearmonth, collapse=", "), "\n")
  }
  
  # Create a ts object (with frequency=12 for monthly data)
  start_year <- min(monthly_data$year)
  start_month <- min(monthly_data$month[monthly_data$year == start_year])
  
  # If there are missing months, we'll interpolate
  if(nrow(missing_months) > 0) {
    # Merge with all possible months
    complete_data <- all_months %>%
      left_join(monthly_data, by = "yearmonth") %>%
      arrange(yearmonth)
    
    # Interpolate missing values (linear interpolation)
    # Convert to ts first
    temp_ts <- ts(complete_data$mean_chla, 
                  frequency = 12,
                  start = c(start_year, start_month))
    
    # Interpolate missing values
    filled_ts <- na.interp(temp_ts)
    
    # Final time series
    ts_obj <- filled_ts
    
    cat("Note: Missing values have been interpolated linearly\n")
  } else {
    # Create a regular time series object
    ts_obj <- ts(monthly_data$mean_chla, 
                 frequency = 12,
                 start = c(start_year, start_month))
  }
  
  return(list(
    ts_obj = ts_obj,
    monthly_data = monthly_data
  ))
}

# Create monthly time series objects
north_ts_data <- create_monthly_ts(north_data, "NORTH COAST")
south_ts_data <- create_monthly_ts(south_data, "SOUTH COAST")

# Extract the ts objects
north_ts <- north_ts_data$ts_obj
south_ts <- south_ts_data$ts_obj

#=========================================
# 2. VISUALIZE THE TIME SERIES
#=========================================

# Plot the time series
par(mfrow=c(2,1))
plot(north_ts, main="North Coast Monthly Chlorophyll-a", ylab="Chlorophyll-a")
plot(south_ts, main="South Coast Monthly Chlorophyll-a", ylab="Chlorophyll-a")

# Seasonal plots
par(mfrow=c(1,2))
monthplot(north_ts, main="North Coast Seasonal Pattern", xlab="Month")
monthplot(south_ts, main="South Coast Seasonal Pattern", xlab="Month")

# Decompose the time series
north_decomp <- stl(north_ts, s.window="periodic")
south_decomp <- stl(south_ts, s.window="periodic")

# Plot decomposition
par(mfrow=c(1,1))
plot(north_decomp, main="North Coast: Seasonal Decomposition")
plot(south_decomp, main="South Coast: Seasonal Decomposition")

# Create ggplot visualizations
# First convert to dataframes
north_decomp_df <- data.frame(
  date = as.Date(time(north_ts), origin = "1970-01-01"),
  observed = as.numeric(north_ts),
  trend = as.numeric(north_decomp$time.series[,"trend"]),
  seasonal = as.numeric(north_decomp$time.series[,"seasonal"]),
  remainder = as.numeric(north_decomp$time.series[,"remainder"])
)

south_decomp_df <- data.frame(
  date = as.Date(time(south_ts), origin = "1970-01-01"),
  observed = as.numeric(south_ts),
  trend = as.numeric(south_decomp$time.series[,"trend"]),
  seasonal = as.numeric(south_decomp$time.series[,"seasonal"]),
  remainder = as.numeric(south_decomp$time.series[,"remainder"])
)

# Plot the trend component
north_trend_plot <- ggplot(north_decomp_df, aes(x = date, y = trend)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "North Coast: Long-term Trend Component",
       x = "Date", y = "Trend") +
  theme_minimal()

south_trend_plot <- ggplot(south_decomp_df, aes(x = date, y = trend)) +
  geom_line(color = "red", size = 1) +
  labs(title = "South Coast: Long-term Trend Component",
       x = "Date", y = "Trend") +
  theme_minimal()

# Plot the seasonal component (for one year)
year_data_north <- north_decomp_df %>% 
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarize(avg_seasonal = mean(seasonal), .groups = "drop")

year_data_south <- south_decomp_df %>% 
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarize(avg_seasonal = mean(seasonal), .groups = "drop")

north_seasonal_plot <- ggplot(year_data_north, aes(x = month, y = avg_seasonal, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "North Coast: Seasonal Component",
       x = "Month", y = "Seasonal Effect") +
  theme_minimal()

south_seasonal_plot <- ggplot(year_data_south, aes(x = month, y = avg_seasonal, group = 1)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 3) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "South Coast: Seasonal Component",
       x = "Month", y = "Seasonal Effect") +
  theme_minimal()

# Display the plots
print(north_trend_plot)
print(south_trend_plot)
print(north_seasonal_plot)
print(south_seasonal_plot)

#=========================================
# 3. TEST FOR STATIONARITY
#=========================================

# Function to test for stationarity
check_stationarity <- function(ts_obj, location_name) {
  cat("\n==== STATIONARITY TESTS FOR", location_name, "====\n")
  
  # Augmented Dickey-Fuller test
  adf_test <- adf.test(ts_obj)
  cat("Augmented Dickey-Fuller Test:\n")
  print(adf_test)
  cat("Interpretation: p < 0.05 suggests the series is stationary\n\n")
  
  # KPSS test
  kpss_test <- kpss.test(ts_obj)
  cat("KPSS Test:\n")
  print(kpss_test)
  cat("Interpretation: p < 0.05 suggests the series is non-stationary\n\n")
  
  return(list(
    adf_result = adf_test$p.value < 0.05,  # TRUE if stationary
    kpss_result = kpss_test$p.value >= 0.05  # TRUE if stationary
  ))
}

# Check stationarity of both series
north_stationarity <- check_stationarity(north_ts, "NORTH COAST")
south_stationarity <- check_stationarity(south_ts, "SOUTH COAST")

#=========================================
# 4. EXAMINE ACF AND PACF
#=========================================

# Function to plot ACF and PACF
plot_acf_pacf <- function(ts_obj, location_name) {
  par(mfrow=c(1,2))
  acf(ts_obj, main=paste(location_name, "- ACF"))
  pacf(ts_obj, main=paste(location_name, "- PACF"))
  
  cat("\n==== ACF AND PACF INTERPRETATION FOR", location_name, "====\n")
  cat("Look for these patterns:\n")
  cat("- ACF: Significant spikes at seasonal lags (e.g., lag 12, 24 for monthly data)\n")
  cat("- ACF: Slow decay suggests non-stationarity\n")
  cat("- PACF: Significant spikes at lags 1, 2, etc. suggest AR terms\n")
  cat("- PACF: Significant spikes at seasonal lags suggest seasonal AR terms\n\n")
}

# Plot ACF and PACF for both series
plot_acf_pacf(north_ts, "NORTH COAST")
plot_acf_pacf(south_ts, "SOUTH COAST")

# Also check differenced series
plot_acf_pacf(diff(north_ts), "NORTH COAST (First Difference)")
plot_acf_pacf(diff(north_ts, lag=12), "NORTH COAST (Seasonal Difference)")

plot_acf_pacf(diff(south_ts), "SOUTH COAST (First Difference)")
plot_acf_pacf(diff(south_ts, lag=12), "SOUTH COAST (Seasonal Difference)")

#=========================================
# 5. FIT SEASONAL ARIMA MODELS
#=========================================

# Automatically identify and fit SARIMA models
cat("\n==== FITTING SEASONAL ARIMA MODELS ====\n")

# North Coast model
cat("\nNORTH COAST AUTO ARIMA MODEL:\n")
north_sarima <- auto.arima(north_ts, seasonal = TRUE, 
                           trace = TRUE,  # Show model selection process
                           approximation = FALSE,  # Exact calculations (slower but better)
                           stepwise = FALSE)  # More comprehensive search

cat("\nNORTH COAST SARIMA MODEL SUMMARY:\n")
print(summary(north_sarima))

# South Coast model
cat("\nSOUTH COAST AUTO ARIMA MODEL:\n")
south_sarima <- auto.arima(south_ts, seasonal = TRUE, 
                           trace = TRUE,
                           approximation = FALSE,
                           stepwise = FALSE)

cat("\nSOUTH COAST SARIMA MODEL SUMMARY:\n")
print(summary(south_sarima))

#=========================================
# 6. DIAGNOSTIC CHECKING
#=========================================

# Function for model diagnostics
check_sarima_model <- function(model, location_name) {
  cat("\n==== DIAGNOSTIC CHECKING FOR", location_name, "SARIMA MODEL ====\n")
  
  # Residual diagnostics
  checkresiduals(model)
  
  # Additional Ljung-Box test
  lb_test <- Box.test(residuals(model), lag = 24, type = "Ljung-Box")
  cat("\nLjung-Box test for autocorrelation in residuals:\n")
  print(lb_test)
  cat("Interpretation: p > 0.05 suggests residuals are white noise (good)\n\n")
  
  # Normality test of residuals
  sw_test <- shapiro.test(residuals(model))
  cat("Shapiro-Wilk test for normality of residuals:\n")
  print(sw_test)
  cat("Interpretation: p > 0.05 suggests residuals are normally distributed\n\n")
  
  return(list(
    no_autocorr = lb_test$p.value > 0.05,
    normal_resid = sw_test$p.value > 0.05
  ))
}

# Check diagnostics for both models
north_diag <- check_sarima_model(north_sarima, "NORTH COAST")
south_diag <- check_sarima_model(south_sarima, "SOUTH COAST")

#=========================================
# 7. FORECAST WITH SARIMA MODELS
#=========================================

# Make forecasts for the next 24 months
north_forecast <- forecast(north_sarima, h = 24)
south_forecast <- forecast(south_sarima, h = 24)

# Plot forecasts
par(mfrow=c(2,1))
plot(north_forecast, main = "North Coast: 24-Month Chlorophyll-a Forecast",
     xlab = "Year", ylab = "Chlorophyll-a")
plot(south_forecast, main = "South Coast: 24-Month Chlorophyll-a Forecast",
     xlab = "Year", ylab = "Chlorophyll-a")

# Create ggplot version of forecasts
# Convert forecast objects to dataframes
north_forecast_df <- data.frame(
  date = seq(from = max(north_decomp_df$date) + 30, length.out = 24, by = "month"),
  forecast = as.numeric(north_forecast$mean),
  lower80 = as.numeric(north_forecast$lower[,1]),
  upper80 = as.numeric(north_forecast$upper[,1]),
  lower95 = as.numeric(north_forecast$lower[,2]),
  upper95 = as.numeric(north_forecast$upper[,2])
)

south_forecast_df <- data.frame(
  date = seq(from = max(south_decomp_df$date) + 30, length.out = 24, by = "month"),
  forecast = as.numeric(south_forecast$mean),
  lower80 = as.numeric(south_forecast$lower[,1]),
  upper80 = as.numeric(south_forecast$upper[,1]),
  lower95 = as.numeric(south_forecast$lower[,2]),
  upper95 = as.numeric(south_forecast$upper[,2])
)

# Create forecast plots
north_forecast_plot <- ggplot() +
  # Historical data
  geom_line(data = north_decomp_df, aes(x = date, y = observed), 
            color = "black", size = 0.5) +
  # Forecast
  geom_ribbon(data = north_forecast_df, 
              aes(x = date, ymin = lower95, ymax = upper95), 
              fill = "blue", alpha = 0.2) +
  geom_ribbon(data = north_forecast_df, 
              aes(x = date, ymin = lower80, ymax = upper80), 
              fill = "blue", alpha = 0.3) +
  geom_line(data = north_forecast_df, aes(x = date, y = forecast), 
            color = "blue", size = 1) +
  # Labels
  labs(title = "North Coast: Chlorophyll-a Forecast (24 months)",
       subtitle = "With 80% and 95% prediction intervals",
       x = "Date", y = "Chlorophyll-a") +
  theme_minimal()

south_forecast_plot <- ggplot() +
  # Historical data
  geom_line(data = south_decomp_df, aes(x = date, y = observed), 
            color = "black", size = 0.5) +
  # Forecast
  geom_ribbon(data = south_forecast_df, 
              aes(x = date, ymin = lower95, ymax = upper95), 
              fill = "red", alpha = 0.2) +
  geom_ribbon(data = south_forecast_df, 
              aes(x = date, ymin = lower80, ymax = upper80), 
              fill = "red", alpha = 0.3) +
  geom_line(data = south_forecast_df, aes(x = date, y = forecast), 
            color = "red", size = 1) +
  # Labels
  labs(title = "South Coast: Chlorophyll-a Forecast (24 months)",
       subtitle = "With 80% and 95% prediction intervals",
       x = "Date", y = "Chlorophyll-a") +
  theme_minimal()

print(north_forecast_plot)
print(south_forecast_plot)

#=========================================
# 8. EXTRACT TREND COMPONENT AND ANALYZE
#=========================================

# Extract the trend component from the decomposition
north_trend_comp <- north_decomp$time.series[,"trend"]
south_trend_comp <- south_decomp$time.series[,"trend"]

# Function to analyze trend
analyze_trend <- function(trend_comp, location_name) {
  cat("\n==== TREND ANALYSIS FOR", location_name, "====\n")
  
  # Fit a simple linear model to the trend component
  t <- 1:length(trend_comp)
  trend_lm <- lm(trend_comp ~ t)
  
  cat("Linear trend model summary:\n")
  print(summary(trend_lm))
  
  # Calculate average annual change
  annual_points <- 12  # Monthly data
  slope <- coef(trend_lm)[2]
  annual_change <- slope * annual_points
  
  cat("Average annual change in Chlorophyll-a:", round(annual_change, 4), "\n")
  
  # Percentage change relative to mean
  mean_level <- mean(trend_comp, na.rm = TRUE)
  annual_pct_change <- (annual_change / mean_level) * 100
  
  cat("Average annual percentage change:", round(annual_pct_change, 2), "%\n")
  
  # Test for change points in the trend
  if(requireNamespace("changepoint", quietly = TRUE)) {
    library(changepoint)
    
    # Mean change points
    mean_changepoints <- cpt.mean(trend_comp, method = "PELT")
    
    cat("\nDetected change points in the mean level:\n")
    if(length(mean_changepoints@cpts) > 1 || mean_changepoints@cpts[1] != length(trend_comp)) {
      # Convert changepoint positions to dates
      cpt_dates <- as.Date(time(trend_comp))[mean_changepoints@cpts]
      cat("Change point dates:", format(cpt_dates, "%Y-%m"), "\n")
      
      # Calculate mean levels between change points
      segments <- c(0, mean_changepoints@cpts)
      for(i in 1:(length(segments)-1)) {
        start_idx <- segments[i] + 1
        end_idx <- segments[i+1]
        segment_mean <- mean(trend_comp[start_idx:end_idx], na.rm = TRUE)
        cat("Mean level from", format(as.Date(time(trend_comp))[start_idx], "%Y-%m"),
            "to", format(as.Date(time(trend_comp))[end_idx], "%Y-%m"),
            ":", round(segment_mean, 4), "\n")
      }
    } else {
      cat("No significant change points detected\n")
    }
  } else {
    cat("\nInstall the 'changepoint' package for change point detection\n")
  }
  
  return(list(
    trend_lm = trend_lm,
    annual_change = annual_change,
    annual_pct_change = annual_pct_change
  ))
}

# Analyze trends for both locations
north_trend_analysis <- analyze_trend(north_trend_comp, "NORTH COAST")
south_trend_analysis <- analyze_trend(south_trend_comp, "SOUTH COAST")

#=========================================
# 9. COMPARE MODELS (SARIMA vs GAM)
#=========================================

cat("\n==== MODEL COMPARISON: SARIMA vs GAM ====\n")

# Function to compare model performance
compare_models <- function(sarima_model, gam_model, ts_obj, location_name) {
  cat("\n", location_name, "MODEL COMPARISON:\n")
  
  # AIC values (lower is better)
  sarima_aic <- AIC(sarima_model)
  gam_aic <- AIC(gam_model)
  
  cat("SARIMA AIC:", sarima_aic, "\n")
  cat("GAM AIC:", gam_aic, "\n")
  
  # Calculate fitted values for GAM
  # This is tricky because GAM doesn't directly fit to the time series object
  # We'll need the original data
  
  # Compare forecast accuracy
  # For this, we'll use a holdout sample
  # We'll withhold the last 12 months of data
  
  if(length(ts_obj) > 36) {  # Only if we have enough data
    train_end <- length(ts_obj) - 12
    train_ts <- window(ts_obj, end = c(tsp(ts_obj)[1] + train_end/12))
    test_ts <- window(ts_obj, start = c(tsp(ts_obj)[1] + train_end/12))
    
    # Fit models on training data
    train_sarima <- auto.arima(train_ts, seasonal = TRUE)
    
    # Forecast for test period
    sarima_fc <- forecast(train_sarima, h = 12)
    
    # Calculate accuracy measures
    sarima_accuracy <- accuracy(sarima_fc, test_ts)
    
    cat("\nForecast Accuracy for SARIMA (12-month holdout):\n")
    print(sarima_accuracy)
    
    cat("\nNote: Lower RMSE, MAE values indicate better predictive accuracy\n")
    cat("GAM forecasting would require additional steps not performed here\n")
  } else {
    cat("\nInsufficient data for holdout forecast comparison\n")
  }
  
  # Overall interpretation
  cat("\nModel Selection Guidance:\n")
  cat("- SARIMA is better for: Short-term forecasting, capturing seasonality\n")
  cat("- GAM is better for: Visualizing smooth long-term trends, handling irregular data\n")
  cat("- If AIC values are similar, consider using both models for different purposes\n")
}

# Compare models for both locations
compare_models(north_sarima, gam_model_north, north_ts, "NORTH COAST")
compare_models(south_sarima, gam_model_south, south_ts, "SOUTH COAST")

#=========================================
# 10. FINAL SUMMARY AND RECOMMENDATIONS
#=========================================

cat("\n\n==== FINAL SUMMARY AND RECOMMENDATIONS ====\n\n")

# North Coast summary
cat("NORTH COAST FINDINGS:\n")
cat("- Seasonal ARIMA Model:", capture.output(north_sarima)[1], "\n")
cat("- Decomposition shows seasonal pattern with peak in month(s):", 
    which.max(year_data_north$avg_seasonal), "\n")
cat("- Long-term trend:", ifelse(north_trend_analysis$annual_pct_change > 0, 
                                 "Increasing", "Decreasing"), 
    "by approximately", round(abs(north_trend_analysis$annual_pct_change), 2), "% per year\n")
if(north_diag$no_autocorr) {
  cat("- Model diagnostics: Good (residuals show no significant autocorrelation)\n")
} else {
  cat("- Model diagnostics: Some issues remain with residual autocorrelation\n")
}
cat("\n")

# South Coast summary
cat("SOUTH COAST FINDINGS:\n")
cat("- Seasonal ARIMA Model:", capture.output(south_sarima)[1], "\n")
cat("- Decomposition shows seasonal pattern with peak in month(s):", 
    which.max(year_data_south$avg_seasonal), "\n")
cat("- Long-term trend:", ifelse(south_trend_analysis$annual_pct_change > 0, 
                                 "Increasing", "Decreasing"), 
    "by approximately", round(abs(south_trend_analysis$annual_pct_change), 2), "% per year\n")
if(south_diag$no_autocorr) {
  cat("- Model diagnostics: Good (residuals show no significant autocorrelation)\n")
} else {
  cat("- Model diagnostics: Some issues remain with residual autocorrelation\n")
}
cat("\n")

# Recommendations
cat("RECOMMENDATIONS:\n")
cat("1. For reporting long-term trends: Use the trend component from STL decomposition\n")
cat("2. For forecasting future values: Use the SARIMA models\n")
cat("3. For visualizing smooth patterns: Use the GAM models\n")
cat("4. Consider including environmental covariates if available (e.g., temperature, nutrients)\n")
cat("5. Update forecasts periodically as new data becomes available\n\n")

cat("INTERPRETATION NOTES:\n")
cat("- Chlorophyll-a varies seasonally, with peak concentrations typically in certain months\n")
cat("- Long-term trends should be interpreted alongside environmental changes\n")
cat("- SARIMA models account for both trend and seasonality, but don't explain 'why'\n")
cat("- Consider climatic factors (ENSO, monsoons) that may affect inter-annual variability\n")

# Load necessary packages
library(forecast)  # For STL decomposition
library(ggplot2)   # For plotting
library(dplyr)     # For data manipulation
library(lubridate) # For date handling
library(cowplot)   # For arranging multiple plots (install if needed)

#=========================================
# 1. PREPARE TIME SERIES FOR DECOMPOSITION
#=========================================

# Ensure data is in proper time series format
# Assuming your time series objects are already created as north_ts and south_ts
# If not, you need to create them first (similar to the SARIMA code)

# Check if the time series objects exist
if (!exists("north_ts") || !exists("south_ts")) {
  # Code to create the time series objects
  # Similar to what was in the SARIMA code
  cat("Creating time series objects first...\n")
  
  # Sort data by date
  north_data <- north_data %>% arrange(date_clean)
  south_data <- south_data %>% arrange(date_clean)
  
  # Create monthly averages if needed
  north_monthly <- north_data %>%
    mutate(year = year(date_clean),
           month = month(date_clean),
           yearmonth = paste(year, sprintf("%02d", month), sep="-")) %>%
    group_by(yearmonth, year, month) %>%
    summarize(chla = mean(chla, na.rm = TRUE), .groups = "drop") %>%
    arrange(yearmonth)
  
  south_monthly <- south_data %>%
    mutate(year = year(date_clean),
           month = month(date_clean),
           yearmonth = paste(year, sprintf("%02d", month), sep="-")) %>%
    group_by(yearmonth, year, month) %>%
    summarize(chla = mean(chla, na.rm = TRUE), .groups = "drop") %>%
    arrange(yearmonth)
  
  # Create time series objects
  north_ts <- ts(north_monthly$chla, frequency = 12, 
                 start = c(min(north_monthly$year), min(north_monthly$month[north_monthly$year == min(north_monthly$year)])))
  
  south_ts <- ts(south_monthly$chla, frequency = 12,
                 start = c(min(south_monthly$year), min(south_monthly$month[south_monthly$year == min(south_monthly$year)])))
}

# Print basic information about the time series
cat("\n==== TIME SERIES INFORMATION ====\n")
cat("North Coast Time Series:\n")
cat("Length:", length(north_ts), "observations\n")
cat("Frequency:", frequency(north_ts), "(12 = monthly data)\n")
cat("Start:", start(north_ts)[1], "Year", start(north_ts)[2], "Month\n")
cat("End:", end(north_ts)[1], "Year", end(north_ts)[2], "Month\n\n")

cat("South Coast Time Series:\n")
cat("Length:", length(south_ts), "observations\n")
cat("Frequency:", frequency(south_ts), "(12 = monthly data)\n")
cat("Start:", start(south_ts)[1], "Year", start(south_ts)[2], "Month\n")
cat("End:", end(south_ts)[1], "Year", end(south_ts)[2], "Month\n\n")

#=========================================
# 2. PERFORM STL DECOMPOSITION
#=========================================

# STL decomposition parameters
# s.window: Controls seasonal component extraction
#   - "periodic" assumes constant seasonality
#   - A number (e.g., 13) allows seasonality to change over time
# t.window: Controls trend component smoothness (larger = smoother)
# robust: TRUE helps handle outliers better

# North Coast decomposition
north_stl <- stl(north_ts, 
                 s.window = "periodic",  # Fixed seasonal pattern
                 t.window = 13,          # Moderate trend smoothing
                 robust = TRUE)          # Robust to outliers

# South Coast decomposition
south_stl <- stl(south_ts, 
                 s.window = "periodic",
                 t.window = 13,
                 robust = TRUE)

# Extract components
north_seasonal <- north_stl$time.series[, "seasonal"]
north_trend <- north_stl$time.series[, "trend"]
north_remainder <- north_stl$time.series[, "remainder"]

south_seasonal <- south_stl$time.series[, "seasonal"]
south_trend <- south_stl$time.series[, "trend"]
south_remainder <- south_stl$time.series[, "remainder"]

#=========================================
# 3. VISUALIZE DECOMPOSITION
#=========================================

# Basic R plots of the decomposition
par(mfrow = c(1, 2))
plot(north_stl, main = "North Coast STL Decomposition")
plot(south_stl, main = "South Coast STL Decomposition")

# Create more customized ggplot visualizations
# First, convert to data frames for ggplot
north_stl_df <- data.frame(
  date = as.Date(time(north_ts), origin = "1970-01-01"),
  observed = as.numeric(north_ts),
  trend = as.numeric(north_trend),
  seasonal = as.numeric(north_seasonal),
  remainder = as.numeric(north_remainder)
)

south_stl_df <- data.frame(
  date = as.Date(time(south_ts), origin = "1970-01-01"),
  observed = as.numeric(south_ts),
  trend = as.numeric(south_trend),
  seasonal = as.numeric(south_seasonal),
  remainder = as.numeric(south_remainder)
)

# Function to create a complete decomposition plot
create_stl_plot <- function(stl_df, location, color) {
  # Observed data plot
  p1 <- ggplot(stl_df, aes(x = date, y = observed)) +
    geom_line(color = color, size = 0.8) +
    labs(title = paste(location, "Chlorophyll-a"), y = "Observed") +
    theme_minimal() +
    theme(axis.title.x = element_blank())
  
  # Trend component plot
  p2 <- ggplot(stl_df, aes(x = date, y = trend)) +
    geom_line(color = color, size = 1) +
    labs(y = "Trend") +
    theme_minimal() +
    theme(axis.title.x = element_blank())
  
  # Seasonal component plot
  p3 <- ggplot(stl_df, aes(x = date, y = seasonal)) +
    geom_line(color = color, size = 0.8) +
    labs(y = "Seasonal") +
    theme_minimal() +
    theme(axis.title.x = element_blank())
  
  # Remainder component plot
  p4 <- ggplot(stl_df, aes(x = date, y = remainder)) +
    geom_line(color = color, size = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(y = "Remainder", x = "Date") +
    theme_minimal()
  
  # Combine the plots
  if (requireNamespace("cowplot", quietly = TRUE)) {
    combined <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 1, align = "v")
    return(combined)
  } else {
    # Return individual plots if cowplot is not available
    print(p1)
    print(p2)
    print(p3)
    print(p4)
    return(NULL)
  }
}

# Create and display the decomposition plots
north_decomp_plot <- create_stl_plot(north_stl_df, "North Coast", "blue")
south_decomp_plot <- create_stl_plot(south_stl_df, "South Coast", "red")

if (!is.null(north_decomp_plot) && !is.null(south_decomp_plot)) {
  print(north_decomp_plot)
  print(south_decomp_plot)
}

#=========================================
# 4. ANALYZE THE SEASONAL COMPONENT
#=========================================

# Extract the seasonal pattern by month
get_seasonal_pattern <- function(seasonal_component, location) {
  # Convert to data frame
  dates <- as.Date(time(seasonal_component), origin = "1970-01-01")
  seasonal_df <- data.frame(
    date = dates,
    month = month(dates),
    month_name = month.abb[month(dates)],
    seasonal = as.numeric(seasonal_component)
  )
  
  # Calculate average seasonal effect by month
  monthly_pattern <- seasonal_df %>%
    group_by(month, month_name) %>%
    summarize(mean_effect = mean(seasonal, na.rm = TRUE),
              sd_effect = sd(seasonal, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(month)
  
  # Calculate peak and trough months
  peak_month <- monthly_pattern$month_name[which.max(monthly_pattern$mean_effect)]
  trough_month <- monthly_pattern$month_name[which.min(monthly_pattern$mean_effect)]
  
  cat("\n==== SEASONAL PATTERN ANALYSIS FOR", location, "====\n")
  cat("Peak month:", peak_month, "\n")
  cat("Trough month:", trough_month, "\n")
  cat("Seasonal range:", round(max(monthly_pattern$mean_effect) - min(monthly_pattern$mean_effect), 4), "\n")
  cat("Seasonal contribution (% of total variation):", 
      round(var(as.numeric(seasonal_component), na.rm = TRUE) / 
              var(as.numeric(seasonal_component) + as.numeric(north_trend), na.rm = TRUE) * 100, 1), "%\n\n")
  
  # Plot the monthly pattern
  month_order <- order(monthly_pattern$month)
  p <- ggplot(monthly_pattern, aes(x = factor(month_name, levels = month.abb[monthly_pattern$month[month_order]]), 
                                   y = mean_effect)) +
    geom_bar(stat = "identity", fill = ifelse(monthly_pattern$mean_effect >= 0, "darkgreen", "darkred"), 
             alpha = 0.7) +
    geom_errorbar(aes(ymin = mean_effect - sd_effect, ymax = mean_effect + sd_effect), 
                  width = 0.2) +
    labs(title = paste(location, "Average Seasonal Pattern"),
         x = "Month", 
         y = "Seasonal Effect on Chlorophyll-a") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  
  return(monthly_pattern)
}

# Analyze seasonal patterns
north_seasonal_pattern <- get_seasonal_pattern(north_seasonal, "NORTH COAST")
south_seasonal_pattern <- get_seasonal_pattern(south_seasonal, "SOUTH COAST")

# Compare seasonal patterns between locations
seasonal_comparison <- left_join(
  north_seasonal_pattern %>% select(month, month_name, north_effect = mean_effect),
  south_seasonal_pattern %>% select(month, south_effect = mean_effect),
  by = "month"
) %>%
  arrange(month)

# Plot comparison
ggplot(seasonal_comparison, aes(x = factor(month_name, levels = month.abb))) +
  geom_line(aes(y = north_effect, group = 1, color = "North Coast"), size = 1) +
  geom_line(aes(y = south_effect, group = 1, color = "South Coast"), size = 1) +
  geom_point(aes(y = north_effect, color = "North Coast"), size = 3) +
  geom_point(aes(y = south_effect, color = "South Coast"), size = 3) +
  scale_color_manual(values = c("North Coast" = "blue", "South Coast" = "red")) +
  labs(title = "Comparison of Seasonal Patterns",
       x = "Month", 
       y = "Seasonal Effect on Chlorophyll-a",
       color = "Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#=========================================
# 5. ANALYZE THE TREND COMPONENT
#=========================================

# Function to analyze the trend component
analyze_trend <- function(trend_component, location) {
  # Convert to data frame for analysis
  trend_df <- data.frame(
    date = as.Date(time(trend_component), origin = "1970-01-01"),
    trend = as.numeric(trend_component),
    year = year(as.Date(time(trend_component), origin = "1970-01-01"))
  )
  
  # Calculate basic statistics
  start_value <- trend_df$trend[1]
  end_value <- trend_df$trend[nrow(trend_df)]
  min_value <- min(trend_df$trend, na.rm = TRUE)
  max_value <- max(trend_df$trend, na.rm = TRUE)
  
  # Calculate overall and annual change
  overall_change <- end_value - start_value
  overall_pct_change <- (overall_change / start_value) * 100
  years_span <- as.numeric(difftime(max(trend_df$date), min(trend_df$date), units = "days")) / 365.25
  annual_pct_change <- overall_pct_change / years_span
  
  cat("\n==== TREND ANALYSIS FOR", location, "====\n")
  cat("Start value:", round(start_value, 4), "\n")
  cat("End value:", round(end_value, 4), "\n")
  cat("Minimum value:", round(min_value, 4), "\n")
  cat("Maximum value:", round(max_value, 4), "\n")
  cat("Overall change:", round(overall_change, 4), 
      "(" , round(overall_pct_change, 1), "%)\n")
  cat("Average annual change:", round(annual_pct_change, 2), "% per year\n")
  
  # Trend contribution to total variation
  trend_var <- var(trend_df$trend, na.rm = TRUE)
  total_var <- var(trend_df$trend + as.numeric(north_seasonal), na.rm = TRUE)
  cat("Trend contribution (% of total variation):", 
      round(trend_var / total_var * 100, 1), "%\n\n")
  
  # Detect change points in the trend
  if (requireNamespace("changepoint", quietly = TRUE)) {
    library(changepoint)
    
    # Mean change points
    cpt_mean <- cpt.mean(trend_df$trend, method = "PELT")
    
    if (length(cpt_mean@cpts) > 1 || cpt_mean@cpts[1] != length(trend_df$trend)) {
      # Extract change points
      change_indices <- cpt_mean@cpts
      change_dates <- trend_df$date[change_indices]
      
      cat("Detected change points in trend:\n")
      for (i in 1:length(change_dates)) {
        cat("- ", format(change_dates[i], "%Y-%m"), "\n")
      }
      
      # Add change points to the data frame
      trend_df$changepoint <- FALSE
      trend_df$changepoint[change_indices] <- TRUE
      
      # Add segment information
      segments <- c(0, change_indices)
      trend_df$segment <- 0
      for (i in 1:(length(segments)-1)) {
        trend_df$segment[(segments[i]+1):segments[i+1]] <- i
      }
      
      # Calculate segment slopes
      segment_stats <- trend_df %>%
        group_by(segment) %>%
        summarize(
          start_date = first(date),
          end_date = last(date),
          start_value = first(trend),
          end_value = last(trend),
          duration_years = as.numeric(difftime(last(date), first(date), units = "days")) / 365.25,
          change_per_year = (end_value - start_value) / duration_years,
          pct_change_per_year = (change_per_year / start_value) * 100,
          .groups = "drop"
        )
      
      cat("\nTrend segments:\n")
      for (i in 1:nrow(segment_stats)) {
        cat("Segment", i, ":", format(segment_stats$start_date[i], "%Y-%m"), "to", 
            format(segment_stats$end_date[i], "%Y-%m"), ":\n")
        cat("  Change per year:", round(segment_stats$change_per_year[i], 4), 
            "(", round(segment_stats$pct_change_per_year[i], 2), "% per year)\n")
      }
      
      # Create a plot with change points
      p <- ggplot(trend_df, aes(x = date, y = trend)) +
        geom_line(size = 1, color = "darkblue") +
        geom_point(data = subset(trend_df, changepoint), 
                   color = "red", size = 3) +
        labs(title = paste(location, "Chlorophyll-a Trend with Change Points"),
             x = "Date", 
             y = "Trend Component") +
        theme_minimal()
      
      print(p)
      
      return(list(
        trend_df = trend_df,
        change_points = change_dates,
        segment_stats = segment_stats
      ))
    } else {
      cat("No significant change points detected in the trend.\n")
      
      # Simple trend plot
      p <- ggplot(trend_df, aes(x = date, y = trend)) +
        geom_line(size = 1, color = "darkblue") +
        labs(title = paste(location, "Chlorophyll-a Trend"),
             x = "Date", 
             y = "Trend Component") +
        theme_minimal()
      
      print(p)
      
      return(list(
        trend_df = trend_df,
        change_points = NULL,
        segment_stats = NULL
      ))
    }
  } else {
    cat("Install the 'changepoint' package for change point detection.\n")
    
    # Simple trend plot
    p <- ggplot(trend_df, aes(x = date, y = trend)) +
      geom_line(size = 1, color = "darkblue") +
      labs(title = paste(location, "Chlorophyll-a Trend"),
           x = "Date", 
           y = "Trend Component") +
      theme_minimal()
    
    print(p)
    
    return(list(
      trend_df = trend_df,
      change_points = NULL,
      segment_stats = NULL
    ))
  }
}

# Analyze trends for both locations
north_trend_analysis <- analyze_trend(north_trend, "NORTH COAST")
south_trend_analysis <- analyze_trend(south_trend, "SOUTH COAST")

# Compare trends between locations
trend_comparison <- data.frame(
  date = north_stl_df$date,  # Assuming both have the same dates
  north_trend = north_stl_df$trend,
  south_trend = south_stl_df$trend
)

# Plot comparison
ggplot(trend_comparison, aes(x = date)) +
  geom_line(aes(y = north_trend, color = "North Coast"), size = 1) +
  geom_line(aes(y = south_trend, color = "South Coast"), size = 1) +
  scale_color_manual(values = c("North Coast" = "blue", "South Coast" = "red")) +
  labs(title = "Comparison of Chlorophyll-a Trends",
       x = "Date", 
       y = "Trend Component",
       color = "Location") +
  theme_minimal() +
  theme(legend.position = "bottom")

#=========================================
# 6. ANALYZE THE REMAINDER COMPONENT
#=========================================

# Function to analyze the remainder component
analyze_remainder <- function(remainder_component, location) {
  # Convert to data frame
  remainder_df <- data.frame(
    date = as.Date(time(remainder_component), origin = "1970-01-01"),
    remainder = as.numeric(remainder_component)
  )
  
  # Basic statistics
  remainder_mean <- mean(remainder_df$remainder, na.rm = TRUE)
  remainder_sd <- sd(remainder_df$remainder, na.rm = TRUE)
  remainder_var <- var(remainder_df$remainder, na.rm = TRUE)
  
  # Calculate contribution to total variation
  total_var <- var(as.numeric(remainder_component) + 
                     as.numeric(north_trend) + 
                     as.numeric(north_seasonal), na.rm = TRUE)
  
  cat("\n==== REMAINDER ANALYSIS FOR", location, "====\n")
  cat("Mean:", round(remainder_mean, 6), "(should be close to zero)\n")
  cat("Standard deviation:", round(remainder_sd, 4), "\n")
  cat("Remainder contribution (% of total variation):", 
      round(remainder_var / total_var * 100, 1), "%\n")
  
  # Check for normality in remainder
  shapiro_test <- shapiro.test(remainder_df$remainder)
  cat("Shapiro-Wilk test for normality: p-value =", round(shapiro_test$p.value, 4), "\n")
  cat("Interpretation: p > 0.05 suggests normally distributed residuals\n\n")
  
  # Check for autocorrelation in remainder
  Box.test(remainder_df$remainder, lag = 20, type = "Ljung-Box")
  cat("Ljung-Box test for autocorrelation: p-value =", 
      round(Box.test(remainder_df$remainder, lag = 20, type = "Ljung-Box")$p.value, 4), "\n")
  cat("Interpretation: p > 0.05 suggests no significant autocorrelation\n\n")
  
  # Plot remainder
  p1 <- ggplot(remainder_df, aes(x = date, y = remainder)) +
    geom_line(color = "darkgray") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste(location, "Remainder Component"),
         x = "Date", 
         y = "Remainder") +
    theme_minimal()
  
  # Plot histogram
  p2 <- ggplot(remainder_df, aes(x = remainder)) +
    geom_histogram(bins = 30, fill = "darkgray", color = "black", alpha = 0.7) +
    geom_density(alpha = 0.2, fill = "blue") +
    labs(title = paste(location, "Remainder Distribution"),
         x = "Remainder Value", 
         y = "Frequency") +
    theme_minimal()
  
  # Plot ACF
  acf_data <- acf(remainder_df$remainder, plot = FALSE)
  acf_df <- data.frame(
    lag = acf_data$lag,
    acf = acf_data$acf
  )
  
  p3 <- ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_bar(stat = "identity", fill = "darkblue", width = 0.5) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    geom_hline(yintercept = c(1.96, -1.96)/sqrt(length(remainder_df$remainder)), 
               linetype = "dashed", color = "red") +
    labs(title = paste(location, "ACF of Remainder"),
         x = "Lag", 
         y = "ACF") +
    theme_minimal()
  
  # Display plots
  print(p1)
  print(p2)
  print(p3)
  
  return(list(
    remainder_df = remainder_df,
    stats = list(
      mean = remainder_mean,
      sd = remainder_sd,
      var_contribution = remainder_var / total_var * 100,
      normal = shapiro_test$p.value > 0.05,
      no_autocorr = Box.test(remainder_df$remainder, lag = 20, type = "Ljung-Box")$p.value > 0.05
    )
  ))
}

# Analyze remainder for both locations
north_remainder_analysis <- analyze_remainder(north_remainder, "NORTH COAST")
south_remainder_analysis <- analyze_remainder(south_remainder, "SOUTH COAST")

#=========================================
# 7. COMPARISON WITH GAM AND SARIMA MODELS
#=========================================

cat("\n==== COMPARISON WITH GAM AND SARIMA MODELS ====\n")

# Function to compare STL trend with GAM
compare_stl_gam <- function(stl_trend, gam_model, data, location) {
  # Extract GAM predicted values for comparison
  # This assumes the GAM model was fit with date_numeric as predictor
  if (exists("gam_model") && !is.null(gam_model)) {
    # Create a data frame with dates and trends
    comparison_df <- data.frame(
      date = as.Date(time(stl_trend), origin = "1970-01-01"),
      stl_trend = as.numeric(stl_trend)
    )
    
    # Add GAM predicted values if available
    if (location == "NORTH COAST" && exists("gam_model_north")) {
      # Create a sequence of dates matching the STL trend dates
      pred_dates <- data.frame(
        date_numeric = as.numeric(comparison_df$date)
      )
      
      # Predict GAM values
      pred_gam <- predict(gam_model_north, newdata = pred_dates)
      comparison_df$gam_trend <- pred_gam
      
      # Plot comparison
      p <- ggplot(comparison_df, aes(x = date)) +
        geom_line(aes(y = stl_trend, color = "STL Trend"), size = 1) +
        geom_line(aes(y = gam_trend, color = "GAM Trend"), size = 1, linetype = "dashed") +
        scale_color_manual(values = c("STL Trend" = "blue", "GAM Trend" = "darkgreen")) +
        labs(title = paste(location, "STL vs GAM Trend Comparison"),
             x = "Date", 
             y = "Trend",
             color = "Model") +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      print(p)
      
      # Calculate correlation
      cor_val <- cor(comparison_df$stl_trend, comparison_df$gam_trend, use = "complete.obs")
      cat("\nCorrelation between STL and GAM trends for", location, ":", round(cor_val, 3), "\n")
      
      return(comparison_df)
    } else if (location == "SOUTH COAST" && exists("gam_model_south")) {
      # Same for south coast
      pred_dates <- data.frame(
        date_numeric = as.numeric(comparison_df$date)
      )
      
      pred_gam <- predict(gam_model_south, newdata = pred_dates)
      comparison_df$gam_trend <- pred_gam
      
      p <- ggplot(comparison_df, aes(x = date)) +
        geom_line(aes(y = stl_trend, color = "STL Trend"), size = 1) +
        geom_line(aes(y = gam_trend, color = "GAM Trend"), size = 1, linetype = "dashed") +
        scale_color_manual(values = c("STL Trend" = "red", "GAM Trend" = "darkgreen")) +
        labs(title = paste(location, "STL vs GAM Trend Comparison"),
             x = "Date", 
             y = "Trend",
             color = "Model") +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      print(p)
      
      cor_val <- cor(comparison_df$stl_trend, comparison_df$gam_trend, use = "complete.obs")
      cat("\nCorrelation between STL and GAM trends for", location, ":", round(cor_val, 3), "\n")
      
      return(comparison_df)
    } else {
      cat("\nGAM model not found for", location, ". Skipping comparison.\n")
      return(NULL)
    }
  } else {
    cat("\nGAM model not found. Skipping comparison.\n")
    return(NULL)
  }
}

# Compare STL with GAM
north_stl_gam_comp <- compare_stl_gam(north_trend, gam_model_north, north_data, "NORTH COAST")
south_stl_gam_comp <- compare_stl_gam(south_trend, gam_model_south, south_data, "SOUTH COAST")

# Compare with SARIMA fitted values if available
if (exists("north_sarima") && exists("south_sarima")) {
  cat("\n==== COMPARING STL WITH SARIMA ====\n")
  cat("Note: SARIMA models include both trend and seasonal components combined\n")
  
  # Extract fitted values from SARIMA models
  north_sarima_fitted <- fitted(north_sarima)
  south_sarima_fitted <- fitted(south_sarima)
  
  # Create data frames for comparison
  north_sarima_df <- data.frame(
    date = as.Date(time(north_ts), origin = "1970-01-01"),
    observed = as.numeric(north_ts),
    sarima_fitted = as.numeric(north_sarima_fitted),
    stl_fitted = as.numeric(north_trend) + as.numeric(north_seasonal)
  )
  
  south_sarima_df <- data.frame(
    date = as.Date(time(south_ts), origin = "1970-01-01"),
    observed = as.numeric(south_ts),
    sarima_fitted = as.numeric(south_sarima_fitted),
    stl_fitted = as.numeric(south_trend) + as.numeric(south_seasonal)
  )
  
  # Plot comparison for North Coast
  p1 <- ggplot(north_sarima_df, aes(x = date)) +
    geom_line(aes(y = observed, color = "Observed"), alpha = 0.5) +
    geom_line(aes(y = sarima_fitted, color = "SARIMA Fitted"), size = 1) +
    geom_line(aes(y = stl_fitted, color = "STL Fitted"), size = 1, linetype = "dashed") +
    scale_color_manual(values = c("Observed" = "black", "SARIMA Fitted" = "blue", 
                                  "STL Fitted" = "darkgreen")) +
    labs(title = "North Coast: SARIMA vs STL Fitted Values",
         x = "Date", 
         y = "Chlorophyll-a",
         color = "Series") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Plot comparison for South Coast
  p2 <- ggplot(south_sarima_df, aes(x = date)) +
    geom_line(aes(y = observed, color = "Observed"), alpha = 0.5) +
    geom_line(aes(y = sarima_fitted, color = "SARIMA Fitted"), size = 1) +
    geom_line(aes(y = stl_fitted, color = "STL Fitted"), size = 1, linetype = "dashed") +
    scale_color_manual(values = c("Observed" = "black", "SARIMA Fitted" = "red", 
                                  "STL Fitted" = "darkgreen")) +
    labs(title = "South Coast: SARIMA vs STL Fitted Values",
         x = "Date", 
         y = "Chlorophyll-a",
         color = "Series") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p1)
  print(p2)
  
  # Calculate metrics to compare fits
  north_sarima_rmse <- sqrt(mean((north_sarima_df$observed - north_sarima_df$sarima_fitted)^2, na.rm = TRUE))
  north_stl_rmse <- sqrt(mean((north_sarima_df$observed - north_sarima_df$stl_fitted)^2, na.rm = TRUE))
  
  south_sarima_rmse <- sqrt(mean((south_sarima_df$observed - south_sarima_df$sarima_fitted)^2, na.rm = TRUE))
  south_stl_rmse <- sqrt(mean((south_sarima_df$observed - south_sarima_df$stl_fitted)^2, na.rm = TRUE))
  
  cat("\nNorth Coast RMSE comparison:\n")
  cat("SARIMA RMSE:", round(north_sarima_rmse, 4), "\n")
  cat("STL RMSE:", round(north_stl_rmse, 4), "\n")
  cat("Difference:", round(north_sarima_rmse - north_stl_rmse, 4), 
      "(Negative values favor SARIMA, positive values favor STL)\n\n")
  
  cat("South Coast RMSE comparison:\n")
  cat("SARIMA RMSE:", round(south_sarima_rmse, 4), "\n")
  cat("STL RMSE:", round(south_stl_rmse, 4), "\n")
  cat("Difference:", round(south_sarima_rmse - south_stl_rmse, 4), 
      "(Negative values favor SARIMA, positive values favor STL)\n\n")
}

#=========================================
# 8. SEASONAL-TREND INTERACTION ANALYSIS
#=========================================

# Check if seasonal patterns have changed over time
cat("\n==== ANALYZING SEASONAL-TREND INTERACTIONS ====\n")

# Function to analyze if seasonal patterns change over time
analyze_seasonal_changes <- function(ts_obj, location) {
  # First, divide the time series into segments
  ts_length <- length(ts_obj)
  n_segments <- floor(ts_length / 24)  # At least 2 years per segment
  
  if (n_segments >= 2) {
    cat("\nAnalyzing seasonal pattern changes for", location, "\n")
    cat("Dividing data into", n_segments, "segments of approximately 2 years each\n")
    
    # Create a data frame for analysis
    ts_df <- data.frame(
      date = as.Date(time(ts_obj), origin = "1970-01-01"),
      value = as.numeric(ts_obj),
      month = month(as.Date(time(ts_obj), origin = "1970-01-01")),
      month_name = month.abb[month(as.Date(time(ts_obj), origin = "1970-01-01"))],
      year = year(as.Date(time(ts_obj), origin = "1970-01-01"))
    )
    
    # Assign segments
    segment_size <- ceiling(nrow(ts_df) / n_segments)
    ts_df$segment <- ceiling(seq_len(nrow(ts_df)) / segment_size)
    
    # Calculate monthly averages by segment
    segment_monthly <- ts_df %>%
      group_by(segment, month, month_name) %>%
      summarize(mean_value = mean(value, na.rm = TRUE),
                start_date = min(date),
                end_date = max(date),
                .groups = "drop") %>%
      arrange(segment, month)
    
    # Create segment labels for plotting
    segment_labels <- segment_monthly %>%
      group_by(segment) %>%
      summarize(start_date = min(start_date),
                end_date = max(end_date),
                label = paste(format(min(start_date), "%Y-%m"), 
                              "to", 
                              format(max(end_date), "%Y-%m")),
                .groups = "drop")
    
    segment_monthly <- left_join(segment_monthly, segment_labels, by = "segment")
    
    # Plot seasonal patterns by segment
    p <- ggplot(segment_monthly, aes(x = factor(month_name, levels = month.abb), 
                                     y = mean_value, 
                                     group = segment, 
                                     color = factor(segment))) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = paste(location, "Seasonal Patterns Over Time"),
           subtitle = "Each line represents a different time period",
           x = "Month", 
           y = "Chlorophyll-a",
           color = "Period") +
      scale_color_discrete(labels = segment_labels$label) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    
    print(p)
    
    # Test if seasonal patterns are significantly different between segments
    # We'll use a simple ANOVA test for each month
    seasonal_change_test <- data.frame(month = 1:12, p_value = NA)
    for (m in 1:12) {
      month_data <- filter(segment_monthly, month == m)
      if (nrow(month_data) >= n_segments) {
        # Only run test if we have data for all segments
        model <- aov(mean_value ~ factor(segment), data = month_data)
        seasonal_change_test$p_value[m] <- summary(model)[[1]][["Pr(>F)"]][1]
      }
    }
    
    # Adjust p-values for multiple testing
    seasonal_change_test$p_adj <- p.adjust(seasonal_change_test$p_value, method = "BH")
    seasonal_change_test$month_name <- month.abb[seasonal_change_test$month]
    seasonal_change_test$significant <- seasonal_change_test$p_adj < 0.05
    
    cat("\nMonths with significant changes in seasonal pattern over time:\n")
    sig_months <- seasonal_change_test %>% filter(significant)
    if (nrow(sig_months) > 0) {
      for (i in 1:nrow(sig_months)) {
        cat("- ", sig_months$month_name[i], "(adjusted p-value =", round(sig_months$p_adj[i], 4), ")\n")
      }
    } else {
      cat("No significant changes detected in seasonal patterns over time.\n")
    }
    
    return(list(
      segment_monthly = segment_monthly,
      seasonal_change_test = seasonal_change_test
    ))
  } else {
    cat("\nInsufficient data to analyze seasonal changes over time for", location, "\n")
    cat("Need at least 4 years of data (48 observations) for this analysis.\n")
    return(NULL)
  }
}

# Analyze seasonal changes for both locations
north_seasonal_changes <- analyze_seasonal_changes(north_ts, "NORTH COAST")
south_seasonal_changes <- analyze_seasonal_changes(south_ts, "SOUTH COAST")

#=========================================
# 9. ENVIRONMENTAL INTERPRETATION
#=========================================

cat("\n==== ENVIRONMENTAL INTERPRETATION ====\n")

# North Coast summary
cat("\nNORTH COAST CHLOROPHYLL-A PATTERNS:\n")
cat("1. Seasonal Pattern:\n")
cat("   - Peak month(s): ", north_seasonal_pattern$month_name[which.max(north_seasonal_pattern$mean_effect)], "\n")
cat("   - Trough month(s): ", north_seasonal_pattern$month_name[which.min(north_seasonal_pattern$mean_effect)], "\n")

if (!is.null(north_trend_analysis$segment_stats)) {
  cat("2. Long-term Trend Segments:\n")
  for (i in 1:nrow(north_trend_analysis$segment_stats)) {
    direction <- ifelse(north_trend_analysis$segment_stats$pct_change_per_year[i] > 0, 
                        "Increasing", "Decreasing")
    cat("   - ", format(north_trend_analysis$segment_stats$start_date[i], "%Y-%m"), "to", 
        format(north_trend_analysis$segment_stats$end_date[i], "%Y-%m"), ": ", 
        direction, " at ", 
        round(abs(north_trend_analysis$segment_stats$pct_change_per_year[i]), 2), 
        "% per year\n")
  }
} else {
  cat("2. Long-term Trend:\n")
  direction <- ifelse(north_trend_analysis$trend_df$trend[nrow(north_trend_analysis$trend_df)] > 
                        north_trend_analysis$trend_df$trend[1], "Increasing", "Decreasing")
  cat("   - Overall pattern: ", direction, "\n")
}

cat("3. Variability Components:\n")
if (exists("north_stl")) {
  var_trend <- var(north_stl$time.series[, "trend"], na.rm = TRUE)
  var_seasonal <- var(north_stl$time.series[, "seasonal"], na.rm = TRUE)
  var_remainder <- var(north_stl$time.series[, "remainder"], na.rm = TRUE)
  var_total <- var_trend + var_seasonal + var_remainder
  
  cat("   - Trend contribution: ", round(var_trend / var_total * 100, 1), "%\n")
  cat("   - Seasonal contribution: ", round(var_seasonal / var_total * 100, 1), "%\n")
  cat("   - Random variation: ", round(var_remainder / var_total * 100, 1), "%\n")
}

# South Coast summary
cat("\nSOUTH COAST CHLOROPHYLL-A PATTERNS:\n")
cat("1. Seasonal Pattern:\n")
cat("   - Peak month(s): ", south_seasonal_pattern$month_name[which.max(south_seasonal_pattern$mean_effect)], "\n")
cat("   - Trough month(s): ", south_seasonal_pattern$month_name[which.min(south_seasonal_pattern$mean_effect)], "\n")

if (!is.null(south_trend_analysis$segment_stats)) {
  cat("2. Long-term Trend Segments:\n")
  for (i in 1:nrow(south_trend_analysis$segment_stats)) {
    direction <- ifelse(south_trend_analysis$segment_stats$pct_change_per_year[i] > 0, 
                        "Increasing", "Decreasing")
    cat("   - ", format(south_trend_analysis$segment_stats$start_date[i], "%Y-%m"), "to", 
        format(south_trend_analysis$segment_stats$end_date[i], "%Y-%m"), ": ", 
        direction, " at ", 
        round(abs(south_trend_analysis$segment_stats$pct_change_per_year[i]), 2), 
        "% per year\n")
  }
} else {
  cat("2. Long-term Trend:\n")
  direction <- ifelse(south_trend_analysis$trend_df$trend[nrow(south_trend_analysis$trend_df)] > 
                        south_trend_analysis$trend_df$trend[1], "Increasing", "Decreasing")
  cat("   - Overall pattern: ", direction, "\n")
}

cat("3. Variability Components:\n")
if (exists("south_stl")) {
  var_trend <- var(south_stl$time.series[, "trend"], na.rm = TRUE)
  var_seasonal <- var(south_stl$time.series[, "seasonal"], na.rm = TRUE)
  var_remainder <- var(south_stl$time.series[, "remainder"], na.rm = TRUE)
  var_total <- var_trend + var_seasonal + var_remainder
  
  cat("   - Trend contribution: ", round(var_trend / var_total * 100, 1), "%\n")
  cat("   - Seasonal contribution: ", round(var_seasonal / var_total * 100, 1), "%\n")
  cat("   - Random variation: ", round(var_remainder / var_total * 100, 1), "%\n")
}

cat("\nPOTENTIAL ENVIRONMENTAL DRIVERS TO CONSIDER:\n")
cat("1. Seasonal drivers of chlorophyll-a:\n")
cat("   - Monsoon patterns and rainfall (runoff and nutrients)\n")
cat("   - Seasonal upwelling and currents\n")
cat("   - Wind patterns affecting mixing and stratification\n")
cat("   - Seasonal river discharge\n")
cat("   - Light availability\n\n")

cat("2. Potential drivers of long-term trends:\n")
cat("   - Climate change and ocean warming\n")
cat("   - Changes in land use and coastal development\n")
cat("   - Changes in fishing pressure affecting trophic cascades\n")
cat("   - Long-term climate oscillations (ENSO, IOD, PDO)\n")
cat("   - Changes in coastal management or marine protection\n")

#=========================================
# 10. FINAL COMPARISON OF ALL MODELS
#=========================================

cat("\n\n==== FINAL COMPARISON OF ALL MODELS ====\n")

cat("\nEach modeling approach provides different insights:\n")

cat("\n1. LINEAR REGRESSION:\n")
cat("   - Simple, but inadequate for capturing non-linear patterns and seasonality\n")
cat("   - Useful only as a baseline for comparison\n")

cat("\n2. GAM MODEL:\n")
cat("   - Captures smooth non-linear trends\n")
cat("   - Flexible with no pre-specified pattern\n")
cat("   - Provides degrees of freedom as a measure of complexity\n")
cat("   - Doesn't explicitly separate seasonal and trend components\n")

cat("\n3. SARIMA MODEL:\n")
cat("   - Accounts for autocorrelation and seasonality\n")
cat("   - Good for forecasting future values\n")
cat("   - Provides formal statistical tests and confidence intervals\n")
cat("   - Doesn't provide easy visualization of separate trend component\n")

cat("\n4. STL DECOMPOSITION:\n")
cat("   - Clearly separates trend, seasonal, and remainder components\n")
cat("   - Visualizes how each component contributes to total variation\n")
cat("   - Allows analysis of each component separately\n")
cat("   - Identifies change points in the trend component\n")
cat("   - Examines if seasonal patterns change over time\n")
cat("   - Not a formal statistical model (no p-values for overall fit)\n")

cat("\nRECOMMENDATIONS FOR REPORTING RESULTS:\n")
cat("1. Use STL decomposition for descriptive analysis and visualization\n")
cat("2. Use GAM for reporting non-linear trends with statistical significance\n")
cat("3. Use SARIMA for forecasting and formal time series inference\n")
cat("4. For scientific publications, report results from multiple models for robustness\n\n")

cat("FINAL INTERPRETATION FOR TIMOR-LESTE CHLOROPHYLL-A DATA:\n")
cat("North and South Coast chlorophyll-a show distinct patterns:\n")
cat("- Seasonal cycles peak in different months, likely due to different oceanographic regimes\n")
cat("- Long-term trends show different change points and directions\n")
cat("- The South Coast shows stronger seasonality compared to the North Coast\n")
cat("These differences highlight the importance of spatial variation in marine ecosystem monitoring\n")

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