# R script for calculating Friedland Index for Timor-Leste fisheries (North and South coasts)
# Based on Link & Watson (2019) ecosystem overfishing framework
# Using updated catch data from Peskas - annual data for each coast from 2019-2024
# Using the following Chl a data:
#SOUTH COAST: Time Series, Area-Averaged of Chlorophyll a concentration (water only) 8-daily 4 km [MODIS-Aqua MODISA_L3m_CHL_8d_4km vR2022.0] mg m-3 over 2019-01-01 00:00:00Z - 2024-12-01 23:59:59Z, Region 125E, 9.5S, 127.33E, 9.15S
#NORTH COAST: Time Series, Area-Averaged of Chlorophyll a concentration (water only) 8-daily 4 km [MODIS-Aqua MODISA_L3m_CHL_8d_4km vR2022.0] mg m-3 over 2019-01-01 00:00:00Z - 2024-12-01 23:59:59Z, Region 124.3E, 8.75S, 127.3E, 8.25S

# =====================================================================
# STEP 1: LOAD REQUIRED LIBRARIES
# =====================================================================
library(dplyr)      # For data manipulation
library(ggplot2)    # For creating plots
library(lubridate)  # For date handling
library(tidyr)      # For reshaping data
library(readr)      # For reading CSV files
library(gridExtra)  # For arranging multiple plots

# =====================================================================
# STEP 2: IMPORT DATA
# =====================================================================
# Import catch data by coast
catch_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Fisheries indices/timor_annual_catch_by_coast.csv")

# Import chlorophyll data
chla_data <- read_csv("Data/Fisheries indices/Chl a_Timor-Leste_2019-2024_Monthly Averages_North+South.csv")


# =====================================================================
# STEP 3: DATA PROCESSING
# =====================================================================
# Examine the structure of the datasets
cat("Structure of catch data:\n")
str(catch_data)

cat("\nStructure of chlorophyll data:\n")
str(chla_data)

# Parse date in chlorophyll data - specifically for format "DD/MM/YY HH:MM"
# Based on examination of the actual data format
chla_data$date <- as.Date(substr(chla_data$date, 1, 8), format = "%d/%m/%y")

# Extract year from date
chla_data$year <- year(chla_data$date)

# Calculate annual mean chlorophyll by location
annual_chla <- chla_data %>%
  filter(!is.na(year)) %>%
  group_by(year, location) %>%
  summarize(
    annual_chla = mean(chla, na.rm = TRUE),
    .groups = "drop"
  )

# Print summary of chlorophyll data
cat("\nAnnual average chlorophyll by location:\n")
print(annual_chla)

# Rename North and South locations if necessary to match catch data
annual_chla <- annual_chla %>%
  mutate(coast = case_when(
    tolower(location) %in% c("north", "north coast") ~ "North",
    tolower(location) %in% c("south", "south coast") ~ "South",
    TRUE ~ location
  ))

# =====================================================================
# STEP 4: PREPARE DATA FOR FRIEDLAND INDEX CALCULATION
# =====================================================================
# Reshape catch data for easier joining
catch_long <- catch_data %>%
  select(year, catch_kg_North, catch_kg_South) %>%
  rename(North = catch_kg_North, South = catch_kg_South) %>%
  pivot_longer(cols = c(North, South),
               names_to = "coast",
               values_to = "annual_catch_kg")

# Join catch data with chlorophyll data
friedland_data <- inner_join(
  catch_long,
  annual_chla %>% select(year, coast, annual_chla),
  by = c("year", "coast")
)

# Print the joined data
cat("\nJoined data for Friedland Index calculation:\n")
print(friedland_data)

# =====================================================================
# STEP 5: CALCULATE FRIEDLAND INDEX
# =====================================================================
# The Friedland Index is the ratio of catches to chlorophyll concentration
# Unlike the Fogarty Ratio which uses primary production, the Friedland Index
# directly relates catch to chlorophyll measurements

# Calculate Friedland Index
# Formula: Catch (kg) / (Chlorophyll (mg/m³) * 1,000,000)
friedland_data <- friedland_data %>%
  mutate(
    # Calculate Friedland Ratio 
    # Note: multiply by 1,000,000 in denominator to scale the ratio
    # This scaling makes the values more interpretable (similar to Fogarty Ratio approach)
    friedland_ratio = annual_catch_kg / (annual_chla * 1000000)
  )

# =====================================================================
# STEP 6: EVALUATE ECOSYSTEM OVERFISHING STATUS
# =====================================================================
# According to Link & Watson (2019), the threshold for EOF is ~1
# Values above this threshold may indicate ecosystem overfishing

friedland_data <- friedland_data %>%
  mutate(
    eof_status = ifelse(friedland_ratio > 1, "Exceeds threshold", "Below threshold"),
    # Calculate how many times the threshold is exceeded
    threshold_factor = friedland_ratio / 1
  )

# =====================================================================
# STEP 7: ANALYZE RESULTS
# =====================================================================
# Print summary of Friedland Index
cat("\nFriedland Index Results:\n")

for(i in 1:nrow(friedland_data)) {
  row <- friedland_data[i,]
  cat(paste0(row$year, " - ", row$coast, 
             ": Friedland Ratio = ", round(row$friedland_ratio, 3), 
             " (", row$eof_status, 
             ", ", round(row$threshold_factor, 1), "× threshold)\n"))
}

# Calculate summary statistics by coast
friedland_summary <- friedland_data %>%
  group_by(coast) %>%
  summarize(
    years_assessed = n(),
    mean_friedland = mean(friedland_ratio),
    min_friedland = min(friedland_ratio),
    max_friedland = max(friedland_ratio),
    years_exceeding = sum(friedland_ratio > 1),
    percent_exceeding = years_exceeding / years_assessed * 100,
    .groups = "drop"
  )

cat("\nSummary by coast:\n")
print(friedland_summary)

# =====================================================================
# STEP 8: VISUALIZE RESULTS
# =====================================================================
# Plot Friedland Index trend for both coasts
p_friedland_trend <- ggplot(friedland_data, aes(x = year, y = friedland_ratio, color = coast)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("North" = "steelblue", "South" = "darkgreen")) +
  labs(title = "Friedland Index Trend for Timor-Leste Coasts",
       subtitle = "Red dashed line indicates ecosystem overfishing threshold (~1)",
       y = "Friedland Ratio",
       x = "Year",
       color = "Coast") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot comparison of factors by which threshold is exceeded
p_threshold_comparison <- ggplot(friedland_data, aes(x = year, y = threshold_factor, fill = coast)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("North" = "steelblue", "South" = "darkgreen")) +
  labs(title = "Ecosystem Overfishing Severity",
       subtitle = "How many times the threshold (1) is exceeded",
       y = "Times threshold exceeded",
       x = "Year",
       fill = "Coast") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Compare ratio with chlorophyll
p_chla_vs_friedland <- ggplot(friedland_data, aes(x = annual_chla, y = friedland_ratio, color = coast)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_text(aes(label = year), hjust = -0.3, vjust = 0.3, size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("North" = "steelblue", "South" = "darkgreen")) +
  labs(title = "Friedland Ratio vs Chlorophyll Concentration",
       subtitle = "Red dashed line indicates ecosystem overfishing threshold (1)",
       x = "Chlorophyll-a (mg/m³)",
       y = "Friedland Ratio",
       color = "Coast") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Calculate Friedland Index model (log-log relationship) for each coast
north_model <- tryCatch({
  with(filter(friedland_data, coast == "North"), 
       lm(log10(annual_catch_kg) ~ log10(annual_chla)))
}, error = function(e) {
  cat("Warning: Could not create North coast model:", e$message, "\n")
  NULL
})

south_model <- tryCatch({
  with(filter(friedland_data, coast == "South"), 
       lm(log10(annual_catch_kg) ~ log10(annual_chla)))
}, error = function(e) {
  cat("Warning: Could not create South coast model:", e$message, "\n")
  NULL
})

# Extract model coefficients and R-squared if models exist
if (!is.null(north_model)) {
  north_coef <- coef(north_model)
  north_r2 <- summary(north_model)$r.squared
} else {
  north_coef <- c(0, 0)
  north_r2 <- NA
}

if (!is.null(south_model)) {
  south_coef <- coef(south_model)
  south_r2 <- summary(south_model)$r.squared
} else {
  south_coef <- c(0, 0)
  south_r2 <- NA
}

# Add model information to the dataset
friedland_data <- friedland_data %>%
  mutate(
    log_chla = log10(annual_chla),
    log_catch = log10(annual_catch_kg),
    predicted_log_catch = case_when(
      coast == "North" & !is.null(north_model) ~ north_coef[1] + north_coef[2] * log10(annual_chla),
      coast == "South" & !is.null(south_model) ~ south_coef[1] + south_coef[2] * log10(annual_chla),
      TRUE ~ NA_real_
    )
  )

# Plot Friedland Index model (log-log relationship) if models exist
p_friedland_model <- ggplot(friedland_data, aes(x = log_chla, y = log_catch, color = coast)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_text(aes(label = year), hjust = -0.3, vjust = 0.3, size = 3) +
  scale_color_manual(values = c("North" = "steelblue", "South" = "darkgreen")) +
  labs(title = "Friedland Index Model: log(Catch) vs log(Chlorophyll)",
       x = "log10(Chlorophyll-a)",
       y = "log10(Catch)",
       color = "Coast") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Add regression lines if models exist
if (!is.null(north_model)) {
  p_friedland_model <- p_friedland_model +
    geom_abline(intercept = north_coef[1], slope = north_coef[2], 
                color = "steelblue", linetype = "dashed")
}

if (!is.null(south_model)) {
  p_friedland_model <- p_friedland_model +
    geom_abline(intercept = south_coef[1], slope = south_coef[2], 
                color = "darkgreen", linetype = "dashed")
}

# Add subtitle with model equations if models exist
model_subtitle <- ""
if (!is.null(north_model)) {
  model_subtitle <- paste0("North: log10(Catch) = ", round(north_coef[1], 2), " + ", 
                           round(north_coef[2], 2), " × log10(Chla), R² = ", round(north_r2, 2))
}
if (!is.null(south_model)) {
  if (model_subtitle != "") model_subtitle <- paste0(model_subtitle, "\n")
  model_subtitle <- paste0(model_subtitle, "South: log10(Catch) = ", round(south_coef[1], 2), " + ", 
                           round(south_coef[2], 2), " × log10(Chla), R² = ", round(south_r2, 2))
}

if (model_subtitle != "") {
  p_friedland_model <- p_friedland_model + labs(subtitle = model_subtitle)
}

# Display the plots
grid.arrange(p_friedland_trend, p_threshold_comparison, p_chla_vs_friedland, p_friedland_model, ncol = 2)

# =====================================================================
# STEP 9: CALCULATE CATCH PER CHLOROPHYLL
# =====================================================================
# An alternative way to express the Friedland relationship is as
# catch per unit chlorophyll (tonnes per mg/m³)
friedland_data <- friedland_data %>%
  mutate(
    # Convert catch to tonnes for more intuitive scale
    annual_catch_tonnes = annual_catch_kg / 1000,
    
    # Calculate catch per unit chlorophyll
    catch_per_chla = annual_catch_tonnes / annual_chla
  )

# Plot catch per unit chlorophyll
p_catch_per_chla <- ggplot(friedland_data, aes(x = year, y = catch_per_chla, color = coast)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("North" = "steelblue", "South" = "darkgreen")) +
  labs(title = "Catch per Unit Chlorophyll",
       subtitle = "Another representation of the Friedland relationship",
       y = "Catch per Chlorophyll (tonnes per mg/m³)",
       x = "Year",
       color = "Coast") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display this additional plot
print(p_catch_per_chla)

# =====================================================================
# STEP 10: SAVE RESULTS
# =====================================================================
# Save the Friedland Index results to a CSV file
write.csv(friedland_data, "friedland_index_results.csv", row.names = FALSE)

# Save the summary data
write.csv(friedland_summary, "friedland_summary_by_coast.csv", row.names = FALSE)

cat("\nResults saved to 'friedland_index_results.csv' and 'friedland_summary_by_coast.csv'\n")

# =====================================================================
# STEP 11: FINAL ASSESSMENT
# =====================================================================
# Determine if ecosystem overfishing is occurring based on Friedland Index
cat("\nFINAL ECOSYSTEM OVERFISHING ASSESSMENT:\n")

for(i in 1:nrow(friedland_summary)) {
  coast_name <- friedland_summary$coast[i]
  years_exceeding <- friedland_summary$years_exceeding[i]
  total_years <- friedland_summary$years_assessed[i]
  percent <- friedland_summary$percent_exceeding[i]
  mean_ratio <- friedland_summary$mean_friedland[i]
  
  cat(paste0(coast_name, " Coast: ", years_exceeding, "/", total_years, 
             " years exceed threshold (", round(percent, 1), "%).\n"))
  
  if(percent > 50) {
    cat(paste0("  CONCLUSION: Evidence suggests ECOSYSTEM OVERFISHING is occurring in the ", 
               coast_name, " Coast.\n"))
  } else {
    cat(paste0("  CONCLUSION: Evidence does NOT suggest ecosystem overfishing is occurring in the ", 
               coast_name, " Coast.\n"))
  }
  
  cat(paste0("  Average Friedland Ratio: ", round(mean_ratio, 2), 
             " (", round(mean_ratio/1, 1), "× threshold)\n\n"))
}

# =====================================================================
# STEP 12: COMPARISON WITH OTHER REGIONS (INFORMATIONAL)
# =====================================================================
cat("\nCOMPARISON WITH OTHER REGIONS (BASED ON LITERATURE):\n")
cat("The Friedland Ratio threshold of ~1 is based on Link & Watson (2019).\n")
cat("For context, typical values in different regions include:\n")
cat("- Sustainably fished tropical regions: < 1\n")
cat("- Moderately fished coral reef ecosystems: 1-5\n")
cat("- Heavily exploited Southeast Asian waters: > 5\n")
cat("- Global average for tropical ecosystems: ~3\n")
cat("Note: These are approximate values based on published literature.\n")

# =====================================================================
# END OF SCRIPT
# =====================================================================