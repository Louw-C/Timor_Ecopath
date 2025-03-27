# R script for calculating Fogarty Index for Timor-Leste fisheries (North and South coasts)
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
# STEP 4: DEFINE PARAMETERS
# =====================================================================
# Define areas for each coast in km²
# NOTE: Replace these with actual measurements of Timor-Leste coastal fishing grounds
north_coast_area_km2 <- 10000  # Example value, replace with actual area
south_coast_area_km2 <- 10000  # Example value, replace with actual area
coast_areas <- c(North = north_coast_area_km2, South = south_coast_area_km2)

# Define conversion factor for primary production
# This converts chlorophyll-a (mg/m³) to primary production (gC/m²/yr)
# Value of 65 based on Link & Watson (2019) and regional studies
conversion_factor <- 65  # gC/mgChla

# =====================================================================
# STEP 5: CALCULATE PRIMARY PRODUCTION
# =====================================================================
# Calculate annual primary production from chlorophyll
annual_chla <- annual_chla %>%
  mutate(annual_pp = annual_chla * conversion_factor)

# =====================================================================
# STEP 6: PREPARE DATA FOR FOGARTY INDEX CALCULATION
# =====================================================================
# Reshape catch data for easier joining
catch_long <- catch_data %>%
  select(year, catch_kg_North, catch_kg_South) %>%
  rename(North = catch_kg_North, South = catch_kg_South) %>%
  pivot_longer(cols = c(North, South),
               names_to = "coast",
               values_to = "annual_catch_kg")

# Join catch data with chlorophyll data
fogarty_data <- inner_join(
  catch_long,
  annual_chla %>% select(year, coast, annual_chla, annual_pp),
  by = c("year", "coast")
)

# Print the joined data
cat("\nJoined data for Fogarty Index calculation:\n")
print(fogarty_data)

# =====================================================================
# STEP 7: CALCULATE FOGARTY INDEX
# =====================================================================
# The Fogarty Index is the ratio of catches to primary production
# It expresses how much of the ecosystem's primary production is being harvested

# IMPORTANT: For proper scaling, we need total primary production for each region
# First, convert catch from kg to g for consistent units
fogarty_data <- fogarty_data %>%
  mutate(
    # Convert catch from kg to g
    annual_catch_g = annual_catch_kg * 1000,
    
    # Calculate total annual primary production (gC) for the region
    # area_km2 * 1,000,000 converts to m²
    area_km2 = ifelse(coast == "North", north_coast_area_km2, south_coast_area_km2),
    total_pp_gC = annual_pp * area_km2 * 1000000,
    
    # Calculate Fogarty Ratio (‰ - per mil)
    # Formula: (Catch in g) / (Total Primary Production in gC) * 1000
    fogarty_ratio = (annual_catch_g / total_pp_gC) * 1000
  )

# =====================================================================
# STEP 8: EVALUATE ECOSYSTEM OVERFISHING STATUS
# =====================================================================
# According to Link & Watson (2019), the threshold for EOF is ~1‰
# Values above this threshold may indicate ecosystem overfishing

fogarty_data <- fogarty_data %>%
  mutate(
    eof_status = ifelse(fogarty_ratio > 1, "Exceeds threshold", "Below threshold"),
    # Calculate how many times the threshold is exceeded
    threshold_factor = fogarty_ratio / 1
  )

# =====================================================================
# STEP 9: ANALYZE RESULTS
# =====================================================================
# Print summary of Fogarty Index
cat("\nFogarty Index Results:\n")

for(i in 1:nrow(fogarty_data)) {
  row <- fogarty_data[i,]
  cat(paste0(row$year, " - ", row$coast, 
             ": Fogarty Ratio = ", round(row$fogarty_ratio, 3), 
             " ‰ (", row$eof_status, 
             ", ", round(row$threshold_factor, 1), "× threshold)\n"))
}

# Calculate summary statistics by coast
fogarty_summary <- fogarty_data %>%
  group_by(coast) %>%
  summarize(
    years_assessed = n(),
    mean_fogarty = mean(fogarty_ratio),
    min_fogarty = min(fogarty_ratio),
    max_fogarty = max(fogarty_ratio),
    years_exceeding = sum(fogarty_ratio > 1),
    percent_exceeding = years_exceeding / years_assessed * 100,
    .groups = "drop"
  )

cat("\nSummary by coast:\n")
print(fogarty_summary)

# =====================================================================
# STEP 10: VISUALIZE RESULTS
# =====================================================================
# Plot Fogarty Index trend for both coasts
p_fogarty_trend <- ggplot(fogarty_data, aes(x = year, y = fogarty_ratio, color = coast)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("North" = "steelblue", "South" = "darkgreen")) +
  labs(title = "Fogarty Index Trend for Timor-Leste Coasts",
       subtitle = "Red dashed line indicates ecosystem overfishing threshold (~1‰)",
       y = "Fogarty Ratio (‰)",
       x = "Year",
       color = "Coast") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot comparison of factors by which threshold is exceeded
p_threshold_comparison <- ggplot(fogarty_data, aes(x = year, y = threshold_factor, fill = coast)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("North" = "steelblue", "South" = "darkgreen")) +
  labs(title = "Ecosystem Overfishing Severity",
       subtitle = "How many times the threshold (1‰) is exceeded",
       y = "Times threshold exceeded",
       x = "Year",
       fill = "Coast") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Compare ratio with primary production
p_pp_vs_fogarty <- ggplot(fogarty_data, aes(x = annual_pp, y = fogarty_ratio, color = coast)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_text(aes(label = year), hjust = -0.3, vjust = 0.3, size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("North" = "steelblue", "South" = "darkgreen")) +
  labs(title = "Fogarty Ratio vs Primary Production",
       subtitle = "Red dashed line indicates ecosystem overfishing threshold (1‰)",
       x = "Primary Production (gC/m²/yr)",
       y = "Fogarty Ratio (‰)",
       color = "Coast") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Calculate Fogarty Index model (log-log relationship) for each coast
north_model <- tryCatch({
  with(filter(fogarty_data, coast == "North"), 
       lm(log10(annual_catch_kg) ~ log10(annual_pp)))
}, error = function(e) {
  cat("Warning: Could not create North coast model:", e$message, "\n")
  NULL
})

south_model <- tryCatch({
  with(filter(fogarty_data, coast == "South"), 
       lm(log10(annual_catch_kg) ~ log10(annual_pp)))
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
fogarty_data <- fogarty_data %>%
  mutate(
    log_pp = log10(annual_pp),
    log_catch = log10(annual_catch_kg),
    predicted_log_catch = case_when(
      coast == "North" & !is.null(north_model) ~ north_coef[1] + north_coef[2] * log10(annual_pp),
      coast == "South" & !is.null(south_model) ~ south_coef[1] + south_coef[2] * log10(annual_pp),
      TRUE ~ NA_real_
    )
  )

# Plot Fogarty Index model (log-log relationship) if models exist
p_fogarty_model <- ggplot(fogarty_data, aes(x = log_pp, y = log_catch, color = coast)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_text(aes(label = year), hjust = -0.3, vjust = 0.3, size = 3) +
  scale_color_manual(values = c("North" = "steelblue", "South" = "darkgreen")) +
  labs(title = "Fogarty Index Model: log(Catch) vs log(Primary Production)",
       x = "log10(Primary Production)",
       y = "log10(Catch)",
       color = "Coast") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Add regression lines if models exist
if (!is.null(north_model)) {
  p_fogarty_model <- p_fogarty_model +
    geom_abline(intercept = north_coef[1], slope = north_coef[2], 
                color = "steelblue", linetype = "dashed")
}

if (!is.null(south_model)) {
  p_fogarty_model <- p_fogarty_model +
    geom_abline(intercept = south_coef[1], slope = south_coef[2], 
                color = "darkgreen", linetype = "dashed")
}

# Add subtitle with model equations if models exist
model_subtitle <- ""
if (!is.null(north_model)) {
  model_subtitle <- paste0("North: log10(Catch) = ", round(north_coef[1], 2), " + ", 
                           round(north_coef[2], 2), " × log10(PP), R² = ", round(north_r2, 2))
}
if (!is.null(south_model)) {
  if (model_subtitle != "") model_subtitle <- paste0(model_subtitle, "\n")
  model_subtitle <- paste0(model_subtitle, "South: log10(Catch) = ", round(south_coef[1], 2), " + ", 
                           round(south_coef[2], 2), " × log10(PP), R² = ", round(south_r2, 2))
}

if (model_subtitle != "") {
  p_fogarty_model <- p_fogarty_model + labs(subtitle = model_subtitle)
}

# Display the plots
grid.arrange(p_fogarty_trend, p_threshold_comparison, p_pp_vs_fogarty, p_fogarty_model, ncol = 2)

# =====================================================================
# STEP 11: SAVE RESULTS
# =====================================================================
# Save the Fogarty Index results to a CSV file
write.csv(fogarty_data, "fogarty_index_results.csv", row.names = FALSE)

# Save the summary data
write.csv(fogarty_summary, "fogarty_summary_by_coast.csv", row.names = FALSE)

cat("\nResults saved to 'fogarty_index_results.csv' and 'fogarty_summary_by_coast.csv'\n")

# =====================================================================
# STEP 12: FINAL ASSESSMENT
# =====================================================================
# Determine if ecosystem overfishing is occurring based on Fogarty Index
cat("\nFINAL ECOSYSTEM OVERFISHING ASSESSMENT:\n")

for(i in 1:nrow(fogarty_summary)) {
  coast_name <- fogarty_summary$coast[i]
  years_exceeding <- fogarty_summary$years_exceeding[i]
  total_years <- fogarty_summary$years_assessed[i]
  percent <- fogarty_summary$percent_exceeding[i]
  mean_ratio <- fogarty_summary$mean_fogarty[i]
  
  cat(paste0(coast_name, " Coast: ", years_exceeding, "/", total_years, 
             " years exceed threshold (", round(percent, 1), "%).\n"))
  
  if(percent > 50) {
    cat(paste0("  CONCLUSION: Evidence suggests ECOSYSTEM OVERFISHING is occurring in the ", 
               coast_name, " Coast.\n"))
  } else {
    cat(paste0("  CONCLUSION: Evidence does NOT suggest ecosystem overfishing is occurring in the ", 
               coast_name, " Coast.\n"))
  }
  
  cat(paste0("  Average Fogarty Ratio: ", round(mean_ratio, 2), 
             " ‰ (", round(mean_ratio/1, 1), "× threshold)\n\n"))
}

# =====================================================================
# END OF SCRIPT
# =====================================================================