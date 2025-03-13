# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)

# Read in the data files
# Assuming chlorophyll data has columns: date, location, chlorophyll_a
chla_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Chl a_Timor-Leste_2019-2024_Monthly Averages_North+South.csv", stringsAsFactors = FALSE)

# Assuming catch data has columns: date, location, species, catch_kg
catch_data <- read.csv("catch_data.csv", stringsAsFactors = FALSE)

# Make sure date columns are in proper date format
chla_data$date <- as.Date(chla_data$date)
catch_data$date <- as.Date(catch_data$date)

# Ensure the locations are consistent between datasets
unique(chla_data$location)
unique(catch_data$location)

# Create year and month columns for aggregation
chla_data$year <- year(chla_data$date)
chla_data$month <- month(chla_data$date)
catch_data$year <- year(catch_data$date)
catch_data$month <- month(catch_data$date)

# Create a function to estimate primary production from chlorophyll a
# This is a simplified model - you may want to use a more sophisticated one
estimate_primary_production <- function(chla) {
  # Simple conversion factor (example only - adjust based on literature for your region)
  # Typically ranges from 30-100 gC/mgChla depending on conditions
  conversion_factor <- 65 #considering upwelling events on both the north (Kim et al. XXX) and south coast (Algongi et al. 2013)
  return(chla * conversion_factor)
}

# Calculate primary production
chla_data$primary_production <- estimate_primary_production(chla_data$chlorophyll_a)

# Aggregate to annual values by location for Fogarty Index analysis
annual_pp <- chla_data %>%
  group_by(location, year) %>%
  summarize(annual_pp = mean(primary_production, na.rm = TRUE))

annual_catch <- catch_data %>%
  group_by(location, year) %>%
  summarize(annual_catch = sum(catch_kg, na.rm = TRUE))

# Join the datasets
fogarty_data <- inner_join(annual_pp, annual_catch, by = c("location", "year"))

# Calculate Fogarty Index
# Log transform for power relationship
fogarty_data$log_pp <- log10(fogarty_data$annual_pp)
fogarty_data$log_catch <- log10(fogarty_data$annual_catch)

# Create models for each location
locations <- unique(fogarty_data$location)
models <- list()

for (loc in locations) {
  loc_data <- fogarty_data %>% filter(location == loc)
  models[[loc]] <- lm(log_catch ~ log_pp, data = loc_data)
  print(paste("Fogarty Index for", loc, ":"))
  print(summary(models[[loc]]))
}

# Visualize the relationship
ggplot(fogarty_data, aes(x = log_pp, y = log_catch, color = location)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Fogarty Index: Log(Fishery Yield) vs Log(Primary Production)",
       x = "Log(Primary Production)",
       y = "Log(Catch)") +
  theme_minimal() +
  facet_wrap(~location, scales = "free")

# Time lag analysis
# Create lagged versions of the chlorophyll data
# Examining 1-6 month lags

# First, convert to monthly data
monthly_chla <- chla_data %>%
  group_by(location, year, month) %>%
  summarize(monthly_chla = mean(chlorophyll_a, na.rm = TRUE),
            monthly_pp = mean(primary_production, na.rm = TRUE))

monthly_catch <- catch_data %>%
  group_by(location, year, month) %>%
  summarize(monthly_catch = sum(catch_kg, na.rm = TRUE))

# Create date column for easier joining
monthly_chla$date <- as.Date(paste(monthly_chla$year, monthly_chla$month, "01", sep = "-"))
monthly_catch$date <- as.Date(paste(monthly_catch$year, monthly_catch$month, "01", sep = "-"))

# Create lagged datasets
lag_correlations <- data.frame()

for (loc in locations) {
  loc_chla <- monthly_chla %>% filter(location == loc)
  loc_catch <- monthly_catch %>% filter(location == loc)
  
  for (lag in 0:6) {
    # Create lagged dataset
    loc_chla_lagged <- loc_chla
    loc_chla_lagged$lagged_date <- loc_chla_lagged$date %m+% months(lag)
    
    # Join with catch data
    lagged_data <- left_join(
      loc_catch, 
      loc_chla_lagged %>% select(location, lagged_date, monthly_pp),
      by = c("location" = "location", "date" = "lagged_date")
    )
    
    # Calculate correlation
    if (nrow(lagged_data) > 5) {  # Ensure enough data for correlation
      cor_val <- cor(lagged_data$monthly_catch, lagged_data$monthly_pp, 
                     use = "pairwise.complete.obs")
      
      # Add to results
      lag_correlations <- rbind(lag_correlations, 
                                data.frame(location = loc, 
                                           lag_months = lag, 
                                           correlation = cor_val))
    }
  }
}

# Visualize lag correlations
ggplot(lag_correlations, aes(x = lag_months, y = correlation, color = location, group = location)) +
  geom_line() +
  geom_point() +
  labs(title = "Correlation between Catch and Chlorophyll by Time Lag",
       x = "Lag (months)",
       y = "Correlation Coefficient") +
  theme_minimal()

# Create a visualization with both time series
monthly_data <- full_join(
  monthly_chla %>% select(location, date, monthly_chla),
  monthly_catch %>% select(location, date, monthly_catch),
  by = c("location", "date")
)

# Plot time series
ggplot(monthly_data, aes(x = date)) +
  geom_line(aes(y = scale(monthly_chla), color = "Chlorophyll a")) +
  geom_line(aes(y = scale(monthly_catch), color = "Catch")) +
  labs(title = "Standardized Chlorophyll a and Catch Over Time",
       y = "Standardized Value",
       color = "Measure") +
  facet_wrap(~location) +
  theme_minimal()

# Adjust for best lag time based on correlation analysis
best_lags <- lag_correlations %>%
  group_by(location) %>%
  filter(correlation == max(correlation)) %>%
  select(location, best_lag = lag_months)

print(best_lags)

# Final Fogarty Index with optimal lag
optimal_lag_models <- list()

for (i in 1:nrow(best_lags)) {
  loc <- best_lags$location[i]
  lag <- best_lags$best_lag[i]
  
  # Create optimally lagged dataset
  loc_chla <- monthly_chla %>% filter(location == loc)
  loc_catch <- monthly_catch %>% filter(location == loc)
  
  loc_chla_lagged <- loc_chla
  loc_chla_lagged$lagged_date <- loc_chla_lagged$date %m+% months(lag)
  
  # Join with catch data
  optimal_data <- left_join(
    loc_catch, 
    loc_chla_lagged %>% select(location, lagged_date, monthly_pp),
    by = c("location" = "location", "date" = "lagged_date")
  )
  
  # Aggregate to annual
  optimal_annual <- optimal_data %>%
    mutate(year = year(date)) %>%
    group_by(location, year) %>%
    summarize(
      annual_catch = sum(monthly_catch, na.rm = TRUE),
      annual_pp = mean(monthly_pp, na.rm = TRUE)
    )
  
  # Calculate Fogarty Index with optimal lag
  optimal_annual$log_pp <- log10(optimal_annual$annual_pp)
  optimal_annual$log_catch <- log10(optimal_annual$annual_catch)
  
  # Create model
  if (nrow(optimal_annual) > 3) {
    optimal_lag_models[[loc]] <- lm(log_catch ~ log_pp, data = optimal_annual)
    print(paste("Optimal lag Fogarty Index for", loc, "(lag =", lag, "months):"))
    print(summary(optimal_lag_models[[loc]]))
  }
}

# Save results
write.csv(fogarty_data, "fogarty_annual_data.csv", row.names = FALSE)
write.csv(lag_correlations, "lag_correlation_results.csv", row.names = FALSE)