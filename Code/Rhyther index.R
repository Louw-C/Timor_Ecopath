# R script for calculating ecosystem-level indicators for Timor-Leste fisheries
# Based on Link & Watson (2019) indicators

# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

# 1. DATA IMPORT AND PROCESSING
# Import catch data
catch_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Fisheries indices/South Coast_Catch Data_Test 1.csv")

# Convert date format (assuming MM/DD/YY format from the analysis)
catch_data$date <- as.Date(catch_data$date, format = "%m/%d/%y")
catch_data$year <- year(catch_data$date)

# Summarize total annual catch
annual_catch <- catch_data %>%
  group_by(year) %>%
  summarize(total_catch_kg = sum(catch_kg, na.rm = TRUE),
            total_catch_tonnes = total_catch_kg / 1000)

# 2. AREA DEFINITION
# Define the fishing area of the south coast of Timor-Leste in km²
# NOTE: Replace this with actual area measurement of Timor-Leste south coast fishing grounds
south_coast_area_km2 <- 1000  # Example value, replace with actual area

# 3. CALCULATE RYTHER INDEX
# RI = Total catch per area per year (tonnes/km²/year)
annual_catch <- annual_catch %>%
  mutate(ryther_index = total_catch_tonnes / south_coast_area_km2)

# 7. ANALYZE RESULTS
# Print summary of indicators
print(annual_catch)

# 8. VISUALIZE RESULTS
# Plot Ryther Index against threshold
ggplot(annual_catch, aes(x = year, y = ryther_index)) +
  geom_line() +
  geom_point(size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "orange") +
  geom_hline(yintercept = 3, linetype = "dashed", color = "red") +
  labs(title = "Ryther Index for Timor-Leste South Coast",
       subtitle = "Ecosystem overfishing threshold (1 t/km²/yr) and tipping point (3 t/km²/yr)",
       y = "Catch per area (t/km²/yr)",
       x = "Year") +
  theme_minimal()
