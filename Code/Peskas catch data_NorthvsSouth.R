#-----------------------------------------------------------
##Peskas catch data from 2018 to March 2025##
##North vs South Coast#
#-----------------------------------------------------------

# Load required libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

# Read the CSV file
catch_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Peskas_Catch/timor_catch_2018_mar2025.csv", stringsAsFactors = FALSE)

# Handle dates correctly
catch_data <- catch_data %>%
  mutate(date_bin_start = dmy(date_bin_start)) %>%
  mutate(
    year = year(date_bin_start),
    month = month(date_bin_start),
    month_name = month(date_bin_start, label = TRUE, abbr = TRUE)
  )

# Add the coast column based on region
catch_data <- catch_data %>%
  mutate(coast = case_when(
    region %in% c("Baucau", "Lautem", "Manatuto", "Dili", "Liquica", "Bobonaro", "Oecusse", "Atauro") ~ "North",
    region %in% c("Viqueque", "Manufahi", "Covalima", "Ainaro") ~ "South",
    TRUE ~ "Unknown"  # Fallback for any regions not explicitly mapped
  ))

# Check the distribution of regions by coast
coast_summary <- catch_data %>%
  group_by(region, coast) %>%
  summarize(total_catch = sum(estimated_catch_kg, na.rm = TRUE)) %>%
  arrange(coast, desc(total_catch))

# View the results
print(coast_summary)

# Alternatively, if you want to see the mapping of regions to coasts
region_to_coast <- catch_data %>%
  select(region, coast) %>%
  distinct() %>%
  arrange(coast, region)

print(region_to_coast)

# Optional: Compare catch statistics between north and south coasts
coast_comparison <- catch_data %>%
  group_by(coast) %>%
  summarize(
    total_catch_kg = sum(estimated_catch_kg, na.rm = TRUE),
    percentage = total_catch_kg / sum(catch_data$estimated_catch_kg, na.rm = TRUE) * 100,
    n_observations = n()
  )

print(coast_comparison)


#-----------------------------------------------------------
##Peskas catch data from 2018 to Dec 2024##
##North vs South Coast - annual catch#
#-----------------------------------------------------------

# Filter data for years 2018-2024
catch_data_filtered <- catch_data %>%
  filter(year >= 2018 & year <= 2024)

# Calculate total annual catch by coast
annual_catch_by_coast <- catch_data_filtered %>%
  group_by(year, coast) %>%
  summarize(total_catch_kg = sum(estimated_catch_kg, na.rm = TRUE),
            total_catch_tonnes = total_catch_kg / 1000) %>%
  filter(coast != "Unknown") %>%  # Remove any unknown regions
  arrange(year, coast)

# View the results
print(annual_catch_by_coast)

# Create a summary of the total catch for each coast across all years
coast_summary <- catch_data_filtered %>%
  filter(coast != "Unknown") %>%
  group_by(coast) %>%
  summarize(total_catch_tonnes = sum(estimated_catch_kg, na.rm = TRUE) / 1000,
            percentage = total_catch_tonnes / sum(catch_data_filtered$estimated_catch_kg, na.rm = TRUE) * 100)

print(coast_summary)

# Create a bar chart to visualize the comparison
ggplot(annual_catch_by_coast, aes(x = year, y = total_catch_tonnes, fill = coast)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Annual Catch: North vs South Coast of Timor-Leste (2018-2024)",
       x = "Year",
       y = "Total Catch (tonnes)",
       fill = "Coast") +
  scale_fill_manual(values = c("North" = "steelblue", "South" = "darkgreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "bottom") +
  scale_x_continuous(breaks = 2018:2024)

# Create a line chart showing trends over time
ggplot(annual_catch_by_coast, aes(x = year, y = total_catch_tonnes, color = coast, group = coast)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Fishing Catch Trends: North vs South Coast (2018-2024)",
       x = "Year",
       y = "Total Catch (tonnes)",
       color = "Coast") +
  scale_color_manual(values = c("North" = "steelblue", "South" = "darkgreen")) +
  theme_minimal() +
  scale_x_continuous(breaks = 2018:2024)

# First make sure tidyr is loaded
library(tidyr)

# The issue is that we need to ungroup before spreading
catch_ratio <- annual_catch_by_coast %>%
  ungroup() %>%  # This is the key fix - remove grouping before spreading
  select(year, coast, total_catch_tonnes) %>%  # Only keep necessary columns
  spread(coast, total_catch_tonnes) %>%
  mutate(north_south_ratio = North / South)

print(catch_ratio)

# Calculate year-over-year growth rates for each coast
annual_growth <- annual_catch_by_coast %>%
  arrange(coast, year) %>%
  group_by(coast) %>%
  mutate(previous_year_catch = lag(total_catch_tonnes),
         growth_rate = (total_catch_tonnes - previous_year_catch) / previous_year_catch * 100) %>%
  filter(!is.na(growth_rate))

print(annual_growth)

#-----------------------------------------------------------
# Create and save comprehensive annual catch datasets
#-----------------------------------------------------------

# Create a wide format dataset with North, South, and Total catches
annual_catch_wide <- annual_catch_by_coast %>%
  ungroup() %>%
  select(year, coast, total_catch_kg) %>%
  pivot_wider(names_from = coast, 
              values_from = total_catch_kg, 
              names_prefix = "catch_kg_") %>%
  mutate(total_catch_kg = catch_kg_North + catch_kg_South,
         catch_tonnes_North = catch_kg_North / 1000,
         catch_tonnes_South = catch_kg_South / 1000,
         total_catch_tonnes = total_catch_kg / 1000,
         north_percentage = (catch_kg_North / total_catch_kg) * 100,
         south_percentage = (catch_kg_South / total_catch_kg) * 100) %>%
  arrange(year)

# Create separate CSV for South coast only
south_coast_annual <- annual_catch_by_coast %>%
  filter(coast == "South") %>%
  select(year, total_catch_kg) %>%
  rename(estimated_catch_kg = total_catch_kg) %>%
  arrange(year)

# Create separate CSV for North coast only
north_coast_annual <- annual_catch_by_coast %>%
  filter(coast == "North") %>%
  select(year, total_catch_kg) %>%
  rename(estimated_catch_kg = total_catch_kg) %>%
  arrange(year)

# Save the datasets as CSV files
# 1. Complete dataset with both coasts
write.csv(annual_catch_wide, "timor_annual_catch_by_coast.csv", row.names = FALSE)

# 2. South coast annual catch (for Fogarty analysis)
write.csv(south_coast_annual, "south_coast_annual_catch.csv", row.names = FALSE)

# 3. North coast annual catch
write.csv(north_coast_annual, "north_coast_annual_catch.csv", row.names = FALSE)

# Print a confirmation message
cat("\nCSV files created successfully:\n")
cat("1. timor_annual_catch_by_coast.csv - Complete annual catch data for both coasts\n")
cat("2. south_coast_annual_catch.csv - Annual catch data for South coast only\n")
cat("3. north_coast_annual_catch.csv - Annual catch data for North coast only\n")

# For additional analysis, create monthly data as well
monthly_catch_by_coast <- catch_data %>%
  filter(year >= 2018 & year <= 2024, coast != "Unknown") %>%
  group_by(year, month, coast) %>%
  summarize(monthly_catch_kg = sum(estimated_catch_kg, na.rm = TRUE)) %>%
  arrange(year, month, coast)

# Create a date column for easier time series analysis
monthly_catch_by_coast <- monthly_catch_by_coast %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")),
         date_bin_start = format(date, "%d/%m/%y"))

# Save monthly data (optional)
write.csv(monthly_catch_by_coast, "timor_monthly_catch_by_coast.csv", row.names = FALSE)
cat("4. timor_monthly_catch_by_coast.csv - Monthly catch data by coast\n")

# Filter just south coast monthly data for Fogarty analysis
south_coast_monthly <- monthly_catch_by_coast %>%
  filter(coast == "South") %>%
  select(date_bin_start, monthly_catch_kg) %>%
  rename(estimated_catch_kg = monthly_catch_kg)

write.csv(south_coast_monthly, "south_coast_monthly_catch.csv", row.names = FALSE)
cat("5. south_coast_monthly_catch.csv - Monthly catch data for South coast only\n")