# Load required libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# We already have the annual catch by coast data
# As seen in the catch_ratio table - see Peskas catch data_NorthvsSouth:
annual_catch_by_coast <- data.frame(
  year = c(2018, 2018, 2019, 2019, 2020, 2020, 2021, 2021, 2022, 2022, 2023, 2023, 2024, 2024),
  coast = c("North", "South", "North", "South", "North", "South", "North", "South", "North", "South", "North", "South", "North", "South"),
  total_catch_tonnes = c(5411, 560, 6437, 528, 6212, 611, 5851, 779, 6156, 648, 7499, 693, 7416, 770)
)

# Define the fishing area for north and south coasts in square kilometers
# NOTE: Replace these with the actual areas of Timor-Leste's north and south fishing grounds
north_coast_area_km2 <- 7500  # Example value
south_coast_area_km2 <- 10000  # Example value

# Create a lookup table for the areas
coast_areas <- data.frame(
  coast = c("North", "South"),
  area_km2 = c(north_coast_area_km2, south_coast_area_km2)
)

# Join area data to catch data
annual_catch_with_area <- annual_catch_by_coast %>%
  left_join(coast_areas, by = "coast")

# Calculate Ryther index for each coast and year
ryther_index_by_coast <- annual_catch_with_area %>%
  mutate(ryther_index = total_catch_tonnes / area_km2)

# Compare with thresholds from Link and Watson paper
ryther_index_by_coast <- ryther_index_by_coast %>%
  mutate(status = case_when(
    ryther_index < 0.3 ~ "Low fishing pressure",
    ryther_index >= 0.3 & ryther_index < 1 ~ "Moderate fishing pressure",
    ryther_index >= 1 & ryther_index < 3 ~ "High fishing pressure (potential EOF)",
    ryther_index >= 3 ~ "Extreme fishing pressure (likely EOF)",
    TRUE ~ "Unknown"
  ))

# Create a wide format for comparison
ryther_index_wide <- ryther_index_by_coast %>%
  select(year, coast, ryther_index) %>%
  pivot_wider(names_from = coast, values_from = ryther_index) %>%
  rename(ryther_index_north = North, ryther_index_south = South)

# Print the results
print(ryther_index_by_coast)
print(ryther_index_wide)

# Visualize the Ryther index over time by coast
ggplot(ryther_index_by_coast, aes(x = year, y = ryther_index, color = coast, group = coast)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.3, linetype = "dashed", color = "green", alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "orange", alpha = 0.7) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "red", alpha = 0.7) +
  annotate("text", x = max(ryther_index_by_coast$year), y = 0.3, label = "Low threshold (0.3)", 
           hjust = 1, vjust = -0.5, color = "green4") +
  annotate("text", x = max(ryther_index_by_coast$year), y = 1, label = "EOF threshold (1.0)", 
           hjust = 1, vjust = -0.5, color = "darkorange") +
  annotate("text", x = max(ryther_index_by_coast$year), y = 3, label = "Extreme threshold (3.0)", 
           hjust = 1, vjust = -0.5, color = "darkred") +
  labs(title = "Ryther Index for North and South Coasts of Timor-Leste (2018-2024)",
       subtitle = "Ecosystem overfishing thresholds indicated by dashed lines",
       y = "Ryther Index (t/km²/yr)",
       x = "Year",
       color = "Coast") +
  scale_color_manual(values = c("North" = "steelblue", "South" = "darkgreen")) +
  theme_minimal() +
  scale_x_continuous(breaks = 2018:2024)
