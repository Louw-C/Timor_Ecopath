# Incremental Ecological Analysis of Fisheries Data
# =============================================
# This script breaks down the analysis into clear steps,
# with each section producing viewable results.

# Step 1: Load required libraries
# ------------------------------
# Install any missing packages first if needed:
# install.packages(c("dplyr", "lubridate", "ggplot2", "tidyr"))

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# Step 2: Load your catch data
# ---------------------------
# Replace with your actual file path
file_path <- "/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Fisheries indices/South Coast_Catch Data_Biomass Indices.csv"
catch_data <- read.csv(file_path)

# View the first few rows to understand the structure
head(catch_data)

# Check summary statistics
summary(catch_data)

# Step 3: Add habitat classifications
# ---------------------------------
# Define habitat classes
demersal_species <- c("Snapper/seaperch", "Jobfish")
pelagic_species <- c("Jacks/Trevally/Other Scad", "Garfish", "Mackerel scad", 
                     "Sardines/pilchards", "Short bodied mackerel", 
                     "Tuna/Bonito/Other Mackerel", "Flying fish", "Long tom", 
                     "Moonfish", "Fusilier")

# Add habitat column
catch_data$habitat <- ifelse(catch_data$group %in% demersal_species, "Demersal",
                             ifelse(catch_data$group %in% pelagic_species, "Pelagic", "Unknown"))

# Check the new column
table(catch_data$habitat)

# Step 4: Add feeding type classifications
# ---------------------------------------
piscivorous_species <- c("Snapper/seaperch", "Tuna/Bonito/Other Mackerel", 
                         "Long tom", "Jobfish")
planktivorous_species <- c("Sardines/pilchards", "Mackerel scad", "Fusilier")
omnivorous_species <- c("Jacks/Trevally/Other Scad", "Flying fish", "Garfish", 
                        "Moonfish", "Short bodied mackerel")

# Add feeding type column
catch_data$feeding_type <- ifelse(catch_data$group %in% piscivorous_species, "Piscivorous",
                                  ifelse(catch_data$group %in% planktivorous_species, "Planktivorous",
                                         ifelse(catch_data$group %in% omnivorous_species, "Omnivorous", "Unknown")))

# Check the new column
table(catch_data$feeding_type)

# Step 5: Add trophic level information
# -----------------------------------
# Define trophic levels
trophic_levels <- c(
  "Tuna/Bonito/Other Mackerel" = 4.4,
  "Long tom" = 4.2,
  "Jobfish" = 3.9,
  "Snapper/seaperch" = 3.8,
  "Jacks/Trevally/Other Scad" = 3.7,
  "Short bodied mackerel" = 3.6,
  "Moonfish" = 3.3,
  "Garfish" = 3.2,
  "Flying fish" = 3.2,
  "Fusilier" = 2.9,
  "Mackerel scad" = 2.8,
  "Sardines/pilchards" = 2.3
)

# Add trophic level column
catch_data$trophic_level <- trophic_levels[catch_data$group]

# Check the distribution of trophic levels
summary(as.numeric(catch_data$trophic_level))
hist(as.numeric(catch_data$trophic_level), 
     main="Distribution of Trophic Levels", 
     xlab="Trophic Level", 
     breaks=10)

# Step 6: Convert and format dates
# ------------------------------
# Convert date to proper format (day/month/year)
catch_data$date <- dmy(catch_data$date)

# Create year and month columns
catch_data$year <- year(catch_data$date)
catch_data$month <- month(catch_data$date)
catch_data$year_month <- format(catch_data$date, "%Y-%m")

# Check the range of dates
range(catch_data$date)

# Step 7: Calculate basic monthly indices
# -------------------------------------
# Group by year-month and calculate indices
monthly_indices <- catch_data %>%
  group_by(year_month) %>%
  summarize(
    # Total catches
    total_catch = sum(catch_kg, na.rm = TRUE),
    
    # Catches by habitat
    pelagic_catch = sum(catch_kg[habitat == "Pelagic"], na.rm = TRUE),
    demersal_catch = sum(catch_kg[habitat == "Demersal"], na.rm = TRUE),
    
    # Catches by feeding type
    planktivorous_catch = sum(catch_kg[feeding_type == "Planktivorous"], na.rm = TRUE),
    piscivorous_catch = sum(catch_kg[feeding_type == "Piscivorous"], na.rm = TRUE),
    omnivorous_catch = sum(catch_kg[feeding_type == "Omnivorous"], na.rm = TRUE),
    
    # Calculate ratios
    pelagic_demersal_ratio = pelagic_catch / demersal_catch,
    planktivorous_piscivorous_ratio = planktivorous_catch / piscivorous_catch,
    
    # Mean trophic level (weighted by catch)
    mean_trophic_level = sum(catch_kg * as.numeric(trophic_level), na.rm = TRUE) / 
      sum(catch_kg[!is.na(trophic_level)], na.rm = TRUE)
  )

# Look at the first few rows of the monthly indices
head(monthly_indices)

# Step 8: Calculate yearly indices
# ------------------------------
yearly_indices <- catch_data %>%
  group_by(year) %>%
  summarize(
    # Total catches
    total_catch = sum(catch_kg, na.rm = TRUE),
    
    # Catches by habitat
    pelagic_catch = sum(catch_kg[habitat == "Pelagic"], na.rm = TRUE),
    demersal_catch = sum(catch_kg[habitat == "Demersal"], na.rm = TRUE),
    
    # Catches by feeding type
    planktivorous_catch = sum(catch_kg[feeding_type == "Planktivorous"], na.rm = TRUE),
    piscivorous_catch = sum(catch_kg[feeding_type == "Piscivorous"], na.rm = TRUE),
    omnivorous_catch = sum(catch_kg[feeding_type == "Omnivorous"], na.rm = TRUE),
    
    # Calculate ratios
    pelagic_demersal_ratio = pelagic_catch / demersal_catch,
    planktivorous_piscivorous_ratio = planktivorous_catch / piscivorous_catch,
    
    # Mean trophic level (weighted by catch)
    mean_trophic_level = sum(catch_kg * as.numeric(trophic_level), na.rm = TRUE) / 
      sum(catch_kg[!is.na(trophic_level)], na.rm = TRUE)
  )

# Look at the yearly indices
print(yearly_indices)

# Step 9: Visualize monthly mean trophic level
# ------------------------------------------
# Convert year_month to Date for proper time series plotting
monthly_indices$date <- as.Date(paste0(monthly_indices$year_month, "-01"))

# Plot monthly mean trophic level
ggplot(monthly_indices, aes(x = date, y = mean_trophic_level)) +
  geom_line() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(x = "Date", y = "Mean Trophic Level", 
       title = "Monthly Mean Trophic Level of Catch") +
  theme_minimal()

# Step 10: Visualize yearly mean trophic level
# ------------------------------------------
ggplot(yearly_indices, aes(x = year, y = mean_trophic_level)) +
  geom_line() +
  geom_point(size = 3) +
  labs(x = "Year", y = "Mean Trophic Level", 
       title = "Annual Mean Trophic Level of Catch") +
  theme_minimal()

# Step 11: Visualize pelagic to demersal ratio
# ------------------------------------------
ggplot(monthly_indices, aes(x = date, y = pelagic_demersal_ratio)) +
  geom_line() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(x = "Date", y = "Pelagic:Demersal Ratio", 
       title = "Monthly Pelagic to Demersal Ratio") +
  theme_minimal()

# Step 12: Visualize overall catch composition by habitat
# ----------------------------------------------------
habitat_composition <- catch_data %>%
  group_by(habitat) %>%
  summarize(total_catch = sum(catch_kg, na.rm = TRUE)) %>%
  filter(habitat != "Unknown")

ggplot(habitat_composition, aes(x = "", y = total_catch, fill = habitat)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Total Catch by Habitat", fill = "Habitat") +
  theme_void() +
  scale_fill_brewer(palette = "Set2")

# Step 13: Visualize overall catch composition by feeding type
# ---------------------------------------------------------
feeding_composition <- catch_data %>%
  group_by(feeding_type) %>%
  summarize(total_catch = sum(catch_kg, na.rm = TRUE)) %>%
  filter(feeding_type != "Unknown")

ggplot(feeding_composition, aes(x = "", y = total_catch, fill = feeding_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Total Catch by Feeding Type", fill = "Feeding Type") +
  theme_void() +
  scale_fill_brewer(palette = "Set1")

# Step 14: Create stacked area chart of catch by feeding type over time
# ------------------------------------------------------------------
feeding_time_data <- catch_data %>%
  filter(feeding_type != "Unknown") %>%
  group_by(year_month, feeding_type) %>%
  summarize(catch = sum(catch_kg, na.rm = TRUE), .groups = 'drop')

feeding_time_data$date <- as.Date(paste0(feeding_time_data$year_month, "-01"))

ggplot(feeding_time_data, aes(x = date, y = catch, fill = feeding_type)) +
  geom_area() +
  labs(x = "Date", y = "Catch (kg)", 
       title = "Catch Composition by Feeding Type Over Time",
       fill = "Feeding Type") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Step 15: Calculate Coefficient of Variation (CV) index
# ---------------------------------------------------
# Calculate CV for each species group
species_cv <- catch_data %>%
  group_by(group) %>%
  summarize(
    mean_catch = mean(catch_kg, na.rm = TRUE),
    sd_catch = sd(catch_kg, na.rm = TRUE),
    cv = sd_catch / mean_catch
  )

# Calculate system-level CV
system_data <- catch_data %>%
  group_by(date) %>%
  summarize(total_catch = sum(catch_kg, na.rm = TRUE))

system_cv <- sd(system_data$total_catch, na.rm = TRUE) / 
  mean(system_data$total_catch, na.rm = TRUE)

print(paste("System-level CV:", round(system_cv, 3)))

# Calculate proportion of species exceeding system CV
cv_summary <- species_cv %>%
  mutate(exceeds_system = cv > system_cv) %>%
  summarize(
    total_species = n(),
    species_exceeding = sum(exceeds_system, na.rm = TRUE),
    proportion_exceeding = species_exceeding / total_species
  )

print(cv_summary)

# Step 16: Visualize CV analysis
# ----------------------------
ggplot(species_cv, aes(x = reorder(group, cv), y = cv)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = system_cv, linetype = "dashed", color = "red") +
  labs(x = "Species/Group", y = "Coefficient of Variation", 
       title = "Coefficient of Variation (CV) by Species Group",
       subtitle = paste0("System CV = ", round(system_cv, 3), 
                         " | ", round(cv_summary$proportion_exceeding * 100, 1), 
                         "% of species exceed system CV")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 17: Create cumulative biomass-trophic level curve
# ----------------------------------------------------
# First, sum biomass by trophic level
trophic_biomass <- catch_data %>%
  filter(!is.na(trophic_level)) %>%
  mutate(trophic_level = as.numeric(trophic_level)) %>%
  group_by(trophic_level) %>%
  summarize(total_biomass = sum(catch_kg, na.rm = TRUE)) %>%
  arrange(trophic_level)

# Calculate cumulative biomass
trophic_biomass$cumulative_biomass <- cumsum(trophic_biomass$total_biomass)
trophic_biomass$relative_cumulative <- trophic_biomass$cumulative_biomass / sum(trophic_biomass$total_biomass)

# Find approximate inflection point (where cumulative biomass crosses 50%)
inflection_point <- trophic_biomass %>%
  filter(relative_cumulative >= 0.5) %>%
  slice(1)

print(trophic_biomass)
print(paste("Inflection point at trophic level:", round(inflection_point$trophic_level, 2)))

# Plot cumulative biomass-trophic level curve
ggplot(trophic_biomass, aes(x = trophic_level, y = relative_cumulative)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_point(data = inflection_point, color = "red", size = 4) +
  geom_text(data = inflection_point,
            aes(label = paste0("Inflection point\nTL = ", round(trophic_level, 2))),
            vjust = -1.5, hjust = 0.5, color = "red") +
  labs(x = "Trophic Level", 
       y = "Relative Cumulative Biomass", 
       title = "Cumulative Biomass-Trophic Level Curve") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# Step 18: Save enriched data for future use
# ----------------------------------------
# Save the enriched data to a new CSV file
write.csv(catch_data, "South_Coast_Catch_Data_Enriched.csv", row.names = FALSE)

# Step 19: Save key visualizations
# ------------------------------
# Save plots as needed
# ggsave("trophic_level_monthly.png", width = 10, height = 6)
# ggsave("pelagic_demersal_ratio.png", width = 10, height = 6)
# ggsave("habitat_composition.png", width = 8, height = 8)