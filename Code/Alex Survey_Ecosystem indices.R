# Ecological Indices Analysis for Timor Biomass Survey Data
# =========================================================
# This script calculates ecological indices from survey data,
# specifically designed for the Timor biomass survey dataset.

# Step 1: Load required libraries
# ------------------------------
library(readxl)  # For reading Excel files
library(dplyr)
library(ggplot2)
library(tidyr)

# Step 2: Load the survey data
# --------------------------
# Adjust file path as needed
survey_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Timor_biomass_survey_2016_with_species.csv")

# Examine the data structure
head(survey_data)
summary(survey_data)

# Step 3: Basic data cleanup and preparation
# ----------------------------------------
# Check for any missing values in key fields
colSums(is.na(survey_data))

# Remove any rows with missing biomass values if needed
survey_data <- survey_data %>% 
  filter(!is.na(Biomass_kg))

# Step 4: Examine the functional groups
# ----------------------------------
# Check available functional groups
table(survey_data$Functional_Group)

# Verify the distribution of biomass across functional groups
func_group_biomass <- survey_data %>%
  group_by(Functional_Group) %>%
  summarize(
    Total_Biomass_kg = sum(Biomass_kg, na.rm = TRUE),
    Percent = Total_Biomass_kg / sum(survey_data$Biomass_kg, na.rm = TRUE) * 100
  ) %>%
  arrange(desc(Total_Biomass_kg))

print(func_group_biomass)

# Step 5: Examine the survey sites
# -----------------------------
# Get overview of sampling locations and sites
location_summary <- survey_data %>%
  group_by(Location, Site) %>%
  summarize(
    Transects = n_distinct(Transect),
    Total_Biomass_kg = sum(Biomass_kg, na.rm = TRUE),
    Species_Count = n_distinct(Species)
  )

print(location_summary)

# Step 6: Calculate Large Fish Index (LFI)
# --------------------------------------
# Define "large" fish (typically > 40cm for marine surveys, but can be adjusted)
large_threshold <- 40  # cm

# Calculate LFI for each site
lfi_by_site <- survey_data %>%
  group_by(Location, Site) %>%
  summarize(
    Total_Biomass_kg = sum(Biomass_kg, na.rm = TRUE),
    Large_Fish_Biomass_kg = sum(Biomass_kg[Length >= large_threshold], na.rm = TRUE),
    LFI = Large_Fish_Biomass_kg / Total_Biomass_kg
  ) %>%
  arrange(desc(LFI))

print(lfi_by_site)

# Overall LFI for the entire survey
overall_lfi <- sum(survey_data$Biomass_kg[survey_data$Length >= large_threshold], na.rm = TRUE) / 
  sum(survey_data$Biomass_kg, na.rm = TRUE)

print(paste("Overall Large Fish Index:", round(overall_lfi, 3)))

# Step 7: Calculate Size Spectra
# ---------------------------
# Group fish by size bins
bin_width <- 5  # cm
max_size <- max(survey_data$Length, na.rm = TRUE)
size_bins <- seq(0, max_size + bin_width, by = bin_width)

size_spectra_data <- survey_data %>%
  mutate(Size_Class = cut(Length, breaks = size_bins)) %>%
  group_by(Size_Class) %>%
  summarize(
    Total_Biomass_kg = sum(Biomass_kg, na.rm = TRUE),
    Number_of_Fish = sum(Number, na.rm = TRUE)
  ) %>%
  filter(!is.na(Size_Class))

# Extract bin midpoints for size spectra analysis - fixed approach
# Convert Size_Class to character
size_classes_char <- as.character(size_spectra_data$Size_Class)

# Extract the numeric values from the interval notation
midpoints <- numeric(length(size_classes_char))
for (i in 1:length(size_classes_char)) {
  # Extract numbers from the interval notation (e.g., "(10,15]")
  numbers <- as.numeric(unlist(regmatches(size_classes_char[i], 
                                          gregexpr("\\d+\\.?\\d*", size_classes_char[i]))))
  # Calculate midpoint
  midpoints[i] <- mean(numbers)
}

# Add midpoints to the dataframe
size_spectra_data$Midpoint <- midpoints

# Log transform for size spectra analysis
size_spectra_data$Log10_Size <- log10(size_spectra_data$Midpoint)
size_spectra_data$Log10_Abundance <- log10(size_spectra_data$Number_of_Fish)
size_spectra_data$Log10_Biomass <- log10(size_spectra_data$Total_Biomass_kg)

# Fit linear model to calculate size spectra slope
size_spectra_model <- lm(Log10_Abundance ~ Log10_Size, data = size_spectra_data)
spectra_slope <- coef(size_spectra_model)[2]
spectra_intercept <- coef(size_spectra_model)[1]

print(paste("Size spectra slope:", round(spectra_slope, 3)))
print(paste("Size spectra intercept:", round(spectra_intercept, 3)))


# Step 8: Calculate Biomass Ratios
# -----------------------------
# Define trophic pathways based on functional groups
pelagic_groups <- c("Planktivore")  # Add more if needed
demersal_groups <- c("Benthic", "Corallivore", "Detritivore", "Grazer", "Scraper")  # Add more if needed

# Calculate pelagic to demersal ratio
pelagic_biomass <- sum(survey_data$Biomass_kg[survey_data$Functional_Group %in% pelagic_groups], na.rm = TRUE)
demersal_biomass <- sum(survey_data$Biomass_kg[survey_data$Functional_Group %in% demersal_groups], na.rm = TRUE)
pelagic_demersal_ratio <- pelagic_biomass / demersal_biomass

# Calculate planktivore to piscivore ratio
planktivore_biomass <- sum(survey_data$Biomass_kg[survey_data$Functional_Group == "Planktivore"], na.rm = TRUE)
piscivore_biomass <- sum(survey_data$Biomass_kg[survey_data$Functional_Group == "Piscivore"], na.rm = TRUE)
planktivore_piscivore_ratio <- planktivore_biomass / piscivore_biomass

print(paste("Pelagic to Demersal Ratio:", round(pelagic_demersal_ratio, 3)))
print(paste("Planktivore to Piscivore Ratio:", round(planktivore_piscivore_ratio, 3)))

# Step 9: Calculate Mean Trophic Level
# ---------------------------------
# First, we need to assign trophic levels to each functional group
# These are approximate values based on literature and could be refined
trophic_levels <- c(
  "Piscivore" = 4.2,
  "Invertivore" = 3.6,
  "Planktivore" = 3.0,
  "Omnivore" = 3.4,
  "Corallivore" = 2.8,
  "Benthic" = 3.2,
  "Grazer" = 2.2,
  "Scraper" = 2.4,
  "Detritivore" = 2.0
)

# Assign trophic levels to the dataset
survey_data$Trophic_Level <- trophic_levels[survey_data$Functional_Group]

# Calculate biomass-weighted mean trophic level
mean_trophic_level <- sum(survey_data$Biomass_kg * survey_data$Trophic_Level, na.rm = TRUE) / 
  sum(survey_data$Biomass_kg[!is.na(survey_data$Trophic_Level)], na.rm = TRUE)

print(paste("Mean Trophic Level:", round(mean_trophic_level, 3)))

# Calculate mean trophic level by site
mtl_by_site <- survey_data %>%
  group_by(Location, Site) %>%
  summarize(
    Mean_Trophic_Level = sum(Biomass_kg * Trophic_Level, na.rm = TRUE) / 
      sum(Biomass_kg[!is.na(Trophic_Level)], na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Trophic_Level))

print(mtl_by_site)

# Step 10: Calculate CV of Biomass
# -----------------------------
# Calculate CV for each species
species_cv <- survey_data %>%
  group_by(Species) %>%
  summarize(
    Mean_Biomass = mean(Biomass_kg, na.rm = TRUE),
    SD_Biomass = sd(Biomass_kg, na.rm = TRUE),
    CV = SD_Biomass / Mean_Biomass
  ) %>%
  filter(!is.na(CV)) %>%
  arrange(desc(CV))

# Calculate system-level CV using transect as sampling unit
transect_total <- survey_data %>%
  group_by(Location, Site, Transect) %>%
  summarize(Total_Biomass = sum(Biomass_kg, na.rm = TRUE))

system_cv <- sd(transect_total$Total_Biomass) / mean(transect_total$Total_Biomass)

print(paste("System-level CV:", round(system_cv, 3)))

# Count species with CV exceeding system CV
exceeded_count <- sum(species_cv$CV > system_cv, na.rm = TRUE)
total_species <- nrow(species_cv)
proportion_exceeded <- exceeded_count / total_species

print(paste("Proportion of species with CV exceeding system CV:", 
            round(proportion_exceeded, 3), 
            "(", exceeded_count, "out of", total_species, "species)"))

# Step 11: Create cumulative biomass-trophic level curve
# --------------------------------------------------
# Prepare data for curve
trophic_biomass <- survey_data %>%
  filter(!is.na(Trophic_Level)) %>%
  group_by(Trophic_Level) %>%
  summarize(Total_Biomass = sum(Biomass_kg, na.rm = TRUE)) %>%
  arrange(Trophic_Level) %>%
  mutate(
    Cumulative_Biomass = cumsum(Total_Biomass),
    Relative_Cumulative = Cumulative_Biomass / sum(Total_Biomass)
  )

# Find inflection point (simplified approach)
inflection_point <- trophic_biomass %>%
  filter(Relative_Cumulative >= 0.5) %>%
  slice(1)

print(trophic_biomass)
print(paste("Inflection point at trophic level:", round(inflection_point$Trophic_Level, 2)))

# Step 12: Visualizations
# --------------------
# 1. Size spectra plot
size_spectra_plot <- ggplot(size_spectra_data, aes(x = Log10_Size, y = Log10_Abundance)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue") +
  annotate("text", x = min(size_spectra_data$Log10_Size, na.rm = TRUE) + 0.1, 
           y = max(size_spectra_data$Log10_Abundance, na.rm = TRUE) - 0.1,
           label = paste("Slope =", round(spectra_slope, 3),
                         "\nIntercept =", round(spectra_intercept, 3)),
           hjust = 0) +
  labs(x = "Log10 Size (cm)", y = "Log10 Abundance", 
       title = "Size Spectra Analysis") +
  theme_minimal()

print(size_spectra_plot)

# 2. Functional group composition
func_group_plot <- ggplot(func_group_biomass, 
                          aes(x = reorder(Functional_Group, Total_Biomass_kg), 
                              y = Total_Biomass_kg)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Functional Group", y = "Total Biomass (kg)", 
       title = "Biomass by Functional Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(func_group_plot)

# 3. Cumulative biomass-trophic level curve
trophic_curve_plot <- ggplot(trophic_biomass, aes(x = Trophic_Level, y = Relative_Cumulative)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_point(data = inflection_point, color = "red", size = 4) +
  geom_text(data = inflection_point,
            aes(label = paste0("Inflection point\nTL = ", round(Trophic_Level, 2))),
            vjust = -1.5, hjust = 0.5, color = "red") +
  labs(x = "Trophic Level", 
       y = "Relative Cumulative Biomass", 
       title = "Cumulative Biomass-Trophic Level Curve") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

print(trophic_curve_plot)

# 4. Large Fish Index by site
lfi_plot <- ggplot(lfi_by_site, aes(x = reorder(paste(Location, Site), LFI), y = LFI)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_hline(yintercept = overall_lfi, linetype = "dashed", color = "red") +
  labs(x = "Site", y = "Large Fish Index", 
       title = "Large Fish Index by Site",
       subtitle = paste("Overall LFI =", round(overall_lfi, 3))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

print(lfi_plot)

# 5. Mean Trophic Level by site
mtl_plot <- ggplot(mtl_by_site, aes(x = reorder(paste(Location, Site), Mean_Trophic_Level), 
                                    y = Mean_Trophic_Level)) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_hline(yintercept = mean_trophic_level, linetype = "dashed", color = "red") +
  labs(x = "Site", y = "Mean Trophic Level", 
       title = "Mean Trophic Level by Site",
       subtitle = paste("Overall Mean Trophic Level =", round(mean_trophic_level, 3))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

print(mtl_plot)

# 6. CV analysis plot
cv_plot <- ggplot(head(species_cv, 20), aes(x = reorder(Species, CV), y = CV)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_hline(yintercept = system_cv, linetype = "dashed", color = "red") +
  labs(x = "Species", y = "Coefficient of Variation", 
       title = "Top 20 Species by Coefficient of Variation",
       subtitle = paste0("System CV = ", round(system_cv, 3), 
                         " | Proportion exceeding = ", round(proportion_exceeded, 3))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(cv_plot)

# Step 13: Save results (optional)
# -----------------------------
# Save key metrics to a summary file
summary_metrics <- data.frame(
  Metric = c("Large Fish Index", "Size Spectra Slope", "Size Spectra Intercept",
             "Pelagic:Demersal Ratio", "Planktivore:Piscivore Ratio", 
             "Mean Trophic Level", "System CV", "Proportion of Species Exceeding System CV",
             "Trophic Level at Inflection Point"),
  Value = c(overall_lfi, spectra_slope, spectra_intercept,
            pelagic_demersal_ratio, planktivore_piscivore_ratio,
            mean_trophic_level, system_cv, proportion_exceeded,
            inflection_point$Trophic_Level)
)

print(summary_metrics)

# write.csv(summary_metrics, "Timor_Survey_Ecosystem_Indices.csv", row.names = FALSE)

# Optionally save plots
# ggsave("size_spectra.png", size_spectra_plot, width = 8, height = 6)
# ggsave("functional_groups.png", func_group_plot, width = 8, height = 6)
# ggsave("trophic_curve.png", trophic_curve_plot, width = 8, height = 6)
# ggsave("large_fish_index.png", lfi_plot, width = 10, height = 6)
# ggsave("mean_trophic_level.png", mtl_plot, width = 10, height = 6)
# ggsave("cv_analysis.png", cv_plot, width = 8, height = 6)