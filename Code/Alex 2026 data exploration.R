#Data visualisation for Alex 2016 fish data
#Aim is to look at species composition and also the contribution of different species to larger groups

# ---- 1. Setup Environment ----

#load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Set working directory - modify this path to match your system

# For example: "C:/Users/YourName/Documents/Projects/TimorBiomass"
working_dir <- "/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data"
setwd(working_dir)

# ---- 2. Import Data ----

# Load the Excel file
timor_data <- read_excel("Timor_biomass_survey_2016_with_species.xlsx", sheet = 1)

# Display the structure of the data
str(timor_data)

# Preview the first few rows
head(timor_data)

# Check for missing values
colSums(is.na(timor_data))

# ---- 3. Basic Data Summary ----

# Summary statistics for numeric variables
summary(select(timor_data,Length, Number, a, b, Biomass_g, Biomass_kg))

# Count observations by location
location_counts <- timor_data %>%
  group_by(Location) %>%
  summarize(n_observations = n(),
            n_sites = n_distinct(Site),
            n_families = n_distinct(Family),
            n_species = n_distinct(Species)) %>%
  arrange(desc(n_observations))

print(location_counts)

# Count observations by family and functional group
family_group_counts <- timor_data %>%
  group_by(Family, Functional_Group) %>%
  summarize(n_observations = n(),
            n_species = n_distinct(Species),
            total_biomass = sum(Biomass_kg, na.rm = TRUE)) %>%
  arrange(desc(n_observations))

print(family_group_counts)

# ---- 4. Visualizations ----

# Biomass distribution
ggplot(timor_data, aes(x = Biomass_kg)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Biomass",
       x = "Biomass (kg)",
       y = "Count")

# Species richness by location
timor_data %>%
  group_by(Location) %>%
  summarize(species_richness = n_distinct(Species)) %>%
  ggplot(aes(x = reorder(Location, species_richness), y = species_richness)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Total Species Richness by Location",
       x = "Location",
       y = "Number of Species")

# ---- 5. Size Distribution Analysis ----

# Length distribution by family for Adarai with observation counts
timor_data %>%
  filter(Location == "Adarai") %>%
  # First calculate counts by family to use in labels
  group_by(Family) %>%
  mutate(n_count = n()) %>%
  ungroup() %>%
  # Create the plot
  ggplot(aes(x = Length, fill = Family)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  # Add text with counts
  geom_text(
    data = . %>% group_by(Family) %>% summarize(n_count = first(n_count)),
    aes(x = Inf, y = Inf, label = paste0("n=", n_count)),
    hjust = 1, vjust = 1, size = 3.5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~Family, scales = "free_y") +
  theme_minimal() +
  guides(fill = "none") +  # Remove the legend
  labs(title = "Length Distribution by Family at Adarai",
       x = "Length (cm)",
       y = "Count")

# Subsetting Lutjanidae in Adarai - looking at species specific length frequency

# Length distribution for Lutjanidae species in Adarai
lutjanidae_adarai <- timor_data %>%
  filter(Location == "Adarai", Family == "Lutjanidae")

# Create a cleaner version with fixed aesthetics
lutjanidae_adarai %>%
  # Calculate counts and stats by species
  group_by(Scientific_name) %>%
  mutate(
    n_count = n(),
    avg_length = mean(Length, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Create the plot
  ggplot(aes(x = Length)) +  # Removed the fill=Species that was causing problems
  geom_histogram(bins = 15, alpha = 0.7, fill = "steelblue") +  # Set a fixed fill color
  # Add mean line
  geom_vline(aes(xintercept = avg_length), linetype = "dashed", color = "red") +
  # Add counts info
  geom_text(
    data = . %>% 
      group_by(Scientific_name) %>%
      summarize(
        n_count = n(),
        avg_length = mean(Length, na.rm = TRUE),
        .groups = "drop"
      ),
    aes(x = Inf, y = Inf, 
        label = paste0("n=", n_count, "\nMean=", round(avg_length, 1), "cm")),
    hjust = 1, vjust = 1, size = 3
  ) +
  facet_wrap(~Scientific_name, scales = "free_y") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "italic"),
    plot.title = element_text(face = "bold")
  ) +
  labs(title = "Length Distribution of Lutjanidae Species at Adarai",
       x = "Length (cm)",
       y = "Count")

##---------##
#Get species L50 from Fishbase

# Install and load the rfishbase package if needed
if (!require("rfishbase")) install.packages("rfishbase")
library(rfishbase)

# Length distribution for Lutjanidae species in Adarai
lutjanidae_adarai <- timor_data %>%
  filter(Location == "Adarai", Family == "Lutjanidae")

# Get unique scientific names for the species
lutjanidae_species <- lutjanidae_adarai %>%
  select(Scientific_name) %>%
  distinct() %>%
  pull(Scientific_name)

# Manually created data frame with L50 values from scientific literature
lutjanidae_maturity <- data.frame(
  Scientific_name = c(
    "Lutjanus rivulatus", 
    "Lutjanus fulviflamma", 
    "Lutjanus decussatus", 
    "Lutjanus fulvus", 
    "Lutjanus rufolineatus", 
    "Lutjanus lunulatus", 
    "Lutjanus monostigma", 
    "Lutjanus gibbus",
    "Lutjanus lemniscatus"
  ),
  L50 = c(45, 21.5, 24, 29.6, 22, 28.2, 35.7, 31.2, 43.5),  # Example values in cm - replace with your scientific values
  source = c(
    "Allen 1985", 
    "Grandcourt et al. 2006", 
    "Kamukuru et al. 2005", 
    "Nanami et al. 2010", 
    "Allen 1985", 
    "Evans et al. 2008", 
    "Heupel et al. 2010", 
    "Nanami et al. 2010",
    "Davis & West 1992"
  )  # Optional: add citation source 
)

# Join with the survey data
lutjanidae_with_maturity <- lutjanidae_adarai %>%
  left_join(lutjanidae_maturity, by = "Scientific_name")

# Add source information to the plot
lutjanidae_with_maturity %>%
  group_by(Scientific_name) %>%
  mutate(
    n_count = n(),
    avg_length = mean(Length, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  ggplot(aes(x = Length)) +
  geom_histogram(bins = 15, alpha = 0.7, fill = "steelblue") +
  # Add mean line
  geom_vline(aes(xintercept = avg_length), linetype = "dashed", color = "red") +
  # Add maturity line
  geom_vline(aes(xintercept = L50), linetype = "solid", color = "darkgreen", linewidth = 0.8) +
  # Add counts and lengths info with source
  geom_text(
    data = . %>% 
      group_by(Scientific_name) %>%
      summarize(
        n_count = n(),
        avg_length = mean(Length, na.rm = TRUE),
        L50 = first(L50),
        source = first(source),
        .groups = "drop"
      ),
    aes(x = Inf, y = Inf, 
        label = paste0("n=", n_count, 
                       "\nMean=", round(avg_length, 1), "cm",
                       "\nL₅₀=", L50, "cm",
                       "\n(", source, ")")),
    hjust = 1, vjust = 1, size = 3
  ) +
  facet_wrap(~Scientific_name, scales = "free_y") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "italic"),
    plot.title = element_text(face = "bold")
  ) +
  labs(title = "Length Distribution of Lutjanidae Species at Adarai",
       subtitle = "Red dashed line = mean length; Green solid line = length at first maturity (L₅₀)",
       x = "Length (cm)",
       y = "Count")

###############################################################################
#Looking at the contribution of different snapper species to the larger group

# Filter data for Lutjanidae at Adarai
lutjanidae_adarai <- timor_data %>%
  filter(Location == "Adarai", Family == "Lutjanidae")

# Analyze species contribution by abundance (count)
abundance_contribution <- lutjanidae_adarai %>%
  group_by(Scientific_name) %>%
  summarize(
    count = n(),
    proportion = n() / nrow(lutjanidae_adarai) * 100
  ) %>%
  arrange(desc(count))

# Print abundance contribution
print("Species contribution by abundance:")
print(abundance_contribution)

# Visualization 1: Abundance contribution bar plot
ggplot(abundance_contribution, 
       aes(x = reorder(Scientific_name, proportion), y = proportion)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = paste0(round(proportion, 1), "%")), 
            hjust = -0.2, size = 3) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(face = "italic"),
    plot.title = element_text(face = "bold")
  ) +
  labs(title = "Lutjanidae Species Contribution by Abundance at Adarai",
       x = NULL,
       y = "Percentage of Total Count (%)")


# Analyze species contribution by biomass
biomass_contribution <- lutjanidae_adarai %>%
  group_by(Scientific_name) %>%
  summarize(
    total_biomass_kg = sum(Biomass_kg, na.rm = TRUE),
    proportion = sum(Biomass_kg, na.rm = TRUE) / sum(lutjanidae_adarai$Biomass_kg, na.rm = TRUE) * 100
  ) %>%
  arrange(desc(total_biomass_kg))

# Print biomass contribution
print("Species contribution by biomass:")
print(biomass_contribution)

# Visualization 2: Biomass contribution bar plot
ggplot(biomass_contribution, 
       aes(x = reorder(Scientific_name, proportion), y = proportion)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  geom_text(aes(label = paste0(round(proportion, 1), "%")), 
            hjust = -0.2, size = 3) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(face = "italic"),
    plot.title = element_text(face = "bold")
  ) +
  labs(title = "Lutjanidae Species Contribution by Biomass at Adarai",
       x = NULL,
       y = "Percentage of Total Biomass (%)")

# Visualization 3: Combined stacked bar chart
# Prepare data for stacked bar
contribution_data <- lutjanidae_adarai %>%
  group_by(Scientific_name) %>%
  summarize(
    count = n(),
    count_percent = n() / nrow(lutjanidae_adarai) * 100,
    biomass = sum(Biomass_kg, na.rm = TRUE),
    biomass_percent = sum(Biomass_kg, na.rm = TRUE) / sum(lutjanidae_adarai$Biomass_kg, na.rm = TRUE) * 100
  ) %>%
  arrange(desc(count)) %>%
  # Create shorter names for display
  mutate(
    short_name = sub("Lutjanus ", "L. ", Scientific_name)
  )

# Convert to long format for plotting
contribution_long <- contribution_data %>%
  select(short_name, count_percent, biomass_percent) %>%
  pivot_longer(
    cols = c(count_percent, biomass_percent),
    names_to = "metric",
    values_to = "percentage"
  ) %>%
  mutate(
    metric = case_when(
      metric == "count_percent" ~ "By Abundance",
      metric == "biomass_percent" ~ "By Biomass",
      TRUE ~ metric
    )
  )

# Create stacked bar chart
ggplot(contribution_long, aes(x = metric, y = percentage, fill = reorder(short_name, -percentage))) +
  geom_col() +
  geom_text(
    data = contribution_long %>% 
      group_by(short_name, metric) %>% 
      filter(percentage >= 5), # Only label segments > 5% for clarity
    aes(label = paste0(round(percentage, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Contribution of Lutjanidae Species at Adarai",
    x = NULL,
    y = "Percentage (%)",
    fill = "Species"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "italic"),
    axis.text.x = element_text(face = "bold")
  )

install.packages("ggrepel") 
library(ggrepel)

# Scatter plot comparing abundance vs biomass contribution
ggplot(contribution_data, aes(x = count_percent, y = biomass_percent)) +
  geom_point(aes(size = biomass), color = "darkblue", alpha = 0.7) +
  geom_text_repel(
    aes(label = short_name),
    fontface = "italic",
    size = 3,
    box.padding = 0.5,
    point.padding = 0.5
  ) +
  geom_abline(linetype = "dashed", color = "gray50") +
  theme_minimal() +
  labs(
    title = "Lutjanidae Species: Abundance vs. Biomass Contribution",
    subtitle = "Points above line contribute more biomass than their numerical abundance would suggest",
    x = "Contribution by Abundance (%)",
    y = "Contribution by Biomass (%)",
    size = "Total Biomass (kg)"
  )

# Calculate weighted average biomass based on biomass contribution
biomass_weighted_avg <- lutjanidae_adarai %>%
  # Group by species
  group_by(Scientific_name) %>%
  summarize(
    # Total biomass for each species
    species_total_biomass = sum(Biomass_kg, na.rm = TRUE),
    # Count of individuals
    count = n(),
    # Average biomass per individual for the species
    species_avg_biomass = mean(Biomass_kg, na.rm = TRUE)
  ) %>%
  # Calculate biomass proportion
  mutate(
    # What fraction of total biomass comes from this species
    biomass_proportion = species_total_biomass / sum(species_total_biomass),
    # Weighted contribution using biomass proportion
    weighted_contribution = species_avg_biomass * biomass_proportion
  ) %>%
  ungroup()

# Calculate the biomass-weighted average
overall_biomass_weighted_avg <- sum(biomass_weighted_avg$weighted_contribution)

# Print results
print(biomass_weighted_avg)
print(paste0("Biomass-weighted average: ", round(overall_biomass_weighted_avg, 4), " kg"))
