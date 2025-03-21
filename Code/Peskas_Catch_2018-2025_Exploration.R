#-----------------------------------------------------------
##Peskas catch data from 2018 to March 2025##
##Initial data exploration##
#-----------------------------------------------------------

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For handling dates
library(ggplot2)    # For data visualization
library(scales)     # For better axis scaling in plots

options(scipen = 999)

#-----------------------------------------------------------
##LOAD DATA AND INITIAL CHECK
#-----------------------------------------------------------

# Read the catch data
catch_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Peskas_Catch/timor_catch_2018_mar2025.csv", stringsAsFactors = FALSE)

# Examine the structure of the data
str(catch_data)

# View the first few rows
head(catch_data)

# Summary statistics
summary(catch_data)

#-----------------------------------------------------------
##SORT OUT DATES AND ADD MONTH + YEAR COLUMNS
#-----------------------------------------------------------

# Convert date_bin_start to Date format
catch_data$date_bin_start <- as.Date(catch_data$date_bin_start)

# Check for missing values
colSums(is.na(catch_data))

# Create year and month variables for temporal analysis
catch_data <- catch_data %>%
  mutate(
    year = year(date_bin_start),
    month = month(date_bin_start),
    month_name = month(date_bin_start, label = TRUE)
  )

# Summary of the cleaned data
summary(catch_data)

#-----------------------------------------------------------
##BASIC EXPLORATORY DATA ANALYSIS
#-----------------------------------------------------------

# Summary statistics by region
region_summary <- catch_data %>%
  group_by(region) %>%
  summarise(
    total_recorded_catch = sum(recorded_catch_kg, na.rm = TRUE),
    total_estimated_catch = sum(estimated_catch_kg, na.rm = TRUE),
    avg_recorded_catch = mean(recorded_catch_kg, na.rm = TRUE),
    avg_estimated_catch = mean(estimated_catch_kg, na.rm = TRUE),
    n_landings = sum(n_landings_per_boat, na.rm = TRUE),
    n_observations = n()
  ) %>%
  arrange(desc(total_recorded_catch))

print(region_summary)

# Summary statistics by year
yearly_summary <- catch_data %>%
  group_by(year) %>%
  summarise(
    total_recorded_catch = sum(recorded_catch_kg, na.rm = TRUE),
    total_estimated_catch = sum(estimated_catch_kg, na.rm = TRUE),
    avg_recorded_catch = mean(recorded_catch_kg, na.rm = TRUE),
    avg_estimated_catch = mean(estimated_catch_kg, na.rm = TRUE),
    n_landings = sum(n_landings_per_boat, na.rm = TRUE),
    n_observations = n()
  ) %>%
  arrange(year)

print(yearly_summary)

#-----------------------------------------------------------
# VISUALIZING TEMPORAL PATTERNS WITH STANDARD ERRORS
#-----------------------------------------------------------

#-----------------------------------------------------------
# MONTHLY PATTERNS - TOTAL ESTIMATED CATCH
#-----------------------------------------------------------
# Monthly catch trends - total (not average)
monthly_catch <- catch_data %>%
  group_by(year, month, month_name) %>%
  summarise(
    total_estimated_catch = sum(estimated_catch_kg, na.rm = TRUE)/1000
  ) %>%
  ungroup()

# Create a date column for proper time series plotting
monthly_catch$date <- as.Date(paste(monthly_catch$year, monthly_catch$month, "15", sep = "-"))

# Plot monthly trends of total catch
time_series_plot <- ggplot(monthly_catch, aes(x = date)) +
  geom_line(aes(y = total_estimated_catch), color = "darkblue", size = 1.2) +
  labs(
    title = "Monthly Total Estimated Catch",
    x = NULL,
    y = "Total Estimated Catch (tonnes)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(time_series_plot)

#-----------------------------------------------------------
# ANNUAL CHANGES ACROSS SPACE AND TIME - MEANS WITH SE
#-----------------------------------------------------------
# Annual changes by region with SE
annual_region_se <- catch_data %>%
  # Group by region and year to get annual totals for each region
  group_by(region, year) %>%
  summarise(
    annual_estimated_catch = sum(estimated_catch_kg, na.rm = TRUE)/1000
  ) %>%
  # Then group by region to calculate mean and SE across years
  group_by(region) %>%
  summarise(
    mean_annual_catch = mean(annual_estimated_catch, na.rm = TRUE),
    se_annual_catch = sd(annual_estimated_catch, na.rm = TRUE) / sqrt(n()),
    n_years = n()
  ) %>%
  ungroup() %>%
  arrange(desc(mean_annual_catch))

# Plot annual means by region with SE
annual_region_plot <- ggplot(annual_region_se, 
                             aes(x = reorder(region, mean_annual_catch), 
                                 y = mean_annual_catch)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_annual_catch - se_annual_catch, 
                    ymax = mean_annual_catch + se_annual_catch),
                width = 0.2, color = "darkred") +
  labs(
    title = "Mean Annual Estimated Catch by Region (with SE)",
    x = "Region",
    y = "Mean Annual Estimated Catch (tonnes)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(annual_region_plot)

# Annual changes over time with SE
annual_time_se <- catch_data %>%
  # Group by year and region to get annual totals for each region
  group_by(year, region) %>%
  summarise(
    annual_estimated_catch = sum(estimated_catch_kg, na.rm = TRUE)/1000
  ) %>%
  # Then group by year to calculate mean and SE across regions
  group_by(year) %>%
  summarise(
    mean_annual_catch = mean(annual_estimated_catch, na.rm = TRUE),
    se_annual_catch = sd(annual_estimated_catch, na.rm = TRUE) / sqrt(n()),
    n_regions = n()
  ) %>%
  ungroup()

# Plot annual means over time with SE
annual_time_plot <- ggplot(annual_time_se, 
                           aes(x = year, y = mean_annual_catch, group = 1)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  geom_errorbar(aes(ymin = mean_annual_catch - se_annual_catch, 
                    ymax = mean_annual_catch + se_annual_catch),
                width = 0.2, color = "darkblue") +
  labs(
    title = "Mean Annual Estimated Catch Over Time (with SE)",
    x = "Year",
    y = "Mean Annual Estimated Catch (tonnes)"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(annual_time_se$year))

print(annual_time_plot)

#-----------------------------------------------------------
# SEASONAL CHANGES ACROSS SPACE AND TIME - MEANS WITH SE
#-----------------------------------------------------------
# Seasonal patterns (monthly averages across years) with SE
seasonal_patterns_se <- catch_data %>%
  # Group by year, month, and region to get monthly totals for each region in each year
  group_by(year, month, month_name, region) %>%
  summarise(
    monthly_estimated_catch = sum(estimated_catch_kg, na.rm = TRUE)/1000
  ) %>%
  # Then group by month to calculate mean and SE across years and regions
  group_by(month, month_name) %>%
  summarise(
    mean_monthly_catch = mean(monthly_estimated_catch, na.rm = TRUE),
    se_monthly_catch = sd(monthly_estimated_catch, na.rm = TRUE) / sqrt(n()),
    n_observations = n()
  ) %>%
  ungroup()

# Order month names correctly
seasonal_patterns_se$month_name <- factor(seasonal_patterns_se$month_name, 
                                          levels = month.abb)

# Create seasonal plot with standard errors
seasonal_plot_se <- ggplot(seasonal_patterns_se, 
                           aes(x = month_name, 
                               y = mean_monthly_catch, 
                               group = 1)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  geom_errorbar(aes(ymin = mean_monthly_catch - se_monthly_catch, 
                    ymax = mean_monthly_catch + se_monthly_catch),
                width = 0.2, color = "darkblue") +
  labs(
    title = "Seasonal Patterns in Estimated Catch (with SE)",
    x = "Month",
    y = "Mean Monthly Estimated Catch (tonnes)"
  ) +
  theme_minimal()

print(seasonal_plot_se)

# Seasonal means with SE by year (averaging across sites/regions)
monthly_means_by_year <- catch_data %>%
  # Group by year, month, and region to get monthly totals for each region in each year
  group_by(year, month, month_name, region) %>%
  summarise(
    regional_monthly_catch = sum(estimated_catch_kg, na.rm = TRUE)/1000
  ) %>%
  # Then group by year and month to calculate mean and SE across regions
  group_by(year, month, month_name) %>%
  summarise(
    mean_monthly_catch = mean(regional_monthly_catch, na.rm = TRUE),
    se_monthly_catch = sd(regional_monthly_catch, na.rm = TRUE) / sqrt(n()),
    n_regions = n()
  ) %>%
  ungroup()

# Order month names correctly
monthly_means_by_year$month_name <- factor(monthly_means_by_year$month_name, 
                                           levels = month.abb)

# Add season column 
monthly_means_by_year <- monthly_means_by_year %>%
  mutate(season = case_when(
    month %in% c(12, 1, 2) ~ "Summer",
    month %in% c(3, 4, 5) ~ "Fall",
    month %in% c(6, 7, 8) ~ "Winter",
    month %in% c(9, 10, 11) ~ "Spring",
    TRUE ~ NA_character_
  ))

# Create a data frame for the season rectangles
seasons_rect <- data.frame(
  season = c("Summer", "Fall", "Winter", "Spring"),
  start = c(0.5, 3.5, 6.5, 9.5),
  end = c(3.5, 6.5, 9.5, 12.5),
  fill = c("lightpink", "orange1", "lightblue", "lightgreen"),
  alpha = c(0.3, 0.3, 0.3, 0.3)
)

# Create seasonal plot faceted by year, with season backgrounds and error bars
seasonal_plot_by_year <- ggplot(monthly_means_by_year) +
  # Add rectangles for seasons
  geom_rect(data = seasons_rect, 
            aes(xmin = start, xmax = end, 
                ymin = -Inf, ymax = Inf, 
                fill = season),
            alpha = 0.3,
            inherit.aes = FALSE) +
  # Add data points and lines with error bars
  geom_line(aes(x = as.numeric(month_name), 
                y = mean_monthly_catch, 
                group = 1),
            color = "darkblue", size = 1) +
  geom_point(aes(x = as.numeric(month_name), 
                 y = mean_monthly_catch),
             color = "darkblue", size = 2) +
  geom_errorbar(aes(x = as.numeric(month_name),
                    ymin = mean_monthly_catch - se_monthly_catch, 
                    ymax = mean_monthly_catch + se_monthly_catch),
                width = 0.2, color = "darkblue") +
  facet_wrap(~year, ncol = 2) +
  scale_x_continuous(breaks = 1:12, 
                     labels = month.abb) +
  scale_fill_manual(values = c("Summer" = "lightpink", 
                               "Fall" = "orange1", 
                               "Winter" = "lightblue", 
                               "Spring" = "lightgreen"),
                    name = "Season") +
  labs(
    title = "Monthly Mean Estimated Catch by Year (with SE)",
    subtitle = "Summer (Dec-Feb), Fall (Mar-May), Winter (Jun-Aug), Spring (Sep-Nov)",
    x = "Month",
    y = "Mean Estimated Catch (tonnes)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(seasonal_plot_by_year)

# Seasonal patterns by region averaged over all years (for all regions)
seasonal_region_se <- catch_data %>%
  # Group by region, year, and month to get monthly totals for each region in each year
  group_by(region, year, month, month_name) %>%
  summarise(
    monthly_estimated_catch = sum(estimated_catch_kg, na.rm = TRUE)/1000
  ) %>%
  ungroup()

# Calculate the mean and SE for each region and month (across years)
seasonal_region_means <- seasonal_region_se %>%
  group_by(region, month, month_name) %>%
  summarise(
    mean_catch = mean(monthly_estimated_catch, na.rm = TRUE),
    se_catch = sd(monthly_estimated_catch, na.rm = TRUE) / sqrt(n()),
    n_years = n()
  ) %>%
  ungroup()

# Order month names correctly
seasonal_region_means$month_name <- factor(seasonal_region_means$month_name, 
                                           levels = month.abb)

# Add season column 
seasonal_region_means <- seasonal_region_means %>%
  mutate(season = case_when(
    month %in% c(12, 1, 2) ~ "Summer",
    month %in% c(3, 4, 5) ~ "Fall",
    month %in% c(6, 7, 8) ~ "Winter",
    month %in% c(9, 10, 11) ~ "Spring",
    TRUE ~ NA_character_
  ))

# Create a data frame for the season rectangles
seasons_rect <- data.frame(
  season = c("Summer", "Fall", "Winter", "Spring"),
  start = c(0.5, 3.5, 6.5, 9.5),
  end = c(3.5, 6.5, 9.5, 12.5),
  fill = c("lightpink", "orange1", "lightblue", "lightgreen"),
  alpha = c(0.3, 0.3, 0.3, 0.3)
)

# Create seasonal patterns by region plot with facet grid
seasonal_region_plot <- ggplot(seasonal_region_means) +
  # Add rectangles for seasons
  geom_rect(data = seasons_rect, 
            aes(xmin = start, xmax = end, 
                ymin = -Inf, ymax = Inf, 
                fill = season),
            alpha = 0.3,
            inherit.aes = FALSE) +
  # Add lines and points
  geom_line(aes(x = as.numeric(month_name), 
                y = mean_catch,
                group = 1),
            color = "darkblue", size = 0.8) +
  geom_point(aes(x = as.numeric(month_name), 
                 y = mean_catch),
             color = "darkblue", size = 2) +
  # Add error bars
  geom_errorbar(aes(x = as.numeric(month_name), 
                    ymin = mean_catch - se_catch, 
                    ymax = mean_catch + se_catch),
                width = 0.2, color = "darkblue") +
  # Create a facet grid with regions
  facet_wrap(~ region, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = 1:12, 
                     labels = month.abb) +
  scale_fill_manual(values = c("Summer" = "lightpink", 
                               "Fall" = "orange1", 
                               "Winter" = "lightblue", 
                               "Spring" = "lightgreen"),
                    name = "Season") +
  labs(
    title = "Seasonal Patterns in Estimated Catch by Region",
    subtitle = "Summer (Dec-Feb), Fall (Mar-May), Winter (Jun-Aug), Spring (Sep-Nov)",
    x = "Month",
    y = "Mean Monthly Estimated Catch (tonnes)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.spacing.x = unit(0.5, "lines"),
    panel.spacing.y = unit(0.5, "lines")
  )

print(seasonal_region_plot)

#-----------------------------------------------------------
# CATCH COMPOSITION
#-----------------------------------------------------------

# Top species by total catch (both recorded and estimated)
species_summary <- catch_data %>%
  group_by(catch_taxon, catch_name_en) %>%
  summarise(
    total_recorded_catch = sum(recorded_catch_kg, na.rm = TRUE)/1000,
    total_estimated_catch = sum(estimated_catch_kg, na.rm = TRUE)/1000
  ) %>%
  # Determine the top 15 based on recorded catch
  arrange(desc(total_recorded_catch)) %>%
  ungroup() %>%
  slice_head(n = 15) %>%
  # Convert to long format for plotting
  pivot_longer(
    cols = c(total_recorded_catch, total_estimated_catch),
    names_to = "catch_type",
    values_to = "catch_tonnes"
  ) %>%
  # Clean up the catch type labels
  mutate(catch_type = case_when(
    catch_type == "total_recorded_catch" ~ "Recorded",
    catch_type == "total_estimated_catch" ~ "Estimated",
    TRUE ~ catch_type
  ))

# Create the plot with facets for each catch type
species_plot <- ggplot(species_summary, 
                       aes(x = reorder(catch_name_en, catch_tonnes, 
                                       function(x) sum(x[species_summary$catch_type == "Recorded"])), 
                           y = catch_tonnes,
                           fill = catch_type)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for better readability
  facet_wrap(~ catch_type, scales = "free_x") +  # Use free scales for x-axis
  scale_fill_manual(values = c("Recorded" = "steelblue", "Estimated" = "tomato")) +
  labs(
    title = "Top 15 Species by Total Catch",
    subtitle = "Comparing Recorded vs Estimated Catch Patterns",
    x = "Species (English Name)",
    y = "Catch (tonnes)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend as facet labels serve the purpose
    axis.text.y = element_text(size = 9),
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

print(species_plot)

# Let's first examine the actual species in the dataset
species_check <- catch_data %>%
  select(catch_taxon, catch_name_en) %>%
  distinct() %>%
  arrange(catch_taxon)

# Species composition by region - revised approach
region_species <- catch_data %>%
  # Add catch_name_en to the grouping to preserve it for the species groups
  group_by(region, catch_taxon, catch_name_en) %>%
  summarise(
    total_catch = sum(recorded_catch_kg, na.rm = TRUE)/1000
  ) %>%
  ungroup() %>%
  # Create species groups based on actual names in the dataset
  mutate(species_group = case_when(
    # Match common fish families and groups based on both taxonomic name and English name
    grepl("Scombridae|tuna|mackerel|Thunnus|Scomberomorus|Katsuwonus", catch_taxon, ignore.case = TRUE) | 
      grepl("tuna|mackerel", catch_name_en, ignore.case = TRUE) ~ "Tuna & Mackerel",
    
    grepl("Carangidae|Caranx|Seriola|Trachinotus", catch_taxon, ignore.case = TRUE) | 
      grepl("trevall|jack|pompano", catch_name_en, ignore.case = TRUE) ~ "Trevally & Jacks",
    
    grepl("Lutjanidae|Lutjanus|Aprion", catch_taxon, ignore.case = TRUE) | 
      grepl("snapper", catch_name_en, ignore.case = TRUE) ~ "Snappers",
    
    grepl("Serranidae|Epinephelus|Cephalopholis|Plectropomus", catch_taxon, ignore.case = TRUE) | 
      grepl("grouper|cod", catch_name_en, ignore.case = TRUE) ~ "Groupers",
    
    grepl("Lethrinidae|Lethrinus", catch_taxon, ignore.case = TRUE) | 
      grepl("emperor|bream", catch_name_en, ignore.case = TRUE) ~ "Emperors",
    
    grepl("Clupeidae|Sardinella|Sardine", catch_taxon, ignore.case = TRUE) | 
      grepl("sardine|herring", catch_name_en, ignore.case = TRUE) ~ "Sardines",
    
    grepl("shark|ray|Carcharhinidae|Sphyrnidae|Dasyatidae", catch_taxon, ignore.case = TRUE) | 
      grepl("shark|ray", catch_name_en, ignore.case = TRUE) ~ "Sharks & Rays",
    
    grepl("octopus|squid|cuttlefish|Octopodidae|Loliginidae|Sepiidae", catch_taxon, ignore.case = TRUE) | 
      grepl("octopus|squid|cuttlefish", catch_name_en, ignore.case = TRUE) ~ "Cephalopods",
    
    grepl("shrimp|lobster|crab|Penaeidae|Palinuridae|Portunidae", catch_taxon, ignore.case = TRUE) | 
      grepl("shrimp|prawn|lobster|crab", catch_name_en, ignore.case = TRUE) ~ "Crustaceans",
    
    # Add more specific groupings based on actual dataset content
    TRUE ~ "Other"
  )) %>%
  # Now group by region and species_group
  group_by(region, species_group) %>%
  summarise(
    total_catch = sum(total_catch, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # For each region, get the top 5 species groups
  group_by(region) %>%
  mutate(rank = dense_rank(desc(total_catch))) %>%
  filter(rank <= 5) %>%
  ungroup()

# Order regions by total catch
region_order <- region_species %>%
  group_by(region) %>%
  summarise(total = sum(total_catch)) %>%
  arrange(desc(total)) %>%
  pull(region)

# Convert region to a factor with ordered levels
region_species$region <- factor(region_species$region, levels = region_order)

# Create a better color palette
group_colors <- c(
  "Tuna & Mackerel" = "#E41A1C",
  "Trevally & Jacks" = "#377EB8",
  "Snappers" = "#4DAF4A",
  "Groupers" = "#984EA3",
  "Emperors" = "#FF7F00",
  "Sardines" = "#FFFF33",
  "Sharks & Rays" = "#A65628",
  "Cephalopods" = "#F781BF",
  "Crustaceans" = "#999999",
  "Other" = "#66C2A5"
)

# Create the composition plot
composition_plot <- ggplot(region_species, 
                           aes(x = region, y = total_catch, fill = species_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Top 5 Species Groups by Region",
    x = "Region",
    y = "Total Recorded Catch (tonnes)",
    fill = "Species Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))  # Arrange legend in multiple rows

print(composition_plot)

#-----------------------------------------------------------
# DATA RELATIONSHIPS
#-----------------------------------------------------------

# Relationship between number of landings and total catch
landing_catch_plot <- ggplot(catch_data, 
                             aes(x = n_landings_per_boat, 
                                 y = recorded_catch_kg)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Relationship Between Number of Landings and Recorded Catch",
    x = "Number of Landings per Boat",
    y = "Recorded Catch (kg)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  coord_cartesian(ylim = c(0, quantile(catch_data$recorded_catch_kg, 0.99, na.rm = TRUE)))  # Remove outliers for better visualization

print(landing_catch_plot)

# Correlation between recorded and estimated catch
correlation_plot <- ggplot(catch_data, 
                           aes(x = recorded_catch_kg, 
                               y = estimated_catch_kg)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Correlation Between Recorded and Estimated Catch",
    x = "Recorded Catch (kg)",
    y = "Estimated Catch (kg)"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(
    xlim = c(0, quantile(catch_data$recorded_catch_kg, 0.99, na.rm = TRUE)),
    ylim = c(0, quantile(catch_data$estimated_catch_kg, 0.99, na.rm = TRUE))
  )  # Remove outliers for better visualization

print(correlation_plot)
