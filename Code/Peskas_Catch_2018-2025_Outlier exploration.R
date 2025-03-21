# Load necessary packages
library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For date handling
library(scales)     # For better plot formatting

# Read the catch data
catch_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Peskas_Catch/timor_catch_2018_mar2025.csv", stringsAsFactors = FALSE)

# Take a quick look at the data structure
str(catch_data)
head(catch_data)
summary(catch_data)

# Convert date string to proper date format
catch_data$date_bin_start <- as.Date(catch_data$date_bin_start)

# Check for missing values in each column
colSums(is.na(catch_data))

# Basic data cleaning - remove rows with NA in important columns
catch_clean <- catch_data %>%
  filter(!is.na(recorded_catch_kg) & !is.na(estimated_catch_kg))

# Create logical columns to flag potential issues
catch_clean <- catch_clean %>%
  mutate(
    zero_recorded = recorded_catch_kg == 0,
    zero_estimated = estimated_catch_kg == 0,
    negative_recorded = recorded_catch_kg < 0,
    negative_estimated = estimated_catch_kg < 0,
    est_much_larger = estimated_catch_kg > 5 * recorded_catch_kg & recorded_catch_kg > 0,
    rec_much_larger = recorded_catch_kg > 5 * estimated_catch_kg & estimated_catch_kg > 0
  )

# Summarize these flags
flag_summary <- catch_clean %>%
  summarise(
    zero_recorded_count = sum(zero_recorded),
    zero_estimated_count = sum(zero_estimated),
    negative_recorded_count = sum(negative_recorded),
    negative_estimated_count = sum(negative_estimated),
    est_much_larger_count = sum(est_much_larger),
    rec_much_larger_count = sum(rec_much_larger)
  )

print(flag_summary)

# Create histograms for recorded and estimated catch that handle zeros properly
p1 <- ggplot(catch_clean, aes(x = recorded_catch_kg)) +
  geom_histogram(bins = 50, fill = "steelblue") +
  # Only apply log scale to positive values
  scale_x_continuous(
    trans = scales::pseudo_log_trans(base = 10),  # Use pseudo-log to handle zeros
    labels = comma
  ) +
  labs(title = "Distribution of Recorded Catch (kg)", 
       x = "Recorded Catch (kg) - Pseudo-log Scale", 
       y = "Count") +
  theme_minimal()

p2 <- ggplot(catch_clean, aes(x = estimated_catch_kg)) +
  geom_histogram(bins = 50, fill = "darkgreen") +
  scale_x_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    labels = comma
  ) +
  labs(title = "Distribution of Estimated Catch (kg)", 
       x = "Estimated Catch (kg) - Pseudo-log Scale", 
       y = "Count") +
  theme_minimal()

print(p1)
print(p2)

# Boxplots by region to see regional variations
p3 <- ggplot(catch_clean, aes(x = region, y = recorded_catch_kg)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Recorded Catch by Region", x = "Region", y = "Recorded Catch (kg) - Log Scale") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)

# Properly analyze zero catches at the taxonomic group level

# First, confirm we have zeros at the taxonomic group level
zero_check <- catch_data %>%
  summarise(
    total_records = n(),
    zero_catch_records = sum(recorded_catch_kg == 0, na.rm = TRUE),
    zero_percentage = round(100 * sum(recorded_catch_kg == 0, na.rm = TRUE) / n(), 1)
  )

print(zero_check)

