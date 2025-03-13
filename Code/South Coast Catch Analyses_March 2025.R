# South Coast Catch Data Analysis
# This script cleans and visualizes fishing catch data

# Load required libraries
library(tidyverse)
library(lubridate)
library(scales)

# Read the CSV file
catch_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/South Coast_Catch Data_Test 1.csv", stringsAsFactors = FALSE)

# Display basic information about the dataset
cat("Original data dimensions:", dim(catch_data), "\n")
cat("Column names:", names(catch_data), "\n")
str(catch_data)
summary(catch_data)

##Major outliers in the data:

##Snapper/seaperch catch from Ainaro region in July 2023: 496,859 kg
##Jacks/Trevally/Other Scad catch from Viqueque region in July 2023: 86,683 kg
##Mackerel scad catch from Ainaro region in July 2023: 81,357 kg

# Clean the data
clean_catch <- catch_data %>%
  # Convert date from DD/MM/YY format to proper Date object
  mutate(date = dmy(date)) %>%
  # Remove outliers from July 2023
  filter(!(date == as.Date("2023-07-01") & catch_kg > 50000)) %>%
  # Select only the columns we need
  select(date, group, catch_kg) %>%
  # Remove any rows with NA values
  drop_na() %>%
  # Add year and month columns for easier analysis
  mutate(
    year = year(date),
    month = month(date, label = TRUE),
    year_month = floor_date(date, "month")
  )

# Check the cleaned data
cat("\nCleaned data dimensions:", dim(clean_catch), "\n")

# Check the impact of removing outliers
cat("\nNumber of outliers removed:", nrow(catch_data) - nrow(clean_catch), "\n")
cat("Percentage of data points removed:", round((nrow(catch_data) - nrow(clean_catch))/nrow(catch_data) * 100, 2), "%\n")

# Calculate and display the total catch before and after outlier removal
total_catch_original <- sum(catch_data$catch_kg, na.rm = TRUE)
total_catch_clean <- sum(clean_catch$catch_kg, na.rm = TRUE)
cat("Total catch before outlier removal:", format(total_catch_original, big.mark = ","), "kg\n")
cat("Total catch after outlier removal:", format(total_catch_clean, big.mark = ","), "kg\n")
cat("Percentage of total catch removed:", round((total_catch_original - total_catch_clean)/total_catch_original * 100, 2), "%\n")

summary(clean_catch)

# Calculate total catch by group
total_by_group <- clean_catch %>%
  group_by(group) %>%
  summarize(
    total_catch = sum(catch_kg),
    percentage = total_catch / sum(clean_catch$catch_kg) * 100
  ) %>%
  arrange(desc(total_catch))

# Display total catch by group
print(total_by_group)

# Calculate catch trends over time
catch_by_month <- clean_catch %>%
  group_by(year_month) %>%
  summarize(total_catch = sum(catch_kg))

catch_by_year <- clean_catch %>%
  group_by(year) %>%
  summarize(total_catch = sum(catch_kg))

# Calculate catch by group and year
catch_by_group_year <- clean_catch %>%
  group_by(group, year) %>%
  summarize(total_catch = sum(catch_kg)) %>%
  ungroup()

# Top 5 groups by total catch
top_groups <- total_by_group %>%
  head(5) %>%
  pull(group)

# Filter data for top 5 groups
top_groups_data <- clean_catch %>%
  filter(group %in% top_groups)

# VISUALIZATIONS

# 1. Total catch by group (bar chart)
ggplot(total_by_group, aes(x = reorder(group, total_catch), y = total_catch)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Total Catch by Fish Group",
    x = "Fish Group",
    y = "Total Catch (kg)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma)

ggsave("total_catch_by_group.png", width = 10, height = 8)

# 2. Monthly catch trends
ggplot(catch_by_month, aes(x = year_month, y = total_catch)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Monthly Catch Trends (Outliers Removed)",
    x = "Date",
    y = "Total Catch (kg)",
    caption = "Note: Extreme outliers (>50,000 kg) from July 2023 have been removed"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y")

ggsave("monthly_catch_trends.png", width = 12, height = 6)

# 2b. Compare original vs cleaned monthly catch (focus on 2023)
catch_by_month_2023 <- clean_catch %>%
  filter(year >= 2023) %>%
  group_by(year_month) %>%
  summarize(total_catch = sum(catch_kg))

# Calculate what original July 2023 data would be
original_july_2023 <- catch_data %>%
  filter(date == "01/07/23") %>%
  summarize(total_catch = sum(catch_kg)) %>%
  pull(total_catch)

comparison_data <- catch_by_month_2023
july_2023_row <- which(format(comparison_data$year_month, "%m-%Y") == "07-2023")
comparison_with_outliers <- comparison_data

if(length(july_2023_row) > 0) {
  comparison_with_outliers[july_2023_row, "total_catch"] <- original_july_2023
}

# Create comparison data frame
comparison_df <- rbind(
  data.frame(
    year_month = comparison_data$year_month,
    total_catch = comparison_data$total_catch,
    dataset = "Without Outliers"
  ),
  data.frame(
    year_month = comparison_with_outliers$year_month,
    total_catch = comparison_with_outliers$total_catch,
    dataset = "With Outliers"
  )
)

# Plot comparison
ggplot(comparison_df, aes(x = year_month, y = total_catch, color = dataset, group = dataset)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "2023 Monthly Catch: Before vs After Outlier Removal",
    x = "Month (2023)",
    y = "Total Catch (kg)",
    color = "Dataset"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("With Outliers" = "red", "Without Outliers" = "blue"))

ggsave("monthly_catch_trends_comparison.png", width = 12, height = 6)

# 3. Yearly catch comparison
ggplot(catch_by_year, aes(x = as.factor(year), y = total_catch)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Yearly Catch Comparison",
    x = "Year",
    y = "Total Catch (kg)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma)

ggsave("yearly_catch_comparison.png", width = 8, height = 6)

# 4. Top 5 groups catch trends over time
ggplot(catch_by_group_year %>% filter(group %in% top_groups), 
       aes(x = year, y = total_catch, color = group, group = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Catch Trends for Top 5 Fish Groups",
    x = "Year",
    y = "Total Catch (kg)",
    color = "Fish Group"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma)

ggsave("top_groups_trends.png", width = 10, height = 6)

# 5. Monthly distribution of catch for top groups
ggplot(top_groups_data, aes(x = month, y = catch_kg, fill = group)) +
  geom_boxplot() +
  labs(
    title = "Monthly Distribution of Catch for Top 5 Fish Groups",
    x = "Month",
    y = "Catch (kg)",
    fill = "Fish Group"
  ) +
  theme_minimal() +
  scale_y_log10(labels = comma) +
  facet_wrap(~ group, scales = "free_y")

ggsave("monthly_distribution_top_groups.png", width = 12, height = 8)

# Export the cleaned data
write.csv(clean_catch, "clean_south_coast_catch.csv", row.names = FALSE)

# ANALYSIS OF GROUP COMPOSITION OVER TIME

# 1. Calculate proportion of each group per year
group_composition_by_year <- clean_catch %>%
  group_by(year, group) %>%
  summarize(total_catch = sum(catch_kg), .groups = "drop") %>%
  group_by(year) %>%
  mutate(
    proportion = total_catch / sum(total_catch) * 100,
    year = as.factor(year)
  ) %>%
  ungroup()

# 2. Calculate proportion of each group per quarter
clean_catch <- clean_catch %>%
  mutate(
    quarter = paste0(year, "-Q", quarter(date))
  )

group_composition_by_quarter <- clean_catch %>%
  group_by(quarter, group) %>%
  summarize(total_catch = sum(catch_kg), .groups = "drop") %>%
  group_by(quarter) %>%
  mutate(
    proportion = total_catch / sum(total_catch) * 100
  ) %>%
  ungroup()

# 3. Calculate the change in composition over time
group_changes <- group_composition_by_year %>%
  select(year, group, proportion) %>%
  group_by(group) %>%
  arrange(year) %>%
  mutate(
    change_from_previous = proportion - lag(proportion),
    change_from_first = proportion - first(proportion)
  ) %>%
  ungroup()

# 4. Find the top 5 groups across all years
top_groups_overall <- clean_catch %>%
  group_by(group) %>%
  summarize(total_catch = sum(catch_kg)) %>%
  arrange(desc(total_catch)) %>%
  head(5) %>%
  pull(group)

# 5. Identify groups whose proportion has changed significantly over time
significant_changes <- group_changes %>%
  filter(!is.na(change_from_first)) %>%
  group_by(group) %>%
  summarize(
    max_abs_change = max(abs(change_from_first)),
    direction = ifelse(
      last(change_from_first) > 0, "Increasing", "Decreasing"
    )
  ) %>%
  arrange(desc(max_abs_change)) %>%
  head(5)

# VISUALIZATIONS FOR GROUP COMPOSITION

# 1. Stacked bar chart showing proportion of each group by year
ggplot(group_composition_by_year, aes(x = year, y = proportion, fill = group)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Fish Group Composition by Year",
    x = "Year",
    y = "Proportion of Total Catch",
    fill = "Fish Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(ncol = 3))

ggsave("group_composition_by_year.png", width = 10, height = 8)

# 2. Stacked area chart showing proportion trends over time (quarters)
# Convert quarter to a proper date for better plotting
quarter_dates <- group_composition_by_quarter %>%
  mutate(
    year_quarter = as.numeric(sub("-(.*)", "", quarter)),
    q_num = as.numeric(sub(".*Q", "", quarter)),
    date = as.Date(paste0(year_quarter, "-", (q_num * 3 - 2), "-01"))
  )

ggplot(quarter_dates, aes(x = date, y = proportion, fill = group)) +
  geom_area(position = "fill", alpha = 0.8) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(
    title = "Fish Group Composition Trends (Quarterly)",
    x = "Quarter",
    y = "Proportion of Total Catch",
    fill = "Fish Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(ncol = 3))

ggsave("group_composition_quarterly.png", width = 12, height = 8)

# 3. Line chart showing proportion changes of top 5 groups
ggplot(
  filter(group_composition_by_year, group %in% top_groups_overall),
  aes(x = year, y = proportion, color = group, group = group)
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Proportion of Catch for Top 5 Fish Groups Over Time",
    x = "Year",
    y = "Percentage of Total Catch",
    color = "Fish Group"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(legend.position = "right")

ggsave("top_groups_proportion_trends.png", width = 10, height = 6)

# 4. Heatmap of group proportions over years
ggplot(group_composition_by_year, aes(x = year, y = group, fill = proportion)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Heatmap of Fish Group Proportions by Year",
    x = "Year",
    y = "Fish Group",
    fill = "% of Total Catch"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("group_proportion_heatmap.png", width = 10, height = 8)

# 5. Bar chart showing groups with the most significant changes
ggplot(significant_changes, aes(x = reorder(group, max_abs_change), y = max_abs_change, fill = direction)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Increasing" = "green3", "Decreasing" = "coral3")) +
  labs(
    title = "Fish Groups with Most Significant Changes in Proportion",
    subtitle = "Based on percentage point change from first to last year",
    x = "Fish Group",
    y = "Absolute Change in Percentage Points",
    fill = "Direction"
  ) +
  theme_minimal()

ggsave("significant_composition_changes.png", width = 10, height = 6)

# 6. Calculate seasonal patterns for each group
seasonal_patterns <- clean_catch %>%
  group_by(group, month) %>%
  summarize(avg_catch = mean(catch_kg), .groups = "drop") %>%
  group_by(group) %>%
  mutate(
    proportion = avg_catch / sum(avg_catch) * 100,
    peak_month = month[which.max(avg_catch)]
  ) %>%
  ungroup()

# Only show seasonal patterns for top groups
ggplot(
  filter(seasonal_patterns, group %in% top_groups_overall),
  aes(x = month, y = proportion, color = group, group = group)
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Seasonal Patterns for Top 5 Fish Groups",
    subtitle = "Showing month-to-month variation averaged across all years",
    x = "Month",
    y = "Percentage of Annual Catch",
    color = "Fish Group"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

ggsave("seasonal_patterns.png", width = 10, height = 6)

# Export the cleaned data
write.csv(clean_catch, "clean_south_coast_catch.csv", row.names = FALSE)

# Export the composition analysis for further use
write.csv(group_composition_by_year, "group_composition_by_year.csv", row.names = FALSE)
write.csv(group_composition_by_quarter, "group_composition_by_quarter.csv", row.names = FALSE)
write.csv(significant_changes, "significant_composition_changes.csv", row.names = FALSE)

cat("\nAnalysis complete. Cleaned data and composition analysis saved to CSV files\n")
cat("Visualizations saved as PNG files\n")
