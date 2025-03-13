# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)

# Read in the data files
# Assuming chlorophyll data has columns: date, chlorophyll_a
chla_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Fisheries indices/MODISA_L3m_CHL_Monthly_Jan19-Oct23_SOUTH_FOGARTY.csv", stringsAsFactors = FALSE)
# Assuming catch data has columns: date, groups, catch_kg. 3 outliers already removed.
catch_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Fisheries indices/South Coast_Catch Data_Test 1.csv", stringsAsFactors = FALSE)

# Make sure date columns are in proper date format
chla_data$date <- as.Date(chla_data$date, format="%d/%m/%Y")
catch_data$date <- as.Date(catch_data$date, format="%d/%m/%Y")

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
chla_data$primary_production <- estimate_primary_production(chla_data$Chla)
names(chla_data)

# Create a visualization with both time series
monthly_data <- full_join(
  monthly_chla %>% select(date, monthly_chla, monthly_pp),
  monthly_catch %>% select(date, monthly_catch),
  by = "date"
)
# Plot standardized time series
ggplot(monthly_data, aes(x = date)) +
  geom_line(aes(y = scale(monthly_pp), color = "Primary Production")) +
  geom_line(aes(y = scale(monthly_catch), color = "Catch")) +
  labs(title = "Standardized Primary Production and Catch Over Time",
       y = "Standardized Value",
       color = "Measure") +
  theme_minimal()

# Aggregate to annual values by location for Fogarty Index analysis
annual_pp <- chla_data %>%
  group_by(year) %>%
  summarize(annual_pp = mean(primary_production, na.rm = TRUE))
# Print the annual primary production data
print(annual_pp)

annual_catch <- catch_data %>%
  group_by(year) %>%
  summarize(annual_catch = sum(catch_kg, na.rm = TRUE))
print(annual_catch)

#Visualise annual production and catch

#Turn off scientific annotation
options(scipen = 999)

# Create a data frame with annual values
annual_data <- left_join(
  chla_data %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarize(annual_pp = mean(primary_production, na.rm = TRUE)),
  catch_data %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarize(annual_catch = sum(catch_kg, na.rm = TRUE)),
  by = "year"
)

# Create a dual-axis plot for annual values
ggplot(annual_data) +
  # Primary production (left axis)
  geom_line(aes(x = year, y = annual_pp, color = "Primary Production"), 
            size = 1.2) +
  geom_point(aes(x = year, y = annual_pp, color = "Primary Production"), 
             size = 4) +
  # Catch (right axis)
  geom_line(aes(x = year, y = annual_catch/max(annual_catch)*max(annual_pp), 
                color = "Catch"), size = 1.2) +
  geom_point(aes(x = year, y = annual_catch/max(annual_catch)*max(annual_pp), 
                 color = "Catch"), size = 4) +
  # Set up dual axes with appropriate labels
  scale_y_continuous(
    name = "Primary Production (gC/m²/yr)",
    sec.axis = sec_axis(~.*max(annual_data$annual_catch)/max(annual_data$annual_pp), 
                        name = "Catch (kg)")
  ) +
  # Use colors that are distinct and colorblind-friendly
  scale_color_manual(values = c("Primary Production" = "#009E73", 
                                "Catch" = "#0072B2")) +
  # Add informative labels
  labs(title = "Annual Primary Production and Fishery Catch",
       subtitle = paste("Data from", min(annual_data$year), "to", 
                        max(annual_data$year)),
       x = "Year",
       color = "Measure") +
  # Use a clean theme with some customizations
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.y.left = element_text(color = "#009E73", face = "bold"),
    axis.title.y.right = element_text(color = "#0072B2", face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    legend.title = element_text(face = "bold")
  )

# Join the datasets
fogarty_data <- inner_join(annual_pp, annual_catch, by = c("year"))

# Calculate Fogarty Index
# Log transform for power relationship
fogarty_data$log_pp <- log10(fogarty_data$annual_pp)
fogarty_data$log_catch <- log10(fogarty_data$annual_catch)

# Create the Fogarty model
fogarty_model <- lm(log_catch ~ log_pp, data = fogarty_data)
print("Fogarty Index results:")
print(summary(fogarty_model))

# Extract the a and b parameters from the model
# log(Fishery Yield) = a + b × log(Primary Production)
a_coefficient <- coef(fogarty_model)[1]
b_coefficient <- coef(fogarty_model)[2]
print(paste("Fogarty Index equation: log10(Catch) =", round(a_coefficient, 4), "+", round(b_coefficient, 4), "× log10(Primary Production)"))

# Visualize the relationship
ggplot(fogarty_data, aes(x = log_pp, y = log_catch)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Fogarty Index: Log(Fishery Yield) vs Log(Primary Production)",
       x = "Log(Primary Production)",
       y = "Log(Catch)",
       caption = paste("log10(Catch) =", round(a_coefficient, 4), "+", round(b_coefficient, 4), "× log10(Primary Production)")) +
  theme_minimal()


# Time lag analysis 
# Create monthly data for lag analysis
monthly_chla <- chla_data %>%
  group_by(year, month) %>%
  summarize(monthly_chla = mean(Chla, na.rm = TRUE),
            monthly_pp = mean(primary_production, na.rm = TRUE))

monthly_catch <- catch_data %>%
  group_by(year, month) %>%
  summarize(monthly_catch = sum(catch_kg, na.rm = TRUE))

# Create date column for easier joining
monthly_chla$date <- as.Date(paste(monthly_chla$year, monthly_chla$month, "01", sep = "-"))
monthly_catch$date <- as.Date(paste(monthly_catch$year, monthly_catch$month, "01", sep = "-"))

# Create lagged datasets and calculate correlations
lag_correlations <- data.frame()

for (lag in 0:6) {
  # Create lagged dataset
  chla_lagged <- monthly_chla
  chla_lagged$lagged_date <- chla_lagged$date %m+% months(lag)
  
  # CORRECTED JOIN:
  # We're trying to join monthly_catch (current dates) with 
  # chlorophyll data from 'lag' months ago
  lagged_data <- left_join(
    monthly_catch, 
    chla_lagged %>% select(lagged_date, monthly_pp),
    by = c("date" = "lagged_date")
  )
  
  # Calculate correlation (only if we have enough data points)
  if (nrow(na.omit(lagged_data)) > 5) {
    cor_val <- cor(lagged_data$monthly_catch, lagged_data$monthly_pp, 
                   use = "pairwise.complete.obs")
    
    # Add to results
    lag_correlations <- rbind(lag_correlations, 
                              data.frame(lag_months = lag, 
                                         correlation = cor_val))
  }
}

# Now run diagnostics after the data frame has been populated
print("Lag correlation data:")
print(lag_correlations)

# Get the range of correlation values
if (nrow(lag_correlations) > 0) {
  corr_range <- range(lag_correlations$correlation)
  print(paste("Correlation range: From", round(corr_range[1], 3), "to", round(corr_range[2], 3)))
} else {
  print("No correlation values calculated - check your data")
}

# Find the optimal lag time
best_lag <- lag_correlations %>%
  filter(correlation == max(correlation)) %>%
  pull(lag_months)

# Print the optimal lag time
print(paste("Optimal lag time:", best_lag, "months"))

# Visualize lag correlations
ggplot(lag_correlations, aes(x = lag_months, y = correlation)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Correlation between Catch and Primary Productivity by Time Lag",
       x = "Lag (months)",
       y = "Correlation Coefficient") +
  theme_minimal() +
  # Add a horizontal line at y=0 for reference
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  # Ensure consistent y-axis scaling for better interpretation
  scale_y_continuous(limits = function(x) {
    max_abs <- max(abs(x))
    c(-max_abs, max_abs)
  })

# Create optimally lagged dataset
chla_lagged <- monthly_chla
chla_lagged$lagged_date <- chla_lagged$date %m+% months(best_lag)

# Join with catch data using the optimal lag
optimal_data <- left_join(
  monthly_catch, 
  chla_lagged %>% select(lagged_date, monthly_pp),
  by = c("date" = "lagged_date")
)

# Aggregate to annual values for the Fogarty Index
optimal_annual <- optimal_data %>%
  filter(!is.na(monthly_pp)) %>%  # Remove rows with missing productivity data
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(
    annual_catch = sum(monthly_catch, na.rm = TRUE),
    annual_pp = mean(monthly_pp, na.rm = TRUE)
  )

# Calculate Fogarty Index with optimal lag
optimal_annual$log_pp <- log10(optimal_annual$annual_pp)
optimal_annual$log_catch <- log10(optimal_annual$annual_catch)

# Create model with optimal lag
optimal_model <- lm(log_catch ~ log_pp, data = optimal_annual)
print(paste("Optimal lag Fogarty Index (lag =", best_lag, "months):"))
print(summary(optimal_model))

# Extract model coefficients
opt_a_coefficient <- coef(optimal_model)[1]
opt_b_coefficient <- coef(optimal_model)[2]

# Print the equation
print(paste("Optimal lag Fogarty Index equation: log10(Catch) =", 
            round(opt_a_coefficient, 4), "+", 
            round(opt_b_coefficient, 4), "× log10(Primary Production)"))

# Create a prediction function
predict_catch <- function(pp) {
  return(10^(opt_a_coefficient + opt_b_coefficient * log10(pp)))
}

# Example prediction
example_pp <- mean(optimal_annual$annual_pp, na.rm = TRUE)
predicted_catch <- predict_catch(example_pp)
print(paste("For a primary production of", round(example_pp, 2), 
            "gC/m²/yr, predicted catch is", round(predicted_catch, 2), "kg"))

# Visualize the relationship
ggplot(optimal_annual, aes(x = log_pp, y = log_catch)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = paste("Fogarty Index with Optimal Lag of", best_lag, "Months"),
       x = "Log(Primary Production)",
       y = "Log(Catch)",
       caption = paste("log10(Catch) =", round(opt_a_coefficient, 4), "+", 
                       round(opt_b_coefficient, 4), "× log10(Primary Production)")) +
  theme_minimal()


###Seasonal assessment####

# Extract month from date
monthly_data <- monthly_data %>%
  mutate(month = month(date))

# Add season based on month
monthly_data <- monthly_data %>%
  mutate(season = case_when(
    month %in% c(12, 1, 2) ~ "Summer",
    month %in% c(3, 4, 5) ~ "Fall",
    month %in% c(6, 7, 8) ~ "Winter",
    month %in% c(9, 10, 11) ~ "Spring"
  ))

# Also create month as a factor for modeling
monthly_data <- monthly_data %>%
  mutate(month_factor = factor(month))

# Visualize monthly patterns
ggplot(monthly_data, aes(x = factor(month, labels = month.abb))) +
  stat_summary(aes(y = monthly_pp, color = "Primary Production"), 
               fun = "mean", geom = "line", group = 1) +
  stat_summary(aes(y = monthly_pp, color = "Primary Production"), 
               fun = "mean", geom = "point") +
  stat_summary(aes(y = monthly_catch/max(monthly_catch, na.rm = TRUE)*max(monthly_pp, na.rm = TRUE), 
                   color = "Catch"), 
               fun = "mean", geom = "line", group = 1) +
  stat_summary(aes(y = monthly_catch/max(monthly_catch, na.rm = TRUE)*max(monthly_pp, na.rm = TRUE), 
                   color = "Catch"), 
               fun = "mean", geom = "point") +
  scale_y_continuous(
    name = "Average Primary Production (gC/m²/month)",
    labels = scales::comma,
    sec.axis = sec_axis(~.*max(monthly_data$monthly_catch, na.rm = TRUE)/
                          max(monthly_data$monthly_pp, na.rm = TRUE), 
                        name = "Average Catch (kg/month)",
                        labels = scales::comma)
  ) +
  scale_color_manual(values = c("Primary Production" = "#009E73", "Catch" = "#0072B2")) +
  labs(title = "Monthly Patterns in Primary Production and Catch",
       x = "Month",
       color = "Measure") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.y.left = element_text(color = "#009E73"),
    axis.title.y.right = element_text(color = "#0072B2")
  )

##Checking seasons
# Ensure month is a factor ordered correctly
monthly_data <- monthly_data %>%
  mutate(
    year = year(date),
    month_name = factor(month.abb[month], levels = month.abb)
  )

# Create a seasonal plot faceted by year
ggplot(monthly_data, aes(x = month_name)) +
  # Primary production
  geom_line(aes(y = monthly_pp, group = 1, color = "Primary Production"), 
            size = 1) +
  geom_point(aes(y = monthly_pp, color = "Primary Production"), 
             size = 3) +
  # Catch (scaled to fit on same axis)
  geom_line(aes(y = monthly_catch/max(monthly_catch, na.rm = TRUE)*max(monthly_pp, na.rm = TRUE), 
                group = 1, color = "Catch"), 
            size = 1) +
  geom_point(aes(y = monthly_catch/max(monthly_catch, na.rm = TRUE)*max(monthly_pp, na.rm = TRUE), 
                 color = "Catch"), 
             size = 3) +
  # Facet by year
  facet_wrap(~year, ncol = 1) +
  # Set up dual axes
  scale_y_continuous(
    name = "Primary Production (gC/m²/month)",
    labels = scales::comma,
    sec.axis = sec_axis(~.*max(monthly_data$monthly_catch, na.rm = TRUE)/
                          max(monthly_data$monthly_pp, na.rm = TRUE), 
                        name = "Catch (kg/month)",
                        labels = scales::comma)
  ) +
  # Colors
  scale_color_manual(values = c("Primary Production" = "#009E73", 
                                "Catch" = "#0072B2")) +
  # Labels
  labs(title = "Seasonal Patterns in Primary Production and Catch by Year",
       x = "Month",
       color = "Measure") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold"),
    axis.title.y.left = element_text(color = "#009E73"),
    axis.title.y.right = element_text(color = "#0072B2")
  )

# Create a heatmap of primary production by year and month
monthly_data %>%
  mutate(
    year = year(date),
    month_name = factor(month.abb[month], levels = month.abb)
  ) %>%
  ggplot(aes(x = month_name, y = factor(year))) +
  # Primary production heatmap
  geom_tile(aes(fill = monthly_pp)) +
  scale_fill_viridis_c(name = "Primary\nProduction\n(gC/m²/month)") +
  labs(title = "Primary Production by Month and Year",
       x = "Month", 
       y = "Year") +
  theme_minimal()

# Create a similar heatmap for catch
monthly_data %>%
  mutate(
    year = year(date),
    month_name = factor(month.abb[month], levels = month.abb)
  ) %>%
  ggplot(aes(x = month_name, y = factor(year))) +
  # Catch heatmap
  geom_tile(aes(fill = monthly_catch)) +
  scale_fill_viridis_c(name = "Catch\n(kg/month)", 
                       option = "plasma") +
  labs(title = "Catch by Month and Year",
       x = "Month", 
       y = "Year") +
  theme_minimal()

##Seanal modeling - Fogarty Index##

# Prepare monthly data with seasonal indicators
monthly_fogarty <- monthly_data %>%
  # Create log-transformed variables
  mutate(
    log_pp = log10(monthly_pp),
    log_catch = log10(monthly_catch),
    # Create seasonal indicators (using Southern Hemisphere seasons)
    season = case_when(
      month %in% c(12, 1, 2) ~ "Summer",
      month %in% c(3, 4, 5) ~ "Fall",
      month %in% c(6, 7, 8) ~ "Winter",
      month %in% c(9, 10, 11) ~ "Spring"
    ),
    season = factor(season, levels = c("Summer", "Fall", "Winter", "Spring"))
  )

# Model 1: Basic monthly Fogarty Index
monthly_model <- lm(log_catch ~ log_pp, data = monthly_fogarty)

# Model 2: Fogarty Index with seasonal effect
seasonal_model <- lm(log_catch ~ log_pp + season, data = monthly_fogarty)

# Model 3: Fogarty Index with season-specific slopes (interaction)
interaction_model <- lm(log_catch ~ log_pp * season, data = monthly_fogarty)

# Compare models
anova(monthly_model, seasonal_model, interaction_model)
AIC(monthly_model, seasonal_model, interaction_model)

# Summary of best model (choose based on AIC and ANOVA results)
best_model <- interaction_model  # Replace with your best model
summary(best_model)

##Seasonal lag assments
# Create optimal lagged dataset with seasonal information
lag_months <- 6  # Replace with your optimal lag
lagged_monthly <- monthly_data %>%
  # Create the lagged date correctly
  mutate(date_lagged = date %m+% months(-lag_months)) %>%
  select(date, date_lagged, monthly_catch)

# Join with PP data using lagged dates
lagged_data <- left_join(
  lagged_monthly,
  monthly_data %>% select(date, monthly_pp),
  by = c("date_lagged" = "date")
)

# Add season and create log-transformed variables
lagged_data <- lagged_data %>%
  mutate(
    month = month(date),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Summer",
      month %in% c(3, 4, 5) ~ "Fall",
      month %in% c(6, 7, 8) ~ "Winter",
      month %in% c(9, 10, 11) ~ "Spring"
    ),
    season = factor(season, levels = c("Summer", "Fall", "Winter", "Spring")),
    log_pp = log10(monthly_pp),
    log_catch = log10(monthly_catch)
  )

# Seasonal lagged Fogarty model
seasonal_lag_model <- lm(log_catch ~ log_pp + season, data = lagged_data)
summary(seasonal_lag_model)

##Model with an upwelling season - SE monsoon season on the South
# Create upwelling season indicator
monthly_data <- monthly_data %>%
  mutate(upwelling_season = ifelse(month %in% c(5:10), "Upwelling", "Non-upwelling"),
         upwelling_season = factor(upwelling_season),
         log_catch = log10(monthly_catch),
         log_pp = log10(monthly_pp)
  )

# Model with upwelling season interaction
upwelling_season_model <- lm(log_catch ~ log_pp * upwelling_season + season, 
                             data = monthly_data)

# Check the model summary
summary(upwelling_season_model)
