#Looking at future climate projections for Timor-Leste using: https://data.isimip.org/
#Practise code looking at: SSP585 scenario, GFDL-ESM4, 2050-2060
# Install necessary packages if you haven't already
# install.packages(c("ncdf4", "raster", "ggplot2", "lubridate"))

# Load the libraries
library(ncdf4)    # For reading NetCDF files
library(raster)   # For spatial data handling
library(ggplot2)  # For plotting
library(lubridate) # For date handling
install.packages(c("sp", "Rcpp", "units"))
# Install the specific version needed (1.8.5 or newer)
install.packages("terra", version = "1.8.5")
library(terra)


# Make sure raster is installed
install.packages("raster")
library(raster)

# Let the user select the file interactively
nc_file <- file.choose()
print(nc_file)  # See the full path

# Open the NetCDF file
nc <- nc_open(nc_file)

# Print the file information to understand its structure
print(nc)

# Read the dimensions
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")

# Check their lengths
cat("Longitude dimension:", length(lon), "\n")
cat("Latitude dimension:", length(lat), "\n")
cat("Time dimension:", length(time), "\n")

# First, find the name of the temperature variable
print(nc$var)  # This shows all variables

# Let's assume it's called "tas" (common name for temperature)
# Extract the temperature data
temp_data <- ncvar_get(nc, "tas")

# Check the dimensions of temperature data
dim(temp_data)

# Now let's examine our time variable
head(time)  # Show first few values
length(time)  # How many time points we have

# Get time units to understand what these values mean
time_units <- ncatt_get(nc, "time", "units")$value
print(time_units)

# Convert time values to actual dates
# Assuming time_units is something like "days since 2051-01-01"
if(grepl("days since", time_units)) {
  date_string <- gsub("days since ", "", time_units)
  date_string <- strsplit(date_string, " ")[[1]][1]  # Extract just the date part
  # Convert to Date object
  start_date <- as.Date(date_string, format="%Y-%m-%d")  
  # Create the sequence of dates
  dates <- start_date + time
  
  # Look at the first few dates
  head(dates)
  # And how many dates we have
  length(dates)
}

if(length(dim_results) == 3) {
  # Calculate mean across spatial dimensions for each time step
  temp_mean <- apply(temp_data, 3, mean, na.rm=TRUE)
  
  # Check length of temp_mean
  length(temp_mean)
}

  # They match, create the data frame
plot_data <- data.frame(Date = dates, Temperature = temp_mean)
  
  # Convert from Kelvin to Celsius if needed (if mean temp is > 100)
  if(mean(temp_mean, na.rm=TRUE) > 100) {
    plot_data$Temperature <- plot_data$Temperature - 273.15
    temp_unit <- "°C"
  } else {
    temp_unit <- temp_units  # Use original units
}

# Create a simple plot
plot(plot_data$Date, plot_data$Temperature, type = "l",
     main = "Average Temperature in Timor-Leste",
     xlab = "Date", ylab = paste("Temperature (", temp_unit, ")", sep=""))  

# Save the data to a CSV file
write.csv(plot_data, "timor_leste_temperature_timeseries.csv", row.names = FALSE)

#Getting a range for average daily temperatures in 2051

# Step 1: Identify which indices correspond to the year 2051
year_2051_indices <- which(format(dates, "%Y") == "2051")
print(paste("Number of time steps in 2051:", length(year_2051_indices)))

# Step 2: Extract temperature data for just 2051
temp_2051 <- temp_data[, , year_2051_indices]

# Step 3: Calculate different types of temperature ranges
# Daily means across Timor-Leste (spatial average for each day)
daily_means <- apply(temp_2051, 3, mean, na.rm = TRUE)

# Convert to Celsius if needed
if(mean(daily_means, na.rm = TRUE) > 100) {
  daily_means <- daily_means - 273.15
}

# Calculate range metrics
annual_mean <- mean(daily_means)
annual_min <- min(daily_means)
annual_max <- max(daily_means)
annual_range <- annual_max - annual_min

# Calculate monthly averages
months_2051 <- as.numeric(format(dates[year_2051_indices], "%m"))
monthly_means <- tapply(daily_means, months_2051, mean)

# Calculate spatial variation (across grid cells) for the annual average
spatial_mean <- mean(apply(temp_2051, c(1, 2), mean, na.rm = TRUE), na.rm = TRUE)
if(spatial_mean > 100) spatial_mean <- spatial_mean - 273.15
spatial_sd <- sd(apply(temp_2051, c(1, 2), mean, na.rm = TRUE), na.rm = TRUE)
if(spatial_sd > 100) spatial_sd <- spatial_sd - 273.15

# Print results
cat("Projected temperature range for Timor-Leste in 2051 (SSP5-8.5):\n")
cat("Annual mean temperature:", round(annual_mean, 2), "°C\n")
cat("Annual minimum daily mean:", round(annual_min, 2), "°C\n")
cat("Annual maximum daily mean:", round(annual_max, 2), "°C\n")
cat("Annual temperature range:", round(annual_range, 2), "°C\n")
cat("Spatial variation (SD):", round(spatial_sd, 2), "°C\n\n")

# Create monthly temperature range plot
month_data <- data.frame(
  Month = factor(month.abb[as.numeric(names(monthly_means))], levels = month.abb),
  Temperature = monthly_means
)

ggplot(month_data, aes(x = Month, y = Temperature)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label = round(Temperature, 1)), vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(title = "Projected Monthly Mean Temperatures for Timor-Leste in 2050",
       subtitle = "Based on ISIMIP SSP5-8.5 scenario",
       y = "Temperature (°C)",
       x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a daily temperature range plot
day_data <- data.frame(
  Day = dates[year_2051_indices],
  Temperature = daily_means
)

ggplot(day_data, aes(x = Day, y = Temperature)) +
  geom_line() +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "Daily Mean Temperatures for Timor-Leste in 2050",
       subtitle = "With seasonal trend line",
       y = "Temperature (°C)",
       x = "Date in 2051") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month")
