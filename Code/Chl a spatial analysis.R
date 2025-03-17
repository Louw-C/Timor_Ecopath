# Alternative approach to analyze NetCDF chlorophyll data without the raster package
# Install packages if needed
if (!require("ncdf4")) install.packages("ncdf4")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("stars")) install.packages("stars")
if (!require("viridis")) install.packages("viridis")

# Load libraries
library(ncdf4)      # For reading NetCDF files
library(tidyverse)  # For data manipulation and plotting
library(stars)      # For spatial data without raster
library(viridis)    # For nice color palettes

# Set your file path - update this with your actual file location
netcdf_file <- "/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Chla data/MODISA_L3m_CHL_Time-Averaged_2019-2024.nc"

# Basic exploration of NetCDF structure
# Open the NetCDF file
nc <- nc_open(netcdf_file)

# Print structure to understand the file's contents
print(nc)

# List the variables in the file
var_names <- names(nc$var)
cat("Variables in this NetCDF file:", paste(var_names, collapse = ", "), "\n\n")

# List dimensions
dim_names <- names(nc$dim)
cat("Dimensions in this NetCDF file:", paste(dim_names, collapse = ", "), "\n\n")

# Assuming chlorophyll is named "chlor_a" - adjust if your variable has a different name
chl_var_name <- "MODISA_L3m_CHL_Mo_4km_R2022_0_chlor_a"
if(!(chl_var_name %in% var_names)) {
  cat("Variable", chl_var_name, "not found. Please check the variable names listed above.\n")
  cat("You may need to update the chl_var_name variable in this script.\n")
  # Stop execution if variable not found
  stop("Chlorophyll variable not found")
}

# Get variable attributes
chl_attributes <- ncatt_get(nc, chl_var_name)
cat("Chlorophyll variable attributes:\n")
print(chl_attributes)

# Extract coordinate information
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
cat("Longitude range:", min(lon), "to", max(lon), "\n")
cat("Latitude range:", min(lat), "to", max(lat), "\n\n")

# Check if there's a time dimension
has_time <- "time" %in% dim_names
if(has_time) {
  time <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")
  cat("Time units:", time_units$value, "\n")
  cat("Number of time steps:", length(time), "\n\n")
}

# Read the data using stars package
# This approach avoids using the raster package entirely
chl_stars <- read_ncdf("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/WorldFish/Ikan Ba Futura_2023/Science and colabs/Ecological Modeling/Timor-Leste South Coast/Timor_South_Ecopath/Timor_Ecopath/Data/Chla data/MODISA_L3m_CHL_Time-Averaged_2019-2024.nc")

# Examine the stars object
print(chl_stars)

# Create a basic plot using stars
plot_title <- "Chlorophyll a Concentration in Timor-Leste Waters"

# Basic plot
plot(chl_stars, main = plot_title)

# Convert to a data frame for more flexible plotting with ggplot2
# We'll first need to extract the data and reshape it
# If the data has multiple time points, this gets the first one:

# Check how many dimensions we have
ndims <- length(dim(chl_stars[[1]]))

if(ndims == 2) {
  # 2D array - just coordinates (lat, lon)
  chl_df <- as.data.frame(chl_stars, center = FALSE)
} else if(ndims == 3 && has_time) {
  # 3D array with time - get first time point
  cat("Extracting first time point from the data\n")
  chl_stars_t1 <- chl_stars[,,,1]
  chl_df <- as.data.frame(chl_stars_t1, center = FALSE)
} else {
  # Some other structure - try a general approach
  chl_df <- as.data.frame(chl_stars, center = FALSE)
}

# We have a problem where the chlorophyll values include the units as a string
# Let's properly extract and clean the data
cat("\nCleaning chlorophyll values that contain units...\n")

# The current format is "NA [mg/m^3]" or some value like "0.123 [mg/m^3]"
# We need to extract just the numeric part
clean_chl <- function(x) {
  # If the value is already NA, return NA
  if(is.na(x)) return(NA)
  
  # Otherwise, try to extract the number
  # First, remove the " [mg/m^3]" part
  num_str <- sub(" \\[mg/m\\^3\\]$", "", x)
  
  # Convert to numeric, which will give NA if it's "NA" or not a valid number
  as.numeric(num_str)
}

# Create a new data frame with the fixed values
chl_df_fixed <- data.frame(
  lon = chl_df$lon,
  lat = chl_df$lat,
  chl = sapply(as.character(chl_df$MODISA_L3m_CHL_Mo_4km_R2022_0_chlor_a), clean_chl)
)

cat("\nFixed data frame structure:\n")
str(chl_df_fixed)

# Basic statistics on the cleaned data
cat("\nSummary statistics for chlorophyll a:\n")
print(summary(chl_df_fixed$chl))

# Now clean the data by removing NAs - avoid using filter()
# Instead use base R subsetting
chl_df_clean <- chl_df_fixed[!is.na(chl_df_fixed$chl), ]

cat("\nCleaned data frame structure (NAs removed):\n")
str(chl_df_clean)
cat("Number of rows after removing NAs:", nrow(chl_df_clean), "\n")

# Create plot - using log scale for chlorophyll which is often appropriate
# Using geom_tile() instead of geom_raster() to avoid uneven interval warnings
ggplot(chl_df_clean, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = chl)) +
  scale_fill_viridis(
    name = "Chl a (mg/m³)",
    option = "viridis",
    trans = "log10",  # log scale often works better for chl-a
    na.value = "transparent"
  ) +
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "Chlorophyll a Concentration in Timor-Leste Waters",
    subtitle = "Data from NASA Giovanni",
    x = "Longitude",
    y = "Latitude"
  )

# Create north and south coast zones based on latitude
# For Timor-Leste, we can use a simple latitude divider
# The island runs roughly east-west, so we can split by latitude

# Determine a dividing latitude
# This is approximate - you may want to adjust based on local knowledge
dividing_latitude <- -8.85  # Approximate middle latitude through Timor-Leste

# Create a new zone column using base R
chl_df_zones <- chl_df_clean
chl_df_zones$zone <- ifelse(chl_df_zones$lat > dividing_latitude, "North Coast", "South Coast")

# Check the distribution of points in each zone
zone_counts <- table(chl_df_zones$zone)
cat("\nNumber of data points in each zone:\n")
print(zone_counts)

# Calculate zonal statistics using base R
# First, create an empty data frame for results
zone_names <- unique(chl_df_zones$zone)
zone_stats <- data.frame(
  zone = zone_names,
  min_chl = NA,
  max_chl = NA,
  mean_chl = NA,
  median_chl = NA,
  sd_chl = NA,
  n_pixels = NA
)

# Calculate statistics for each zone
for(i in 1:length(zone_names)) {
  zone_data <- chl_df_zones[chl_df_zones$zone == zone_names[i], ]
  zone_stats$min_chl[i] <- min(zone_data$chl, na.rm = TRUE)
  zone_stats$max_chl[i] <- max(zone_data$chl, na.rm = TRUE)
  zone_stats$mean_chl[i] <- mean(zone_data$chl, na.rm = TRUE)
  zone_stats$median_chl[i] <- median(zone_data$chl, na.rm = TRUE)
  zone_stats$sd_chl[i] <- sd(zone_data$chl, na.rm = TRUE)
  zone_stats$n_pixels[i] <- nrow(zone_data)
}

# Display zone statistics
cat("\nZonal Statistics for Chlorophyll a:\n")
print(zone_stats)

# Visualize north-south zones with ggplot2
ggplot(chl_df_zones, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = chl)) +
  scale_fill_viridis(
    name = "Chl a (mg/m³)",
    option = "viridis",
    trans = "log10",
    na.value = "transparent"
  ) +
  geom_hline(yintercept = dividing_latitude, 
             color = "red", linetype = "dashed", size = 1) +
  annotate("text", 
           x = c(min(chl_df_clean$lon) + lon_range/4, 
                 min(chl_df_clean$lon) + 3*lon_range/4),
           y = c(dividing_latitude + 0.5, dividing_latitude - 0.3),
           label = c("North Coast", "South Coast"),
           color = "black", fontface = "bold", size = 5) +
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "Chlorophyll a Concentration in Timor-Leste Watera",
    subtitle = "Time averaged 2019-2024",
    x = "Longitude",
    y = "Latitude"
  )

# Close the NetCDF file
nc_close(nc)

# If you have catch data, you can add analysis code here to correlate
# chlorophyll with catch according to Link & Watson approach
