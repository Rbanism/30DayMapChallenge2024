# Load libraries
library(terra)
library(ggplot2)
library(readr)
library(dplyr)
library(sf)
library(magick)
library(grid)

# Define target CRS for Arctic (EPSG:3995) for consistency
target_crs <- "EPSG:3995"

# Step 1: Load preprocessed LST raster (already reprojected to EPSG:3995)
avg_day_raster <- rast("epsg3995.tif")  # Load the preprocessed raster file

# Convert raster to a data frame for plotting
avg_day_df <- as.data.frame(avg_day_raster, xy = TRUE, na.rm = TRUE)
colnames(avg_day_df) <- c("longitude", "latitude", "temperature")
avg_day_df$temperature <- avg_day_df$temperature - 273.15  # Convert temperature to Celsius if needed

# Step 2: Load and process fire data (transforming to EPSG:3995)
fire_data <- read_csv("fire_archive_M-C61_537241.csv")
fire_data_sf <- st_as_sf(fire_data, coords = c("longitude", "latitude"), crs = 4326)  # Original CRS
fire_data_sf <- st_transform(fire_data_sf, crs = target_crs)  # Transform to EPSG:3995

# Convert fire data to a data frame with coordinates
fire_data_transformed <- as.data.frame(st_coordinates(fire_data_sf))
fire_data_transformed$brightness <- fire_data$brightness

# Calculate quantiles for fire brightness to use in plotting
fire_quantiles <- quantile(fire_data_transformed$brightness, probs = seq(0, 1, 0.25), na.rm = TRUE)

# Step 3: Load preprocessed Arctic Circle shapefile (already in EPSG:3995)
arctic_circle <- st_read("ne_10m_geographic_lines_converted.shp")  # Load preprocessed shapefile

# Step 4: Create the main map plot
map_plot <- ggplot() +
  # Plot the LST data with a reversed purple gradient for temperature
  geom_tile(data = avg_day_df, aes(x = longitude, y = latitude, fill = temperature)) +
  scale_fill_gradientn(colors = c("#F8E1FB", "#E2A3E8", "#93278F", "#4D004B"), 
                       name = "Temperature (°C)") +
  # Add fire points, with color mapped to brightness using quantile-based breaks
  geom_point(data = fire_data_transformed, aes(x = X, y = Y, color = brightness), size = 1, alpha = 0.7) +
  scale_color_gradientn(colors = c("#FFE0CC", "#F7931E"), 
                        name = "Fire Intensity (K)", 
                        limits = range(fire_quantiles),
                        breaks = fire_quantiles,
                        labels = round(fire_quantiles, 0)) +
  # Add Arctic Circle as a vector line
  geom_sf(data = arctic_circle, color = "#00A99D", linetype = "dashed") +
  coord_sf(crs = target_crs) +  
  theme_minimal() +
  labs(title = "Arctic Land Surface Temperature and Fire Events in 2024", 
       x = "Longitude", y = "Latitude") +
  theme(text = element_text(family = "sans"))

#save plot
ggsave("temperature_fire_map_full_extent.png", plot = map_plot, width = 10, height = 10, dpi = 200)


# Create a white background with the same dimensions as the final plot
white_bg <- image_blank(width = image_info(final_plot)$width, 
                        height = image_info(final_plot)$height, 
                        color = "white")

# Composite the final plot onto the white background
final_image <- image_composite(white_bg, final_plot)

# Load and scale down the logo (adjust the path and scale as needed)
rbanism_logo <- image_read("vi_l.jpg") %>%
  image_scale("100x100")

# Combine the main plot and logo, positioning the logo in the top-right corner above the scales
final_image <- image_composite(
  final_image,
  rbanism_logo,
  offset = "+2850-50"  # Adjust the offset to position it in the top-right corner
)

# Add text annotation in the bottom-left corner
final_image <- image_annotate(
  final_image,
  "#30DayMapChallenge, Map by Shawn Tew 2024, Data source: NASA MODIS via NASA Earthdata, NASA FIRMS",
  gravity = "southwest",
  color = "gray40",
  size = 20,  # Adjust font size as needed
  location = "+20+20"  # Adjust the location offset if needed
)

# Save the final composed image with annotation and logo
image_write(final_image, "temperature_fire_map_with_logo_annotation.png")


#TESTING

# Load necessary libraries
library(ggplot2)
library(terra)

# Load  raster (assuming it's in geographic coordinates, like WGS84) test
arctic_raster <- rast("tiles/MOD11A1.061_LST_Day_1km_doy2023304_aid0001.tif")

# Convert to data frame for plotting
arctic_df <- as.data.frame(arctic_raster, xy = TRUE, na.rm = TRUE)
colnames(arctic_df) <- c("longitude", "latitude", "value")  # Adjust column names if necessary

# Plot using ggplot2 with a polar projection look
ggplot() +
  geom_tile(data = arctic_df, aes(x = longitude, y = latitude, fill = value)) +
  scale_fill_viridis_c(name = "Temperature") +  # Adjust color palette as needed
  coord_map("orthographic", orientation = c(90, 0, 0)) +  # Approximate polar look
  theme_minimal() +
  labs(title = "Arctic Region")

# Save the plot
ggsave("arctic_temperature_map2.png", width = 10, height = 10, dpi = 100)



library(ggplot2)
library(terra)

# Load  raster (make sure it’s in geographic coordinates, like WGS84)
arctic_raster <- rast("epsg3995.tif") # Replace with your file path

# Check column names after converting the raster to a data frame
arctic_df <- as.data.frame(arctic_raster, xy = TRUE, na.rm = TRUE)
print(colnames(arctic_df))


# Convert to data frame for plotting
arctic_df <- as.data.frame(arctic_raster, xy = TRUE, na.rm = TRUE)
colnames(arctic_df) <- c("longitude", "latitude", "value")  # Adjust column names if needed

# Custom proj4string for Arctic-centered orthographic projection
custom_proj <- "+proj=ortho +lat_0=90 +lon_0=0 +datum=WGS84"

# Plot using ggplot2 with custom projection test
arctic_plot <- ggplot() +
  geom_tile(data = arctic_df, aes(x = longitude, y = latitude, fill = value)) +
  scale_fill_viridis_c(name = "Temperature") +  # test
  coord_sf(crs = custom_proj, expand = FALSE) +  # Apply custom projection
  theme_minimal() +
  labs(title = "Arctic Land Surface Temperature") +
  theme(panel.background = element_rect(fill = "lightblue"))

# Display plot
print(arctic_plot)

# Save the plot as a PNG file
ggsave("arctic_temperature_map_custom_projection.png", plot = arctic_plot, width = 10, height = 10, dpi = 300)



