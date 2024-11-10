library(terra)
library(ggplot2)
library(sf)
library(dplyr)
library(jpeg)
library(grid)
library(cowplot)
library(magick)


# Load the raster file 
raster_data <- rast("AVG_LST_2024.tif")

# Reproject to EPSG:3995
system.time({
  raster_epsg3995 <- project(raster_data, "EPSG:3995", method = "cubic")
})

# Define Arctic extent for mapping 
arctic_extent <- ext(-5000000, 5000000, -5000000, 5000000)  # Coordinates for EPSG:3995 around the Arctic

# Crop the reprojected raster to the Arctic extent
cropped_raster <- crop(raster_epsg3995, arctic_extent)

# Aggregate the cropped raster to reduce resolution
cropped_raster <- aggregate(cropped_raster, fact = 2, fun = mean)



# Convert to data frame for plotting
raster_df <- as.data.frame(cropped_raster, xy = TRUE, na.rm = TRUE)
colnames(raster_df)[3] <- "LST"

# Convert LST from Kelvin to Celsius
raster_df$LST <- raster_df$LST - 273.15

# Load and process fire data
fire_data <- read.csv("FIRE_LOC_ARC.csv")   # Load the fire CSV file
fire_data_sf <- st_as_sf(fire_data, coords = c("longitude", "latitude"), crs = 4326)  # Original CRS WGS84
fire_data_sf <- st_transform(fire_data_sf, crs = 3995)  # Transform to EPSG:3995

# Convert fire data to a data frame for plotting with coordinates and brightness
fire_data_transformed <- as.data.frame(st_coordinates(fire_data_sf))
fire_data_transformed$brightness <- fire_data$brightness

# Load and process the Arctic Circle shapefile
arctic_circle <- st_read("ne_10m_geographic_lines.shp")  # Load shapefile

# Check if Arctic Circle shapefile is already in EPSG:3995 and transform if needed
if (st_crs(arctic_circle)$epsg != 3995) {
  arctic_circle <- st_transform(arctic_circle, crs = 3995)
}

# Load the logo image
logo <- readJPEG("vi_l.jpg")

# Step 1: Create the main plot with the spherical projection (EPSG:3995)
main_plot <- ggplot() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = LST)) +
  scale_fill_gradientn(colors = c("#E0B8E4", "#E2A3E8", "#93278F", "#4D004B"), name = "LST (°C)") +
  
  geom_sf(data = fire_data_sf, aes(color = brightness, size = brightness), alpha = 0.6) +
  scale_color_gradientn(colors = c("#FFE0CC", "#F7931E"), name = "Fire Brightness (K)") +
  scale_size_continuous(name = "Fire Brightness", range = c(0.5, 1.5)) +
  geom_sf(data = arctic_circle, color = "#00A99D", size = 1.5, linetype = 'dashed') +
  
  # Set projection to EPSG:3995 to keep the spherical appearance
  coord_sf(crs = st_crs(3995), xlim = c(-3000000, 3000000), ylim = c(-3000000, 3000000)) +
  
  # Adjust labels for longitude and latitude
  labs(x = "Longitude", y = "Latitude", title = "Arctic Land Surface Temperature (LST) with 2024 Fire Data") +
  
  # Apply theme with a white panel background
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#E0F7FA", color = NA),  # Light blue background for the panel
    plot.background = element_rect(fill = "white", color = NA),     # White background for the entire plot area
    plot.margin = margin(10, 10, 40, 10)
  )


# Save the plot
ggsave("arctic_lst_with_fire_and_circle_no_bg.png", plot = main_plot, width = 12, height = 8, dpi = 300)

#Load the saved plot image
final_plot <- image_read("arctic_lst_with_fire_and_circle_no_bg.png")


# Step 3: Create a white background with the same dimensions as the plot
white_bg <- image_blank(width = image_info(final_plot)$width, 
                        height = image_info(final_plot)$height, 
                        color = "white")

# Composite the final plot onto the white background
final_image <- image_composite(white_bg, final_plot)

#\ Load and scale down the logo
rbanism_logo <- image_read("vi_l.jpg") %>%
  image_scale("300x300")  # Adjust the scale as needed

# \Overlay the logo in the top-right corner
final_image <- image_composite(
  final_image,
  rbanism_logo,
  offset = "+3250+50"  # Adjust this to position in top-right, relative to the image size
)

# Add text annotation in the bottom-left corner
final_image <- image_annotate(
  final_image,
  "#30DayMapChallenge, Map by Shawn Tew 2024, Data source: NASA MODIS via NASA Earthdata, NASA FIRMS",
  gravity = "southwest",
  color = "gray40",
  size = 30,       # Adjust font size as needed
  location = "+20+20"  # Offset to position text slightly in from the corner
)

# Save the final composed image with annotation and logo
image_write(final_image, "temperature_fire_map_with_logo_annotation.png")

