#install.packages(c("ggplot2", "dplyr", "sf", "ggfx", "showtext", "ggtext","osmdata"))
# Load necessary libraries
library(osmdata)     # For OpenStreetMap data
library(sf)          # For handling spatial data
library(ggplot2)     # For plotting
library(dplyr)       # For data manipulation
library(ggfx)        # For shadows and aesthetic effects
library(showtext)    # For custom fonts
library(ggtext)      # For rich text and title customization
library(cowplot)
library(magick)

# Add a custom font for the aesthetic
showtext_auto()
font_add_google("Roboto Mono", "roboto")

# Define a bounding box around OpenAI Headquarters
bbox <- c(-122.423458,37.754112,-122.403202,37.768143)

# Download data from OpenStreetMap
osm_data_buildings <- opq(bbox = bbox) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()

osm_data_roads <- opq(bbox = bbox) %>%
  add_osm_feature("highway", c("motorway", "primary", "secondary", "tertiary","residential", "living_street", "unclassified", "service", "footway")) %>%
  osmdata_sf()

# Define the coordinates for "ChatGPT house"
parent_coords <- data.frame(
  lon = -122.41462716939638,
  lat = 37.76236783999394
)

# Set the CRS for osm_data_buildings$osm_polygons
osm_data_buildings$osm_polygons <- st_set_crs(osm_data_buildings$osm_polygons, 4326) # Assuming WGS84 CRS

# Ensure both objects have the same CRS
point <- st_sfc(st_point(c(parent_coords$lon, parent_coords$lat)), crs = st_crs(osm_data_buildings$osm_polygons))

# Transform the CRS of the point to match osm_data_buildings$osm_polygons
point <- st_transform(point, st_crs(osm_data_buildings$osm_polygons))

# Calculate the distance
osm_data_buildings$osm_polygons$distance <- st_distance(osm_data_buildings$osm_polygons, point)

# Find the building closest to the coordinates
parent_building <- osm_data_buildings$osm_polygons %>% slice_min(distance, n = 1)

# Define the bounding box coordinates of the map
bbox <- st_bbox(osm_data_buildings$osm_polygons)

# Plot the futuristic map 
p <- ggplot() +
  # Roads with glow effect
  with_shadow(
    geom_sf(data = osm_data_roads$osm_lines, inherit.aes = FALSE, color = "dodgerblue", size = 0.8, alpha = 0.7),
    sigma = 10, x_offset = 1, y_offset = 1, color = "deepskyblue"
  ) +
  
  # Buildings with gradient glow
  with_shadow(
    geom_sf(data = osm_data_buildings$osm_polygons,  inherit.aes = FALSE, fill = "purple", color = "magenta", size = 0.1, alpha = 0.9),
    sigma = 8, x_offset = 2, y_offset = 2, color = "violet"
  ) +
  
  # Highlight the parent building
  with_shadow(
    geom_sf(data = parent_building, inherit.aes = FALSE, fill = "cyan", color = "darkcyan", size = 0.3, alpha = 0.9),
    sigma = 8, x_offset = 2, y_offset = 2, color = "cyan"
  ) +
  
  # Label the parent building
  geom_sf_text(data = parent_building, aes(label = "ChatGPT's Home"), color = "white", size = 20, nudge_y = 0.0005, fontface = "bold") +
  
  # Customize title, subtitle, and caption with futuristic style
  labs(
    title = "Day 9. AI Only - The Futuristic OpenAI Neighborhood",
    subtitle = "San Francisco, CA - OpenAI HQ Neighbourhood in Digital Aesthetics",
    caption = "Map created with OpenStreetMap data by ChatGPT"
  ) +
  
  # Apply custom theme for a minimalist, digital look
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#212020", color = NA),
    panel.background = element_rect(fill = "#212020", color = NA),
    plot.title = element_markdown(
      hjust = 0.5, 
      margin = margin(b = 10),
      face = "bold",
      size = 70,
      color = "deepskyblue",
      family = "roboto",
      lineheight = 1.2
    ),
    plot.subtitle = element_text(hjust = 0.5, color = "gray80", size = 50),
    plot.caption = element_text(hjust = 0.5, color = "gray60", size = 40)
  ) +
  
  # Limit the extent of the map to the bounding box
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]))


# Save the map
ggsave("futuristic_map.png", plot = p, width = 10, height = 10, dpi = 300)

# Load the saved map plot
map_image <- image_read("futuristic_map.png")

# Load the logo image
logo <- image_read(file.path(here::here(), "rbanism_logo_white.png"))

# Combine the roughsf plot and the logo
combined_plot <- ggdraw() +
  draw_image(map_image, 0, 0, 1, 1) +
  draw_image(logo, x = 0.8, y = 0.05, width = 0.1, height = 0.1) +
  draw_label(
    "#30DayMapChallenge. Ignacio Urria Yáñez, 2024.\n Code created by ChatGPT when prompted to map his neighbourhood (openAI HQ). The code was then checked and corrected by the author.", 
    x = 0.5, 
    y = 0.05, 
    size = 35, 
    hjust = 0.5, 
    color = "white"
  ) +
  theme(plot.background = element_rect(fill = "#212020", colour = NA))

# Save the final combined plot
ggsave("final_plot_AI.png", combined_plot, width = 10, height = 15)  