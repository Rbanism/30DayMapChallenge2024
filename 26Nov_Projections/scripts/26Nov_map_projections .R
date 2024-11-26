# Load required libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(patchwork)
library(showtext)
library(magick)
library(cowplot)

# Load the world dataset
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter the Netherlands
netherlands <- world %>% dplyr::filter(name == "Netherlands")

# Define Bonne projections
bonne_proj_europe <- "+proj=bonne +lat_1=45 +lon_0=0 +datum=WGS84"

# Reproject the polygons to Bonne projection
world_bonne_europe <- st_transform(world, crs = bonne_proj_europe)
netherlands_bonne <- st_transform(netherlands, crs = bonne_proj_europe)

# Generate latitude and longitude grid lines
lat_lon_grid <- st_graticule(ndiscr = 50)
lat_lon_grid_europe <- st_transform(lat_lon_grid, crs = bonne_proj_europe)

# Download Rbanism logo and convert to rasterGrob
rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg')
logo_grob <- rasterGrob(as.raster(rbanism_logo), interpolate = TRUE)

# Add Google font
font_add_google("Roboto Mono", "roboto_mono")
showtext_auto()

# Plot Bonne projection map with Netherlands highlighted
map_europe <- ggplot() +
  geom_sf(data = lat_lon_grid_europe, color = "grey70", alpha = 0.8, size = 0.2) +
  geom_sf(data = world_bonne_europe, fill = "#00A99D", color = "white", size = 0.3) +
  geom_sf(data = netherlands_bonne, fill = "#F7931E", color = "white", size = 0.4) +  # Highlight Netherlands
  labs(
    title = "Day 26. Map Projection\n World Map in Bonne Projection",
    caption = "#30DayMapChallenge. Hyeji Joh, 2024. Data: 'rnaturalearth' package."
  ) +
  theme_void() +
  theme(
    plot.title = element_text(family = "roboto_mono", size = 60, face = "bold", hjust = 0.5, vjust = -0.5),
    plot.caption = element_text(family = "roboto_mono", size = 30, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),  # White panel background
    plot.background = element_rect(fill = "white", color = NA)   # White image background
  )

# Add the logo directly into the map layout
map_with_logo <- ggdraw(map_europe) +
  draw_grob(logo_grob, x = 0.85, y = 0.85, width = 0.15, height = 0.15)

# Save the final plot with the logo and white background
ggsave("26Nov_map_projections.png", plot = map_with_logo, width = 10, height = 10, dpi = 300, bg = "white")
