#### Preparation #### ------------------------------

# Define the packages to be used
packages <- c("tidyverse", "sf", "ggspatial", "prettymapr", "maptiles", "grid", "magick", "ggspatial")

# Function to check if packages are installed and load them
load_packages <- function(pkgs) {
  # Check for missing packages
  missing_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  
  # Install missing packages
  if (length(missing_pkgs)) {
    install.packages(missing_pkgs)
  }
  
  # Load all packages
  lapply(pkgs, library, character.only = TRUE)
}

# Load the packages
load_packages(packages)

#### Loading data #### ------------------------------

# Load data about buildings in the Netherlands
data_buildings <- st_read("raw_data/bag-light.gpkg") # Link: https://service.pdok.nl/lv/bag/atom/downloads/bag-light.gpkg 

# Load data about grid cells in the Netherlands
data_grid_cells <- st_read("raw_data/cbs_vk100_2023_v1-2.gpkg") # Link: https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/kaart-van-100-meter-bij-100-meter-met-statistieken

# Load Rbanism logo
rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg') # Download our logo

#### Calculate the share of social housing per grid cells #### ------------------------------

# Correct missing values about social housing in the grid cells
data_grid_cells <- data_grid_cells %>%
  mutate(aantal_huurwoningen_in_bezit_woningcorporaties = replace(aantal_huurwoningen_in_bezit_woningcorporaties, aantal_huurwoningen_in_bezit_woningcorporaties == "-99997", NA))

# Correct missing values about dwellings in the grid cells
data_grid_cells <- data_grid_cells %>%
  mutate(aantal_woningen = replace(aantal_woningen, aantal_woningen == "-99997", NA))

# Calculate the share of social housing per grid cell
data_grid_cells <- data_grid_cells %>%
  mutate(Share_social_housing = aantal_huurwoningen_in_bezit_woningcorporaties / aantal_woningen)

# Keep only the relevant columns
data_grid_cells <- data_grid_cells %>%
  select(crs28992res100m, geom, Share_social_housing)

#### Assign grid cell data to the buildings #### ------------------------------

# Assign the data of grid cells to buildings based on the centroids of the building
data_buildings <- st_join(data_buildings, data_grid_cells["Share_social_housing"])

#### Plot the share of social housing in Amsterdam #### ------------------------------

# Define an expanded bounding box for a wider area around the center of Amsterdam in EPSG:28992
bbox_amsterdam_expanded <- st_bbox(c(
  xmin = 118000, ymin = 485000, xmax = 125000, ymax = 492000
), crs = st_crs(data_buildings))

# Crop the buildings data to the expanded bounding box
data_buildings_amsterdam_expanded <- st_crop(data_buildings, bbox_amsterdam_expanded)

# Download the CartoDB DarkMatter basemap using maptiles
dark_basemap <- get_tiles(
  bbox_amsterdam_expanded,
  provider = "CartoDB.DarkMatter",
  crop = TRUE,
  zoom = 14
)

# Plot the map
ggplot() +
  layer_spatial(dark_basemap) + # Add the basemap
  geom_sf(data = data_buildings_amsterdam_expanded, aes(fill = Share_social_housing), color = NA) + # Add the buildings layer
  scale_fill_gradient(low = "white", high = "#0C7BDC", na.value = "white") + # Set the colour gradient
  theme_minimal() + # Simplify the theme
  theme(
    legend.position = "bottom", # Move legend below the plot
    axis.title.x = element_text(size = 10), # Customize x-axis title
    axis.title.y = element_text(size = 10), # Customize y-axis title
    axis.text = element_text(size = 8),     # Customize axis text
    panel.grid = element_blank(),  # Remove grid lines
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 0.5)),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5) # Reduce top margin to move title closer to the plot
    # Center the title and adjust size
  ) +
  labs(
    title = "Social housing in Amsterdam",
    fill = "Share of Social Housing",
    x = NULL,
    y = NULL, 
    caption = "#30DayMapChallenge. Map by Javier San Millán, 2024. Data: CBS, PDOK-Kadaster, Carto")

# Add the logo using grid.raster
  grid.raster(rbanism_logo, x = 0.9, y=0.9,  # x and y determine the position of the logo (top right)
              width = unit(100, "points")) 



