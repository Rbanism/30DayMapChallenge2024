#### Preparation #### -----------------------------------------------------------

# Define the packages to be used
packages <- c("tidyverse",
              "tidygeocoder",
              "sf",
              "rnaturalearth",
              "rnaturalearthdata",
              "spdep",
              "gganimate",
              "ggspatial",
              "magick",
              "grid")

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

#### Load and clean data #### -------------------------------------------------------------

# Download Rbanism logo
logo_path <- "raw_data/rbanism_logo_white.png"
logo_img <- png::readPNG(logo_path)
logo_grob <- rasterGrob(logo_img, x = 0.9, y = 0.3, width = 0.15, height = 0.25, just = c("right", "top"))

# Increase the timeout limit to 600 seconds (10 minutes)
options(timeout = 600)

# Load private jets data
data <- read.csv("https://private-jets.fra1.digitaloceanspaces.com/leg/v2/all/year=2023/data.csv") # Source: Gössling, S., Humpe, A., & Leitão, J. C. (2024). Private aviation is making a growing contribution to climate change. Communications Earth & Environment, 5(1), 666.

# Remove rows with invalid coordinates
data <- data %>%
  filter(start_lat >= -90 & start_lat <= 90 & start_lon >= -180 & start_lon <= 180)
data <- data %>%
  filter(end_lat >= -90 & end_lat <= 90 & end_lon >= -180 & end_lon <= 180)

# Transform all of them into spatial objects on the basis of the starting location of the flight
data_sf_start <- st_as_sf(data, coords = c("start_lon", "start_lat"), crs = 4326)

# Transform all of them into spatial objects on the basis of the end location of the flight
data_sf_end <- st_as_sf(data, coords = c("end_lon", "end_lat"), crs = 4326)

#### Prepare data to map private jets flights from or to Spain #### ----------------------------

# Get the polygon for Spain
spain <- ne_countries(scale = "medium", returnclass = "sf") %>% # Source: Massicotte P, South A (2024). rnaturalearth: World Map Data from Natural Earth. R package version 1.0.1.9000, https://github.com/ropensci/rnaturalearth, https://docs.ropensci.org/rnaturalearthhires/, https://docs.ropensci.org/rnaturalearth/.
  filter(admin == "Spain")

# Get the first-level administrative divisions (regions) for Spain
spain_regions <- ne_states(country = "Spain", returnclass = "sf")

# Set a bounding box to include only mainland Spain and the Balearic Islands
mainland_spain <- st_crop(spain, xmin = -10, xmax = 5, ymin = 35, ymax = 45)
spain_regions <- st_crop(spain_regions, xmin = -10, xmax = 5, ymin = 35, ymax = 45)

# Ensure the CRS of Spain matches your data
mainland_spain <- st_transform(mainland_spain, crs = 4326)
spain_regions <- st_transform(spain_regions, crs = 4326)

# Get indices of flights starting in Spain
start_indices <- which(st_intersects(data_sf_start, mainland_spain, sparse = FALSE)[,1])

# Create a spatial object for flights starting in Spain
flights_start_in_spain_sf <- data_sf_start[start_indices, ]

# Get indices of flights ending in Spain
end_indices <- which(st_intersects(data_sf_end, mainland_spain, sparse = FALSE)[,1])

# Create a spatial object for flights ending in Spain
flights_end_in_spain_sf <- data_sf_end[end_indices, ]

# Convert the 'start' column to Date format (keeping only day, month, and year)
flights_start_in_spain_sf_time <- flights_start_in_spain_sf %>%
  mutate(start = as.Date(ymd_hms(start)))  # This keeps only the date part

# Convert the 'start' column to Date format (keeping only day, month, and year)
flights_end_in_spain_sf_time <- flights_end_in_spain_sf %>%
  mutate(start = as.Date(ymd_hms(start)))  # This keeps only the date part

# Create new variables to distinguish between take-offs and landings
flights_start_in_spain_sf_time$type <- "Take-off"
flights_end_in_spain_sf_time$type <- "Landing"

# Bind the dataframes
flights_in_spain_sf_time <- bind_rows(flights_start_in_spain_sf_time, flights_end_in_spain_sf_time)

#### Animate distinguishing among take-offs and landing #### --------------------------------------------------------------

# Create the animated plot
animated_plot <- ggplot() +
  geom_sf(data = spain_regions, fill = "lightgray", color = "white", size = 0.3) + # Plot Spanish regions with a light fill color
  geom_sf(data = flights_in_spain_sf_time, aes(color = type), size = 8, alpha = 1) +
  scale_color_manual(
    values = c("Take-off" = "#0C7BDC", "Landing" = "#FFC20A"),
    labels = c("Take-off", "Landing"),
    name = NULL
  ) +
  theme_minimal() +
  labs(
    title = "Private jet flights across Spain: {frame_time}",
    x = NULL, 
    y = NULL,
    caption = "#30DayMapChallenge: 'A new tool'. Map by Javier San Millán, 2024. Data:\nGössling, Humpe & Cardoso Leitão (2024); Massicotte & South (2024)") +
  coord_sf(clip = "off") + # Prevent clipping
  theme(
    plot.background = element_rect(fill = "#2d2d2d", color = NA), # Dark background
    plot.title = element_text(face = "bold", color = "white", size = 32, hjust = 0.5),
    plot.caption = element_text(color = "white", size = 26, hjust = 0.5),
    plot.caption.position = "plot", 
    #legend.title = element_text(color = "white", size = 26, face = "bold"), 
    legend.text = element_text(color = "white", size = 26), # Large legend text
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "top", # Move legend below the map
    legend.direction = "horizontal", # Arrange legend items horizontally
    legend.box = "horizontal" # Keep legend box horizontal
  ) +
  transition_time(as.Date(start)) + # Animate based on the start date
  ease_aes('linear') + # Smooth transition
  annotation_custom(logo_grob)
  
# Animate and save the plot as a video (MP4 format)
anim_video <- animate(
  animated_plot, 
  nframes = 365, 
  fps = 10, 
  renderer = av_renderer("private_jet_flights_landing_and_taking-off.mp4"),
  bg = "#2d2d2d",  # Ensure background color matches the plot
  height = 1920,   # Height for vertical screen (e.g., Instagram Stories)
  width = 1080     # Width for vertical screen
)


#### Repeat the process for a gif #### ------------------------------------------
# Create the animated plot
animated_plot <- ggplot() +
  geom_sf(data = spain_regions, fill = "lightgray", color = "white", size = 0.3) + # Plot Spanish regions with a light fill color
  geom_sf(data = flights_in_spain_sf_time, aes(color = type), size = 8, alpha = 1) +
  scale_color_manual(
    values = c("Take-off" = "#0C7BDC", "Landing" = "#FFC20A"),
    labels = c("Take-off", "Landing"),
    name = NULL
  ) +
  theme_minimal() +
  labs(
    title = "Private jet flights across Spain: {frame_time}",
    x = NULL, 
    y = NULL,
    caption = "#30DayMapChallenge: 'A new tool'. Map by Javier San Millán, 2024. Data:\nGössling, Humpe & Cardoso Leitão (2024); Massicotte & South (2024)") +
  coord_sf(clip = "off") + # Prevent clipping
  theme(
    plot.background = element_rect(fill = "#2d2d2d", color = NA), # Dark background
    plot.title = element_text(face = "bold", color = "white", size = 32, hjust = 0.5),
    plot.caption = element_text(color = "white", size = 22, hjust = 0.5),
    plot.caption.position = "plot", 
    #legend.title = element_text(color = "white", size = 26, face = "bold"), 
    legend.text = element_text(color = "white", size = 26), # Large legend text
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "top", # Move legend below the map
    legend.direction = "horizontal", # Arrange legend items horizontally
    legend.box = "horizontal" # Keep legend box horizontal
  ) +
  transition_time(as.Date(start)) + # Animate based on the start date
  ease_aes('linear') + # Smooth transition
  annotation_custom(logo_grob)

# Animate and save the plot directly
anim <- animate(
  animated_plot, 
  nframes = 365, 
  fps = 10, 
  renderer = gifski_renderer("private_jet_flights_landing_and_taking-off.gif"),
  bg = "#2d2d2d", # Ensure background color matches the plot
  height = 600, width = 800
)
