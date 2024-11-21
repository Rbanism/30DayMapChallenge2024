# Step 1: Load Necessary Libraries
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(magick)
library(grid)
library(maps)
library(cowplot)

# Step 2: Load Conflict Data
conflict_data <- read.csv("USA_Canada_2020_2024_Nov15.csv")

# Filter to only include contiguous U.S.
conflict_data_filtered <- conflict_data %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  filter(longitude > -125 & longitude < -66.5 & latitude > 24 & latitude < 50)  # Contiguous US bounds

# Convert to an sf object
conflict_points <- st_as_sf(conflict_data_filtered, coords = c("longitude", "latitude"), crs = 4326)

# Step 3: Load County Shapefile
counties <- st_read("tl_2020_us_county.shp")

# Filter counties for the contiguous U.S.
contiguous_counties <- counties %>%
  filter(!STATEFP %in% c("02", "15")) %>%  # Exclude Alaska (02) and Hawaii (15)
  st_transform(crs = 4326)

# Step 4: Load State Boundaries from `maps`
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  st_transform(crs = 4326)  # Ensure CRS matches

# Step 5: Load Population Data
# Extract the FIPS code from STATE and COUNTY and select the population column
population_data <- read.csv("co-est2020-alldata.csv") %>%
  mutate(FIPS = sprintf("%02d%03d", STATE, COUNTY)) %>%  # Create FIPS column
  select(FIPS, POPESTIMATE2020)  # Use the most recent population estimate

# Join population data with county geometries
contiguous_counties <- contiguous_counties %>%
  left_join(population_data, by = c("GEOID" = "FIPS"))

# Step 6: Count Events Per County
# Spatial join: Assign conflict points to counties
event_counts <- conflict_points %>%
  st_join(contiguous_counties) %>%
  group_by(GEOID) %>%
  summarise(event_count = n(), .groups = "drop")

# Convert event_counts to a data frame
event_counts_df <- event_counts %>%
  st_drop_geometry()

# Join event counts with counties using a left join
contiguous_counties <- contiguous_counties %>%
  left_join(event_counts_df, by = "GEOID") %>%
  mutate(
    event_per_capita = (event_count / POPESTIMATE2020) * 10000,  # Events per 10,000 people
    event_per_capita = ifelse(is.na(event_per_capita), 0, event_per_capita)  # Replace NAs with 0
  )

# Step 7: Apply Logarithmic Transformation
contiguous_counties <- contiguous_counties %>%
  mutate(log_event_per_capita = log1p(event_per_capita))  # Log transform for better visibility

# Step 8: Load Rbanism Logo
rbanism_logo <- image_read("https://rbanism.org/assets/imgs/about/vi_l.jpg")

# Step 9: Create Density Map
log_density_map <- ggplot() +
  # County-level data
  geom_sf(
    data = contiguous_counties,
    aes(fill = log_event_per_capita),
    color = "grey90",  # Subtle light grey for county borders
    size = 0.00001  # Thinner lines for boundaries
  ) +
  # State boundaries
  geom_sf(
    data = states,
    color = "grey90",
    fill = NA,
    size = 0.0001
  ) +
  scale_fill_viridis(
    name = "Events per 10,000 People (Log)",
    option = "C",
    na.value = "grey100",
    guide = guide_colorbar(
      barwidth = 15,
      barheight = 0.8,
      title.position = "top",
      title.hjust = 0.5
    ),
    breaks = log1p(c(0, 10, 50, 100, 500)),
    labels = c("0", "10", "50", "100", "500")
  ) +
  labs(
    title = "Socio-Political Conflict Event Density Across Contiguous U.S. (2020–2024)",
    subtitle = "#30DayMapChallenge2024",
    caption = "Data Source: ACLED, US Census | Author: Brianna E. Cole | #Rbanism",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(xlim = c(-125, -66.5), ylim = c(24, 50), expand = FALSE) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 10, face = "bold"),
    panel.grid = element_blank()
  )

# Step 10: Overlay Logo in Bottom-Right Corner
final_map <- ggdraw() +
  draw_plot(log_density_map) +  # Add the main map
  draw_image(
    rbanism_logo,
    x = 0.38,  # Adjust X position for bottom-right corner
    y = -0.12,  # Adjust Y position for bottom-right corner
    scale = 0.13  # Adjust logo size
  )

# Display the final map with logo
print(final_map)

# Save the final map with logo
ggsave("Day21_Conflict.png",
       plot = final_map, width = 18, height = 12, dpi = 500)
