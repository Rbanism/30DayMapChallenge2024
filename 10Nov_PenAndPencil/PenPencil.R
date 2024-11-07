library(sf)
library(magick)
library(cowplot)
library(pandoc)
library(dplyr)
library(ggplot2)

# Load cities and South America shapefiles
SA <- rnaturalearth::ne_countries(continent = "South America", returnclass = "sf")
cities <- rnaturalearth::ne_download(type = "populated_places", scale = 50)

# Cast to POLYGON or POINT
SA <- st_cast(SA,"POLYGON")
cities_point <- st_cast(cities, "POINT")

# Standardise projections to EPSG:4326
SA <- st_transform(SA, 4326)
cities_point <- st_transform(cities_point, 4326)

# Subset cities in South America
cities_point <- st_intersection(cities_point, SA)

# Subset cities with more than 500,000 inhabitants
cities_fil <- cities_point[cities_point$MAX_POP10 > 500000,]

# Set aesthetics for cities
cities_fil$color <- "#F7931E"
cities_fil$label <- cities_fil$NAME

# Labels
cities_fil$label_pos <- "e"
cities_fil <- cities_fil %>% 
  mutate(
    label_pos = 
      case_when(
        NAME == "Santiago" ~ "w",
        NAME == "Arequipa" ~ "w",
        NAME == "Arequipa" ~ "w",
        NAME == "Cartagena" ~ "s",
        NAME == "Barranquilla" ~ "w",
        NAME == "Medellín" ~ "w",
        NAME == "Cali" ~ "w",
        NAME == "Goiânia" ~ "w",
        NAME == "Campo Grande" ~ "w",
        NAME == "Maceió" ~ "w",
        NAME == "Teresina" ~ "w",
        NAME == "Recife" ~ "w",
        NAME == "Natal" ~ "w",
        NAME == "Fortaleza" ~ "n",
        NAME == "Salvador" ~ "w",
        NAME == "Curitiba" ~ "s",
        NAME == "São Paulo" ~ "s",
        NAME == "Campinas" ~ "n",
        NAME == "Sorocaba" ~ "w",
        NAME == "Valencia" ~ "s",
        NAME == "Mérida" ~ "s",
        NAME == "Caracas" ~ "e",
        NAME == "Maracaibo" ~ "n",
        .default = "e"
        )
    )

# Set aesthetics for South America
SA$fill <-  "#F7931E"
SA$fillweight <- 2.5
SA$color <- "white"
SA$fillstyle <- "hachure"

# Add a new column for the hachure angle
# Define a function to apply the hachure angle
apply_hachure_angle <- function(data) {
  data$hachureangle <- sample(c(45,60,120,150), nrow(data), replace = TRUE)
  return(data)
}

# Apply the function to the spatial data
SA <- apply_hachure_angle(SA)


# Ocean
# Create a bounding box for South America
bbox <- st_bbox(SA)

# Convert the bounding box to a polygon
bbox_polygon <- st_as_sfc(bbox)

# Convert the polygon to an sf object
bbox_sf <- st_sf(geometry = bbox_polygon)

# Set the CRS to EPSG:4326
bbox_sf <- st_set_crs(bbox_sf, 4326)

# Expand the bounding box (100 km each side)
bbox[c("xmin", "ymin")] <- bbox[c("xmin", "ymin")] - 1
bbox[c("xmax", "ymax")] <- bbox[c("xmax", "ymax")] + 1

# Convert the expanded bounding box to a polygon
bbox_polygon <- st_as_sfc(bbox)

# Convert the polygon to an sf object
bbox_sf <- st_sf(geometry = bbox_polygon)

# Set the CRS to EPSG:4326
bbox_sf <- st_set_crs(bbox_sf, 4326)

# Set aesthetics for the ocean
bbox_sf$fill <- "#037bfc"
bbox_sf$fillweight <- 0.45

# roughsf plot
plot <- roughsf::roughsf(list(bbox_sf,SA, cities_fil),
    title = "Cities* of South America", caption = "*With more than 500,000 inhabitants",
    title_font = "70px Pristina", font = "30px Pristina", caption_font = "25px Pristina",
    roughness = 2, simplification = 0.1,
    width = 900, height = 1200
)
roughsf::save_roughsf(plot, "SA_cities.png", background = "white")

# Load the saved roughsf plot
map_image <- image_read("SA_cities.png")

# Load the logo image
logo <- image_read(file.path(here::here(), "logo.jpg"))

# Combine the roughsf plot and the logo
combined_plot <- ggdraw() +
  draw_image(map_image, 0, 0, 1, 1) +
  draw_image(logo, x = 0.55, y = 0.12, width = 0.25, height = 0.25) +
  draw_label("Day 10 - Pen and Pencil. #30DayMapChallenge. Ignacio Urria Yáñez, 2024.\nSources: Natural Earth Data and roughsf package by David Schoch", x = 0.5, y = 0.05, size = 12, hjust = 0.5) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Save the final combined plot
ggsave("final_map_SA.png", combined_plot, width = 10, height = 15)