# Load libraries
library(ggplot2)
library(sf)      # For working with spatial data
library(tmap)    # For thematic maps
library(here)    # For managing file paths

# Define paths to shapefiles
disappeared_molen <- ("23Nov_Memory/verdwenenmolensPoint.shp")


# Read shapefiles
nl <- st_read("nl_shp/nl.shp")

ex_molen <- st_read(here(disappeared_molen)) |>
  st_transform(crs = st_crs(nl)) |>
  st_crop(sf::st_bbox(nl))

ex_molen |> st_graticule()
# Inspect data
str(ex_molen)

head(ex_molen)
plot(ex_molen)

# Plot the ex_molens

ggplot() +
  geom_sf(data = nl, fill = "grey90") +
  geom_sf(data = ex_molen, size = 1, aes(colour = type)) +
  theme_minimal() + 
  guides(legend="none") +
  coord_sf(datum = 4326) +
  labs(title = "Basic Map Of Molens") 

