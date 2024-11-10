#Install packages (if not already installed)
#install.packages("ggplot2", "dplyr", "ggridges", "sf", "terra", "elevatr", "rnaturalearth", "showtext", "ggtext")

# Load libraries
library(ggplot2)
library(dplyr)
library(ggridges)
library(sf)
library(terra)
library(elevatr)
library(rnaturalearth)
library(showtext)
library(ggtext)

# Load Chile shapefile
chile_sf <- rnaturalearth::ne_states("Chile", returnclass = "sf")
names(chile_sf)

# Extract Santiago
santiago_sf <- chile_sf[chile_sf$code_hasc == "CL.RM", ]

# Load elevation data (reduce z value if it takes too much time to download)
elevation <- elevatr::get_elev_raster(santiago_sf, z = 12) %>%
  terra::rast() %>%
  mask(vect(santiago_sf))

# Rename layer for further manipulation
names(elevation) <- "elev"

# Reduce the number of rows (horizontal ridge lines in plot)
nrow(elevation)
factor <- round(nrow(elevation) / 150)
elevation_agg <- aggregate(elevation, factor)
nrow(elevation_agg)

# Transform CRS to a CRS in meters
elevation_agg <- terra::project(elevation_agg, "EPSG:5361")
santiago_sf <- st_transform(santiago_sf, crs = 5361)

# As data frame
elevation_agg[elevation_agg < 0] <- 0
elevation_agg[is.na(elevation_agg)] <- 0
elevation_df <- as.data.frame(elevation_agg, xy = TRUE, na.rm = FALSE)
as_tibble(elevation_df)

# Download font
showtext_auto()
font_add_google("Cinzel", "cinzel")

# Set 2 colours
background_color <- "#A13941"
text_color <- "#BD7F37"

# Set bounding box
bbox <- st_bbox(santiago_sf)

# Plot
p <- ggplot() +
  # Just for the scales, pass with NA arguments so it is not shown
  geom_sf(data = santiago_sf, color = NA, fill = NA) +
  geom_ridgeline(
    data = elevation_df, aes(
      x = x, y = y,
      group = y,
      height = elev
    ),
    scale = 5,
    fill = background_color,
    color = text_color,
    size = .25,
    min_height = 0.1
  ) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]+10000)
  ) +
  theme_void() +
  labs(
    title = "Santiago Metropolitan Region",
    subtitle = "Unkown Pleasures with Elevation Data",
    caption = "Day 22. Two Colours - #30DayMapChallenge. Ignacio Urria Yáñez, 2024.<br><span style = 'text-align:center;'>Elevation data from elevatr package.</span>"
  ) +
  theme(
    plot.background = element_rect(fill = background_color,  colour = background_color),
    plot.title = element_markdown(
      hjust = 0.5, 
      face = "bold",
      size = 80,
      color = text_color,
      family = "cinzel",
      lineheight = 1
    ),
    plot.subtitle = element_text(family = "cinzel", hjust = 0.5, color = text_color, size = 50),
    plot.caption = element_markdown(
      hjust = 0.5, 
      size = 30,
      face = "bold",
      color = text_color,
      family = "cinzel",
      lineheight = 0.5
    )
  )

# Add logo to the plot
logo <- png::readPNG("rbanism_logo_inca_gold.png")
logo_grob <- grid::rasterGrob(logo, x = 0.02, y = 0.00, width = 0.07, height = 0.10, just = c("left", "bottom"))
p <- p + annotation_custom(logo_grob)

# Save plot
ggsave("joyplot_Santiago_logo.png", plot = p, width = 10, height = 9, dpi = 300)
