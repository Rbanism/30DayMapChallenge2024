# Define the packages to be used
packages <- c("sf", "ggplot2", "raster", "terra", "grid", "magick", "extrafont", "tidyverse", "viridis", "gridExtra", "showtext")

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

# Add the Roboto font from Google Fonts
font_add_google("Roboto", "roboto")

# Activate the font system-wide (in R plots)
showtext_auto()

# Download our logo
rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg') 

# Read the GeoPackage file
heat_map <- rast("data/Hittestress door warme nachten Huidig.tif")

# Convert raster to a data frame for ggplot2
raster_df <- as.data.frame(heat_map, xy = TRUE)

# Create the heatmap
p <- ggplot() +
  geom_tile(data = raster_df , aes(x = x, y = y, fill = `Hittestress door warme nachten Huidig`)) +
  scale_fill_viridis_c(option = "H") +
  coord_equal() +
  theme_minimal()+
  labs(title = "Heat stress due to warm nights Current in the Netherlands", 
       subtitle = "30DayMapChallenge2024, Day25\nAuthor: Yaying Hao, Source: Klimaat Atlas", 
       x = "", 
       y = "", 
       fill = "index of the number of hot nights at regional level"
  ) + 
  theme(
    plot.background = element_rect(fill = "white",  colour = "white"),
    plot.title = element_text(family = "Roboto", size = 16, face = "bold"), 
    plot.subtitle = element_text(family = "Roboto", size = 12),
    axis.text = element_blank(),  
    axis.title = element_blank()
  )

logo <- rbanism_logo
logo_grob <- grid::rasterGrob(logo, x = 0.1, y = 0.1, width = 0.2)
p <- p + annotation_custom(logo_grob)
ggsave("heatmap/output/heatstress_map.png", width =  6 , height =6, dpi = 300)

