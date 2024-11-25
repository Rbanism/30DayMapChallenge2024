

# SECTION 0: INSTALL NECESSARY PACKAGES -----------------------------------

# List of required packages
required_packages <- c("ggspatial", "ggplot2", "sf", "tmap", "here", "magick", "grid", "cowplot")

# Install packages that are not already installed
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
# Load required libraries
library(ggplot2)       # For creating plots
library(sf)            # For working with spatial data
library(tmap)          # For thematic mapping
library(here)          # For managing file paths
library(magick)        # For image manipulation (logo)
library(grid)          # For working with grid graphics
library(cowplot)       # For combining plots and adding elements (e.g., logos)
library(ggspatial)     # For scale bars and north arrows in ggplot maps

           

# SECTION 1: DEFINE PATHS AND LOAD SPATIAL DATA ---------------------------

# Set working directory (modify as needed)
setwd("F:/R-WorkSpaces/R-30dayMapChallange/")


# Paths to shapefiles
disappeared_molen <- "23-Memory/data/shp/verdwenenmolens.shp"  # Disappeared mills
existing_molen <- "23-Memory/data/shp/Molens.shp"             # Existing mills

# Read shapefiles for various spatial features
north_sea <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/NorthSea.shp")
gr_border <- st_read("23-Memory/data/shp/GR.shp")            # Germany border
bl_border <- st_read("23-Memory/data/shp/BG.shp")            # Belgium border
nl_border <- st_read("23-Memory/data/shp/gadm41_NLD_0.shp")  #  national border
nl_stats_border <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/gadm41_NLD_1.shp")  # Provinces borders
nl_cities_border <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/gadm41_NLD_2.shp")  # Citis borders
oppervlaktewater <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/oppervlaktewater.shp")  # Surface water
nl_populated_palces <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/populated_places.shp")  # Populated places

# Load and preprocess shapefiles for disappeared mills
ex_molen <- st_read(here(disappeared_molen)) |> 
  st_transform(crs = st_crs(nl_border)) |> 
  st_crop(sf::st_bbox(nl_border))  # Crop to Netherlands' bounding box

# Load existing mills shapefile
molen <- st_read(existing_molen)



# SECTION 2: DATA INSPECTION ----------------------------------------------

# Inspect first few rows of each dataset
head(molen)
head(ex_molen)
print(nl_populated_palces)
head(nl_stats_border)

# Summarize datasets
summary(molen)
summary(ex_molen)

# Count number of features in each dataset
nrow(ex_molen)  # Count of disappeared mills
num_disappeared_mills <- nrow(ex_molen)
nrow(molen)     # Count of existing mills
num_existing_mills <- nrow(molen)

# Get the bounding box of the Netherlands shapefile
bbox <- st_bbox(nl_border)



# SECTION 3: CREATE MAIN PLOT ---------------------------------------------

main_plot <- ggplot() +
  
  # Add geographic background features
  geom_sf(data = north_sea, fill = "lightblue", color = NA, alpha = 0.5) + # North Sea
  geom_sf(data = gr_border, color = NA, alpha = 0.5) +                     # Germany border
  geom_sf(data = bl_border, color = NA, alpha = 0.5) +                     # Belgium border
  geom_sf(data = oppervlaktewater, fill = "lightblue", color = NA, alpha = 0.5) +       # Surface water in Netherlands
  geom_sf(data = nl_border, fill = "black", color = NA, alpha = 0.3) +     # Netherlands national border (dark mode)
  # geom_sf(data = nl_border, fill = NA, color = "black") +     # Netherlands national border (light mode)
  geom_sf(data = nl_stats_border, fill = NA , color = NA) +            # Netherlands Province borders
  
  # Add mills data
  geom_sf(data = ex_molen, aes(color = "Disappeared Mills"), size = 0.5) +  # Disappeared mills
  geom_sf(data = molen, aes(color = "Existing Mills"), size = 0.5) +        # Existing mills
  
  geom_sf(data = nl_populated_palces, aes(shape = "circle"), size = 2, show.legend = FALSE) +
  
  # Add labels
  geom_text(data = nl_populated_palces, 
            aes(x = st_coordinates(geometry)[, 1], 
                y = st_coordinates(geometry)[, 2], 
                label = name),  # Add city names as labels
            size = 3,  # Adjust size of labels
            nudge_y = 0.05,  # Adjust vertical position of labels to move them up
            nudge_x = 0,  # Adjust horizontal position if needed
            color = "black",  # Label color
            fontface ="bold",  # Font style
            check_overlap = FALSE) +  # Prevent overlap of labels
  
  # Customize color legend for mills
  scale_color_manual(
    name = "▪ Legend",  # Legend title
    values = c(
      "Existing Mills" = "#993404",  # Darker brown for existing mills
      "Disappeared Mills" = "#fec44f"  # Lighter yellow for disappeared mills
    ),
    labels = c(
      paste0("Disappeared Mills\n (", num_disappeared_mills, ")"),
      paste0("Existing Mills \n (", num_existing_mills, ")")
    )
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 3))  # Customize legend symbols
  )+
  
  # Add title, subtitle, and captions
  labs(
    title = "▪ Mills in the Netherlands",
    subtitle = "▪ Existing and disappeared",
    caption = "▪ #30DayMapChallenge| Data Source: www.molendatabase.org | Map by Massoud Ghaderian, 2024",
    x = NULL,  # Remove x-axis label
    y = NULL   # Remove y-axis label
  ) +
  
  # Set the map extent to the bounding box
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  
  # Apply minimal theme with customizations
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = -0.01, size = 16, face = "bold", margin = margin(b = 0)),
    plot.subtitle = element_text(hjust = -0.01, size = 12, margin = margin(t = 0)),
    plot.caption = element_text(hjust = -0.01, size = 8, face = "italic", margin = margin(t = 15)),
    plot.margin = margin(t = 30, r = 20, b = 50, l = 20),
    # Move legend to bottom-right
    # legend.position = c(0.95, 0.05),  # x and y position (percent of plot)
    legend.justification = c("right", "bottom"),  # Align legend's bottom-right corner
    # legend.box.margin = margin(5, 5, 5, 5),  # Add some space around the legend
    legend.background = element_rect(fill = "white", color = "white", size = 0.5),  # Optional: Add background and border to legend
    # Customizing  grid lines (for finer latitude and longitude)
    panel.grid.major = element_line(color = "lightgray", size = 0.5),  # Major grid lines: gray color, thickness 0.
    panel.grid.minor = element_line(color = "lightgray", size = 0.5),  # Minor grid lines: light gray, thinner
    
    # Ticks for axis (optional)
    axis.ticks.x = element_line(color = "black", size = 1),  # Ticks for top
    axis.ticks.y = element_line(color = "black", size = 1),  # Ticks for right
    
    axis.text = element_text(
      size = 7,  # Change font size of numbers
      color = "gray",  # Change font color
      face = "italic",  # Make numbers bold (optional)
      family = "sans"  # Set font family (optional)
    ),
    
    # Moving axis labels inside the plot
    axis.text.x = element_text(
      size = 7, hjust = 0.5, vjust = 1  # Move x-axis labels to the right (hjust = 1)
    ),
    axis.text.y = element_text(
      size = 7, hjust = 1, vjust = 0.5  # Move y-axis labels up (vjust = 1.5)
    ),
    
  ) +
  # Add a north arrow
  annotation_north_arrow(
    location = "bl", # Position: 'tl' = top-left, 'tr' = top-right, etc.
    which_north = "true", # "true" for true north, "grid" for grid north
    style = north_arrow_fancy_orienteering(fill = c("white", "white"), line_col = "black"),# Choose a style for the north arrow
     
    height = unit(1, "cm"),  # Adjust size
    width = unit(1, "cm"),
    pad_x = unit(1.7, "cm"),# Horizontal padding
    pad_y = unit(1, "cm")  # Vertical padding# Adjust size
  ) +
  
  # Add a scale bar
  annotation_scale(
    location = "bl", # Position: 'bl' = bottom-left
    width_hint = 0.2, # Adjust the width relative to the map
    line_width = 1,
    height = unit(0.1, "cm"), # Adjust the height of the scale bar
    pad_x = unit(1.25, "cm"),
    pad_y = unit(.75, "cm"),
    bar_cols = c("white", "white")
  )

# Display the main plot
main_plot


# SECTION 4: ADD LOGO AND EXPORT FINAL PLOT -------------------------------

# Read and convert logo to raster
rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg')  # Download logo
rbanism_logo_raster <- grid::rasterGrob(rbanism_logo, interpolate = TRUE)

# Combine main plot with logo using cowplot
final_plot <- ggdraw(main_plot) +
  draw_grob(rbanism_logo_raster, x = 0.76, y = 0.75, width = 0.20, height = 0.20)

# Display the final plot
final_plot

# Save the final plot as a PDF
ggsave("Mills.pdf", plot = final_plot, 
       width = 8.27, height = 10, dpi = 600, 
       path = "/R-WorkSpaces/R-30dayMapChallange/23-Memory/outputs/")

