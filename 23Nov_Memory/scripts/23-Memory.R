install.packages("ggspatial")

# Load libraries
library(ggplot2)
library(sf)      # For working with spatial data
library(tmap)    # For thematic maps
library(here)    # For managing file paths
## for logo
library(magick)
library(grid)
library(cowplot)
library(ggspatial)

# Define paths to shapefiles
setwd("F:/R-WorkSpaces/R-30dayMapChallange/")

disappeared_molen <- ("23-Memory/data/shp/verdwenenmolens.shp")
existing_molen <- ("23-Memory/data/shp/Molens.shp")

north_sea <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/NorthSea.shp")
gr_border <- st_read("23-Memory/data/shp/GR.shp")
bl_border <- st_read("23-Memory/data/shp/BG.shp")
nl_border <- st_read("23-Memory/data/shp/gadm41_NLD_shp/gadm41_NLD_0.shp")
nl_stats_border <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/gadm41_NLD_shp/gadm41_NLD_1.shp")
nl_cities_border <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/gadm41_NLD_shp/gadm41_NLD_2.shp")
oppervlaktewater <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/oppervlaktewater.shp")
nl_populated_palces <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/populated_places.shp")
head(nl_populated_palces )
ex_molen <- st_read(here(disappeared_molen)) |> 
  st_transform(crs = st_crs(nl_border)) |> 
  st_crop(sf::st_bbox(nl_border))

molen <- st_read(existing_molen)


#inspect data
head(molen)
head(ex_molen)

summary(molen)
summary(ex_molen)


nrow(ex_molen)
nrow(molen)


# Get the bounding box of the shapefile
bbox <- st_bbox(nl_border)

#plotting 

main_plot <- ggplot() +
  geom_sf(data= north_sea, fill="lightblue", color=NA, alpha = 0.5)+
  geom_sf(data=gr_border, color=NA, alpha = 0.5)+
  geom_sf(data=bl_border, color=NA, alpha = 0.5)+
  geom_sf(data = nl_stats_border,fill = NA ,color="white") +
  geom_sf(data = nl_border, fill = "black",color=NA, alpha = 0.3) +
  geom_sf(data=oppervlaktewater, fill="lightblue", color=NA)+
  geom_sf(data = ex_molen , aes(color = "Disappeared Mills"), size = 0.5) +
  geom_sf(data = molen, aes(color = "Existing Mills"), size = 0.5)+
  geom_sf(data = nl_populated_palces, aes(shape = "circle"), size = 2,show.legend = FALSE)+
  # geom_text(data = nl_populated_palces, 
  #           aes(label =name),  # Add city names as labels
  #           size = 3,  # Adjust size of labels
  #           nudge_y = 0.1,  # Adjust vertical position of labels if needed
  #           nudge_x = 0.1,  # Adjust horizontal position of labels if needed
  #           color = "black",  # Label color
  #           fontface = "bold",  # Font style
  #           check_overlap = TRUE) +  # Prevent overlap of labels
  
  scale_color_manual(
    name = "Mills Legend",  # Legend title
    values = c("Existing Mills" = "#993404", "Disappeared Mills" = "#fec44f")
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 3)  # Set the size of shapes in the legend
    )
  ) +
  labs(
    title = "▪ Mills in the Netherlands",
    subtitle = "▪ Existing and disappeared",
    caption = "▪ #30DayMapChallenge| Data Source: www.molendatabase.org | Map by Massoud Ghaderian, 2024",
    x = NULL,  # Remove x-axis title
    y = NULL,  # Remove y-axis title
  )+
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +  # Set the zoom extent to the bounding box
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = -0.01 , size = 16, face = "bold", margin = margin(b = 0)),
    plot.subtitle = element_text(hjust = -0.01, size = 12, margin = margin(t =0)),
    plot.caption = element_text(hjust = -0.01, size = 10, face = "italic", margin = margin(t = 15)),
    plot.margin = margin(t = 30, r = 20, b = 50, l = 20),
    # legend.position = c(1, 0.1),   # Position legend at top-left (relative coordinates)
    legend.justification = c("left", "bottom"),  # Adjust alignment of the legend box
    # legend.box = "horizontal"   # Legend items in horizontal layout (optional)
    # Customizing major grid lines (for latitude and longitude)
    panel.grid.major = element_line(color = "lightgray", size = .5),  # Major grid lines: gray color, thickness 0.5
    
    # Customizing minor grid lines (for finer latitude and longitude)
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
    location = "bl",  # Position: 'tl' = top-left, 'tr' = top-right, etc.
    which_north = "true",  # "true" for true north, "grid" for grid north
    style = north_arrow_fancy_orienteering(
      fill = c ("white", "white"),
      line_col = "black"
    ),  # Choose a style for the north arrow
    height = unit(1, "cm"),  # Adjust size
    width = unit(1, "cm") ,
    pad_x = unit(1.7, "cm"),       # Horizontal padding
    pad_y = unit(1, "cm")       # Vertical padding# Adjust size
  ) +
  # Add a scale bar
  annotation_scale(
    location = "bl",         # Position: 'bl' = bottom-left
    width_hint = 0.2,        # Adjust the width relative to the map
    line_width = 1,
    height = unit(0.1, "cm"), # Adjust the height of the scale bar
    pad_x = unit(1.25, "cm"),
    pad_y = unit(.75, "cm"),
    bar_cols = c("white", "white"),
  )


main_plot

# Convert the logo to a raster object for cowplot
rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg') # Download our logo
rbanism_logo_raster <- grid::rasterGrob(rbanism_logo, interpolate = TRUE)


# Combine the plot and logo using cowplot::ggdraw
final_plot <- ggdraw(main_plot) +
  draw_grob(rbanism_logo_raster, x = 0.76, y = 0.75, width = 0.20, height = 0.20)

final_plot

# Save the combined plot
ggsave("Molen_ex.pdf", plot = final_plot, 
       width = 8.27, height = 10, dpi = 600, 
       path = "/R-WorkSpaces/R-30dayMapChallange/23-Memory/outputs/")

