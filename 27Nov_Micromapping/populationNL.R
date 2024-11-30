# Install packages 
install.packages(c("dplyr", "ggplot2", "cowplot", "ggspatial"))

# Install and load necessary libraries
install.packages(c("ggplot2", "viridis", "cowplot", "dplyr"))
library(ggplot2)
library(viridis)
library(cowplot)
library(dplyr)

# read the shape.file
elderlyplus_60 <- read.csv("population/NLD_elderly_60.csv")



# Different sizes of samples 
elderlyplus_60_sampled <- elderlyplus_60[sample(nrow(elderlyplus_60), 10000), ]
elderlyplus_60_sampled_1 <- elderlyplus_60[sample(nrow(elderlyplus_60), 100000), ]
elderlyplus_60_sampled_2 <- elderlyplus_60[sample(nrow(elderlyplus_60), 1000000), ]
elderlyplus_60_sampled_3 <- elderlyplus_60[sample(nrow(elderlyplus_60), 4000000), ]



library(ggplot2)
library(cowplot)

# Use a consistent scale for all maps
shared_scale <- scale_fill_gradient(low = "white", high = "blue", name = "Density of Elderly (60+)")

# Plot 1: Sample 10,000
map_elderly <- ggplot(elderlyplus_60_sampled) +
  stat_density_2d(
    aes(x = longitude, y = latitude, fill = ..density..),
    geom = "raster",
    contour = FALSE
  ) +
  shared_scale +
  theme_minimal() +
  labs(title = "Sample 10,000") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none", 
    plot.title = element_text(size = 8)
  )

# Plot 2: Sample 100,000
map_elderly_1 <- ggplot(elderlyplus_60_sampled_1) +
  stat_density_2d(
    aes(x = longitude, y = latitude, fill = ..density..),
    geom = "raster",
    contour = FALSE
  ) +
  shared_scale +
  theme_minimal() +
  labs(title = "Sample 100,000") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 8)
  )

# Plot 3: Sample 1,000,000
map_elderly_2 <- ggplot(elderlyplus_60_sampled_2) +
  stat_density_2d(
    aes(x = longitude, y = latitude, fill = ..density..),
    geom = "raster",
    contour = FALSE
  ) +
  shared_scale +
  theme_minimal() +
  labs(title = "Sample 1,000,000") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 8)
  )

# Plot 4: Sample 4,000,000
map_elderly_3 <- ggplot(elderlyplus_60_sampled_3) +
  stat_density_2d(
    aes(x = longitude, y = latitude, fill = ..density..),
    geom = "raster",
    contour = FALSE
  ) +
  shared_scale +
  theme_minimal() +
  labs(title = "Sample 4,000,000") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 8)
  )

# Combine the maps into a 2x2 grid
combined_maps <- plot_grid(
  map_elderly,
  map_elderly_1,
  map_elderly_2,
  map_elderly_3,
  ncol = 2, # Create a 2x2 grid
  align = "hv",
  label_size = 5 
)

# Add a global legend (scale)
legend <- get_legend(
  ggplot(elderlyplus_60_sampled) +
    stat_density_2d(
      aes(x = longitude, y = latitude, fill = ..density..),
      geom = "raster",
      contour = FALSE
    ) +
    shared_scale +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.key.size = unit(0.4, "cm") #
    )
)

# Add the title and combine everything
final_plot <- plot_grid(
  ggdraw() + draw_label("The Power of Microdata", fontface = "bold", size = 14, hjust = 0.5),
  combined_maps,
  legend,
  ncol = 1,
  rel_heights = c(0.1, 1, 0.1) 
)

# Display the final plot
print(final_plot)

# Save the plot
ggsave("combined_maps_with_title.png", plot = final_plot, width = 12, height = 8)
