# Install R packages if not installed -------------------------------------

if (!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if (!require("sf")) install.packages("sf", dependencies = TRUE)
if (!require("rnaturalearth")) install.packages("rnaturalearth", dependencies = TRUE)
if (!require("countrycode")) install.packages("countrycode", dependencies = TRUE)
if (!require("magick")) install.packages("magick", dependencies = TRUE)
if (!require("grid")) install.packages("grid", dependencies = TRUE)
if (!require("ggrepel")) install.packages("ggrepel", dependencies = TRUE)
if (!require("ggrsfepel")) install.packages("sf", dependencies = TRUE)

# for MacOS and Linux, to install "sf" see https://r-spatial.github.io/sf/#installing

library("rnaturalearth")
library("countrycode") # get ISO code from country names
library("ggrepel") # "ggplot2" extension for overlapping text labels
library(here)
library(magick)
library(grid)
library(sf)
library(tidyverse)

world <- ne_countries(scale = "small", returnclass = "sf")
head(world)

# Plot an empty world map -------------------------------------------------

world %>%
  ggplot() +
  geom_sf()


# Change map projection ---------------------------------------------------

# projection list: https://proj.org/operations/projections/
# examples: "+proj=robin", "+proj=moll", "+proj=aeqd", "+proj=goode"
world %>%
  st_transform(crs = "+proj=robin") %>%
  ggplot() +
  geom_sf() +
  theme_minimal()

# fixing some proj bugs by removing the graticule
world %>%
  filter(admin != "Antarctica") %>%
  st_transform(crs = "+proj=wintri") %>%
  ggplot() +
  geom_sf() +
  coord_sf(datum = NA) + # no graticule
  theme_minimal()


###PreFinal Code
world %>%
  filter(admin != "Antarctica", gdp_year==2019) %>%
  st_transform(crs = "+proj=wintri") %>%
  ggplot() +
  geom_sf(color = "gray30") + 
  geom_sf(data = world, aes(fill = economy)) + 
  scale_fill_manual(values = c('#93278F', "#00A99D", "goldenrod1",
                               "#F7931E", "firebrick3", "lightblue", "beige")) +
coord_sf(datum = NA) + # no graticule
  theme_minimal() +
theme(plot.title = element_text(face = "bold"),
      axis.text.x = element_blank(),
      legend.position = "bottom") +
  labs(title = "Day 14. Partition of the world according to economic development levels.",
       #subtitle = "Economy of Countries",
       x = NULL, y = NULL,
       caption = "#30DayMapChallenge. 'A World Map' by Soroush Saffarzadeh, 2024. Data: `rnaturalearth`.")

rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg') # Download our logo

grid.raster(rbanism_logo, x = 0.9, y=0.9,  # x and y determine the position of the logo (top right)
            width = unit(100, "points"))   # width determines the size of the logo
