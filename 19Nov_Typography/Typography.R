library(ggplot2)
library(ggrepel)
library(sf)
library(dplyr)
library(png)
install.packages("grid")
install.packages("magick")
library(grid)
library(magick)


buffer_interno <- st_buffer(atena_lucana, dist = -0.01)
rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg')


parole_fuori <- parole_sf[!st_within(parole_sf, buffer_interno, sparse = FALSE), ]

if(nrow(parole_fuori) > 0){
  parole_fuori <- parole_fuori %>%
    mutate(geometry = st_nearest_points(geometry, buffer_interno)) %>% 
             st_geometry()
           }

parole_sf <- rbind(
  parole_sf[st_within(parole_sf, buffer_interno, sparse = FALSE), ],
  parole_fuori)

palette_colori_arancio <- c("#FF7F50", "#FF6347", "#FFA07A", "#FF4500", "#FFD700")
parole_sf$color <- palette_colori_arancio[seq_len(nrow(parole_sf)) %% length(palette_colori_arancio) + 1]


ggplot() +
  geom_sf(data = atena_lucana, fill = "black", color = "black") +
  # geom_text_repel(
  #   data = parole_sf,
  #   aes(label = word, geometry = geometry, size = size, color = color),
  #   stat = "sf_coordinates",
  #   min.segment.length = 0,
  #   family = "serif",
  #   fontface = "bold",
  #   box.padding = unit(0.1, "cm"),
  #   point.padding = unit(0.1, "cm"),
  #   max.overlaps = 50,
  #   force = 0.9) +
geom_text_repel(data = parole_sf, 
                aes(label = word, geometry = geometry, size = size, color = color),
                stat = "sf_coordinates",
                min.segment.length = 0.5,
                family = "serif",
                fontface = "bold",
                box.padding = unit(0.3, "cm"),
                point.padding = unit(0.2, "cm"),
                max.overlaps = Inf,
                force = 2) +
  scale_size(range = c(3, 12)) +
  scale_color_identity() +
  xlab("") + ylab("") +
  labs(
    title = "19 Nov. Typography\nValues of Atena Lucana",
    caption = "#30DayMapChallenge_2024. Map by Benedetta Grieco, 2024. Author's data.",
    size = "Importance \nof value"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(size = 16, hjust = 0, vjust = 1.5, face = "bold"),  
    plot.caption = element_text(size = 12, hjust = 0, vjust = -2, family = "serif"),
    plot.margin = margin(20, 20, 20, 20), 
    panel.grid = element_line(color = "grey85", linetype = "dashed"),
    panel.background = element_blank()
  ) +
  labs()
 
grid.raster(rbanism_logo,
            x = 0.9, y=0.9,
            width = unit(100, "points"))
