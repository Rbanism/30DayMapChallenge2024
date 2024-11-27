#load the packages
library(here)
library(tidyverse)
library(sf)
library(magick)
library(grid)

#load logo
rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg') # Download our logo

#import shp
points_geothermal<-st_read("data/Vergunningen_open_bodemenergieopslagsystemenPoint.shp")
points_geothermal
names(points_geothermal)
  #vergund=licensed
  #watervoere=water feed
  #onderkant=bottom
  #bovenkant=top
  #bron=source

province_utrecht<-st_read("data/province_Utrecth.shp")
  
summary(points_geothermal)

#convert charater to numeric
points_geothermal$VERGUND_DE <- as.numeric(points_geothermal$VERGUND_DE)
points_geothermal$ONDERKANT_ <- as.numeric(points_geothermal$ONDERKANT_)
points_geothermal$BOVENKANT_ <- as.numeric(points_geothermal$BOVENKANT_)
points_geothermal$BOVENKANT0 <- as.numeric(points_geothermal$BOVENKANT0)
points_geothermal$ONDERKANT0 <- as.numeric(points_geothermal$ONDERKANT0)


summary(points_geothermal)

#plot
ggplot()+
  geom_sf(
    data=province_utrecht,
    fill="white",
    color="black",
  )+
  geom_sf(
    data=points_geothermal,
    aes(color=ONDERKANT0),
    size=1,
  )+
  scale_color_viridis_c(option="D")+
  coord_sf(datum = st_crs(28992))+
  labs(
    color="temperature",
    title="Geothermal energy",
    subtitle = "location of the heat storage in Utrecht"
  )

#add logo
grid.raster(rbanism_logo, x = 0.9, y=0.74,  # x and y determine the position of the logo (top right)
            width = unit(55, "points"))   # width determines the size of the logo


#save
ggsave(
  filename=here("PNG","plot.png"),
  width = 8,
  height = 8,
  dpi=300,)
  