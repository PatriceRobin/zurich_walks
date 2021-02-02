if (!require(rgdal)) {install.packages('rgdal')}
if (!require(sf)) {install.packages('sf')}
if (!require(rasterVis)) {install.packages('rasterVis')}
if (!require(ggplot2)) {install.packages('ggplot2')}


z.zones <- st_read("./Arbeitszonenbewirtschaftung/ARBEITSZONE_F.shp")
head(z.zones)

areas <- st_area(z.zones)
hist(areas)

plot(z.zones)

ggplot() + 
  geom_sf(data = z.zones, size = 0.5, color = "black", fill = NA) + 
  ggtitle("Arbeitszonen") + 
  coord_sf()
