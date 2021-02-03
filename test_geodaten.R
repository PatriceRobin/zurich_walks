if (!require(rgdal)) {install.packages('rgdal')}
if (!require(sf)) {install.packages('sf')}
if (!require(rasterVis)) {install.packages('rasterVis')}
if (!require(ggplot2)) {install.packages('ggplot2')}


zzones <- st_read("./Quartieranalyse/QUARTIERE_F.shp")


summary(zzones)


ggplot() + 
  geom_sf(data = zzones, size = 0.5, color = "black", fill = NA) + 
  ggtitle("Quartiere") + 
  coord_sf()