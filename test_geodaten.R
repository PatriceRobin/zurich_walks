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





zstation.zones["Zones"] <- sapply(zstation.zones$typ, switch,
                                  W4 = 'Living', #Viergeschossige  Wohnzone
                                  W5 = 'Living',  #Fünfgeschossige Wohnzone
                                  Z6 =  'Center', #Sechsgeschossige Zentrumszone
                                  Z5 =  'Center', #Fünfgeschossige Zentrumszone
                                  "K" =  'Center', #Kernzone
                                  "F" = 'Empty', #Freihaltezone
                                  "FP" = 'Empty', #Freihaltezone - Parking
                                  "WLD" = 'Recreation', #Wald
                                  "QI" =  'Recreation', #Quartier Erhaltungszone
                                  "GWS" = 'Recreation', #Gewässer
                                  "IG I" =  'Industry', #Kernzone
)

zstation.zones["zones"] <- zstation.zones$typ

zstation.zones$zones[zstation.zones$zones == "W4"] <- "Living"
zstation.zones$zones[zstation.zones$zones == "W5"] <- "Living"
zstation.zones$zones[zstation.zones$zones == "Z6"] <- "Center"
zstation.zones$zones[zstation.zones$zones == "Z5"] <- "Center"
zstation.zones$zones[zstation.zones$zones == "K"] <- "Center"
zstation.zones$zones[zstation.zones$zones == "F"] <- "Empty"
zstation.zones$zones[zstation.zones$zones == "FP"] <- "Empty"
zstation.zones$zones[zstation.zones$zones == "WLD"] <- "Recreation"
zstation.zones$zones[zstation.zones$zones == "QI"] <- "Recreation"
zstation.zones$zones[zstation.zones$zones == "GWS"] <- "Recreation"
zstation.zones$zones[zstation.zones$zones == "IG I"] <- "Industry"