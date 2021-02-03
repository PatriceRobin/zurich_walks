#############################################################################
#load packages
#############################################################################
if (!require(tidyverse)) {install.packages('"tidyverse"')}
if (!require(ggplot2)) {install.packages('ggplot2')}
if (!require(scales)) {install.packages('scales')}
if (!require(rgdal)) {install.packages('rgdal')}
if (!require(sf)) {install.packages('sf')}
if (!require(rasterVis)) {install.packages('rasterVis')}
if (!require(dplyr)) {install.packages('dplyr')}


#Sys.setlocale("LC_TIME", "English")

#############################################################################
#load data & prepare it
#############################################################################

#source : https://ourworldindata.org/coronavirus/country/switzerland?country=~CHE
covid <- read.csv("covid-data.csv", encoding="UTF-8")
covid$date <- as.Date(covid$date,format = "%d.%m.%y")


#source: https://data.stadt-zuerich.ch/dataset/ted_taz_verkehrszaehlungen_werte_fussgaenger_velo
zwalks2020 <- read.csv("2020_verkehrszaehlungen_werte_fussgaenger_velo.csv")
zwalks2019 <- read.csv("2019_verkehrszaehlungen_werte_fussgaenger_velo.csv")

zwalks <- dplyr::union(zwalks2020, zwalks2019)

#extract date from datetime
zwalks['date'] <- as.Date(zwalks$DATUM)

#lower cases column names
zwalks <- setNames(zwalks, tolower(names(zwalks)))

#replace NA with 0
zwalks[is.na(zwalks)] <- 0

#rename columns
zwalks <- zwalks %>%
  rename(datetime = datum)


#summarize bicycles and pedestrians
zwalks['fuss_all'] <- zwalks$fuss_in + zwalks$fuss_out
zwalks['velo_all'] <- zwalks$velo_in + zwalks$velo_out
zwalks['people_all'] <-
  zwalks$velo_all + zwalks$fuss_all

#add months and weekdays
zwalks$year <- format(zwalks$date, "%Y")
zwalks$month <- format(zwalks$date, "%B")
zwalks$week <- format(zwalks$date, "%Y-%V")
zwalks$weekday <- weekdays(zwalks$date)


#summarize data on a daily basis
zwalks.day <-
  aggregate(zwalks[c("velo_in", "velo_out", "velo_all", "fuss_in", "fuss_out", "fuss_all", "people_all")],
            by = zwalks[c("fk_zaehler", "fk_standort", "date", "year", "month", "week", "weekday", "ost", "nord")],
            FUN = sum)

weekday_order <- c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday")


# overall pedestrians and bicycles
zwalks.agg <-
  aggregate(zwalks.day[c("people_all", "fuss_all", "velo_all")],
            by = zwalks.day[c( "date", "month", "weekday", "week","year")],
            FUN = sum)

# merge zwalks.agg with covid data
zwalks.agg <- (merge(covid, zwalks.agg, by = 'date'))


zwalks.agg2 <- zwalks.day %>%
  group_by(fk_zaehler, year) %>%
  summarize(people_all)

zwalks.agg2


#############################################################################
# Look at the data
#############################################################################
#Look 
sapply(zwalks, n_distinct)

#############################################################################
# PLOTS
#############################################################################

#Aggregated Data - Daily 2020
ggplot(zwalks.agg, aes(x=date, y=people_all)) +
  geom_point(na.rm = TRUE, size = 0.75) +
  ggtitle("Pedestrian and Bicycles in Zurich through 2020") +
  xlab("2020") +
  ylab("Pedestrians and Bicycles") +
  scale_x_date(
    labels = date_format("%B"),
    breaks = "1 month") + 
  theme(axis.text.x = element_text(angle = 45)) +
  geom_point(aes(colour = factor(year)), size = 4)


#Months
ggplot(zwalks.agg, aes(x=factor(month, level=month.name), y=people_all)) +
  stat_summary(fun = "sum", geom = "bar")+
  ggtitle("Pedestrian and Bicycles in Zurich through 2020") +
  xlab("Month") +
  ylab("Pedestrians and Bicycles")


#Weekdays
ggplot(zwalks.agg, aes(x=factor(weekday, level=weekday_order), y=people_all)) +
  stat_summary(fun = "mean", geom = "bar")+
  ggtitle("Pedestrian and Bicycles in Zurich through 2020") +
  xlab("Weekday") +
  ylab("Pedestrians and Bicycles")

# COVID Cases
p <- ggplot(covid, aes(x=date, y=new_cases_smoothed)) +
  geom_line() + 
  ggtitle("New COVID Cases (smoothed)") +
  ylab("COVID Cases") + 
  xlab("Date")
p

#############################################################################
# ANALYZIS
#############################################################################
### define lockdown period
### chronic of lockdowns https://www.stv-fst.ch/de/chronik-coronavirus
lockdown.1 <- seq(as.Date("2020/03/16"), by = "day", length.out = 57)
lockdown.2 <- seq(as.Date("2020/12/22"), by = "day", length.out = 10)

### add lockdown flag to dataset zwalks.agg
zwalks.agg$lockdown <- FALSE
zwalks.agg$lockdown01 <- 0

zwalks.agg[zwalks.agg$date %in% lockdown.1 , ]$lockdown <- TRUE
zwalks.agg[zwalks.agg$date %in% lockdown.1 , ]$lockdown01 <- 1

zwalks.agg[zwalks.agg$date %in% lockdown.2 , ]$lockdown <- TRUE
zwalks.agg[zwalks.agg$date %in% lockdown.1 , ]$lockdown01 <- 1


###  Boxplot 
ggplot(mapping = aes(y = zwalks.agg$people_all,
                     x = zwalks.agg$lockdown)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  ylab("people_all") +
  xlab("lockdown")

###  Plot People vs Cases 
plot(y = zwalks.agg$people_all, 
     x = zwalks.agg$new_cases_smoothed,
     main = " ",
     xlab = "new_cases_smoothed",
     ylab = "people_all" )


### Logistic regression
zwalks.agg.glm <- glm(lockdown ~  people_all, family = "binomial", 
                      data = zwalks.agg)
summary(zwalks.agg.glm)


### Plot the Logistic regression
ggplot(data = zwalks.agg,
       mapping = aes(y = lockdown01,
                     x = people_all)) + 
  geom_point() +
  geom_smooth(method = "glm", 
              se = FALSE,
              method.args = list(family = "binomial")) 


#############################################################################
# LOCATION
#############################################################################


###  Boxplot per location
ggplot(mapping = aes(y = zwalks.day$people_all,
                    group = zwalks.day$fk_standort)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  ylab("people_all") +
  xlab("locations")



#load Stadtkreise
#source https://data.stadt-zuerich.ch/dataset/geo_stadtkreise
zkreis <- st_read("./Stadtkreise/stzh.adm_stadtkreise_v.shp")
head(zkreis)
summary(zkreis)


#load countingstations
#source https://data.stadt-zuerich.ch/dataset/geo_standorte_der_automatischen_fuss__und_velozaehlungen

zstation <- st_read("./Countingstation/taz.view_eco_standorte.shp")
head(zstation)

zstation <- cbind(zstation, st_coordinates(zstation))

summary(zstation)



#Districts
#source: https://data.stadt-zuerich.ch/dataset/ktzh_quartieranalyse
zdistricts <- st_read("./Quartieranalyse/QUARTIERE_F.shp")
summary(zdistricts)



#Bauzonen
#sources https://data.stadt-zuerich.ch/dataset/geo_nutzungsplanung___kommunale_bau__und_zonenordnung__bzo__jahresendstand_2020
zzones <- st_read("./Bauzonen/afs.bzo_zone_v.shp")
summary(zzones)


#plot Stadtkreise and Coutingstations
plot.map <- ggplot()+
  geom_sf(data = zkreis, size = 0, color = "black", fill="lightblue")+
  geom_sf(data = zzones, size = 0.25, color = "darkgrey", fill="white")

plot.kreis <- plot.map +
    geom_sf(data = zkreis, size = 1, color = "black", fill= NA )+
    geom_sf_text(data = zkreis, aes(label = knr), colour = "black", size=10)
  
#counting stations  
plot.station <- plot.kreis + geom_point(data = zstation, aes(x=X, y=Y), size = 3, 
                                    shape = 23, fill = "red")
  
plot.station


#join the shape files - left on station
zstation.kreis <- st_join(zstation, zkreis, 
                  join = st_within)


## Bauzonen
zstation.zones <- st_join(zstation.kreis, zzones,
                          join = st_within)

head(zstation.zones)
summary(zstation.zones)
unique((zstation.zones$typ))



zstation.zones["zones"] <- zstation.zones$typ

zstation.zones$zones[zstation.zones$zones == "W4"] <- "Living" #Viergeschossige  Wohnzone
zstation.zones$zones[zstation.zones$zones == "W5"] <- "Living" #Fünfgeschossige Wohnzone
zstation.zones$zones[zstation.zones$zones == "Z6"] <- "Center" #Sechsgeschossige Zentrumszone
zstation.zones$zones[zstation.zones$zones == "Z5"] <- "Center" #Fünfgeschossige Zentrumszone
zstation.zones$zones[zstation.zones$zones == "K"] <- "Center" #Kernzone
zstation.zones$zones[zstation.zones$zones == "F"] <- "Empty" #Freihaltezone
zstation.zones$zones[zstation.zones$zones == "FP"] <- "Empty" #Parking
zstation.zones$zones[zstation.zones$zones == "WLD"] <- "Recreation" #Wald
zstation.zones$zones[zstation.zones$zones == "QI"] <- "Recreation" #Quartier Erhaltungszone
zstation.zones$zones[zstation.zones$zones == "GWS"] <- "Recreation" #Gewässer
zstation.zones$zones[zstation.zones$zones == "IG I"] <- "Industry" #Industrie







#############################################################################
# MEASUREMENTS and LOCATION
#############################################################################

zwalks.day.knr <- zwalks.day %>% left_join (dplyr::select(zstation.zones,
                                                fk_zaehler,
                                                knr,
                                                kname,
                                                zones),
                                            by = "fk_zaehler")