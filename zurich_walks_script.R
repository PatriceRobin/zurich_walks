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
if (!require(av)) {install.packages('av')}
if (!require(ggpubr)) {install.packages('ggpubr')}




Sys.setlocale("LC_TIME", "English")

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
covid$new_cases_smoothed[is.na(covid$new_cases_smoothed)] <- 0

#rename columns
zwalks <- zwalks %>%
  rename(datetime = datum)

#summarize data on a daily basis
zwalks.day <-
  aggregate(zwalks[c("velo_in", "velo_out", "fuss_in", "fuss_out")],
            by = zwalks[c("fk_zaehler", "fk_standort", "date", "ost", "nord")],
            FUN = sum)

#summarize bicycles and pedestrians
zwalks.day['fuss_all'] <- zwalks.day$fuss_in + zwalks.day$fuss_out
zwalks.day['velo_all'] <- zwalks.day$velo_in + zwalks.day$velo_out
zwalks.day['people_all'] <-
  zwalks.day$velo_all + zwalks.day$fuss_all

#add months and weekdays
zwalks.day$year <- format(zwalks.day$date, "%Y")
zwalks.day$month <- format(zwalks.day$date, "%b")
zwalks.day$week <- format(zwalks.day$date, "%W") #create weeks from date
zwalks.day$week <- as.numeric(zwalks.day$week) + 1 #make numeric and add 1 so that it goes from 1 to 53

#weekdays
zwalks.day$weekday <- weekdays(zwalks.day$date)

weekday_order <- c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday")


# overall pedestrians and bicycles
zwalks.agg <-
  aggregate(zwalks.day[c("people_all", "fuss_all", "velo_all")],
            by = zwalks.day[c( "date", "month", "weekday", "week","year")],
            FUN = sum)

# left join zwalks agg and covid on date
zwalks.covid <- zwalks.agg %>% left_join(dplyr::select(covid,
                                                       date,
                                                       new_cases_smoothed),
                                         by = "date")

#replace NA with 0 in covid cases
zwalks.covid[is.na(zwalks.covid)] <- 0

zwalks.covid.month <-
  aggregate(zwalks.covid[c("people_all", "fuss_all", "velo_all","new_cases_smoothed")],
            by = zwalks.covid[c( "year", "month")],
            FUN = sum)



#pivot wide table on months
zwalks.month.wide <- zwalks.covid.month %>%
  dplyr::select(year, people_all, month) %>%
  pivot_wider(names_from = year,
              values_from = people_all)


#############################################################################
# Look at the data
#############################################################################
#Look 
sapply(zwalks, n_distinct)

#############################################################################
# PLOTS
#############################################################################

#Aggregated Data - Daily 2019/2020
p.months1 <- ggplot(zwalks.covid, aes(x=date, y=people_all)) +
  geom_point(na.rm = TRUE, size = 0.75) +
  xlab("") +
  ylab("Daily Total") +
  scale_x_date(
    labels = date_format("%b"),
    breaks = "1 month") + 
  theme(axis.text.x = element_text(angle = 45)) +
  geom_point(aes(colour = factor(year)), size = 2)+
  theme(legend.position = "none")

# 
# p.months2 <- ggplot(zwalks.covid, aes(x=factor(month, level=month.abb), y=people_all)) +
#   geom_point(na.rm = TRUE, size = 0.75) +
#   xlab(" ") +
#   ylab("Total Pedestrians and Cyclists") +
#   theme(axis.text.x = element_text(angle = 45)) +
#   geom_point(aes(colour = factor(year)), size = 4)


p.months3 <- ggplot(zwalks.covid.month, aes(x=factor(month, level=month.abb), y=people_all)) +
  geom_line(na.rm = TRUE, size = 0.75) +
  xlab("") +
  ylab("Monthly Total") +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_point(aes(colour = factor(year)), size = 4)+
  labs(color = "Year") 


p.months4 <-ggplot(zwalks.covid.month, aes(x=factor(month, level=month.abb), y=people_all, group=year, colour=year))+
  geom_line(size=2)+
  xlab("") +
  ylab(" ") +
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_discrete(name = "Year")

plot.months <- ggarrange(
  p.months1,
  ggarrange(
    p.months3,
    p.months4,
    common.legend = TRUE,
    legend = "bottom"
  ),
  nrow = 2
)

plot.months <- annotate_figure(
  plot.months,
  top = text_grob(
    "Pedestirans and cyclists in 2019 and 2020",
    color = "black",
    face = "bold",
    size = 14),
  left = text_grob("Counted Pedestrians and Bicycles", color = "black", rot = 90)
)

plot.months



#Months
ggplot(zwalks.covid, aes(fill=year, x=factor(month, level=month.abb), y=people_all)) +
  geom_bar(position = "dodge", stat="identity") +
  ggtitle("Pedestrian and Cyclists in Zurich") +
  xlab("Month") +
  ylab("Pedestrians and Bicycles")

#Months
ggplot(zwalks.covid, aes(fill=year, x=factor(month, level=month.abb), y=velo_all)) +
  geom_bar(position = "dodge", stat="identity") +
  ggtitle("Cyclists in Zurich") +
  xlab("Month") +
  ylab("Pedestrians and Bicycles")


#Months
ggplot(zwalks.covid, aes(fill=year, x=factor(month, level=month.abb), y=fuss_all)) +
  geom_bar(position = "dodge", stat="identity") +
  ggtitle("Pedestrian in Zurich") +
  xlab("Month") +
  ylab("Pedestrians and Bicycles")

#Weekdays
ggplot(zwalks.covid, aes(fill=year, x=factor(weekday, level=weekday_order), y=people_all)) +
  geom_bar(position = "dodge", stat="identity") +
  ggtitle("Pedestrian and Bicycles in Zurich through") +
  xlab("Weekday") +
  ylab("Pedestrians and Bicycles")

# COVID Cases
p.covid <- ggplot(covid, aes(x=date, y=new_cases_smoothed)) +
  geom_line() + 
  ggtitle("New COVID Cases (smoothed)") +
  ylab("COVID Cases") + 
  xlab("Date")
p.covid

#############################################################################
# ANALYZIS
#############################################################################
### define lockdown period
### chronic of lockdowns https://www.stv-fst.ch/de/chronik-coronavirus
lockdown.1 <- seq(as.Date("2020/03/16"), by = "day", length.out = 57)
lockdown.2 <- seq(as.Date("2020/12/22"), by = "day", length.out = 10)

### add lockdown flag to dataset zwalks.covid
zwalks.covid$lockdown <- FALSE
zwalks.covid$lockdown01 <- 0

zwalks.covid[zwalks.covid$date %in% lockdown.1 , ]$lockdown <- TRUE
zwalks.covid[zwalks.covid$date %in% lockdown.1 , ]$lockdown01 <- 1

zwalks.covid[zwalks.covid$date %in% lockdown.2 , ]$lockdown <- TRUE
zwalks.covid[zwalks.covid$date %in% lockdown.1 , ]$lockdown01 <- 1


###  Boxplot 
covid.box <- ggplot(data = zwalks.covid %>% filter(year == 2020),
       mapping = aes(y = people_all,
                     x = lockdown)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  ylab("Pedestrians and Cyclists") +
  xlab("Lockdown")+
  ylim(0, 125000)

###  Plot People vs Cases 
covid.plot <- ggplot(data = zwalks.covid %>% filter(year == 2020),
       aes(y = people_all,
           x = new_cases_smoothed))+
         geom_point() +
  xlab("New Covid Cases Smoothed")+
  ylab("Pedestrians and Cyclists")+
  ylim(0, 125000)+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2))



###  Plot People vs Cases 
covid.bar <- ggplot(data = zwalks.covid %>% filter(year == 2020),
                     aes(y = people_all,
                         x = new_cases_smoothed))+
  geom_bar(stat='identity')+
  xlab("New Covid Cases Smoothed")+
  ylab("Pedestrians and Cyclists")+
  ylim(0, 125000)




### Logistic regression
zwalks.covid.glm <- glm(lockdown ~  people_all, family = "binomial", 
                      data = zwalks.covid)
summary(zwalks.covid.glm)


### Plot the Logistic regression
covid.lr <- ggplot(data = zwalks.covid,
       mapping = aes(y = lockdown01,
                     x = people_all)) + 
  geom_point() +
  geom_smooth(method = "glm", 
              se = FALSE,
              method.args = list(family = "binomial")) 



p.covid <- ggarrange(
  covid.plot,
  covid.box,
  covid.lr,
  align = "hv",
  nrow = 2)


p.covid <- annotate_figure(
  p.covid,
  top = text_grob(
    "Pedestrians and cyclists in 2020 compared to covid cases and lockdown",
    color = "black",
    face = "bold",
    size = 14)
)

p.covid

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



#pivot wide table -- week zones
zwalks.week.zones <- zwalks.day.knr %>%
  group_by(year, week, zones) %>%
  summarize(people_sum = sum(people_all))


zwalks.week.zones <- zwalks.week.zones %>%
  pivot_wider(names_from = year,
              values_from = people_sum)

zwalks.week.zones["difference_people"] <- zwalks.week.zones['2020'] - zwalks.week.zones['2019'] 



#pivot wide table -- week knr
zwalks.week.knr <- zwalks.day.knr %>%
  group_by(year, week, knr) %>%
  summarize(people_sum = sum(people_all))


zwalks.week.knr <- zwalks.week.knr %>%
  pivot_wider(names_from = year,
              values_from = people_sum)

zwalks.week.knr["difference_people"] <- zwalks.week.knr['2020'] - zwalks.week.knr['2019'] 


#############################################################################
# Boxplot locations
#############################################################################


#Cyclists per stadtkreis
ggplot(zwalks.day.knr, aes(fill=year, x=factor(month, level=month.abb), y=velo_all)) +
  geom_bar(position = "dodge", stat="identity") +
  ggtitle("Cyclists in Zurich") +
  xlab("Month") +
  ylab("Total Cyclist") +
  facet_wrap( ~ knr, scales = "free_y")

#Cyclists
ggplot(zwalks.day.knr, aes(fill=year, x=factor(month, level=month.abb), y=velo_all)) +
  geom_bar(position = "dodge", stat="identity") +
  ggtitle("Cyclists in Zurich") +
  xlab("Month") +
  ylab("Total Cyclist") +
  facet_wrap( ~ zones, scales = "free_y")




#############################################################################
# LOCATION animation weekly book1
#############################################################################


# create new directory & subdirectory
ifelse(!dir.exists(file.path("../png_output/")), {dir.create(file.path("../png_output/")); "Directory created"}, "Directory already exists")
ifelse(!dir.exists(file.path("../png_output/zwalks_week")), {dir.create(file.path("../png_output/zwalks_week")); "Subdirectory created"}, "Subirectory already exists")
png_output_path = "../png_output/zwalks_week/"


# -------------------------- prapare plot --------------------------
#load Stadtkreise
zkreis <- st_read("./Stadtkreise/stzh.adm_stadtkreise_v.shp")
head(zkreis)
summary(zkreis)


#load countingstations
zstation <- st_read("./Countingstation/taz.view_eco_standorte.shp")
head(zstation)
zstation <- cbind(zstation, st_coordinates(zstation))
summary(zstation)

# Basic colors
empty_kreis_color = "grey75"
missing_percentile_color = "grey75"
basic_color = "lightblue"
png_output_path = "../png_output/"
zkreis$color <- basic_color


# -------------------------- prapare weekly data --------------------------

### calculate percentile
zwalks.week.knr <- zwalks.week.knr %>%
  group_by(knr) %>% 
  mutate(percrank=(ceiling(percent_rank(difference_people)*10)))
zwalks.week.knr

### Color Codes for percentiles
colfunc <- colorRampPalette(c("darkgreen", "red")) #ff9696
col_list <- colfunc(10)
plot(rep(1,10),col=col_list,pch=19,cex=3)
zwalks.week.knr$color = basic_color

## Set colors for percentile
for (i in 1:nrow(zwalks.week.knr)){
  current.percentile <- zwalks.week.knr$percrank[i]
  ### check if percentile is na
  if(is.na(current.percentile)){
    ### no percentile value found
    ### set color to missing_percentile_color
    zwalks.week.knr$color[i] <- missing_percentile_color
  } else {
    ### value is not na, so there should be a percentile
    ### make sure percentile is a number
    if(is.numeric(current.percentile)){
      if(current.percentile == 0){
        zwalks.week.knr$color[i] <- col_list[1]
      } else {
        ### assign color 
        zwalks.week.knr$color[i] <- col_list[current.percentile]
      }
    }
  }
}


### walk throguh weeks and generate plot image
for (i_week in 1:53){
  
  print(paste("------------------- week: " , toString(i_week)))
  zwalks.currentweek <- zwalks.week.knr %>% filter(week == i_week)
  ### debug
  print(zwalks.currentweek)
  ### assign colors to current week zkreis (zkreis= df to be plotted as a map)
  for (i_kreis in 1:nrow(zkreis)){
    ### reset color to base color in order to avoid missing values not being displayed as last week color
    zkreis$color[i_kreis] <- basic_color
    
    tmp.color <- zwalks.currentweek  %>% filter(knr == i_kreis)
    
    ### check if data for kreis exists.
    if(nrow(tmp.color) == 0) {
      ## kreis dosn't exists. Set color to missing_percentile_color 
      zkreis$color[i_kreis] <-missing_percentile_color
    } else {
      zkreis$color[i_kreis] <-tmp.color$color
    }
  }
  
  ### debug
  # print(zkreis)
  
  # -------------------------- plot map --------------------------
  
  plot.kreis <- ggplot(zkreis) +
    geom_sf(size = 1, color = "black", fill= zkreis$color) +
    geom_sf_text(aes(label = knr), colour = "black") +
    xlab(paste("week: " , toString(i_week)))
  
  plot.station <- plot.kreis 
  print(plot.station)
  
  filenmae <- paste( png_output_path , toString(i_week), "_week_zwalks.png" ,  sep = "" )
  print(filenmae)
  ggsave(filenmae)
}




#############################################################################
# Create Video
#############################################################################


file_name<- list.files(path=png_output_path)
file_path <- paste(png_output_path , file_name,   sep = "" )

file_list <- data.frame(file_name, file_path)
print(file_list)
file_list$sort_index = 0
print(file_list)



for (i in 1:nrow(file_list)){ 
  file_list$sort_index[i] <- strtoi(strsplit(file_list$file_name[i], "_")[[1]][1])
}

file_list_sorted <- arrange(file_list, sort_index)
file_list_sorted

av::av_encode_video(file_list_sorted$file_path, 'zwalks_animation.avi', framerate = 3)


# create new directory & subdirectory
ifelse(!dir.exists(file.path("../png_output/")), {dir.create(file.path("../png_output/")); "Directory created"}, "Directory already exists")
ifelse(!dir.exists(file.path("../png_output/zwalks_week")), {dir.create(file.path("../png_output/zwalks_week")); "Subdirectory created"}, "Subirectory already exists")
png_output_path = "../png_output/zwalks_week"
