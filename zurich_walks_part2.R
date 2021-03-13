## ----setup, include=FALSE--------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE ,
                      message=FALSE,
                      warning = FALSE,
                      results = "hide"
                      )



## ----load packages---------------------------------------------------------------------------------------------------------
# -------------------------- Load Packages --------------------------
# this line of code only runs, if these packages are required
if (!require(tidyverse)) {install.packages('"tidyverse"')}
if (!require(ggplot2)) {install.packages('ggplot2')}
if (!require(scales)) {install.packages('scales')}
if (!require(rgdal)) {install.packages('rgdal')}
if (!require(sf)) {install.packages('sf')}
if (!require(rasterVis)) {install.packages('rasterVis')}
if (!require(dplyr)) {install.packages('dplyr')}
if (!require(av)) {install.packages('av')}
if (!require(ggpubr)) {install.packages('ggpubr')}
if (!require(gdtools)) {install.packages('gdtools')}
if (!require(kableExtra)) {install.packages("kableExtra", dependencies = TRUE)}
if (!require(janitor)) {install.packages("janitor")}
if (!require(stargazer)) {install.packages("stargazer")}
if (!require(vembedr)) {install.packages("vembedr")}
if (!require(rmarkdown)) {install.packages("rmarkdown")}
if (!require(devtools)) {install.packages("devtools")}
if (!require(usethis)) {install.packages("usethis")}



## ----english_local---------------------------------------------------------------------------------------------------------
# set local working directory to English
Sys.setlocale("LC_TIME", "English")


## ----load_covid------------------------------------------------------------------------------------------------------------
# load covid data
# source : https://ourworldindata.org/coronavirus/country/switzerland?country=~CHE
covid <- read.csv("./Data/covid-data.csv", encoding="UTF-8")
covid$date <- as.Date(covid$date,format = "%d.%m.%y")



## ----load_zwalks-----------------------------------------------------------------------------------------------------------
# load pedestrian and cyclist data
# source: https://data.stadt-zuerich.ch/dataset/ted_taz_verkehrszaehlungen_werte_fussgaenger_velo
zwalks2020 <- read.csv("./Data/2020_verkehrszaehlungen_werte_fussgaenger_velo.csv") # 2019
zwalks2019 <- read.csv("./Data/2019_verkehrszaehlungen_werte_fussgaenger_velo.csv") # 2020

zwalks <- dplyr::union(zwalks2020, zwalks2019)



## ----load_kreis------------------------------------------------------------------------------------------------------------
# load Stadtkreise
# source https://data.stadt-zuerich.ch/dataset/geo_stadtkreise
zkreis <- st_read("./Data/Stadtkreise/stzh.adm_stadtkreise_v.shp")



## ----load_zstation---------------------------------------------------------------------------------------------------------
# load countingstations
# source https://data.stadt-zuerich.ch/dataset/geo_standorte_der_automatischen_fuss__und_vlozaehlungen

zstation <- st_read("./Data/Countingstation/taz.view_eco_standorte.shp")
zstation <- cbind(zstation, st_coordinates(zstation)) # split coordinates into X and Y



## ----load_zdistricts-------------------------------------------------------------------------------------------------------
# Districts
# source: https://data.stadt-zuerich.ch/dataset/ktzh_quartieranalyse
zdistricts <- st_read("./Data/Quartieranalyse/QUARTIERE_F.shp")



## ----load_zzones-----------------------------------------------------------------------------------------------------------
# Bauzonen
# sources https://data.stadt-zuerich.ch/dataset/geo_nutzungsplanung___kommunale_bau__und_zonenordnung__bzo__jahresendstand_2020
zzones <- st_read("./Data/Bauzonen/afs.bzo_zone_v.shp")


## ----clean_data------------------------------------------------------------------------------------------------------------
# extract date from datetime
zwalks['date'] <- as.Date(zwalks$DATUM)

# lower cases column names
zwalks <- setNames(zwalks, tolower(names(zwalks)))

# replace NA with 0
zwalks[is.na(zwalks)] <- 0

# rename columns
zwalks <- zwalks %>%
  rename(datetime = datum)


## ----summarize_daily-------------------------------------------------------------------------------------------------------
# -------------------------- Aggregate People --------------------------
# summarize data on a daily basis
zwalks.day <-
  aggregate(zwalks[c("velo_in", "velo_out", "fuss_in", "fuss_out")],
            by = zwalks[c("fk_zaehler", "fk_standort", "date", "ost", "nord")],
            FUN = sum)

# summarize bicycles and pedestrians
zwalks.day['fuss_all'] <- zwalks.day$fuss_in + zwalks.day$fuss_out
zwalks.day['velo_all'] <- zwalks.day$velo_in + zwalks.day$velo_out
zwalks.day['people_all'] <- zwalks.day$velo_all + zwalks.day$fuss_all


## ----zwalks_date-----------------------------------------------------------------------------------------------------------
# -------------------------- Date Format --------------------------
# add months and weekdays
zwalks.day$year <- format(zwalks.day$date, "%Y")
zwalks.day$month <- format(zwalks.day$date, "%b")
zwalks.day$week <- format(zwalks.day$date, "%W") # create weeks from date
zwalks.day$week <- as.numeric(zwalks.day$week) + 1 # make numeric and add 1 so that it goes from 1 to 53

# weekdays
zwalks.day$weekday <- weekdays(zwalks.day$date)

weekday_order <- c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday")


## ----join_zwalks_covid-----------------------------------------------------------------------------------------------------
# overall pedestrians and bicycles
zwalks.agg <-
  aggregate(zwalks.day[c("people_all", "fuss_all", "velo_all")],
            by = zwalks.day[c( "date", "month", "weekday", "week","year")],
            FUN = sum)

# left join zwalks agg and covid on date
zwalks.covid <- zwalks.agg %>%
  left_join(dplyr::select(covid,
                          date,
                          new_cases_smoothed),
            by = "date")



## ----agg_monthly-----------------------------------------------------------------------------------------------------------
# replace NA with 0 in covid cases
zwalks.covid[is.na(zwalks.covid)] <- 0

zwalks.covid.month <-
  aggregate(zwalks.covid[c("people_all", "fuss_all", "velo_all","new_cases_smoothed")],
            by = zwalks.covid[c( "year", "month")],
            FUN = sum)


# pivot wide table on months
zwalks.month.wide <- zwalks.covid.month %>%
  dplyr::select(year, people_all, month) %>%
  pivot_wider(names_from = year,
              values_from = people_all)


## --------------------------------------------------------------------------------------------------------------------------
# -------------------------- Lockdown Periods --------------------------
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



## ----counting.stations-----------------------------------------------------------------------------------------------------
# -------------------------- Mapping Counting Stations --------------------------

# plot Stadtkreise and counting stations
plot.map <- ggplot()+
  geom_sf(data = zkreis, size = 0, color = "black", fill="lightblue")+
  geom_sf(data = zzones, size = 0.25, color = "darkgrey", fill="white")

plot.kreis <- plot.map +
    geom_sf(data = zkreis, size = 1, color = "black", fill= NA )+
    geom_sf_text(data = zkreis, aes(label = knr), colour = "black", size=10)
  
# counting stations  
plot.station <- plot.kreis + geom_point(data = zstation, aes(x=X, y=Y), size = 3, 
                                    shape = 23, fill = "red")
  
plot.station


# join the shape files - left on station
zstation.kreis <- st_join(zstation, zkreis, 
                  join = st_within)




## ----table.stations, results="markup"--------------------------------------------------------------------------------------
# -------------------------- Counting Stations --------------------------

# left Stadtkreise
counting.stations <- tibble(District = 1:12) %>% # empty tibble with 12 district
  left_join(zstation.kreis %>% # left join
  group_by(knr) %>%
  summarise(length(unique(abkuerzung))) %>%
  rename("Quantity of Stations" = "length(unique(abkuerzung))"),
    by = c("District" = "knr"))

counting.stations <- counting.stations[0:2]

counting.stations <- counting.stations %>%
  adorn_totals("row") 


options(knitr.kable.NA = "0")

knitr::kable(counting.stations,
             align = "cr",
             caption = "Couting Stations per District") %>%
  kable_styling(bootstrap_options = "striped") %>%  # from the kabelExtra package
  row_spec(7, color = 'red', bold = TRUE) # District 7 has 0 counting stations --> mark red and bold




## ----month_plots, results="markup"-----------------------------------------------------------------------------------------
# -------------------------- Counted Pedestrians and Cyclists --------------------------

# Aggregated Data - Daily 2019/2020
p.months1 <- ggplot(zwalks.covid, aes(x=date, y=people_all)) +
  geom_point(na.rm = TRUE, size = 0.75) +
  xlab("") +
  ylab("Daily per Location ") +
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


p.months3 <- ggplot(zwalks.covid.month, aes(x=factor(month, level=month.abb), y=people_all))+
  geom_line(na.rm = TRUE, size = 0.75) +
  xlab("") +
  ylab("Monthly Total") +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_point(aes(colour = factor(year)), size = 2)+
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
    "Pedestrians and cyclists in 2019 and 2020",
    color = "black",
    face = "bold",
    size = 14),
    left = text_grob("Counted Pedestrians and Bicycles", color = "black", rot = 90, hjust=0.5)
  )

plot.months


## ----covid_plots, results="markup"-----------------------------------------------------------------------------------------
# -------------------------- Covid and Lockdown --------------------------

###  Boxplot
covid.box <-
  ggplot(
    data = zwalks.covid %>% filter(date >= "2020-03-01"),
    mapping = aes(y = people_all,
                  x = lockdown)
  ) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  ylab("People") +
  xlab("Lockdown") +
  ylim(0, 125000)

###  Plot People vs Cases
covid.plot <-
  ggplot(data = zwalks.covid %>% filter(date >= "2020-03-01"),
         aes(y = people_all,
             x = new_cases_smoothed)) +
  geom_point() +
  xlab("New Covid Cases Smoothed") +
  ylab("People") +
  ylim(0, 125000) +
  geom_smooth(method = "lm")

### Logistic regression
zwalks.covid.glm <-
  glm(lockdown ~  people_all, family = "binomial",
      data = zwalks.covid %>% filter(date >= "2020-03-01"))


### Plot the Logistic regression
covid.lr1 <- ggplot(data = zwalks.covid %>% filter(date >= "2020-03-01"),
                   mapping = aes(y = lockdown01,
                                 x = people_all)) +
  xlab("Pedestrians and Cyclists") +
  ylab("Lockdown")+
  geom_point() +
  geom_smooth(
    method = "glm",
    se = FALSE,
    method.args = list(family = "binomial")
  )


# arrange plots
p.covid <- ggarrange(covid.plot,
                     covid.box,
                     covid.lr1,
                     align = "hv",
                     nrow = 3)


# annotate figures
p.covid <- annotate_figure(
  p.covid,
  top = text_grob(
    "Pedestrians and cyclists in 2020 compared to covid cases and lockdown",
    color = "black",
    face = "bold"
  )
)

p.covid



## ----covid_glm-------------------------------------------------------------------------------------------------------------
stargazer(zwalks.covid.glm, title="Regression Results", type = 'text') # nice print fo the glm binomial logistic regression


## ----zones-----------------------------------------------------------------------------------------------------------------
# -------------------------- Zones --------------------------
### Bauzonen / usage and location zones
zstation.zones <- st_join(zstation.kreis, zzones,
                          join = st_within)

# look at the created zstation.zones
head(zstation.zones)
summary(zstation.zones)
unique((zstation.zones$typ))

# create a copy of typ
zstation.zones["zones"] <- zstation.zones$typ


### summarize zones
zstation.zones$zones[zstation.zones$zones == "W4"] <- "Living" # Viergeschossige  Wohnzone
zstation.zones$zones[zstation.zones$zones == "W5"] <- "Living" # Fünfgeschossige Wohnzone
zstation.zones$zones[zstation.zones$zones == "Z6"] <- "Center" # Sechsgeschossige Zentrumszone
zstation.zones$zones[zstation.zones$zones == "Z5"] <- "Center" # Fünfgeschossige Zentrumszone
zstation.zones$zones[zstation.zones$zones == "K"] <- "Center" # Kernzone
zstation.zones$zones[zstation.zones$zones == "F"] <- "Empty/Parking" # Freihaltezone
zstation.zones$zones[zstation.zones$zones == "FP"] <- "Empty/Parking" # Parking
zstation.zones$zones[zstation.zones$zones == "WLD"] <- "Recreation" # Wald
zstation.zones$zones[zstation.zones$zones == "QI"] <- "Recreation" # Quartier Erhaltungszone
zstation.zones$zones[zstation.zones$zones == "GWS"] <- "Recreation" # Gewässer
zstation.zones$zones[zstation.zones$zones == "IG I"] <- "Industry" # Industrie


## ----tables_zones, results="markup"----------------------------------------------------------------------------------------
### table with zones, type and counting stations
table.zones.typ <- zstation.zones %>%
  group_by(typ, zones) %>%
  summarise(length(unique(abkuerzung))) %>%
  rename("Quantity of Stations" = "length(unique(abkuerzung))") %>%
  arrange(desc(zones))


# nice table on the counting stations per zone
knitr::kable(st_drop_geometry(table.zones.typ), #remove geometry
             align = "cr",
             caption = "Counting Stations per zone") %>%
  kable_styling(bootstrap_options = "striped") # from the kabelExtra package




### table with only the zones

table.zones <- zstation.zones %>%
  group_by(zones) %>%
  summarise(length(unique(abkuerzung))) %>%
  rename("Quantity of Stations" = "length(unique(abkuerzung))") %>%
  arrange(desc(zones))

#remove geometry

knitr::kable(st_drop_geometry(table.zones),
             align = "cr",
             caption = "Summarized Zones") %>%
  kable_styling(bootstrap_options = "striped") # from the kabelExtra package


## ----zwalks.difference-----------------------------------------------------------------------------------------------------
# join everything into one table
zwalks.day.knr <- zwalks.day %>% left_join (dplyr::select(zstation.zones,
                                                fk_zaehler,
                                                knr,
                                                kname,
                                                zones),
                                            by = "fk_zaehler")



# group by-- week zones
zwalks.week.zones <- zwalks.day.knr %>%
  group_by(year, week, zones) %>%
  summarize(people_sum = sum(people_all))

# pivot wide table -- year zones
zwalks.week.zones <- zwalks.week.zones %>%
  pivot_wider(names_from = year,
              values_from = people_sum)

# calculate the difference between the 2020 and 2019
zwalks.week.zones["difference_people"] <- zwalks.week.zones['2020'] - zwalks.week.zones['2019']



#pivot wide table -- week knr
zwalks.week.knr <- zwalks.day.knr %>%
  group_by(year, week, knr) %>%
  summarize(people_sum = sum(people_all))


zwalks.week.knr <- zwalks.week.knr %>%
  pivot_wider(names_from = year,
              values_from = people_sum)

zwalks.week.knr["difference_people"] <- zwalks.week.knr['2020'] - zwalks.week.knr['2019'] 


## ----plot_zones, results="markup", fig.width = 12, fig.height = 6----------------------------------------------------------
# plot: people per Zone

ggplot(zwalks.day.knr, aes(fill=year, x=factor(month, level=month.abb), y=people_all)) +
  geom_bar(position = "dodge", stat="identity") +
  ggtitle("Pedestrians and cyclists in the different zones") +
  xlab("Month") +
  ylab("Total People") +
  facet_wrap( ~ zones, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45))+
  theme(legend.position = 'bottom')


## ----plot_twelve_districts, results="markup", fig.width = 12, fig.height = 6-----------------------------------------------
# plot: people per district

ggplot(zwalks.day.knr, aes(fill=year, x=factor(month, level=month.abb), y=people_all)) +
  geom_bar(position = "dodge", stat="identity") +
  ggtitle("Pedestrians and cyclists in the districts ") +
  xlab("Month") +
  ylab("Total People") +
  facet_wrap( ~ knr, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45)) +
  labs(fill = "Year")+
  theme(legend.position = 'bottom')



## ----create_video, fig.show='hide'-----------------------------------------------------------------------------------------
# -------------------------- animated video --------------------------

# create new directory & subdirectory
ifelse(!dir.exists(file.path("./Video/")), {dir.create(file.path("./Video/")); "Directory created"}, "Directory already exists")
ifelse(!dir.exists(file.path("./Video/png_output/")), {dir.create(file.path("./Video/png_output/")); "Subdirectory created"}, "Subirectory already exists")
png_output_path = "./Video/png_output/"


# -------------------------- prapare plot --------------------------
#load Stadtkreise
zkreis <- st_read("./Data/Stadtkreise/stzh.adm_stadtkreise_v.shp")
head(zkreis)
summary(zkreis)


#load countingstations
zstation <- st_read("./Data/Countingstation/taz.view_eco_standorte.shp")
head(zstation)
zstation <- cbind(zstation, st_coordinates(zstation))
summary(zstation)

# Basic colors
empty_kreis_color = "grey75"
missing_percentile_color = "grey75"
basic_color = "lightblue"
png_output_path = "./Video/png_output/"
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
tmp_current_date <- as.Date("2020-01-01")
for (i_week in 1:53){
  print(i_week)
  if(i_week ==1){
    tmp_current_date <- as.Date("2020-01-01")
  } else if (i_week == 2){
    tmp_current_date <- as.Date("2020-01-06")
  } else {
    tmp_current_date <- (tmp_current_date + 7)
  }
  print(tmp_current_date)
    
  ## bookpat
  print(i)
  print(paste("current week: " , toString(i_week)))


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
    xlab(paste("Date: " , (tmp_current_date)))
  
  plot.station <- plot.kreis 
  
  
  ### Plot COVID Cases
  min_date_on_plot <- "2020-03-1"
  if(as.Date(tmp_current_date) > as.Date(min_date_on_plot)) {
    date_plot_end <- tmp_current_date
    print("date is larger")
  } else {
    date_plot_end <-toString(tmp_current_date)
    date_plot_end <- min_date_on_plot
    print("date is smaller")
  }
  
  # -------------------------- plot covid cases --------------------------
  ### Plot COVID Cases
  min_date_on_plot <- "2020-01-06"
  if(as.Date(tmp_current_date) > as.Date(min_date_on_plot)) {
    date_plot_end <- tmp_current_date
    print("date is larger")
  } else {
    date_plot_end <-toString(tmp_current_date)
    date_plot_end <- min_date_on_plot
    print("date is smaller")
  }
  
  
  covid.daterange<- filter(covid, between(covid$date, as.Date("2020-01-01"), as.Date(toString(date_plot_end))))
  #covid.daterange<- filter(covid, between(covid$date, as.Date("2020-01-01"), as.Date("2020-04-01")))
  print(covid.daterange$date)
  print(covid.daterange$new_cases_smoothed)
  
  p.covid <- ggplot(covid.daterange, aes(x=date, y=new_cases_smoothed)) +
    geom_line(color="green", size=0.7) + 
    ggtitle("COVID Cases") +
    ylab("") + 
    xlab("")
  #print(p.covid)
  
  ### Arrange multiple plots in one figure
  plot.arranged <- ggarrange(plot.station, p.covid , heights = c(2, 0.7),
                             ncol = 1, nrow = 2)
  print(plot.arranged )
  
  filenmae <- paste( png_output_path , '/', toString(i_week), "_week_zwalks.png" ,  sep = "" )
  print(filenmae)
  ggsave(filenmae)
}


# -------------------------- create video --------------------------


file_name<- list.files(path=png_output_path)
file_path <- paste(png_output_path , '/', file_name,   sep = "" )

file_list <- data.frame(file_name, file_path)
print(file_list)
file_list$sort_index = 0
print(file_list)



for (i in 1:nrow(file_list)){ 
  file_list$sort_index[i] <- strtoi(strsplit(file_list$file_name[i], "_")[[1]][1])
}

file_list_sorted <- arrange(file_list, sort_index)
file_list_sorted

av::av_encode_video(file_list_sorted$file_path, './Video/zurich_walks_video.avi', framerate = 2)




## ----vimeo_url-------------------------------------------------------------------------------------------------------------
suggest_embed("https://vimeo.com/516652769")


## ----vimeo_link, results="asis"--------------------------------------------------------------------------------------------
embed_vimeo("516652769")


## ----biblio_packages, include=FALSE----------------------------------------------------------------------------------------

used_packages <- names(sessionInfo()$otherPkgs)
knitr::write_bib(used_packages, file = './Sources/packages.bib')

