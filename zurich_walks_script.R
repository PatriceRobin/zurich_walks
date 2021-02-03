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

zwalks = read.csv("2020_verkehrszaehlungen_werte_fussgaenger_velo.csv")

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
zwalks$month <- format(zwalks$date, "%B")
zwalks$week <- format(zwalks$date, "%Y-%V")
zwalks$weekday <- weekdays(zwalks$date)


#summarize data on a daily basis
zwalks.day <-
  aggregate(zwalks[c("velo_in", "velo_out", "velo_all", "fuss_in", "fuss_out", "fuss_all", "people_all")],
            by = zwalks[c("fk_zaehler", "fk_standort", "date", "month", "week", "weekday", "ost", "nord")],
            FUN = sum)

weekday_order <- c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday")


# overall pedestrians and bicycles
zwalks.agg <-
  aggregate(zwalks.day[c("people_all", "fuss_all", "velo_all")],
            by = zwalks.day[c( "date", "month", "weekday", "week")],
            FUN = sum)

# merge zwalks.agg with covid data
zwalks.agg <- (merge(covid, zwalks.agg, by = 'date'))


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
  theme(axis.text.x = element_text(angle = 45))


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
zkreis <- st_read("./Stadtkreise/stzh.adm_stadtkreise_v.shp")
head(zkreis)
summary(zkreis)


#load countingstations
zstation <- st_read("./Countingstation/taz.view_eco_standorte.shp")
head(zstation)

zstation <- cbind(zstation, st_coordinates(zstation))

summary(zstation)



#Districts
zdistricts <- st_read("./Quartieranalyse/QUARTIERE_F.shp")
summary(zdistricts)



#Bauzonen
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





#############################################################################
# LOCATION animation
#############################################################################



# get kreis list
zwalks.day.knr.per_kreis <-zwalks.day.knr[zwalks.day.knr$date == "2020-01-03" , ]
kreis_list <-zwalks.day.knr.per_kreis %>%
  group_by(zwalks.day.knr.per_kreis$knr) %>%
  summarize(total_kreis = sum(people_all, na.rm = TRUE))
kreis_list <- kreis_list[,1]
kreis_list <- t(kreis_list)
kreis_list

kreis_list <- 1:12
kreis_list

## create a day range
day_range <- seq(as.Date("2020/01/01"), by = "day", length.out = 120) #
day_range

## creating a data frame for the summary output
zwalks.summary <- data.frame(day_range)
colnames(zwalks.summary)[1] <- "day"

## add each kreis to the summary
for (i_kreis in 1:length(kreis_list)){ #ncol
  zwalks.summary <- add_column(zwalks.summary, !!(toString(kreis_list[i_kreis])) := 0)
}

zwalks.summary
zwalks.summary[i_day]

for (i_day in 1:nrow(zwalks.summary)){
  print(zwalks.summary$day[i_day])
  zwalks.day.knr.per_kreis <-zwalks.day.knr[zwalks.day.knr$date == zwalks.summary$day[i_day] , ]
  #zwalks.day.knr.per_kreis <-zwalks.day.knr[zwalks.day.knr$date == "2020-01-03" , ]
  zwalks.day.knr.per_kreis
  kreis_people_per_day <-(t(zwalks.summary_daykreis <-zwalks.day.knr.per_kreis %>%
                              group_by(zwalks.day.knr.per_kreis$knr) %>%
                              summarize(total_kreis = sum(people_all, na.rm = TRUE))))
  print(kreis_people_per_day)
  print(length(kreis_people_per_day))
  
  if (length(kreis_people_per_day) > 21) {
    ### add empty data for kreis 7 & merge columns
    kreis_people_per_day_dummy <- cbind(kreis_people_per_day[,1:6] , c(7,0), kreis_people_per_day[,7:11])
    kreis_people_per_day <- kreis_people_per_day_dummy
    kreis_people_per_day
    
    for (icol in 1:ncol(kreis_people_per_day)){
      zwalks.summary[i_day,icol+1]= kreis_people_per_day[2,icol]
    }
    
    zwalks.summary
    
  } else {
    print("missing values for day:")
    print(zwalks.summary$day[i_day])
  }
  
}

# remove empty rows
zwalks.summary <- filter(zwalks.summary, zwalks.summary$`1` > 0)


zwalks.summary



# moving average
zwalks.summary.ma <- zwalks.summary

for (i_col in 2:ncol(zwalks.summary)){
  tmp_ma <- zwalks.summary %>% 
    mutate(
      lag0=(zwalks.summary[,i_col]),
      lag1=lag(zwalks.summary[,i_col]),
      lag2=lag(zwalks.summary[,i_col],2),
      lag3=lag(zwalks.summary[,i_col],3),
      lag4=lag(zwalks.summary[,i_col],4),
      lag5=lag(zwalks.summary[,i_col],5),
      lag6=lag(zwalks.summary[,i_col],6),
      ma=(lag0+lag1+lag2+lag3+lag4+lag5+lag6)/7)
  zwalks.summary.ma[,i_col] <- tmp_ma$ma
}
zwalks.summary.ma


write.csv(zwalks.summary.ma, "zwalks.summary.ma.csv", row.names = FALSE)



### calculate percentile
### calc pct using exact numbers
#zwalks.summary.pct <- zwalks.summary

### calc pct using moving average
zwalks.summary.pct <- zwalks.summary.ma
zwalks.summary.pct = mutate(zwalks.summary.pct, `1` = ntile(zwalks.summary.ma$`1`,10))
zwalks.summary.pct = mutate(zwalks.summary.pct, `2` = ntile(zwalks.summary.ma$`2`,10))
zwalks.summary.pct = mutate(zwalks.summary.pct, `3` = ntile(zwalks.summary.ma$`3`,10))
zwalks.summary.pct = mutate(zwalks.summary.pct, `4` = ntile(zwalks.summary.ma$`4`,10))
zwalks.summary.pct = mutate(zwalks.summary.pct, `5` = ntile(zwalks.summary.ma$`5`,10))
zwalks.summary.pct = mutate(zwalks.summary.pct, `6` = ntile(zwalks.summary.ma$`6`,10))
zwalks.summary.pct = mutate(zwalks.summary.pct, `7` = 5) #ntile(zwalks.summary$`7`,10))
zwalks.summary.pct = mutate(zwalks.summary.pct, `8` = ntile(zwalks.summary.ma$`8`,10))
zwalks.summary.pct = mutate(zwalks.summary.pct, `9` = ntile(zwalks.summary.ma$`9`,10))
zwalks.summary.pct = mutate(zwalks.summary.pct, `10` = ntile(zwalks.summary.ma$`10`,10))
zwalks.summary.pct = mutate(zwalks.summary.pct, `11` = ntile(zwalks.summary.ma$`11`,10))
zwalks.summary.pct = mutate(zwalks.summary.pct, `12` = ntile(zwalks.summary.ma$`12`,10))

### remove first 7 days because of moving average
zwalks.summary.pct <- zwalks.summary.pct[7:nrow(zwalks.summary.pct),]

tail(zwalks.summary.pct)
zwalks.summary
write.csv(zwalks.summary, "zwalks.summary.csv", row.names = FALSE)
write.csv(zwalks.summary.pct, "zwalks.summary.pct.csv", row.names = FALSE)


### Color Codes for percentiles
colfunc <- colorRampPalette(c("white", "#ff9696"))
col_list <- colfunc(10)
plot(rep(1,10),col=col_list,pch=19,cex=3)

## Set colors for percentile
zwalks.summary.colors <-zwalks.summary.pct
for (i_col in 1:length(col_list)){
  print(col_list[i_col])
  zwalks.summary.colors[zwalks.summary.colors == i_col] <- col_list[i_col]   #"#FF0000"
}

zwalks.summary.colors



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
zkreis$color = "lightblue"
png_output_path = "../png_output/"

#zwalks.summary <- filter(zwalks.summary, zwalks.summary$`1` > 0)



# -------------------------- plot map and save png --------------------------
for (i_day_index in 7:nrow(zwalks.summary)){ 
  print("Prepare day:")
  tmp_current_date <- zwalks.summary$day[i_day_index]
  print(tmp_current_date)
  
  # set color of current day
  for (i_col in 1:nrow(zkreis)){
    #zkreis$color[zkreis$knr == i_col] <- zwalks.summary.colors[i_day_index,i_col + 1] 
    #print(i_col)
    #print(zkreis[zkreis$knr == i_col,]$color)
    zkreis[zkreis$knr == i_col,]$color <- zwalks.summary.colors[i_day_index, (i_col + 1)]
    zkreis[zkreis$knr == 7,]$color  <- empty_kreis_color
  }
  
  #plot Stadtkreise and Coutingstations
  plot.kreis <- ggplot(zkreis) +
    geom_sf(size = 1, color = "black", fill= zkreis$color) +
    geom_sf_text(aes(label = knr), colour = "black") +
    xlab(tmp_current_date)
  
  
  ### Change plot title when lockdown
  if (length(lockdown.1[lockdown.1 == tmp_current_date]) > 0) {
    plot_title <- "Lockdown !"
  } else {
    plot_title <- ""
  } 
  
  
  plot.station <- plot.kreis + ggtitle(plot_title)
  #+ geom_point(data = zstation, aes(x=X, y=Y), size = 3,  shape = 23, fill = "darkred")
  
  
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
  
  
  covid.daterange<- filter(covid, between(covid$date, as.Date("2020-01-01"), as.Date(toString(date_plot_end))))
  #covid.daterange<- filter(covid, between(covid$date, as.Date("2020-01-01"), as.Date("2020-04-01")))
  print(covid.daterange$date)
  print(covid.daterange$new_cases_smoothed)
  
  p.covid <- ggplot(covid.daterange, aes(x=date, y=new_cases_smoothed)) +
    geom_line(color="red", size=0.7) + 
    ggtitle("COVID Cases") +
    ylab("") + 
    xlab("")
  print(p.covid)
  
  ### Arrange multiple plots in one figure
  plot.arranged <- ggarrange(plot.station, p.covid, heights = c(2, 0.7),
                             ncol = 1, nrow = 2)
  
  #print(plot.station)
  print(plot.arranged)
  
  filenmae <- paste( png_output_path , toString(i_day_index), "_zwalks.png" ,  sep = "" )
  print(filenmae)
  ggsave(filenmae)
  
  
}  

# -------------------------- create video --------------------------

file_name<- list.files(path=png_output_path)
file_path <- paste(png_output_path , file_name,   sep = "" )

file_list <- data.frame(file_name, file_path)
file_list$sort_index = 0



for (i in 1:nrow(file_list)){ 
  file_list$sort_index[i] <- strtoi(strsplit(file_list$file_name[i], "_")[[1]][1])
}

file_list_sorted <- arrange(file_list, sort_index)


av::av_encode_video(file_list_sorted$file_path, 'zwalks_animation.mp4', framerate = 3)