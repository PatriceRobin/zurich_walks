#load packages
if (!require(tidyverse)) {install.packages('"tidyverse"')}

if (!require(ggplot2)) {install.packages('ggplot2')}


if (!require(scales)) {install.packages('scales')}



#load data
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


#summarize data on a daily basis
zwalks.day <-
  aggregate(zwalks[c("velo_in", "velo_out", "fuss_in", "fuss_out")],
            by = zwalks[c("fk_zaehler", "fk_standort", "date", "ost", "nord")],
            FUN = sum)


zwalks.day['fuss_all'] <- zwalks.day$fuss_in + zwalks.day$fuss_out
zwalks.day['velo_all'] <- zwalks.day$velo_in + zwalks.day$velo_out
zwalks.day['person_all'] <-
  zwalks.day$velo_all + zwalks.day$fuss_all

zwalks.day$month <- format(zwalks.day$date, "%Y-%m")
zwalks.day$weekday <- weekdays(zwalks.day$date)
weekday_order <- c( "Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag","Samstag","Sonntag")

#geographical data

#install.packages("rgdal")
#ibrary(rgdal)

#look at data
# plot air temp

ggplot(zwalks.day, aes(x=date, y=person_all)) +
  geom_point(na.rm = TRUE, size = 0.75) +
  ggtitle("Pedestrian and Bicycles in Zurich through 2020") +
  xlab("2020") +
  ylab("Pedestrians and Bicycles")



# overall pedestrians and bicycles / daily
zwalks.agg <-
  aggregate(zwalks.day[c("person_all", "fuss_all", "velo_all")],
            by = zwalks.day[c( "date", "month", "weekday")],
            FUN = sum)



#ggplot monthly 
ggplot(zwalks.agg, aes(x=date, y=person_all)) +
  geom_point(na.rm = TRUE, size = 0.75) +
  ggtitle("Pedestrian and Bicycles in Zurich through 2020") +
  xlab("2020") +
  ylab("Pedestrians and Bicycles") +
  scale_x_date(
    labels = date_format("%B"),
    breaks = "1 month") + 
  theme(axis.text.x = element_text(angle = 45))


ggplot(zwalks.agg, aes(x=month, y=person_all)) +
  stat_summary(fun = "sum", geom = "bar")+
  ggtitle("Pedestrian and Bicycles in Zurich through 2020") +
  xlab("2020") +
  ylab("Pedestrians and Bicycles")


ggplot(zwalks.agg, aes(x=factor(weekday, level=weekday_order), "Dienstag" , y=person_all)) +
  stat_summary(fun = "mean", geom = "bar")+
  ggtitle("Pedestrian and Bicycles in Zurich through 2020") +
  xlab("2020") +
  ylab("Pedestrians and Bicycles")
