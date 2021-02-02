#############################################################################
#load packages
#############################################################################
if (!require(tidyverse)) {install.packages('"tidyverse"')}
if (!require(ggplot2)) {install.packages('ggplot2')}
if (!require(scales)) {install.packages('scales')}

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
