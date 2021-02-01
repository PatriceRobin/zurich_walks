#############################################################################
#load packages
#############################################################################
if (!require(tidyverse)) {install.packages('"tidyverse"')}

if (!require(ggplot2)) {install.packages('ggplot2')}


if (!require(scales)) {install.packages('scales')}


#############################################################################
#load data & prepare it
#############################################################################

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

#summarize bicycles and pedestrians
zwalks.day['fuss_all'] <- zwalks.day$fuss_in + zwalks.day$fuss_out
zwalks.day['velo_all'] <- zwalks.day$velo_in + zwalks.day$velo_out
zwalks.day['person_all'] <-
  zwalks.day$velo_all + zwalks.day$fuss_all

#add months and weekdays
zwalks.day$month <- format(zwalks.day$date, "%Y-%m")
zwalks.day$week <- format(zwalks.day$date, "%Y-%V")
zwalks.day$weekday <- weekdays(zwalks.day$date)

weekday_order <- c( "Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag","Samstag","Sonntag")


# overall pedestrians and bicycles
zwalks.agg <-
  aggregate(zwalks.day[c("person_all", "fuss_all", "velo_all")],
            by = zwalks.day[c( "date", "month", "weekday", "week")],
            FUN = sum)


#############################################################################
# PLOTS
#############################################################################

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
ggplot(mapping = aes(y = zwalks.agg$person_all,
                     x = zwalks.agg$lockdown)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  ylab("person_all") +
  xlab("lockdown")


### Logistic regression
zwalks.agg.glm <- glm(lockdown ~  person_all, family = "binomial", 
                      data = zwalks.agg)
summary(zwalks.agg.glm)


### Plot the Logistic regression
ggplot(data = zwalks.agg,
       mapping = aes(y = lockdown01,
                     x = person_all)) + 
  geom_point() +
  geom_smooth(method = "glm", 
              se = FALSE,
              method.args = list(family = "binomial")) 
