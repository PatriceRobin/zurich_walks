#load packages
if (!require(dplyr)) {
  install.packages('dplyr')
}
if (!require(ggplot2)) {
  install.packages('ggplot2')
}



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
