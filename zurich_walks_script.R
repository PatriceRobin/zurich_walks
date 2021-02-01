#load packages
if(!require(dplyr)){install.packages('dplyr')}

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
  rename(
    datetime = datum
  )


#summarize data on a daily basis
zwalks.day <- aggregate(zwalks[c("velo_in","velo_out","fuss_in","fuss_out")],
                               by = zwalks[c("fk_zaehler","fk_standort", "date","ost", "nord")],
                               FUN=sum
                        )



#geographical data

#install.packages("rgdal")
#ibrary(rgdal)
