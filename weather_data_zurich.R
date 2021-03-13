#source: https://data.stadt-zuerich.ch/dataset/ugz_meteodaten_tagesmittelwerte

zweather20 <- read.csv("./Data/weather_zurich_2020.csv") # 2020
zweather19 <- read.csv("./Data/weather_zurich_2019.csv") # 2019

zweather_u <- dplyr::union(zweather20, zweather19)

zweather_u <- setNames(zweather_u, tolower(names(zweather_u)))

#day
zweather_u['date'] <- as.Date(zweather_u$ï..datum)


#filter standort 
zweather_filter <- zweather_u %>%
  dplyr::filter(standort == "Zch_Schimmelstrasse")


# pivot wide table on months
zweather <- zweather_filter %>%
  dplyr::select(date, standort, parameter, wert) %>%
  pivot_wider(names_from = parameter,
              values_from = wert)

#rename values
zweather <- zweather %>%
  rename(temp_C = T) %>%
  rename(rain_min = RainDur) %>%
  rename(airpres_hPa = p) %>%
  rename(maxtemp_C = T_max_h1)


