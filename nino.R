library(tsibble)
library(tidyverse)
library(feasts)
library(fabletools)


path <- "https://www.cpc.ncep.noaa.gov/data/indices/sstoi.indices"

download.file(path, destfile="temp.data")

colnames <- read_fwf("temp.data", 
                     col_positions=fwf_empty("temp.data"), 
                     n_max=1) %>% gather()

data <- read_fwf("temp.data", skip=1, na = c("99.9")) %>% 
  set_names(c("Year", "Month", "NINO1+2", "NINO1+2.ANOM", "NINO3", "NINO3.ANOM", "NINO4", "NINO4.ANOM", "NINO3+4", "NINO3+4.ANOM")) %>% 
  mutate(Date  = tsibble::make_yearmonth(Year, Month)) %>% 
  select(-Year, -Month) %>% 
  pivot_longer(cols=-Date, names_to = "NinoMeasure", values_to="Anomaly") %>% 
  as_tsibble(index=Date, key=NinoMeasure) %>% 
  model(STL(Anomaly ~ trend() + season(period=12))) %>% 
  components()

write_csv(data %>% mutate(Date=as.Date(Date)), "data/nino.csv")

