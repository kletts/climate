
library(tsibble)
library(tidyverse)
library(feasts)
library(fabletools)


path <- "https://www.cpc.ncep.noaa.gov/data/indices/sstoi.indices"

download.file(path, destfile="temp.data")

data <- read_fwf("temp.data", skip=1, na = c("99.9")) %>% 
  set_names(c("Year", "Month", "NINO1+2.LEVEL", "NINO1+2.ANOM", "NIaNO3.LEVEL", "NINO3.ANOM", "NINO4.LEVEL", "NINO4.ANOM", 
              "NINO3+4.LEVEL", "NINO3+4.ANOM")) %>% 
  mutate(Date  = tsibble::make_yearmonth(Year, Month)) %>% 
  select(-Year, -Month) %>% 
  pivot_longer(cols=-Date, names_to = c("NinoModel", "Measure"), names_sep="\\.", values_to="Value") %>% 
  pivot_wider(id_cols=c(Date, NinoModel), names_from = Measure, values_from = Value) %>% 
  as_tsibble(index=Date, key=NinoModel) %>% 
  model(stl=STL(ANOM ~ trend() + season(period=12))) %>% 
  components() 

write_csv(data %>% mutate(Date=as.Date(Date)), "data/nino.csv")

#BOM data, updated more regularly 

