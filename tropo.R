
# Troposphere temperature trends

library(tidyverse)
library(readr)
library(tsibble)
library(feasts)


colnames <- c("Year", "Month", 
  "Globe-Total", "Globe-Land", "Globe-Ocean",
  "NorthernHemisphere-Total", "NorthernHemisphere-Land", "NorthernHemisphere-Ocean",  
  "SourthernHemisphere-Total", "SourthernHemisphere-Land", "SourthernHemisphere-Ocean", 
  "Tropics-Total", "Tropics-Land", "Tropics-Ocean", 
  "NorthernTemperate-Total", "NorthernTemperate-Land", "NorthernTemperate-Ocean", 
  "SouthernTemperate-Total" , "SouthernTemperate-Land", "SouthernTemperate-Ocean", 
  "NorthernPolar-Total", "NorthernPolar-Land", "NorthernPolar-Ocean", 
  "SouthernPolar-Total", "SouthernPolar-Land", "SouthernPolar-Ocean", 
  "UnitedStatesLower48-Land", "UnitedStatesContinental49-Land", "Australia-Land") 

url <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt" 
download.file(url, "temp.txt")
data <- read_fwf("temp.txt", 
  col_positions=fwf_empty(file="temp.txt", n = 10, 
                          col_names = colnames), 
  skip=1) %>% 
  filter(row_number() < grep("Year", Year)) %>% 
  mutate(across(everything(), as.numeric), 
         Date=tsibble::make_yearmonth(Year, Month)) %>% 
  select(-Year, -Month) %>% 
  pivot_longer(cols = -Date, names_sep="-",
               names_to = c("ClimateZone", "Type"), 
               values_to = "TemperatureAnomaly") %>% 
  as_tsibble(index="Date", key=c("ClimateZone", "Type")) %>% 
  model(STL(TemperatureAnomaly ~ trend() + season(period=12))) %>% 
  components()

write_csv(data %>% mutate(Date=as.Date(Date)), "data/tropo.csv")


