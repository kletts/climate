
library(tidyverse)
library(httr)
library(rvest)
library(tsibble)
library(jsonlite)
library(feasts)
library(fabletools)

read_soi <- function() { 
  path <- "https://www.cpc.ncep.noaa.gov/data/indices/soi"
  download.file(path, destfile="temp.data")
  colnames <- read_fwf("temp.data", 
                     col_positions=fwf_empty("temp.data",  skip=3, n = 13), 
                     skip=3, n_max=1) %>% 
    gather()
  read_fwf("temp.data",  
         col_positions=fwf_empty("temp.data", skip=3, n = 13, col_names=colnames$value),  
         skip=4, n_max=80, 
         na = c("99.9")) %>% 
  pivot_longer(cols=-1, names_to="month", values_to="Anomaly") %>%  
  mutate(Date= yearmonth(as.Date(paste(YEAR, month, 1, sep="-"), "%Y-%b-%d"))) %>% 
  select(-YEAR, -month) %>% 
  drop_na(Anomaly) %>% 
  mutate(Month=month(Date), 
         Year=year(Date)) } 

data <- read_soi() %>% 
  as_tsibble() %>% 
  model(STL(Anomaly ~ trend() + season(period=12))) %>% 
  components()


write_json(data %>% mutate(Date=as.Date(Date)), "data/soi.json", force=TRUE)
write_csv(data %>% mutate(Date=as.Date(Date)), "data/soi.csv")

