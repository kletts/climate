
library(tidyverse)
library(tsibble)
library(jsonlite)
library(feasts)
library(fabletools)


make_file_name <- function(hemisphere=c("north", "south"), x) { 
  hemisphere <- match.arg(hemisphere)
  url <- list(north="https://noaadata.apps.nsidc.org/NOAA/G02135/north/monthly/data", 
              south="https://noaadata.apps.nsidc.org/NOAA/G02135/south/monthly/data") 
  prefix <- list(north="N", south="S")
  file <- paste(prefix[[hemisphere]], sprintf( "%02d", x), "extent_v3.0.csv", sep="_")
  file.path(url[[hemisphere]], file) } 

extract_data <- function(hemisphere=c("north", "south")) { 
  hemisphere <- match.arg(hemisphere)
  map_dfr(1:12, ~read_csv(make_file_name(hemisphere, .x), na = "-9999") %>%  
            mutate(Date = make_yearmonth(year, mo))) %>% 
    drop_na() %>% 
    as_tsibble() %>% 
    fill_gaps() %>% 
    mutate(mo = month(Date), 
           year = year(Date)) }

data <- extract_data("north") 


mod <- lm(extent ~ year + factor(mo) - 1, data)
data <- data %>% 
  mutate(extent = ifelse(is.na(extent), predict(mod, .), extent)) %>% 
  model(STL(extent ~ trend() + season(period=12))) %>% 
  components()

write_csv(data %>% mutate(Date = as.Date(Date)), "data/ice.csv")
write_json(data %>% mutate(Date = as.Date(Date)), "data/ice.json") 

data <- extract_data("south")

mod <- lm(extent ~ year + factor(mo) - 1, data)
data <- data %>% 
  mutate(extent = ifelse(is.na(extent), predict(mod, .), extent)) %>% 
  model(STL(extent ~ trend() + season(period=12))) %>% 
  components()

write_csv(data %>% mutate(Date = as.Date(Date)), "data/ices.csv")
write_json(data %>% mutate(Date = as.Date(Date)), "data/ices.json") 
