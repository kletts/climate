
library(ncdf4)
library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(fabletools)
library(here)

netcdf <- "https://crudata.uea.ac.uk/cru/data/temperature/HadCRUT.5.0.1.0.analysis.anomalies.ensemble_mean.nc"
download.file(netcdf, "temp.nc")

ncdata <- nc_open("temp.nc")

extract_origin <- function(nc) { 
  origin <- ncatt_get(nc, "time", "units")
  as.Date(str_extract(origin$value, "\\d{4}\\-\\d{2}\\-\\d{2}")) }

get_ts_at_latlong <- function(nc, varid="tas_mean", long, lat) {  
  longnc <- map_int(long, ~which.min(abs(nc$dim$longitude$vals - .x)))
  latnc <- map_int(lat, ~which.min(abs(nc$dim$latitude$vals - .x)))
  n <- length(longnc)
  time <- ncvar_get(nc, "time")
  time <- as.Date(floor(time), extract_origin(nc))
  data <- map2(longnc, latnc, ~ncvar_get(nc, varid=varid, start=c(.x, .y, 1), count=c(1,1,-1)))
  imap_dfr(data, ~tibble(lat=lat[.y], long=long[.y], time, !!varid:= .x))
  }

lookup <- tribble(~city, ~lat, ~long, 
                "Melbourne", -37.840935, 144.946457, 
                "Sydney", -33.865143,	151.209900, 
                "Darwin", -12.462827,	130.841782,
                "Berlin", 52.520008, 13.404954, 
                "Kolkata", 22.572645, 88.363892, 
                "Wellington", -41.286461, 174.776230, 
                "Chicago", 41.878113, -87.629799, 
                "Tokyo", 35.689487, 139.691711, 
                "Moscow", 55.755825, 37.617298)

data <- get_ts_at_latlong(ncdata, varid="tas_mean", long=lookup$long, lat=lookup$lat) %>% 
  inner_join(lookup, by=c("lat", "long")) %>% 
  mutate(period = yearmonth(time)) %>% 
  tsibble(index=period, key=city) %>% 
  group_by_key() %>% 
  fill(tas_mean, .direction = "down") %>% 
  drop_na(tas_mean) %>% 
  model(STL(tas_mean ~ trend(window=240) + season(period=12))) %>% 
  components()

write.csv(data %>% mutate(.model=NULL, period = as.Date(period)), 
          here("data/csst.csv"))
unlink("temp.nc")

  
