
library(ncdf4)
library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(fabletools)
library(here)

netcdf <- "https://crudata.uea.ac.uk/cru/data/temperature/HadCRUT.5.0.2.0.analysis.anomalies.ensemble_mean.nc"
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


  
ncvar_get(nc, varid=varid, start=c(.x, .y, 1), count=c(1,1,-1)

length(time)

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




# ----  
library(sf)
library(rmapshaper)
library(tidyverse)

aumap <- read_sf("~/Documents/ABSMAPS/2021 Structures/ASGS_2021_Main_Structure_GDA2020.gpkg", 
                 layer="STE_2021_AUST_GDA2020") |> 
  filter(!STATE_CODE_2021 %in% c("9", "Z")) |> 
  ms_simplify(keep=0.01)

grid <- sf::st_make_grid(aumap, 
             square=FALSE, 
             cellsize= units::as_units(1, "degrees"), 
             what="centers") |> 
  sf::st_as_sf() |> 
  sf::st_filter(aumap)


temp <- expand_grid(x=seq_along(long), y=seq_along(lat)) |> 
  mutate(long = long[x], 
         lat = lat[y]) |> 
  rowwise() |> 
  mutate(latest = ncvar_get(nc, varid=varid, start=c(x, y, length(time)), count=c(1,1,1)),
         trend = mean(ncvar_get(nc, varid=varid, start=c(x, y, length(time)-12), count=c(1,1,12)))) |> 
  st_as_sf(coords = c("long", "lat"), crs=4326) |> 
  st_transform(st_crs(aumap)) |> 
  st_crop(st_as_sfc(st_bbox(aumap))) 

ggplot(aumap) + geom_sf() + geom_sf(data=temp, col="red") + geom_sf(data=grid, col="blue")

library(gstat)

inner_join(
  idw(latest ~ 1, temp, grid, nmax=4) |>  
      mutate(long = st_coordinates(geometry)[,1], 
             lat = st_coordinates(geometry)[,2]) |> 
      as_tibble() |> 
    rename(latest = var1.pred), 
  idw(trend ~ 1, temp, grid, nmax=4) |>  
    mutate(long = st_coordinates(geometry)[,1], 
           lat = st_coordinates(geometry)[,2]) |> 
    as_tibble() |> 
    rename(trend = var1.pred), 
  by=c("lat", "long")) |> 
  select(lat, long, latest, trend) |> 
  write_csv("~/Projects/bss-web-site/docs/refdata/aumap-temp.csv")
  


