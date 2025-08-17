
library(tidyverse)
library(httr)
library(rvest)
library(tsibble)
library(feasts)
library(fabletools)


read_temp <- function() { 
  list("Globe"="https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",
       "North"="https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.csv", 
       "South"="https://data.giss.nasa.gov/gistemp/tabledata_v4/SH.Ts+dSST.csv") |>  
    imap_dfr(~readr::read_csv(.x, skip=1, col_select=1:13, na="***") |>
               pivot_longer(cols=-1, names_to="Month", values_to="AvTemp") |>  
               mutate(Date = yearmonth(paste(Year, Month), "%Y %b"), 
                      Region=.y)) |>  
    as_tsibble(index=Date, key=Region) |>  
    drop_na() } 

data <- read_temp() |> 
  model(STL(AvTemp ~ trend(window=120) + season(period=12))) |> 
  components()

data |> 
  as_tibble() |> 
  mutate(Date = as.Date(Date)) |> 
  select(Region, Date, Temp=AvTemp, Trend=trend, SeasonalAdj=season_adjust) |> 
  format_csv() |> 
  cat() 