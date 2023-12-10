
library(tidyverse)
library(lubridate)

url <- "https://www.ncei.noaa.gov/data/oceans/woa/DATA_ANALYSIS/3M_HEAT_CONTENT/DATA/basin/pentad/pent_h22-w0-2000m.dat"

data  <- read_fwf(url, skip=1,
                  col_positions=fwf_widths(widths=rep(8,7), 
                                           col_names=c("YEAR", "WO", "WOse", "NH", "NHse", "SH", "SHse"))) %>% 
  mutate(Date = ymd(paste(floor(YEAR), "12-31", sep="-"))) %>%  
  select(-YEAR) %>% 
  select(Date, everything()) 

write_csv(data, "data/ocean.csv")
