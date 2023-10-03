
#typical meteroligical year 
#source: https://www.marcoernst.net/downloads/one-minute-tmy-data-australia.html

 
location <- tribble(
  ~BomStation, ~StationName, ~State,  ~Latitude, ~Longitude, ~Climate, ~TimeZone, 
  "003003", "Broome Airport","WA",  -17.9475, 122.2353, "Grassland", "Australia/West",
  "014015", "Darwin Airport", "NT", -12.4239, 130.8925, "Tropical", "Australia/Darwin",
  "015590", "Alice Springs Airport", "NT", -23.7951, 133.889, "Desert", "Australia/North",
  "023034", "Adelaide Airport", "SA", -34.9524, 138.5204, "Grassland",  "Australia/Adelaide",
  "039083", "Rockhampton Aero", "QLD", -23.3753, 150.4775, "Sub-tropical", "Australia/Queensland",
  "072150", "Wagga Wagga AMO", "NSW", -35.1583, 147.4575, "Grassland", "Australia/NSW",
  "086282", "Melbourne Airport", "VIC", -37.6655, 144.8321, "Termperate", "Australia/Melbourne") %>%  
  mutate(Source_File=paste0(BomStation, "TMY_60min.csv"))

path <- here("data", "TMY_Dataset_60min")
data <- pmap_dfr(location, \(Source_File, ...) 
             read_csv(file.path(path, Source_File)) %>% mutate(Source_File=Source_File)) %>% 
       inner_join(location, by="Source_File") %>% 
  group_by(TimeZone) %>% 
  mutate(Date = ymd_hm(paste(Year, Month, Day, Hour, `Minute (Local Time)`, sep="-"), tz=head(TimeZone,1))) 



#CSIRO TMY ------

library(here)
library(tidyverse)

colnames <- 
 c("Year", 
 "Month", 
 "Day", 
 "Hour", 
 "Minute", 
 "Data Source and Uncertainty Flags", 
 "Dry Bulb Temp",
 "Dew Point Temp", 
 "Relative Humidity", 
 "Atmospheric Station Pressure", 
 "Extraterrestrial Horizontal Radiation", 
 "Extraterrestrial Direct Normal Radiation", 
 "Horizontal Infrared Radiation from Sky", 
 "Global Horizontal Radiation", 
 "Direct Normal Radiation", 
 "Diffuse Horizontal Radiation", 
 "Global Horizontal Illuminance", 
 "Direct Normal Illuminance", 
 "Diffuse Horizontal Illuminance", 
 "Zenith Luminance", 
 "Wind Direction", 
 "Wind Speed", 
 "Total Sky Cover", 
 "Opaque Sky Cover", 
 "Visibility", 
 "Ceiling Height", 
 "Present Weather Observation", 
 "Present Weather Codes", 
 "Precipitable Water", 
 "Aerosol Optical Depth", 
 "Snow Depth", 
 "Days Since Last Snowfall")

read_tmy <- function(x) { 
  location.colnames <- c("Location",   "City", "State Province Region", "Country", "Data Source", "WMO Number", "Latitude", 
                         "Longitude", "Time Zone", "Elevation") 
  data <- read_csv(x, skip=8, col_names = colnames) %>% 
    mutate(Date = lubridate::ymd_h(paste("2023", Month, Day, Hour,  sep="-"))) %>%  
    select(-`Data Source and Uncertainty Flags`)
  location <-  read_csv(x, n_max=1, col_names = location.colnames) 
  data <- data %>% bind_cols(location %>% select(-Location, -`WMO Number`))
  return(data) } 


path <- here("data/CSIRO TMYWeatherFilesEpw_20210712")
files <- list.files(path) %>%  {.[grep("\\.epw$", .)] }
files <- file.path(path, files)


data  <- map_dfr(files, read_tmy) 
write_rds(data %>% filter(grepl("Tullamarine", City)) %>% 
            select(Date, Radiation = `Direct Normal Radiation`), 
          'tullamarine_radiation.rds') 

# ---- radiation --- 

data %>% 
  filter(grepl("Tullamarine", City)) %>% 
  group_by(Month, Hour) %>% 
  summarise(Radiation=mean(`Direct Normal Radiation`)) %>% 
  ungroup() %>% 
  mutate(Month= factor(Month, labels=levels(data.shade$month))) %>% 
  filter(Radiation>0) %>% 
  ggplot(aes(x=Hour, y=Month, fill=Radiation)) + 
  geom_tile(height=0.8) + 
  scale_fill_viridis_c(option="A") + 
  scale_y_discrete(limits=rev) + 
  theme_bw() 


