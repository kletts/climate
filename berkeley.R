

url <- "https://berkeley-earth-temperature-hr.s3.amazonaws.com/Gridded/Australasia_TAVG_Gridded_1.nc"
file <- here("data/Australasia_TAVG_Gridded_1.nc")

library(ncdf4)
library(raster)
library(lubridate)


b <- brick(file)
proj4string(b)=CRS("+proj=longlat +datum=WGS84")
b.df = raster::as.data.frame(b, xy = TRUE)
plot(b[[2065]])

#using stars --- slow but logical and works
library(ncmeta)
library(stars)
library(lubridate)


data <- read_ncdf(file, var="temperature", make_time = FALSE)
data <- data %>% 
  st_as_sf() %>% 
  mutate(grid=row_number()) %>% 
  pivot_longer(cols=-c(grid, geometry), 
               names_to="date", 
               names_prefix="temperature\\.V", 
               values_to="temp") %>% 
  mutate(date = as.numeric(date), 
         date = as.Date("1850-01-01") + months(date))




