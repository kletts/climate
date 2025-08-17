
# see https://cds.climate.copernicus.eu/user-guide

library(ncdf4)

extract_origin <- function(nc) { 
  origin <- ncatt_get(nc, "valid_time", "units")
  as.Date(str_extract(origin$value, "\\d{4}\\-\\d{2}\\-\\d{2}")) }

data <- nc_open(list.files("data", pattern="download.nc", full.names = TRUE)[1])
time <- ncvar_get(data, "valid_time")
time <- as.POSIXlt(time, tz="UTC", extract_origin(data))

grid <- sf::st_make_grid(aumap, 
                         square=FALSE, 
                         cellsize= units::as_units(0.5, "degrees"), 
                         what="centers") |> 
  sf::st_as_sf() |> 
  sf::st_filter(aumap) |> 
  st_coordinates()

lookup_long <- function(data, long) { 
  xl <- data$dim$longitude$vals 
  which.min(abs(xl - long)) }

lookup_lat <- function(data, lat) { 
  xl <- data$dim$latitude$vals 
  which.min(abs(xl - lat)) }

longnc <- map_int(grid[,"X"], ~lookup_long(data, .x))
latnc <- map_int(grid[,"Y"], ~lookup_lat(data, .x))
n <- length(longnc)
nt <- length(time)
temp <- map2(longnc, latnc, ~ncvar_get(data, varid="t2m", start=c(.x, .y, nt), count=c(1, 1, 1)))
temp <- imap_dfr(temp, ~tibble(lat=grid[,"Y"][.y], long=grid[,"X"][.y], temp = .x))

ggplot(temp, aes(x=long, y=lat, col=temp- 273)) + 
  geom_point() + 
  scale_color_viridis_c()

