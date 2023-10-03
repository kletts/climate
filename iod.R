library(jsonlite)

url <- "https://sealevel.jpl.nasa.gov/api/v1/chartable_values?category=254&per_page=-1&order=x+asc"

data <- read_json(url,  simplifyVector = TRUE)$items %>% 
  select(date=x, iod=y) 

