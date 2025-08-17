
library(tidyverse)

download.file("ftp://ftp.bom.gov.au/anon/gen/clim_data/IDCK000081.zip", 
              "temp.zip")
unzip("temp.zip", exdir="./temp") 

files <- c("nino_1"="Nino1", 
           "nino_2"="Nino2", 
           "nino_3"="Nino3", 
           "nino_3.4"="Nino3+4", 
           "nino_4"="Nino4", 
           "iod"="IOD", 
           "soi"="SOI")

data <- imap(files, \(x, idx) 
    read_csv(paste("./temp/", idx, ".csv", sep=""))  |> 
    mutate(date = as.Date(as.character(end_date), "%Y%m%d"), 
           series = x) |>
      rename(anon=.data[[idx]]) |> 
    select(date, anon, series)) 

data$soi <- data$soi |> filter(date %in% data$nino_1$date) 

write_csv(bind_rows(data), "allnino.csv")

unlink(c("temp.zip", "./temp"), recursive=TRUE)
