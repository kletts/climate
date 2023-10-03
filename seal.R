library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(fabletools)

url <- "https://www.star.nesdis.noaa.gov/socd/lsa/SeaLevelRise/slr"

seas <- list(
     "Globe"="slr_sla_gbl_free_txj1j2_90.csv",
     "Pacific Ocean"="slr_sla_pac_free_txj1j2_90.csv", 
     "North Pacific Ocean"="slr_sla_np_free_txj1j2.csv", 
     "Tropics"="slr_sla_t_free_txj1j2.csv", 
     "Nino3.4"="slr_sla_nino34_free_txj1j2.csv", 
     "Atlantic Ocean"="slr_sla_atl_free_txj1j2_90.csv", 
     "North Atlantic Ocean"="slr_sla_na_free_txj1j2.csv", 
     "Indian Ocean"="slr_sla_ind_free_txj1j2_90.csv", 
     "Adriatic Sea"="slr_sla_ads_free_txj1j2_90.csv", 
     "Andaman Sea"="slr_sla_ans_free_txj1j2_90.csv", 
     "Arabian Sea"="slr_sla_ars_free_txj1j2_90.csv", 
     "Baltic Sea"="slr_sla_bas_free_txj1j2_90.csv", 
     "Bay of Bengal"="slr_sla_bob_free_txj1j2_90.csv", 
     "Bering Sea"="slr_sla_brs_free_txj1j2_90.csv", 
     "Caribbean Sea"="slr_sla_crs_free_txj1j2_90.csv", 
     "Gulf of Mexico"="slr_sla_gom_free_txj1j2_90.csv", 
     "Indonesian"="slr_sla_int_free_txj1j2_90.csv", 
     "North Sea"="slr_sla_nrs_free_txj1j2_90.csv", 
     "Mediterranean Sea"="slr_sla_mds_free_txj1j2_90.csv", 
     "Persian Gulf"="slr_sla_prs_free_txj1j2_90.csv", 
     "South China Sea"="slr_sla_scs_free_txj1j2_90.csv", 
     "Southern Ocean"="slr_sla_so_free_txj1j2.csv") 

data <- map(seas, ~file.path(url, .x)) %>% 
  imap_dfr(~read_csv(.x, skip=5) %>% 
        mutate(region=.y, 
               year= date_decimal(year),
               sealevel = coalesce(`Jason-3`,  `Jason-2`, `Jason-1`,`TOPEX/Poseidon`))
  )

eomdata <- tibble(
    Date = seq.Date(from=as.Date(ceiling_date(min(data$year), unit = "month")), 
                    to=as.Date(floor_date(max(data$year), unit="month")), 
                    by="month")) %>% 
  mutate(Date = as_datetime(Date), 
         Region=list(unique(data$region))) %>% 
  unnest_longer(Region) %>% 
  arrange(Region, Date) %>% 
  group_by(Region) %>% 
  mutate(Sealevel = approx(x=subset(data, region==Region[1])$year, 
                           y=subset(data, region==Region[1])$sealevel, 
                           xout=Date)$y,  
         Period = yearmonth(Date)) %>% 
  tsibble(index=Period, key=Region) %>% 
  model(STL(Sealevel ~ trend() + season(period=12))) %>% 
  components()

write_csv(eomdata %>% mutate(Date=as.Date(Period)), 
          "data/seal.csv")



