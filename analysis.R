
data %>% 
  model(STL(Anomaly ~ trend() + season(12))) %>%
  components() %>% 
  mutate(MacroCycle=case_when(trend < -0.5 ~ "La Nina", 
                              trend < 0.5 ~ "Neutral", 
                              TRUE ~ "El Nino"), 
         MacroCycle=factor(MacroCycle, levels=c("El Nino", "Neutral", "La Nina"))) %>% 
  ggplot(aes(x=Date)) + 
  geom_line(aes(y=Anomaly, linetype="Anomaly"), col="lightgray") + 
  ggforce::geom_link2(aes(y=trend, col=MacroCycle, group=1), lwd=1.1) + 
  theme_bw() + 
  scale_x_yearmonth(date_labels="%Y") + 
  labs(x=NULL, y="Anomaly °C", 
       title="Southern Ocean Oscilliation Index", 
       linetype=NULL, col="Trend") + 
  theme(panel.grid.minor.x=element_blank(), 
        panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_blank(), 
        panel.grid.major.y=element_blank())

# ----- 
# Monthly temperature data 

temp <- readr::read_csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv", 
                        skip=1, col_select=1:13, na="***") %>% 
  pivot_longer(cols=-1, names_to="Month", values_to="AvTemp") %>% 
  mutate(Period = yearmonth(paste(Year, Month), "%Y %b")) %>% 
  as_tsibble(index=Period) %>% 
  drop_na()


temp.mod <- temp %>% 
  model(STL(AvTemp ~ trend(window=120) + season(period=12))) %>% 
  components()

data %>%
  model(STL(Anomaly ~ trend(window=Inf) + season(period=12))) %>% 
  components() %>% 
  select(Date, SOIanomaly=remainder) %>% 
  inner_join(temp.mod %>% select(Period, SSTanomaly=remainder), 
             by=c("Date"="Period"))  %>% 
  cor.test(~ SOIanomaly + SSTanomaly, data=.) 

data %>%
  model(STL(Anomaly ~ trend(window=Inf) + season(period=12))) %>% 
  components() %>% 
  select(Date, SOIanomaly=remainder) %>% 
  inner_join(temp.mod %>% select(Period, SSTanomaly=remainder), 
             by=c("Date"="Period"))  %>% 
  lm(SSTanomaly ~ lag(SOIanomaly, 2), data=.) %>% 
  summary()
