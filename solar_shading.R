
#https://www.pveducation.org/pvcdrom/properties-of-sunlight/air-mass




library(oce)
library(lubridate)
library(grid)
library(gridExtra)
library(gtable)

direct_insolation.f <- function(altitude){ 
  # https://en.wikipedia.org/wiki/Direct_insolation
  theta <- (90 - pmax(altitude,0))/180*pi
  am <- 1/cos(theta)
  1.353*0.7^(am^0.678) }

make_table_grob <- function(x, ...) { 
  tableGrob(x, rows=NULL, theme = ttheme_dcr, ...) %>% 
    ttheme_add_borders(.) } 

ttheme_dcr <- ttheme_minimal(
  base_size=10, 
  padding = unit(c(3, 3), "mm"),
  core=list(fg_params=list(col="black"), 
            bg_params=list(fill="white")),
  colhead=list(fg_params=list(col="#51127c"), 
               bg_params=list(fill="white"))
)

ttheme_add_borders <- function(g) {
  line <- grid::segmentsGrob(x0 = unit(0,"npc"), y0 = unit(0,"npc"),
                             x1 = unit(1,"npc"), y1 = unit(0,"npc"), 
                             gp = gpar(col="#51127c", lwd=2)) 
  g <- gtable::gtable_add_grob(g, grobs=line, t=1, b=1, l=1, r=ncol(g)) 
  gtable::gtable_add_grob(g, grobs=line, t=nrow(g), b=nrow(g), l=1, r=ncol(g)) }         

make_table_wtitle <- function(x, title=NULL, ...) { 
  t1 <- make_table_grob(x)
  if (is.null(title)==FALSE) { 
    title <- textGrob(title, gp=gpar(fontsize=10, col="#51127c", fontface="bold")) 
    t1 <- gtable::gtable_add_rows(t1, heights = grobHeight(title) + unit(1, "mm"), pos = 0)
    t1 <- gtable::gtable_add_grob(t1, title, 1, 1, 1, ncol(t1)) } 
  return(t1) } 

calc_angles <- function(obst, panel) { 
  we <- obst["long"] - panel["long"] 
  ns <- obst["lat"] - panel["lat"]
  dist <- sqrt(we^2 + ns^2)*111139
  height <- obst["altitude"] - panel["altitude"]
  list(
    angle.azimuth = unname(atan(we/ns)*180/pi), 
    angle.altitude = unname(atan(height/dist)*180/pi)
    ) }

direct_tilted_insolation <- function(altitude, azimuth, panel_tilt, panel_azimuth) { 
  gamma_z <- azimuth/180*pi
  theta_z <- (90 - pmax(altitude,0))/180*pi
  beta <-  panel_tilt/180*pi 
  gamma <- panel_azimuth/180*pi 
  theta <- cos(theta_z)*cos(beta) + sin(theta_z)*sin(beta)*cos(gamma_z- gamma)
  am <- 1/theta
  1.353*0.7^(am^0.678) }


calc_shading <- function(obst, panel, panel.tilt=0, panel.azimuth=0) { 
  x <- "2023-01-01 6:00:00"
  x <- as_datetime(x, tz = "Australia/Melbourne")
  y <- "2023-12-31 20:00:00"
  y <- as_datetime(y, tz = "Australia/Melbourne")
  t <- seq(x, y, by="1 hour")
  angle.altitude <- calc_angles(obst, panel)$angle.altitude
  angle.azimuth <- calc_angles(obst, panel)$angle.azimuth
  panel.tilt <- 15
  panel.azimuth <- 15 #west  
  data <- sunAngle(t, panel["long"], panel["lat"],  useRefraction=TRUE) %>% bind_rows() 
  data.shade <- data %>% 
    filter(altitude>0) %>% 
    mutate(
      time = as_datetime(time, tz="Australia/Melbourne"), 
      month= factor(month(time), labels = unique(format(time, "%b"))),
      hour = hour(time), 
      azimuth = ifelse(azimuth>180, azimuth-360, azimuth),  
      tilt_insolation = direct_tilted_insolation(altitude, azimuth, panel.tilt, panel.azimuth),  
      shaded = ifelse(altitude < angle.altitude & azimuth < angle.azimuth, TRUE, FALSE), 
      tilt_insolation = ifelse(is.nan(tilt_insolation), 0, tilt_insolation))
  return(data.shade) } 


#----- 259 Sydney Rd Brunswick ----- 

panel <- c("lat"=-37.771308, "long"=144.960665, "altitude"=20)
obstruction <- c("lat"=-37.771128, "long"=144.960545, "altitude"=45)
panelsqm <- 18*1.7 #square meters of panels



tmy.tulla <- read_rds("tullamarine_radiation.rds") %>% 
  mutate(Radiation = Radiation/1000)

data.shade %>% 
  calc_shading(panel, obstruction, panel.tilt=0, panel.azimuth=15) %>% 
  left_join(tmy.tulla, by=c("time"="Date")) %>% 
  mutate(Radiation = Radiation*tilt_insolation/max(tilt_insolation))


ggmin_shade <- function(data, month.select) { 
  p1 <- data %>% ungroup() %>% 
    filter(month==month.select) %>% 
    ggplot(aes(x=hour, y="1", fill=Insolation, alpha=Shaded)) + 
    scale_x_continuous(breaks=c(8, 12, 16, 20), limits=c(6,20)) + 
    scale_alpha_manual(values=c(1, 0.2)) + 
    scale_fill_viridis_c(option="A", limits =range(data$Insolation)) + 
    geom_tile(height=0.9, width=1) + 
    theme_void() + 
    guides(fill="none", alpha="none") 
  ggplot_image(p1, height = 30, aspect_ratio=7.5)
  }


data <- data.shade %>% 
  group_by(hour, month) %>% 
  summarise(Insolation=mean(Radiation), 
            Shaded = mean(!shaded)<0.5)

data.shade %>% 
  group_by(month) %>% 
  summarise(Current=sum(Radiation)*panelsqm/4, 
            Shaded=sum(Radiation*shaded)*panelsqm/4) %>%  
  mutate(Cost = Shaded*0.06, 
         plot = month) %>%
  gt(rowname_col = "month") %>% 
  text_transform(
    locations = cells_body(columns = plot),
    fn = function(x) { lapply(x, \(x) ggmin_shade(data, x)) }) %>% 
  fmt_number(columns = c(Current, Shaded), decimals=0) %>% 
  fmt_currency(columns=Cost, decimals=2) %>% 
  tab_options(data_row.padding = px(1)) %>%  
  cols_label(
    Current ~ md("**Current**<br>{{kWh 30.6m^2}}"),
    Shaded ~ md("**Shaded**<br>{{kWh 30.6m^2}}"), 
    Cost ~ md("**Cost**<br>{{$/month @ 6c/kWh}}"), 
    plot ~ "Average Time of Day Effects") %>% 
  grand_summary_rows(fns=list(label = "Total", fn = "sum"))  

t1 <- data.shade %>% 
  group_by(Month=month) %>% 
  summarise(`Current Insolation`=sum(direct_insolation), 
            `Lost Insolation` = sum(direct_insolation*shaded)) %>%
  { bind_rows(., summarise(., Month="Total", across(-Month, sum))) } %>% 
  mutate(across(-Month, ~round(.x, 1))) %>% 
  make_table_wtitle(title="Total Direct Insolation") %>% 
  ttheme_add_borders()
  

p1 <- data.shade %>% 
  group_by(hour, month) %>% 
  summarise(Insolation=mean(Radiation), 
            Shaded = mean(!shaded)<0.5) %>% 
  ggplot(aes(x=hour, y=month, fill=Insolation, alpha=Shaded)) + 
  scale_y_discrete(limits=rev) + 
  scale_x_continuous(breaks=c(8, 12, 16, 20)) + 
  scale_alpha_manual(values=c(1, 0.2)) + 
  geom_tile(height=0.8) + 
  scale_fill_viridis_c(option="A") + 
  theme_bw() + 
  theme(panel.grid=element_blank()) + 
  labs(subtitle="Direct Solar Insolation, Tilt Adjusted",
       fill=expression(Wh/m^2), 
       alpha="Shaded\nPanel", 
       x="Hour of the day", 
       y="Month of the year")
  
tt1 <- textGrob(label="Solar Panel Insolation Loss from Over-shadowing", 
         gp =gpar(fontsize=14))
tt2 <- textGrob(label="Assessment for solar panels at 8/259 Sydney Rd Brunswick 
         in response to shading by proposed development at 10 Dawson St Brunswick", 
         gp=gpar(fontsize=9))
textGrob(label="Assumptions: Development height: 45m; ")
pts <- grid.arrange(tt1, tt2, p1, t1, nrow=4, heights=c(1, 1, 12, 8))
ggsave("solarOvershadowing.pdf", pts, height=29, width=21, units="cm", device="pdf")