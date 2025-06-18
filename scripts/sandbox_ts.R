## view timeseries of MODIS ice
library(readr)
library(lubridate)
library(tidyverse)
library(sf)
library(xts)
library(zoo)
library(dplyr)
library(chillR)


mod_ts <- read_csv("data/manyLakes_ts.csv")
mod_ts <- read_csv("data/manyLakes_ts_12lakes.csv")
unique(mod_ts$lake_id)

lakes_100_shp <- read_sf("data/pld_100_random_AR.shp") #get shapefile 
lakes_12_shp <- read_sf("data/pld_dozen.shp")


lakes_merra_temp <- read_csv("data/manyLakesTempTS_2024_12_03.csv") 
lakes_merra_temp$dttm <- lubridate::as_datetime(lakes_merra_temp$dttm)## big note that noon is 12 and midnight is also 12! 
lakes_merra_temp$TSURF <- ifelse(lakes_merra_temp$TSURF == -9999, NA, lakes_merra_temp$TSURF)

unique(lakes_merra_temp$lake_id)
#remove outliers when there is temperature 
rm_outliers_mod_ts <- function(lake_id){
  #lake_id = 8120058132 
  #lake_id = 8120041152   #this is lakes with NA's for temp
  
  #get temperature date for one lake: 
  onelake <- lakes_merra_temp[lakes_merra_temp$lake_id == lake_id,]
  onelake$TSURF_28d_mean <- zoo::rollmean(onelake$TSURF, k = 672, align = "right", fill = NA, na.pad = T) # value comes from 24 hrs x 28 days
  temp <- onelake %>% group_by(date = date(dttm)) %>% summarise(TSURF_28d_mean = mean(TSURF_28d_mean, na.rm = T))
  
  #create pretend date 
  #temp <- data.frame(date = seq.Date(from = as.Date("2023-03-01"), as.Date("2024-12-03"), by = '1 day'), TSURF_28d_mean = rnorm(644,mean = 273, sd = 25))
  
  #get ice fraction data for one lake
  onelake_icefraction <- mod_ts[mod_ts$lake_id == lake_id,] %>% 
    select(-`system:index`)
  
  #get temp critical 
  onelake_Tc <- left_join(temp, onelake_icefraction, by="date") %>%
    mutate(approx = na.approx(IceFracMOD, na.rm = F)) %>%
    filter(approx < .21 & approx > .19) %>%
    summarise(Tc_K = mean(TSURF_28d_mean, na.rm = T))
  
 lat <- sf::st_bbox(lakes_100_shp[lakes_100_shp$lake_id == lake_id, ])[4]
  
  onelake_daily_ts <- right_join(temp, onelake_icefraction, by = "date") %>%
    mutate(IceFracMOD_rto = ifelse(TSURF_28d_mean > onelake_Tc$Tc_K & 
                    lag(IceFracMOD) < IceFracMOD & 
                    lead(IceFracMOD) < IceFracMOD, 
                  lag(IceFracMOD), IceFracMOD), 
           # if temp > crit temp and there is an spike then replace with previous smaller value 
           daylen = chillR::daylength(latitude = lat, 
                                      JDay = yday(date))$Daylength) %>%
    mutate(IceFracMOD_rdo = ifelse(daylen < 7 & #day less than 7
                                   lag(IceFracMOD_rto) >= .8 & 
                                     #ice fract before must be greater than 80%
                                    IceFracMOD < .8*lag(IceFracMOD), 
                                   # ice frac must be less than 80% of lagged ice frac
                                   lag(IceFracMOD_rto), IceFracMOD_rto))%>%
    mutate(IceFracMOD_rdo = ifelse(TSURF_28d_mean > 291.483, 0, IceFracMOD_rdo), #if the tempreature is greater than 18.3333 degress C, then must be no ice 
           flag = ifelse(IceFracMOD != IceFracMOD_rto | IceFracMOD != IceFracMOD_rdo, 1, 0))
  return(onelake_daily_ts)
}

#remove outliers based on months: 
rm_outliers_mod_ts <- function(lake_id){
  #lake_id = 8120058132 
  #lake_id = 8120041152   #this is lakes with NA's for temp
  
  #get ice fraction data for one lake
  onelake_icefraction <- mod_ts[mod_ts$lake_id == lake_id,] 
  
  #remember to change shp
  lat <- sf::st_bbox(lakes_12_shp[lakes_12_shp$lake_id == lake_id, ])[4]
  
  onelake_daily_ts <- onelake_icefraction %>%
    mutate(IceFracMOD_rto = ifelse(month(date) <9 & month(date) >6 & 
                                     lag(IceFracMOD) < IceFracMOD & 
                                     lead(IceFracMOD) < IceFracMOD, 
                                   lag(IceFracMOD), IceFracMOD), 
           daylen = chillR::daylength(latitude = lat, 
                                      JDay = yday(date))$Daylength) %>%
    mutate(IceFracMOD_rdo = ifelse(daylen < 7 & #day less than 7
                                     lag(IceFracMOD_rto) >= .8 & 
                                     #ice fract before must be greater than 80%
                                     IceFracMOD < .8*lag(IceFracMOD), 
                                   # ice frac must be less than 80% of lagged                                       ice frac
                                   lag(IceFracMOD_rto), IceFracMOD_rto))%>%
    mutate(flag = ifelse(IceFracMOD != IceFracMOD_rto | 
                           IceFracMOD != IceFracMOD_rdo, 1, 0))
  return(onelake_daily_ts)
}

## filter ts 
manylake_daily_ts <- data.table::rbindlist(lapply(unique(mod_ts$lake_id), rm_outliers_mod_ts))

## 

#try to make pdf: 
library(gridExtra)
p <- lapply(unique(mod_ts$lake_id), function(x) {
  lake_attr = lakes_100_shp[lakes_100_shp$lake_id == x,]
  ggplot(manylake_daily_ts %>% filter(lake_id == x) %>% drop_na(IceFracMOD_rdo, date)) + #used to be mod_ts
    geom_line(aes(y = IceFracMOD_rdo, x = date)) + 
    geom_point(aes(y = IceFracMOD_rdo, x = date, 
                   color = (1-cloudMask)*100)) + 
    lims(y = c(0, 1)) + 
    theme_bw() + 
    labs(title = paste0("pld_lake_id: ", x), 
         subtitle = paste0("size: ",lake_attr$max_area, "ha")) + 
    scale_color_continuous("% cloudy") + 
    scale_x_date(date_breaks = "2 months", date_labels = "%m/%y", date_minor_breaks = "1 month")
})

ggsave(
  filename = "figs/100_lakes_ts_outliers.pdf", 
  plot = marrangeGrob(p, nrow=5, ncol=2), 
  width = 12, height = 12
)

#lapply(unique(mod_ts$lake_id), function(x) {
for(i in 1:12) { #length(unique(mod_ts$lake_id))
  x = unique(mod_ts$lake_id)[i] 
  lake_attr = lakes_12_shp[lakes_12_shp$lake_id == x,] #change to many shapes
  plot <- ggplot(manylake_daily_ts %>% filter(lake_id == x) %>% drop_na(IceFracMOD_rdo, date)) + #used to be mod_ts
    geom_line(aes(y = IceFracMOD_rdo, x = date)) + 
    geom_point(aes(y = IceFracMOD_rdo, x = date)) + 
    lims(y = c(0, 1)) + 
    theme_bw() + 
    labs(subtitle = paste0(x, "; size: ",lake_attr$max_area, " ha"), 
         y = "Ice Fraction", 
         x = "Date") + 
   # scale_color_continuous("% cloudy") + 
    scale_x_date(date_breaks = "3 months", date_labels = "%m/%y", date_minor_breaks = "1 month")
  
  pdf(paste0("poster_",x,".pdf"), width = 4.2/1.2, height = 2.75/1.2)
  print(plot)
  dev.off()
  
}

lakes_12_shp[lakes_12_shp$lake_id == 2510280033,]$names





