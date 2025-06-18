## packages
library(sf)
library(gridExtra)
library(dplyr)
library(lubridate)
library(glue)
library(readr)
library(tidyr)
library(ggplot2)
library(zoo)

## load in data
pld_gt_1km2 <- readRDS("pld_gt_1km2_v2.rds")
pld_gt_1km_45lat_sf <- read_sf("data/pld_gt_1km_45lat_1.shp")


angelica_lakes <- read.csv("data/angelica_lakes.csv") 
angelica_subset <- pld_gt_1km2[pld_gt_1km2$lake_id %in% angelica_lakes$lake_id,]
angelica_lakes$gt_1km2 <- ifelse(angelica_lakes$lake_id %in% angelica_subset$lake_id, T, F)
angelica_subset_gt45lat <- pld_gt_1km_45lat_sf[pld_gt_1km_45lat_sf$lake_id %in% angelica_lakes$lake_id,]
angelica_lakes$gt_45lat <- ifelse(angelica_lakes$lake_id %in% angelica_subset_gt45lat$lake_id, T, F)

rm(pld_gt_1km_45lat_sf, pld_gt_1km2)

## ran this on angelica subset 
#write_csv(angelica_lakes %>% select(-X), col_names = F, "angelica_lakes_ANT.csv")

#### pull out chosen lakes thresholds and plot ####
s1fol <- "step1LandsatMODIS"
SLIDE_manylakes <- read_csv(glue("data/{s1fol}/LandsatThresholds_pld1000.csv"))

#reformatting
SLIDE_manylakes <- SLIDE_manylakes %>% rename(time_unix_ms = `system:time_start`) #index_landsat = `system:index`, 
SLIDE_manylakes$DOY <- yday(SLIDE_manylakes$date)
SLIDE_manylakes$satellite <- substr(SLIDE_manylakes$LANDSAT_SCENE_ID,start = 1, stop = 3)

MODIS_manylakes <- read_csv(glue("data/{s1fol}/MODIS_Thresholds_pld1000.csv"))
mod_cols = grep(x = colnames(MODIS_manylakes), pattern = "gt", value = T)

## combining MODIS and Landsat data for many lakes: 
LanMOD <- left_join(MODIS_manylakes %>% select(all_of(mod_cols), date, lake_id), 
                    SLIDE_manylakes %>% select( -time_unix_ms)) %>% 
  select(all_of(mod_cols), date, RFSnowIce, lake_id)

LanMODsummary <- pivot_longer(LanMOD, cols = starts_with("gt"), names_to = "mod", values_to = "ice")%>%
  dplyr::mutate(diff = abs(RFSnowIce - ice)) %>%
  group_by(mod, lake_id) %>%
  summarise(dif = mean(diff, na.rm = T)) %>%
  filter(lake_id %in% angelica_subset$lake_id)

#plot for 100 lakes with their thresholds: 

p <- lapply(unique(LanMODsummary$lake_id), function(x) {
  max_threshold = max(LanMODsummary$dif)
  ggplot(LanMODsummary %>% filter(lake_id == x)) + 
    geom_point(aes(x = mod, y = dif)) + theme_bw() + 
    labs(subtitle = paste0("pld_lake_id: ", x)) + 
    lims(y = c(0, max_threshold)) + 
    scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE)]})
})

ggsave(
  filename = "figs/angelica_thresholds.pdf", #add version
  plot = marrangeGrob(p, nrow=5, ncol=5), 
  width = 12, height = 12
)

## from the thresholds, output pld shapefiles & their threshold value 
lakes_many_shp <- read_sf("data/pld_1000.shp")
head(lakes_many_shp)

thresholds <- LanMODsummary %>% group_by(lake_id) %>% summarise(dif = min(dif, na.rm = F))

pld_many_lakes_with_thresholds <- left_join(thresholds, LanMODsummary) %>%
  select(-dif) %>% 
  mutate(mod = extract_numeric(mod)/100,
         lake_id = as.character(lake_id)) %>% 
  left_join(lakes_many_shp)

pld_many_lakes_with_thresholds

st_write(pld_many_lakes_with_thresholds, "data/step2Thresholds/pld_1000_wThresholds.shp", append = F)

### end ###

#### look at timeseries ####

## get raw timeseries 
pld1000_ts <- read.csv("data/step3ts/manyLakes_ts_1000.csv")
angelica_subset_ts <- pld1000_ts[pld1000_ts$lake_id %in% angelica_subset$lake_id,]
unique(angelica_subset_ts$lake_id)

p <- lapply(unique(angelica_subset_ts$lake_id), function(x) {
  lake_shp = angelica_subset #define shapefile
  manylake_daily_ts = angelica_subset_ts
  manylake_daily_ts$date <- as.Date(manylake_daily_ts$date) #define ts file
  #x = 8220080122 
  lake_attr = lake_shp[lake_shp$lake_id == x,]
  ggplot(manylake_daily_ts %>% filter(lake_id == x)) + #%>% drop_na(IceFracMOD_rdo, date)) + #used to be mod_ts
    geom_line(aes(y = IceFracMOD, x = date)) + 
    geom_point(aes(y = IceFracMOD, x = date, 
                   color = (1-cloudMask)*100)) + 
    lims(y = c(0, 1)) + 
    theme_bw() + 
    labs(title = paste0("pld_lake_id: ", x), 
         subtitle = paste0("size: ",lake_attr$max_area, "ha")) + 
    scale_color_continuous("% cloudy") + 
    scale_x_date(date_breaks = "2 months", date_labels = "%m/%y", date_minor_breaks = "1 month")
})

ggsave(
  filename = "figs/angelica_lakes_ts.pdf", 
  plot = marrangeGrob(p, nrow=5, ncol=2), 
  width = 12, height = 12
)

### filter ts ###

# need shp, ts, and temp

#read in temperature data: 
lakes_merra_temp <- read_csv("data/step3ts/merraTemp_pld1000_v2.csv", col_select = c("lake_id","year", "month", "day", "hour", "median"))
head(lakes_merra_temp)

lakes_merra_temp$dttm <- lubridate::ymd_h(glue("{lakes_merra_temp$year}-
                                                     {lakes_merra_temp$month}-
                                                     {lakes_merra_temp$day}-
                                                     {lakes_merra_temp$hour}")) #this takes a minute
lakes_merra_temp$TSURF <- ifelse(lakes_merra_temp$median == -9999, NA, lakes_merra_temp$median)

#subset to angelica lakes
lakes_merra_temp_angelica <- lakes_merra_temp[lakes_merra_temp$lake_id %in% angelica_lakes$lake_id,] %>% select(lake_id, dttm, TSURF)
#write_csv(lakes_merra_temp_angelica, "angelica_merraTemp.csv")

#remove outliers when there is temperature 
rm_outliers_mod_ts <- function(lake_id, lakes_merra_temp, mod_ts, lakes_shp){
  
  # lakes_merra_temp = lakes_merra_temp_angelica #temperature file
  # mod_ts = angelica_subset_ts #modis series file
  # lakes_shp = angelica_subset
  # lake_id = lakes_merra_temp_angelica$lake_id[1]
  
  #get temperature date for one lake: 
  onelake <- lakes_merra_temp[lakes_merra_temp$lake_id == lake_id,]
  onelake$TSURF_28d_mean <- zoo::rollmean(onelake$TSURF, k = 672, 
                                          align = "right", fill = NA, 
                                          na.pad = T) # value comes from 24 hrs x 28 days
  temp <- onelake %>% group_by(date = date(dttm)) %>% 
    summarise(TSURF_28d_mean = mean(TSURF_28d_mean, na.rm = T))
  onelake_icefraction <- mod_ts[mod_ts$lake_id == lake_id,] 
  
  onelake_icefraction$date <- base::as.Date(onelake_icefraction$date)
  
  #get temp critical 
  onelake_Tc <- left_join(temp, onelake_icefraction, by="date") %>%
    mutate(approx = na.approx(IceFracMOD, na.rm = F)) %>%
    filter(approx < .21 & approx > .19) %>%
    summarise(Tc_K = mean(TSURF_28d_mean, na.rm = T))
  
  lat <- sf::st_bbox(lakes_shp[lakes_shp$lake_id == lake_id, ])[4]
  
  onelake_daily_ts <- right_join(temp, onelake_icefraction, by = "date") %>%
    mutate(IceFracMOD_rto = ifelse(TSURF_28d_mean > onelake_Tc$Tc_K & #flag based on temp (rto) - remove time outlier
                                     lag(IceFracMOD) < IceFracMOD & 
                                     lead(IceFracMOD) < IceFracMOD, 
                                   lag(IceFracMOD), IceFracMOD), 
           # if temp > crit temp and there is an spike then replace with previous smaller value 
           daylen = chillR::daylength(latitude = lat, 
                                      JDay = yday(date))$Daylength) %>%
    mutate(IceFracMOD_rdo = ifelse(daylen < 7 & #day less than 7, flag based on time of day (rdo) - remove day outlier 
                                     lag(IceFracMOD_rto) >= .8 & 
                                     #ice fract before must be greater than 80%
                                     IceFracMOD < .8*lag(IceFracMOD), 
                                   # ice frac must be less than 80% of lagged ice frac
                                   lag(IceFracMOD_rto), IceFracMOD_rto))%>%
    mutate(IceFracMOD_rdo = ifelse(TSURF_28d_mean > 291.483, 0, IceFracMOD_rdo), #if the tempreature is greater than 18.3333 degress C, then must be no ice 
           flag = ifelse(IceFracMOD != IceFracMOD_rto | IceFracMOD != IceFracMOD_rdo, 1, 0))
  return(onelake_daily_ts)
}

## filter ts 
ang_ts_list <- lapply(unique(angelica_subset_ts$lake_id), rm_outliers_mod_ts, 
       lakes_merra_temp = lakes_merra_temp_angelica, #temperature file
       mod_ts = angelica_subset_ts, #modis series file
       lakes_shp = angelica_subset  #shapefile 
                      )


ang_ts_df <- data.table::rbindlist(ang_ts_list)
head(ang_ts_df)
#write_csv(ang_ts_df, "angelica_ts_dataframe.csv")
## 


library(gridExtra)
#make filtered ts plots
make_ts_plots <- function(x, lakes_100_shp, manylake_daily_ts) {
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
  }


p <- lapply(unique(ang_ts_df$lake_id), make_ts_plots, lakes_100_shp = angelica_subset, 
            manylake_daily_ts = ang_ts_df)

ggsave(
  filename = "figs/angelica_lakes_ts_outliers.pdf", 
  plot = marrangeGrob(p, nrow=5, ncol=2), 
  width = 12, height = 12
)


### end ###
