## sandbox look at slide 
library(readr)
library(lubridate)
library(tidyverse)
library(sf)
library(glue)

s1fol <- "step1LandsatMODIS"
suf <- "fasterTest"

#### STEP 1A: LANDSAT DATA ####
# -------------------------- #

{
# SLIDE_firstlake <- read_csv("data/SLIDE_firstlake_v3.csv")
# SLIDE_firstlake <- read_csv("data/SLIDE_firstlake_pld.csv")
# SLIDE_100lakes <- read_csv("data/LandsatThresholds_pld_100.csv") #for AGU 
# SLIDE_manylakes <-read_csv("data/LandsatThresholds_pld_12.csv") #for AGU 
# SLIDE_manylakes <- read_csv(glue("data/{s1fol}/LandsatThresholds_pld1000.csv"))
} #old load-ins
SLIDE_manylakes <- read_csv(glue("data/{s1fol}/LandsatThresholds_{suf}.csv"))

#figure out how many images per collection 
SLIDE_manylakes %>%
  group_by(lake_id) %>%
  summarise(n = length(LANDSAT_PRODUCT_ID)) #%>%
  summarise(mean(n))

#reformatting for first lakes
{
# colnames(SLIDE_firstlake)[4] <- "time_unix_ms"
# SLIDE_firstlake$datetime <- as_datetime(SLIDE_firstlake$time_unix_ms/1000)
# SLIDE_firstlake$date <- date(SLIDE_firstlake$datetime)
# SLIDE_firstlake$DOY <- yday(SLIDE_firstlake$datetime)
# SLIDE_firstlake$satellite <- substr(SLIDE_firstlake$LANDSAT_SCENE_ID,start = 1, stop = 3)
}
#reformatting to 100s of lakes
SLIDE_manylakes <- SLIDE_manylakes %>% 
  rename(time_unix_ms = `system:time_start`) #index_landsat = `system:index`, 
SLIDE_manylakes$DOY <- yday(SLIDE_manylakes$date)
SLIDE_manylakes$satellite <- substr(SLIDE_manylakes$LANDSAT_SCENE_ID,start = 1, stop = 3)

## view 5 random lakes to spot check 
n_lakes <- length(unique(SLIDE_manylakes$lake_id)) #get n lakes
ggplot(data = SLIDE_manylakes[SLIDE_manylakes$lake_id %in% unique(SLIDE_manylakes$lake_id)[sample(x = 1:n_lakes,size = 5,replace = F)],]) + geom_point(aes(x = date, y = RFSnowIce, color = satellite)) + facet_wrap(~lake_id, ncol = 1) #view 5 

## indentify lakes that do not get ice cover or do not thaw
#two flags, no_ice, only_ice >gt 5 observations >75% ice, < 25% no ice #write_csv(append[1:10,], "ice_free_ice_only_log.csv") #creates first log

ice_log <- read_csv("ice_free_ice_only_log.csv") #current log

append <- SLIDE_manylakes %>% #to add 
  group_by(lake_id) %>% 
  summarise(n_ice = sum(RFSnowIce >= .75, na.rm=TRUE), 
            n_iceFree = sum(RFSnowIce <= .25, na.rm = TRUE)) %>%
  filter(n_ice < 5| n_iceFree < 5) %>%
  mutate(flag = ifelse(n_ice < 5, "no_ice", "only_ice")) %>% select(lake_id, flag)

temp <- full_join(x = ice_log, append) #temp joint

#decides whether to append
if(nrow(ice_log) < nrow(temp)){
  write_csv(temp, "ice_free_ice_only_log.csv")
  rm(temp)
} else{
  warning("writing this new csv results in a smaller ice log")
}

rm(ice_log, append)

#look into duplicates 
# 
dups <- SLIDE_manylakes %>% group_by(lake_id, date, satellite) %>% summarise(n = n()) %>% filter(n>1) # no overlapping scenes when filtering out lakes that overlap landsat boundaires 

#take a further look into duplicates
dups_lake <- dups %>% group_by(lake_id) %>% summarise(n_total= sum(n))
dups_sat <- dups %>% group_by(satellite) %>% summarise(n_total= sum(n))

rm(dups, dups_lake, dups_sat)

#### STEP 1B: MODIS DATA ####
# -------------------------- #
{
MODIS_thresh <- read_csv("data/MODIS_thresh_firstlake.csv")
MODIS_thresh <- read_csv("data/MODIS_thresh_firstlake_pld.csv")
MODIS_100lakes <- read_csv("data/MODIS_Thresholds_pld_100.csv")
MODIS_manylakes <- read_csv("data/MODIS_Thresholds_pld_12.csv")
MODIS_manylakes <- read_csv(glue("data/{s1fol}/MODIS_Thresholds_pld1000.csv"))
} #old loads 
MODIS_manylakes <- read_csv(glue("data/{s1fol}/MODIS_Thresholds_{suf}.csv"))

#for one lake 
{
# colnames(MODIS_thresh)[4] <- "time_unix_ms"
# MODIS_thresh$date <- date(as_datetime(MODIS_thresh$time_unix_ms/1000))
# mod_cols = grep(x = colnames(MODIS_thresh), pattern = "gt", value = T)

#for many lakes
#MODIS_manylakes <- MODIS_manylakes %>% rename(index_modis = `system:index`)
}
mod_cols = grep(x = colnames(MODIS_manylakes), pattern = "gt", value = T)

## combining MODIS and Landsat data for one lake: 
{
# LanMOD <- left_join(MODIS_thresh %>% select(all_of(mod_cols), date), 
#           SLIDE_firstlake %>% select(-`system:index`, -time_unix_ms, -datetime)) %>% select(all_of(mod_cols), date, RFSnowIce)
}

#### STEP 1c: COMBINE DATA ####
# -------------------------- #

## combining MODIS and Landsat data for many lakes: 
## filter out lakes that have no ice based on Landsat 

ice_log <- read_csv("ice_free_ice_only_log.csv")
lakes_no_ice <- ice_log[ice_log$flag == "no_ice",]$lake_id #check unique here too 

#comabine Landsat and MODIS
LanMOD <- left_join(MODIS_manylakes %>% select(all_of(mod_cols), date, lake_id), SLIDE_manylakes %>% select( -time_unix_ms)) %>% select(all_of(mod_cols), date, RFSnowIce, lake_id) %>% #-index_landsat,

unique(LanMOD$lake_id) #get lakes 

#this creates threshold plot
LanMODsummary <- pivot_longer(LanMOD, cols = starts_with("gt"), names_to = "mod", values_to = "ice")%>%
  dplyr::mutate(diff = abs(RFSnowIce - ice)) %>%
  group_by(mod, lake_id) %>%
  summarise(dif = mean(diff, na.rm = T))
LanMODsummary

#plot for one lake: 
# ggplot(data = LanMODsummary %>% filter(lake_id == LanMOD$lake_id[10])) + geom_point(aes(x = mod, y = dif))

#plot for 100 lakes with their thresholds: 
ggplot(data = LanMODsummary %>% filter(lake_id == LanMODsummary$lake_id[1])) + geom_point(aes(x = mod, y = dif))

#try to make pdf of thresholds: 
library(gridExtra)
p <- lapply(unique(LanMODsummary$lake_id), function(x) {
  max_threshold = max(LanMODsummary$dif)
  ggplot(LanMODsummary %>% filter(lake_id == x)) + 
    geom_point(aes(x = mod, y = dif)) + theme_bw() + 
    labs(subtitle = paste0("pld_lake_id: ", x)) + 
    lims(y = c(0, max_threshold)) + 
    scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE)]})
})

ggsave(
  filename = glue("figs/{suf}_thresholds.pdf"), #add version
  plot = marrangeGrob(p, nrow=5, ncol=5), 
  width = 12, height = 12
)

## from the thresholds, output pld shapefiles & their threshold value 
lakes_100_shp <- read_sf("data/pld_100_random_AR.shp")
lakes_many_shp <- read_sf("data/pld_dozen.shp")
lakes_many_shp <- read_sf("data/pld_1000.shp")
#need to add in shps for no_overlaps 

head(lakes_many_shp)

#get threholds 
thresholds <- LanMODsummary %>% group_by(lake_id) %>% summarise(dif = min(dif, na.rm = F)) #set to false should be T?

#attach to threshold and add shapes back 
pld_many_lakes_with_thresholds <- left_join(thresholds, LanMODsummary) %>%
  select(-dif) %>% 
  mutate(mod = extract_numeric(mod)/100,
         lake_id = as.character(lake_id)) %>% 
  left_join(lakes_many_shp)

pld_many_lakes_with_thresholds

#write data 
st_write(pld_many_lakes_with_thresholds, glue("data/step2Thresholds/{suf}_wThresholds.shp"), append = F)
#read_sf("data/pld_12_wThresholds.shp")

###### sandbox: 

ModLan1lake <- read_csv("data/modis_landsat_dates_onelake.csv",col_select = c("landsat", "date", "name"))

ModLan1lake %>%
  group_by(date) %>%
  summarise(n = n()) %>%
  filter(n==1)

landsatscenes <- read.csv("data/test_chartArray2.csv")
dup_dates <- landsatscenes %>% group_by(date) %>% summarise(n = length(date)) %>% filter(n>1) %>% select(date) 
dup_dates$dateID <- c(seq(1,7, by = 1))

ls <- landsatscenes %>% filter(date %in% dup_dates$date) %>% left_join(dup_dates) %>% arrange(dateID)
ls
ls[ls$dateID ==1,]
