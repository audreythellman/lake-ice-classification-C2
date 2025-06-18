#threshold output 
library(readr)
library(dplyr)
library(sf)
library(glue)
`%notin%` <- Negate(`%in%`)

# load in thresholds_v1

#thresholds_v1 <- read_csv("data/step2Thresholds/thresholds_v1.csv") #v1 run already 
thresholds_v1 <- read_csv("data/step2Thresholds/thresholds_v2.csv")
colnames(thresholds_v1)
thresholds_v1$mod <- as.numeric(stringr::str_extract(thresholds_v1$threshold, "[0-9]{2}"))/100

# how many lakes 
# filter for n_days > 4 & no duplicates 
thresholds_filtered <- thresholds_v1 %>% filter(n_ice > 4 & n_water >4) %>% distinct()
#lost about 10k observations 

#remove remove already run 
ar1 <- read_csv("data/step2Thresholds/thresholds_v1.csv") #v1 run already 
lakes_ar <- unique(ar1$lake_id)

thresholds_filtered <- thresholds_filtered[thresholds_filtered$lake_id %notin% lakes_ar,] #38,527


#divide into glrip lakes and not glrip lakes 
#load in dataset with glrip matches 
glrip_matches <- read_rds("data/validation/glrip_w_pldID_filtered.rds")
colnames(glrip_matches)

#load in dataset with pld < 1km2 in area 
pld_gt_1km2 <- readRDS("data/shps/pld_gt_1km2_v2.rds")
pld_gt_1km2$lake_id <- as.numeric(pld_gt_1km2$lake_id)

thresholds_glrip <- 
  thresholds_filtered[thresholds_filtered$lake_id %in% glrip_matches$lake_id,] %>% 
  dplyr::left_join(pld_gt_1km2)

thresholds_misc <-
  thresholds_filtered[thresholds_filtered$lake_id %notin% glrip_matches$lake_id,] %>%
  dplyr::left_join(pld_gt_1km2)

#save for GEE 
#############
datestamp <- format(Sys.Date(),"%m%d")

#save as is 
write_sf(thresholds_glrip, glue("data/step2Thresholds/thresh_glrip_{datestamp}.shp"), delete_layer = T)
# write_sf(thresholds_misc, glue("data/step2Thresholds/thresh_{datestamp}.shp"),delete_layer = T)

#save by threshold 
thresholds_split <- split(thresholds_misc, thresholds_misc$threshold)
str(thresholds_split)

save_split_sf <- function(x) {
  write_sf(thresholds_split[[x]], glue("data/step2Thresholds/thresh_{x}_{datestamp}.shp"))
}

lapply(names(thresholds_split), save_split_sf)
