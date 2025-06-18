# this is the sandbox script 
# Audrey Thellman
# date = 12/4/2023
##############################

## load in required packages
#############################

library(tidyverse)
library(plyr)
library(sf)
library(data.table)
library(glue)
library(dplyr)
library(ggpubr)

## load in data using hydrolakes: 
#################################

#left of here! need to use external hardrive 
#hydrolakes_shp <- st_read("F:/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp") #this takes 1 minute
Yang_toa_training <- read_csv("./data/toa_training.csv")
Yang_glrip_lakes <- read_csv("data/lake_ice_fraction_glrip_TOA_06292020_e26b3b2955b1e8165de4882653d059ba.csv") #there are sometimes multiple glrip stations per hydrolake 

rm(hydrolakes_shp)

#there are duplicates of Hylak_ids and GLRIP lakes 
Yang_glrip_lakes %>% select(Hylak_id, LANDSAT_SCENE_ID, doy) %>% arrange(LANDSAT_SCENE_ID, Hylak_id) %>% distinct()
Yang_glrip_lakes %>% add_count(Hylak_id, LANDSAT_SCENE_ID, doy) %>% arrange(LANDSAT_SCENE_ID, Hylak_id) %>% filter(n > 1)

# cleaning up the data to avoid duplicates 
Yang_glrip_lakes_noglrip <- Yang_glrip_lakes %>% select(-glrip_id, -`system:index`) %>% distinct() # now there are 25,483


C2_lakeicefrac <- read_csv("./data/lake_ice_fraction_glrip_TOA_v3.csv") %>% # verson 2 is identical to v3 except v3 has hlak and glrip id 
  select(Hylak_d, glrip_d , LANDSAT_SCENE_ID, doy, RFSnowIce, FmaskSnowIce, cloud) %>%
  dplyr::rename(Hylak_id = Hylak_d, glrip_id = glrip_d, c2_RFSnowIce = RFSnowIce, c2_FmaskSnowIce = FmaskSnowIce, c2_cloud = cloud)

C2_lakeicefrac_noglrip <- C2_lakeicefrac %>% select(-glrip_id) %>% distinct() #24180

# C2_lakeicefrac <- read_csv("./data/lake_ice_fraction_glrip_TOA_.csv") %>% #this was actually re-run with Collection 1 
#   select(Hylak_d, glrip_d , LANDSAT_SCENE_ID, doy, RFSnowIce, FmaskSnowIce, cloud) %>%
#   dplyr::rename(Hylak_id = Hylak_d, glrip_id = glrip_d, c2_RFSnowIce = RFSnowIce, c2_FmaskSnowIce = FmaskSnowIce, c2_cloud = cloud)

# comparing collection 1 to collection 2 w/ duplicates from glrip 
Col1vsCol2 <- right_join(Yang_glrip_lakes, C2_lakeicefrac, by = c("LANDSAT_SCENE_ID", "doy", "Hylak_id", "glrip_id")) 
Col1vsCol2$flag <-ifelse(Col1vsCol2$RFSnowIce > Col1vsCol2$c2_RFSnowIce+ 0.1 | Col1vsCol2$RFSnowIce< Col1vsCol2$c2_RFSnowIce-0.1, 1,0)#1 meaning there is a flag 
Col1vsCol2$diff <- Col1vsCol2$RFSnowIce-Col1vsCol2$c2_RFSnowIce
Col1vsCol2$cloud_diff <- abs(Col1vsCol2$cloud-Col1vsCol2$c2_cloud)

mean(Col1vsCol2$flag, na.rm = T) #.1% of the data is >10% different
length(which(Col1vsCol2$flag == 1)) #31 lakes

#comparing collection 1 to collection 2 w/o duplicates from grlip 
Col1vsCol2_nodups <- right_join(Yang_glrip_lakes_noglrip, C2_lakeicefrac_noglrip, by = c("LANDSAT_SCENE_ID", "doy", "Hylak_id")) 
Col1vsCol2_nodups$flag <-ifelse(Col1vsCol2_nodups$RFSnowIce > Col1vsCol2_nodups$c2_RFSnowIce+ 0.1 | Col1vsCol2_nodups$RFSnowIce< Col1vsCol2_nodups$c2_RFSnowIce-0.1, 1,0)#1 meaning there is a flag 
Col1vsCol2_nodups$diff <- Col1vsCol2_nodups$RFSnowIce-Col1vsCol2_nodups$c2_RFSnowIce
Col1vsCol2_nodups$cloud_diff <- abs(Col1vsCol2_nodups$cloud-Col1vsCol2_nodups$c2_cloud)

mean(Col1vsCol2_nodups$flag, na.rm = T) #.1% of the data is >10% different
length(which(Col1vsCol2_nodups$flag == 1)) #31 lakes w >10% difference 

## plot what the data looks like 

ggplot(Col1vsCol2, aes(x = RFSnowIce, y = c2_RFSnowIce)) + geom_point(alpha = 0.1, size = 0.5) + theme_bw() + labs(x = "Collection 1", y = "Collection 2", title = "SLIDE (random forest snow/ice)", subtitle = "n lake-days = 26,691, pearsons r = 0.95") + geom_density_2d()

cor.test(x = Col1vsCol2$RFSnowIce, y = Col1vsCol2$c2_RFSnowIce, method = "pearson") #.95
ggplot(Col1vsCol2) + geom_point(aes(x = FmaskSnowIce, y = c2_FmaskSnowIce), alpha = 0.5, size = 0.5) + theme_bw() +  labs(x = "Collection 1", y = "Collection 2", title = "Fmask snow/ice", subtitle = "n lake-days = 26,691")

library(RColorBrewer)

ggplot(Col1vsCol2, aes(x = RFSnowIce, y = c2_RFSnowIce)) + geom_point(alpha = 0.5, size = 0.5, aes(color = cloud_diff)) + theme_bw() + labs(x = "Collection 1", y = "Collection 2", title = "SLIDE (random forest snow/ice)", subtitle = "n lake-days = 25,595, pearsons r = 0.99") + scale_color_gradient(low = "grey", high = "red")

ggplot(Col1vsCol2_nodups, aes(x = RFSnowIce, y = c2_RFSnowIce)) + geom_point(alpha = 0.5, size = 0.5, aes(color = cloud_diff)) + theme_bw() + labs(x = "Collection 1", y = "Collection 2", title = "SLIDE (random forest snow/ice)", subtitle = "n lake-days = 24,180, pearsons r = 0.99") + scale_color_gradient(low = "grey", high = "red")

oneproblemlake <- Col1vsCol2_nodups[which(Col1vsCol2_nodups$diff > .2),][4,]

ggplot() + 
  geom_point(data = Col1vsCol2_nodups, aes(x = RFSnowIce, y = c2_RFSnowIce, color = cloud_diff), alpha = 0.5, size = 0.5) + 
  geom_point(data = oneproblemlake, aes(x = RFSnowIce, y = c2_RFSnowIce), color = "blue")+
  theme_bw() + 
  labs(x = "Collection 1", y = "Collection 2", title = "SLIDE (random forest snow/ice)", subtitle = "n lake-days = 24,180, pearsons r = 0.99") +
  scale_color_gradient(low = "grey", high = "red")

ggplot() + 
  geom_point(data = Col1vsCol2_nodups, aes(x = RFSnowIce, y = c2_RFSnowIce), alpha = 0.5, size = 0.5) + 
  #geom_point(data = oneproblemlake, aes(x = RFSnowIce, y = c2_RFSnowIce), color = "blue")+
  theme_bw() + 
  labs(x = "Collection 1", y = "Collection 2", title = "SLIDE (random forest snow/ice)", subtitle = "n lake-days = 24,180, pearsons r = 0.99") 


## attach hydrolake data to chosen hydrolakes  
############################################

#TOA TRAINING: 
# -----------
# slide_hydrolakes <- hydrolakes_shp %>%
#   filter(Hylak_id %in% Yang_toa_training$Hylak_id)
# 
# write_rds(slide_hydrolakes, "./data/slide_hydrolakes.rds")


#GLRIP LAKES: 
# -----------
# glrip_hydrolakes <- hydrolakes_shp %>%
#   filter(Hylak_id %in% unique(Yang_glrip_lakes$Hylak_id)) %>%
#   right_join(Yang_glrip_lakes %>% select(glrip_id,Hylak_id, LANDSAT_SCENE_ID, doy)) %>%
#   dplyr::rename(yang_doy = doy)
# write_rds(glrip_hydrolakes, "./data/glrip_hydrolakes.rds")
# st_write(glrip_hydrolakes, "./data/glrip_hydrolakes.shp") # save a shapefile of the glrip hydrolakes


## load in derivative data: 
#################################
rm(hydrolakes_shp)

slide_hydrolakes <- read_rds("./data/slide_hydrolakes.rds")
slide_hydrolakes_wlssi <- slide_hydrolakes %>% left_join(Yang_toa_training %>% select(Hylak_id, LANDSAT_SCENE_ID) %>% distinct())
#st_write(slide_hydrolakes_wlssi, "./data/slide_hydrolakes_wlssi.shp") # save a shapefile of the slide hydrolakes

# there are no repeating hylak ids 
length(unique(Yang_toa_training$Hylak_id))
nrow(distinct(Yang_toa_training %>% select(Hylak_id, LANDSAT_SCENE_ID)))
Yang_toa_training %>% dplyr::group_by(Landsat) %>% dplyr::summarise(n = n_distinct(Hylak_id)) # there are 652 lakes with Landsat 5 scenes, 84 with Landsat 7 scenes, and 259 with Landsat 8 scenes 