# this is the sandbox script 
# Audrey Thellman
# date = 12/4/2023
##############################

## load in required packages
#############################

library(tidyverse)
library(sf)

## load in data using hydrolakes: 
#################################

#hydrolakes_shp <- st_read("E:/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp") #this takes 1 minute
Yang_toa_training <- read_csv("./data/toa_training.csv")

# slide_hydrolakes <- hydrolakes_shp %>%
#   filter(Hylak_id %in% Yang_toa_training$Hylak_id)
# rm(hydrolakes_shp)
#write_rds(slide_hydrolakes, "./data/slide_hydrolakes.rds")

slide_hydrolakes <- read_rds("./data/slide_hydrolakes.rds")
st_write(slide_hydrolakes, "./data/slide_hydrolakes.shp")

test_lake <- read_csv("./data/pixels_inlake_v2.csv")
test_lake_sample <- test_lake[sample(nrow(test_lake), size=50, replace = F), ]

test_data <- Yang_toa_training %>% filter(LANDSAT_SCENE_ID == "LT51200342003036BJC00")

ggplot() + 
  geom_density(data = test_data, aes(x = Nir), fill = "red", alpha = 0.5) + 
  geom_density(data =test_lake,  aes(x = B4), fill = "blue", alpha  = 0.5) 

# left off calculating SWIR and other factors 
