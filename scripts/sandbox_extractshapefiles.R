# this is the sandbox script 
# Audrey Thellman
# date = 12/4/2023
##############################

## load in required packages
#############################

library(tidyverse)
library(sf)
library(data.table)
library(glue)

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

temp <-  list.files(pattern="pixels_inlake", path = "./data/", full.names = T)
myfiles <- lapply(temp, read_csv)
pixels_inlakes <- data.table::rbindlist(myfiles, idcol = "file", use.names = T, fill = T)

##test_lake <- read_csv("./data/pixels_inlake_v2.csv")
##test_lake_sample <- test_lake[sample(nrow(test_lake), size=50, replace = F), ]

# there are no repeating hylak ids
length(unique(Yang_toa_training$Hylak_id))
nrow(distinct(Yang_toa_training %>% select(Hylak_id, LANDSAT_SCENE_ID)))

plot_density_overlaps <- function(lake){
  lake = lake
 # lake <- unique(pixels_inlakes$Hylak_id)[1]
  test_lake <- pixels_inlakes %>% filter(Hylak_id == lake)
  test_data <- Yang_toa_training %>% filter(Hylak_id == lake)
  
  #band 4 and 5, (landsat 5 & 8) for NIR 
  #band 2 and 3 for Green
  if(test_data$Landsat[1] == 5) {
    band = "B2"}
  if(test_data$Landsat[1] == 8) {
    band = "B3"
  }
  
  plot <- ggplot() + 
    geom_density(data = test_data, aes(x = Green), #Nir or Green
                 fill = "gray", alpha = 0.5) + 
    geom_density(data =test_lake,  aes(x = .data[[band]]), 
                 fill = "orange", alpha  = 0.5) + 
    labs(title = glue("Hydrolake ID = {lake}"), 
         x= "Green", #change label here 
         caption = glue("Landsat TOA Collection {test_data$Landsat}"), 
         subtitle = glue("from sample of {nrow(test_data)} points"))+ 
    theme_bw()
  return(plot)
}

plot_density_overlaps_ratios <- function(lake){
  lake = lake
  # lake <- unique(pixels_inlakes$Hylak_id)[1]
  test_lake <- pixels_inlakes %>% filter(Hylak_id == lake)
  test_data <- Yang_toa_training %>% filter(Hylak_id == lake)
  
  #band 4 and 5, (landsat 5 & 8) for NIR 
  #band 2 and 3 for Green
  #band 1 and 2 for blue 
  #band 3 and 4 for red 
  
  
  if(test_data$Landsat[1] == 5) {
    num = "B3"}
  if(test_data$Landsat[1] == 8) {
    num = "B4"
  } #red
  
  if(test_data$Landsat[1] == 5) {
    denom = "B2"}
  if(test_data$Landsat[1] == 8) {
    denom = "B3"
  }
  #green
  
  plot <- ggplot() + 
    geom_density(data = test_data, aes(x = `R/G`), #Nir or Green
                 fill = "gray", alpha = 0.5) + 
    geom_density(data =test_lake,  aes(x = .data[[num]]/.data[[denom]]), 
                 fill = "orange", alpha  = 0.5) + 
    labs(title = glue("Hydrolake ID = {lake}"), 
         x= "Red/Green", #change label here 
         caption = glue("Landsat TOA Collection {test_data$Landsat}"), 
         subtitle = glue("from sample of {nrow(test_data)} points"))+ 
    theme_bw()
  return(plot)
}


pdf(file = "10lakes_redgreen.pdf", height = 5*1.5, width = 7*1.5)
#plot_density_overlaps(lake =unique(pixels_inlakes$Hylak_id)[5])
plots <- lapply(unique(pixels_inlakes$Hylak_id), plot_density_overlaps_ratios)
marrangeGrob(grobs=plots, nrow=2, ncol=3)
dev.off()



# left off calculating SWIR and other factors 
library(gridExtra)
