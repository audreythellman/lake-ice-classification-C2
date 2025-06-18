# need to run MODIS re-runs by PATH x ROW for a) pld_xx_ lakes that ran on slow code & b) overlap lakes that were not already run
# '''need to have lake surface area, currently in ha 
# '''need to have PATH and ROW 

library(sf)
`%notin%` <- Negate(`%in%`)

setwd("C:/Users/athellma/OneDrive - University of North Carolina at Chapel Hill/Documents/ChapelHill/Research/_LakeIce/lake-ice-classification-C2")


modis_already_run_1 <- read_sf("data/inputsGEE/pld_gt_1km2_wWRS.shp") 
#129,674 lakes
modis_already_run_2 <- read_sf("data/inputsGEE/pld_overlaps_10k_b1.shp")
#10,878


modis_already_run_lakes <- c(unique(modis_already_run_1$lake_id), unique(modis_already_run_2$lake_id)) #140,552 lakes
pld_gt_1km2 <- readRDS("data/shps/pld_gt_1km2_v2.rds")
#168,808 lakes
modis_rerun_lakes <- pld_gt_1km2[pld_gt_1km2$lake_id %notin% modis_already_run_lakes,] 
#30,262 lakes

#fix area in AS from m to ha
#AS <- read_sf("F:/pld2020/Originals/PLD_AS.shp")
#AS <- AS[AS$max_area > 1e6,] #greater than 1km2 (100 ha)
#AS_lakes <- unique(AS$lake_id)
#saveRDS(AS_lakes, "data/AS_lakes_sa_in_m.rds")
AS_lakes <- readRDS("data/AS_lakes_sa_in_m.rds")

#add surface area of lakes with correction: 
# divide m2 by 10000 to get ha
pld_gt_1km2$max_area_corrected <- ifelse(pld_gt_1km2$lake_id %in% AS_lakes, pld_gt_1km2$max_area/10000, pld_gt_1km2$max_area)

modis_rerun_lakes_warea <- modis_rerun_lakes %>% dplyr::left_join(pld_gt_1km2 %>% st_drop_geometry()) #adding left join to fix the area 
colnames(modis_rerun_lakes_warea)


landsat_wrs <- read_sf("data/shps/WRS2_descending_0/WRS2_descending.shp")

sf_use_s2(FALSE)
modis_rerun_lakes_warea_wWRS <- sf::st_join(x = modis_rerun_lakes_warea, y = landsat_wrs, join = st_intersects, left = T) #going to be many y to x 

write_sf(modis_rerun_lakes_warea_wWRS, "data/inputsGEE/modis_reruns_4-15-2025.shp")


quantile(modis_rerun_lakes_warea$max_area_corrected, .98) #>10,000 ha 
#max(paths, na.rm = T)
sum(is.na(modis_rerun_lakes_warea_wWRS_2$PATH)) #there's a lot of NA's 

modis_rerun_lakes_warea_wWRS <- read_sf("data/inputsGEE/modis_reruns_4-15-2025.shp")
#just pick first row x path 

mrlaw <- modis_rerun_lakes_warea_wWRS %>%
  dplyr::group_by(lake_id) %>%
  dplyr::filter(dplyr::row_number()==1)

write_sf(mrlaw, "data/inputsGEE/modis_reruns_4-15-2025_v2.shp")

