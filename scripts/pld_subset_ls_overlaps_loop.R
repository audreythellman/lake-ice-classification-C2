## overlap and non-overlap lakes -- figure out which path & how much area 
library(sf)
library(data.table)
`%notin%` <- Negate(`%in%`)


# get data 
pld_gt_1km2 <- readRDS("data/shps/pld_gt_1km2_v2.rds") 
AS_lakes <- readRDS("data/AS_lakes_sa_in_m.rds")

#add surface area of lakes with correction: 
# divide m2 by 10000 to get ha
pld_gt_1km2$max_area_corrected <- ifelse(pld_gt_1km2$lake_id %in% AS_lakes, pld_gt_1km2$max_area/10000, pld_gt_1km2$max_area)

## get ls_overlaps 
ls_overlaps <- read_sf("data/shps/pld_gt1km_lsoverlaps.shp") %>% dplyr::left_join(pld_gt_1km2 %>% st_drop_geometry())
#25,348 lakes
ls_antioverlaps <- pld_gt_1km2[pld_gt_1km2$lake_id %notin% ls_overlaps$lake_id,]
# 143,460 lakes 

#non overlaps 
# st_intersects to get pass x row 
landsat_wrs <- read_sf("data/shps/WRS2_descending_0/WRS2_descending.shp")

sf_use_s2(FALSE)
ls_anti_int <- sf::st_join(x = ls_antioverlaps, y = landsat_wrs, join = st_intersects, left = T) #going to be many y to x 


#overlaps 
# perform intersection to get multiple pass x row 
# perform area calculation 

st_intersection_faster <- function(x,y,...){
  #faster replacement for st_intersection(x, y,...)
  
  y_subset <-
    st_intersects(x, y) %>%
    unlist() %>%
    unique() %>%
    sort() %>%
    {y[.,]}
  
  st_intersection(x, y_subset,...)
}

t1 <- Sys.time()
ls_over_int <- st_intersection_faster(x = ls_overlaps, y = landsat_wrs)
t2 <- Sys.time()
print(t2-t1)

lsoia <- ls_over_int %>% dplyr::mutate(area_calc = st_area(.))

saveRDS(lsoia, "data/shps/ls_overlaps_intersections_v1.rds")
#10 lakes takes 20 seconds 

lsoai_read <- readRDS("data/shps/ls_overlaps_intersections_v1.rds")
nrow(lsoai_read) #nearly 3.4x as many 'lakes' due to duplicates 
nrow(ls_overlaps)

library(dplyr)
library(sf)
library(units)
library("measurements")

#85-km-cross-track-by-180-km-along-track
# lakes that have overlaps must be > 180 x 180 km

threshold_area = 250*180000 #m2
units(threshold_area) <- "m2"

lsoai_short <- lsoai_read[c('lake_id', 'names', 'max_area_corrected', 'PATH', 'ROW', 'area_calc')]
units(lsoai_short$max_area_corrected) <- "hectare"

lsoai_short$full_img <- ifelse(lsoai_short$area_calc >= 0.98*lsoai_short$max_area_corrected, 1, 0) 
#if area is at least 98% of area
full_PathRow_lakes <- unique(lsoai_short[lsoai_short$full_img == 1,]$lake_id) #24,999 lakes 
partial_PathRow_lakes <- lsoai_short[lsoai_short$lake_id %notin% full_PathRow_lakes,]
unique(partial_PathRow_lakes$lake_id) #approximately 350 lakes 

#for landsat overlap runs: 
saveRDS(partial_PathRow_lakes, "scripts/ls_partial_lakes.rds")


#lakes that have full images from ls overlaps
full_PathRow_lakes_df <- lsoai_short[lsoai_short$lake_id %in% full_PathRow_lakes & lsoai_short$full_img == 1,] 

length(unique(full_PathRow_lakes_df$lake_id)) + length(partial_PathRow_lakes)

full_img_duplicates <- full_PathRow_lakes_df %>% st_drop_geometry()%>% group_by(lake_id, PATH) %>% summarise(n_rows = length(ROW)) %>% filter(n_rows >1)

#images that lie in the intersection of two rows, but are not cut off 
full_img_duplicates_df <- lsoai_short[lsoai_short$lake_id %in% full_img_duplicates$lake_id & lsoai_short$full_img == 1,] 


full_img_duplicates_df2 <- setDT(full_img_duplicates_df %>% mutate(id = paste0(lake_id, "-", PATH)))[, .SD[which.max(area_calc)], id]
saveRDS(full_img_duplicates_df2, "scripts/ls_full_lakes.rds")

## what is left 
full_lakes_vertical <- unique(full_img_duplicates_df2$lake_id)
partial_lakes_vertical <- unique(partial_PathRow_lakes$lake_id)

#each of these have unique paths = unique days! 
other_full_img_lakes <- lsoai_short[lsoai_short$lake_id %notin% c(full_lakes_vertical, partial_lakes_vertical) & lsoai_short$full_img ==1,]
saveRDS(other_full_img_lakes, "scripts/ls_full_lakes_manyPaths.rds")
duplicate_paths_lakes <- unique(other_full_img_lakes$lake_id)

length(duplicate_paths_lakes) + length(full_lakes_vertical) + length(partial_lakes_vertical)

## partial 


# create log file 
partial_PathRow_lakes_df <- readRDS("scripts/ls_partial_lakes.rds")
full_lakes_horizontal_df <- readRDS("scripts/ls_full_lakes_manyPaths.rds")
full_lakes_vertical_df <- readRDS("scripts/ls_full_lakes.rds")

plog <- data.frame(lake_id = unique(partial_PathRow_lakes_df$lake_id), overlap_type = "partial")
flog1 <- data.frame(lake_id = unique(full_lakes_horizontal_df$lake_id), overlap_type = "full_horizontal")
flog2 <- data.frame(lake_id = unique(full_lakes_vertical_df$lake_id), overlap_type = "full_vertical")

overlap_log <- data.table::rbindlist(list(plog, flog1, flog2)) %>% right_join(data.frame(lake_id = unique(pld_gt_1km2$lake_id)))
overlap_log$overlap_type <- ifelse(is.na(overlap_log$overlap_type), "full_none",overlap_log$overlap_type)

write.csv(overlap_log, "data/logs/overlap_log.csv")


## now overlap runs on landsat 
# get already run: 
pld_already_run_Overlap <- read_sf("data/inputsGEE/pld_already_run_Overlap.shp")
already_run_lake_ids <- unique(pld_already_run_Overlap$lake_id) #3593 lakes

#get overlaps to run lakes: 
ls_overlaps_torun <- rbindlist(list(full_lakes_horizontal_df, full_lakes_vertical_df), fill = T)

ls_overlaps_torun2 <- ls_overlaps_torun[ls_overlaps_torun$lake_id %notin% already_run_lake_ids,] %>% select(-area_calc, -full_img, -id)

ls_overlaps_torun2$max_area_corrected <- as.numeric(ls_overlaps_torun2$max_area_corrected)

ls_overlaps_torun3 <- st_as_sf(ls_overlaps_torun2)
ls_overlaps_torun3

write_sf(ls_overlaps_torun3, "data/inputsGEE/ls_overlaps_4-17-2025.shp")

length(unique(ls_overlaps_torun3$lake_id)) #missing 21,431 lakes 

# example 
# ex_polygons <- read_sf("data/shps/example_intersects.shp")
# ex_polygons_int <- st_intersection(x = ex_polygons, y = landsat_wrs)
# epi <- ex_polygons_int %>% mutate(area_calc = st_area(.))

  landsat_wrs <- read_sf("data/shps/WRS2_descending_0/WRS2_descending.shp")
  pdf('overlap_vis_composite_lakes.pdf', height = 10, width = 10)
  par(mfrow = c(3, 3))  ## set the layout to be 3 by 3
  lapply(unique(partial_PathRow_lakes$lake_id), function(x){
    lake = partial_PathRow_lakes[partial_PathRow_lakes$lake_id == x,]
    area = round(conv_unit(lake$max_area_corrected[1], "hectare", "km2"),1)
    plot(st_geometry(lake), col = "blue", bg = "gray")
    plot(st_geometry(landsat_wrs), add = T)
    sp::degAxis(side = 1)
    sp::degAxis(side = 2,las = 2)
    #polygonsLabel(landsat_wrs, landsat_wrs$PATH)
    title(lake$names[1], sub = paste0(lake$lake_id, ": ",area, "km2"))
  })
  dev.off()
