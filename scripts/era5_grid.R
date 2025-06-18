# make era 5 grid cells 
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

file <- list.files(pattern = "*.nc", recursive = T, path = "data/era5/", full.names = T)[1] #file with 0.25 resolution (fa)
era = raster(file)

plot(era)
extent(era)

# -------create centers for ERA-5 temperature:--------------
## need to re-write a portion of lakes into spherical geometry 

pld_wWRS <- read_sf("data/inputsGEE/pld_gt_1km2_wWRS.shp")
pld_wWRS_centers <- st_centroid(st_make_valid(pld_wWRS)) #change to make_valid
#write_sf(pld_wWRS_centers, "data/pld_gt_1km2_wWRS_centers.shp"

pld_overlaps_centers < - st_centroid(st_make_valid(pld_gt_1km2_overlaps_new)) 
#write_sf(pld_overlaps_centers, "data/pld_gt_1km2_overlaps_centers.shp")

#create centers for already run: 
pld_ar_centers <- st_centroid(st_make_valid(st_as_sf(pld_already_run))) #change to make_valid
#write_sf(pld_ar_centers, "data/pld_gt_1km2_alreadyrun_centers.shp", delete_layer = T)


# --------------create era-5 grid----------------------------

pts <- rasterToPoints(era,spatial = T)
names(pts) <- "temp_x2meter" #creates centers of tiles
#shapefile(pts[1:500000,], filename='data/era5/points_v7a.shp', overwrite = T) #this is the correct pts 

shapefile(pts, 'data/era5/points_full.shp')

## create era_ids per point 
pld_overlaps_centers <- read_sf("data/inputsGEE/pld_gt_1km2_overlaps_centers.shp") %>% dplyr::select(lake_id) 
#lakes that overlap paths
pld_ar_centers <- read_sf("data/inputsGEE/pld_gt_1km2_alreadyrun_centers.shp") %>% dplyr::select(lake_id) #lakes run initially
pld_wWRS_centers <- read_sf("data/inputsGEE/pld_gt_1km2_wWRS_centers.shp") %>% dplyr::select(lake_id) 
#lakes that fall within paths 

pld_centers_all <- st_as_sf(data.table::rbindlist(list(pld_overlaps_centers, pld_ar_centers, pld_wWRS_centers)))

#create id value 
pts$era_id <- seq(1:nrow(pts))
pld_era <- st_join(pld_centers_all, st_as_sf(pts),join = st_nearest_feature, left = T) #to get era_ids
#write_sf(pld_era, "data/era5/pld_era_ids.shp")

unique_eras <- unique(pld_era$era_id) #115,000 unique points (1/3 of the dataset

pts_withLakes <- pts[pts$era_id %in% unique_eras,]
#shapefile(pts_withLakes, filename='data/era5/era5_points_wLakes.shp', overwrite = T) #this is the correct pts 


