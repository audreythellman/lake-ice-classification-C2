## pick 100 lakes to choose from PLD
list.of.packages <- c("sf", "magrittr", "ggplot2","raster", "rnaturalearth", "rnaturalearthdata", "doParallel", "foreach")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages) #this takes 1 hour
# lakes must be >1km2(1e6 m2) and freeze! for right now > 45 degrees north or south 

# packages
library(sf)
library(magrittr)
library(ggplot2)
#library(raster)
library("rnaturalearth")
library("rnaturalearthdata")
library(doParallel)
library(foreach)

`%notin%` <- Negate(`%in%`)


#### load in the pld and subset by area: (area in ha) ####
AR <- read_sf("F:/pld2020/Originals/PLD_AR.shp")
AR <- AR[AR$max_area >100,]
AF <- read_sf("F:/pld2020/Originals/PLD_AF.shp")
AF <- AF[AF$max_area > 100,]
AS <- read_sf("F:/pld2020/Originals/PLD_AS.shp")
AS <- AS[AS$max_area > 1e6,] ## only asia is in m2
AU <- read_sf("F:/pld2020/Originals/PLD_AU.shp")
AU <- AU[AU$max_area > 100,]
EU <- read_sf("F:/pld2020/Originals/PLD_EU.shp")
EU <- EU[EU$max_area > 100,]
GR <- read_sf("F:/pld2020/Originals/PLD_GR.shp")
GR <- GR[GR$max_area > 100,]
NAm <- read_sf("F:/pld2020/Originals/PLD_NA.shp")
NAm <- NAm[NAm$max_area > 100,]
SA <- read_sf("F:/pld2020/Originals/PLD_SA.shp")
SA <- SA[SA$max_area > 100,]
SI <- read_sf("F:/pld2020/Originals/PLD_SI.shp")
SI <- SI[SI$max_area > 100,]

#save and load the data: 
rm(AR, AF, AS, AU, EU, GR, NAm, SA, SI)
pld_gt_1km2 <- rbind(AR, AF, AS, AU, EU, GR, NAm, SA, SI) #this takes several minutes
#saveRDS(pld_gt_1km2, "data/shps/pld_gt_1km2_v2.rds") #version 1 had AS data with too many lakes

  #this is how we caught the units error: 
  
  # lakes_gt_1km2_points %>% filter(names == "WINNIPEG") 
  #verifying that the areas are correct 
  # lakes_gt_1km2_points %>% filter(names == "QINGHAI;QINGHAI HU")
  # sort(lakes_gt_1km2_points[!is.na(lakes_gt_1km2_points$names),]$names)

#### output with pld > 1km2 ####
pld_gt_1km2 <- readRDS("data/shps/pld_gt_1km2_v2.rds")
#write_sf(pld_gt_1km2, "data/shps/pld_gt_1km2_v2.shp")

#### subset to lakes > 45 deg lat ####
# ----------------------------------- # 
gt45lat = st_polygon(
  list(
    cbind(c(-180,-180,180,180, -180), 
          c(45,90,90,45,45)))) %>%
  st_sfc(crs = "EPSG:4326")

# need to do this for lt45lat

#sf_use_s2(FALSE) ## have to do this when the bounding box crosses the meridian 
numCores <- detectCores()/4
c1 <- makeCluster(numCores)
registerDoParallel(c1)
getDoParWorkers()

imax = getDoParWorkers()*10
init = round(nrow(pld_gt_1km2)/imax)
ses = 1 #make sure to change this!
#used to create temp, and .combine = rbind

foreach(i=1:imax-1, .packages = c("sf")) %do% { 
  tryCatch({
    sf_use_s2(FALSE) #do this to make sure there are no issues with the meridian
    saveRDS(pld_gt_1km2[seq(init*i+1, by = 1, length.out = init),][gt45lat,], paste0("data/out/out_gt45_",i,"x",ses,".rds"))
    #cat(paste0("trying i #", i, " at ", Sys.time()))
    })
} #this takes 7 minutes for 1 million; each for loop requires 8 GiB of RAM for 6,700 lines


stopCluster(c1)

pld_gt_1km_45lat <- lapply(list.files(path = "data/out/", pattern = "out_", full.names = T), readRDS)
pld_gt_1km_45lat_data <- plyr::rbind.fill(pld_gt_1km_45lat)
#saveRDS(pld_gt_1km_45lat_data, "data/shps/pld_gt_1km_45lat.rds")

#this was completed on 10/24/2024: 
pld_gt_1km_45lat_sf <- st_as_sf(pld_gt_1km_45lat_data)
sf_use_s2(FALSE)
#write_sf(pld_gt_1km_45lat_sf[1:136808,], "data/pld_gt_1km_45lat_1.shp") #last lake having problems

#### output with pld > 45 lat ####
pld_gt_1km_45lat_sf = read_sf("data/shps/pld_gt_1km_45lat_1.shp")

#### make subsets of lakes (~20,000 already run) ####
## --------------------------------------------------
pld_100_AR <- pld_gt_1km_45lat_sf[1:100,]
# write_sf(st_as_sf(pld_100_AR), "data/pld_100_random_AR.shp", delete_layer = T)

#get some lakes 
sel_lake_id <- c(8120764032,
8120662592,
8220065702,
7420832032,
7250195273,
7310158243,
2510280033,
2440082752,
2520113573,
3220493713,
3510136202,
3510340432,
3120584972)

pld_dozen <- pld_gt_1km_45lat_sf[which(unique(pld_gt_1km_45lat_sf$lake_id) %in% sel_lake_id),]
#write_sf(st_as_sf(pld_dozen), "data/pld_dozen.shp", delete_layer = T)

pld_dozen = read_sf("data/pld_dozen.shp")

## scaling up the subset to 1000
## and testing Angelica's lakes: 

angelica_lakes <- read.csv("data/angelica_lakes.csv") 
angelica_subset <- pld_gt_1km2[pld_gt_1km2$lake_id %in% angelica_lakes$lake_id,]
angelica_lakes$gt_1km2 <- ifelse(angelica_lakes$lake_id %in% angelica_subset$lake_id, T, F)
angelica_subset_gt45lat <- pld_gt_1km_45lat_sf[pld_gt_1km_45lat_sf$lake_id %in% angelica_lakes$lake_id,]
angelica_lakes$gt_45lat <- ifelse(angelica_lakes$lake_id %in% angelica_subset_gt45lat$lake_id, T, F)

## subsetting to 1000 lakes 
nrow(angelica_subset)
pld_938 <- pld_gt_1km_45lat_sf[101:(1100-nrow(angelica_subset)),] 
lakes_1000 <- c(angelica_subset$lake_id, pld_938$lake_id)
pld_1000 <- pld_gt_1km2[pld_gt_1km2$lake_id %in% lakes_1000,]

#write_sf(st_as_sf(pld_1000), "data/pld_1000.shp", delete_layer = T)
pld_1000 <- read_sf("data/pld_1000.shp")
pld_1000_centers <- st_centroid(pld_1000)
#write_sf(pld_1000_centers, "data/pld_1000_centers.shp")

# subsetting to 10,000 lakes
pld_10k <- pld_gt_1km_45lat_sf[1039:(1039+10000-1),]
#write_sf(st_as_sf(pld_10k), "data/pld_10k.shp", delete_layer = T)
# create centers for lakes 
pld_10k <- read_sf("data/pld_10k.shp")
pld_10k_centers <- st_centroid(pld_10k)
#write_sf(pld_10k_centers, "data/pld_10k_centers.shp")


# subsetting to 10,000 lakes
pld_10k <- pld_gt_1km_45lat_sf[11039:(11039+10000-1),]
#write_sf(st_as_sf(pld_10k), "data/pld_10k_b2.shp", delete_layer = T)
# create centers for lakes 
pld_10k <- read_sf("data/pld_10k_b2.shp")
pld_10k_centers <- st_centroid(pld_10k)
#write_sf(pld_10k_centers, "data/pld_10k_centers.shp")

plot(rnaturalearth::countries110$geometry)
plot(pld_10k_centers$geometry, add = T)
plot(pld_1000_centers, add = T, col = "blue")

#### subset based on path x row (part 1 - no overlaps > 45 lat) ####
## -------------------------------------------------------

#set global options 
`%notin%` <- Negate(`%in%`)
#sf_use_s2(FALSE) #tried to avoid using this for GEE imports 

#read data: 
pld_gt_1km2 <- readRDS("data/shps/pld_gt_1km2_v2.rds") #lakes > 1km2

landsat_wrs <- read_sf("data/shps/WRS2_descending_0/WRS2_descending.shp")

#if invert sf overlaps then I get get the entire selection in ArcGIS 

pld_subset <- read_sf("data/shps/pld_gt1km_45lat_lsoverlaps.shp") #this is only >45 lat, so included <45 lat and overlaps 
pld_gt_1km2_noOverlaps <- pld_gt_1km2[pld_gt_1km2$lake_id %notin% pld_subset$lake_id,]

#will take several minutes 
pld_gt_1km2_wWRS <- sf::st_join(x = pld_gt_1km2_noOverlaps, y = landsat_wrs, join = st_within, left = T)

#saveRDS(pld_gt_1km2_wWRS, "pld_gt_1km2_wWRS_temp.rds") #this is temporary file of all lakes >1km2, some <45lat cross landsat
#save file as sf later 

#get lakes already run: 
pld_100 <- read_sf("data/pld_100_random_AR.shp")
pld_dozen <-  read_sf("data/pld_dozen.shp")
pld_1000 <- read_sf("data/pld_1000.shp")
pld_10k_b1 <- read_sf("data/pld_10k.shp")
pld_10k_b2 <- read_sf("data/pld_10k_b2.shp")

pld_already_run <- data.table::rbindlist(list(pld_100, pld_dozen, pld_1000, pld_10k_b1, pld_10k_b2))

pld_gt_1km2_wWRS_new <- pld_gt_1km2_wWRS[pld_gt_1km2_wWRS$lake_id %notin% unique(pld_already_run$lake_id),]

paths <- unique(pld_gt_1km2_wWRS_new$PATH)
path_lengths <- c()
for(i in 1:length(paths)){
  path_lengths[i] <- length(unique(pld_gt_1km2_wWRS_new[pld_gt_1km2_wWRS_new$PATH == paths[i],]$lake_id))
}

summary(path_lengths)
sort(paths)

#write_sf(pld_gt_1km2_wWRS_new[c("lake_id", "names", "PATH", "ROW")], "data/pld_gt_1km2_wWRS.shp", delete_layer = T) #accidentally has overlaps (this is what was run)

## need to re-write a portion of lakes into spherical geometry 

pld_wWRS <- read_sf("data/pld_gt_1km2_wWRS.shp")
pld_problem_paths <- pld_wWRS %>% dplyr::filter(PATH %in% 78:99)

# ran into antimeridian problems 
#EPSG:3832 (World Geodetic System 1984) or EPSG:3851 (Global Transverse Mercator)
#reprojected to sinusoidal offline in GIS 
pld_problem_paths_reproj <- st_transform(pld_problem_paths, crs = "EPSG:3832")
#write_sf(pld_problem_paths, "data/pld_problem_paths_78-99.shp", delete_layer = T)


#### get overlapping files for export (part 2 - overlapping lakes) ####
## ---------------------------------------

#set global options 
`%notin%` <- Negate(`%in%`)

pld_overlaps <- read_sf("data/shps/pld_gt1km_lsoverlaps.shp") #full overlaps shp!

#get lakes already run: 
pld_100 <- read_sf("data/pld_100_random_AR.shp")
pld_dozen <-  read_sf("data/pld_dozen.shp")
pld_1000 <- read_sf("data/pld_1000.shp")
pld_10k_b1 <- read_sf("data/pld_10k.shp")
pld_10k_b2 <- read_sf("data/pld_10k_b2.shp")

pld_already_run <- data.table::rbindlist(list(pld_100, pld_dozen, pld_1000, pld_10k_b1, pld_10k_b2))

pld_gt_1km2_overlaps_new <- pld_overlaps[pld_overlaps$lake_id %notin% unique(pld_already_run$lake_id),]

nNewOverlaps <- length(unique(pld_gt_1km2_overlaps_new$lake_id))


#--------write overlap batches that have not been run (batch 1 and 2):

#write_sf(pld_gt_1km2_overlaps_new[1:round(nNewOverlaps/2, 0),], "data/pld_overlaps_10k_b1.shp")
#write_sf(pld_gt_1km2_overlaps_new[(round(nNewOverlaps/2, 0)+1):nNewOverlaps,], "data/pld_overlaps_10k_b2.shp")

#### get already run lakes for MODIS re-run ####
##---------------------------------------------

pld_already_run_notOverlap <- pld_already_run[pld_already_run$lake_id %notin% pld_overlaps$lake_id,] %>% st_as_sf() #17,000 lakes 
pld_already_run_Overlap <- pld_already_run[pld_already_run$lake_id %in% pld_overlaps$lake_id,] %>% st_as_sf()

landsat_wrs <- read_sf("data/shps/WRS2_descending_0/WRS2_descending.shp")

sf_use_s2(FALSE)
pld_already_run_notOverlap_wWRS <- sf::st_join(x = pld_already_run_notOverlap, y = landsat_wrs, join = st_within, left = T)

write_sf(pld_already_run_notOverlap_wWRS, "data/inputsGEE/pld_already_run_notOverlap.shp", delete_layer = T)
#write_sf(pld_already_run_Overlap, "data/inputsGEE/pld_already_run_Overlap.shp", delete_layer = T)


# ----------------- MODIS re-runs by PATHxROW ----------------# 

# need to run MODIS re-runs by PATH x ROW for a) pld_xx_ lakes that ran on slow code & b) overlap lakes that were not already run
# '''need to have lake surface area, currently in ha 
# '''need to have PATH and ROW 

modis_already_run <- read_sf("data/inputsGEE/pld_gt_1km2_wWRS.shp") #129,674 lakes
modis_already_run_2 <- read_sf("data/inputsGEE/pld_overlaps_10k_b1.shp")
modis_already_run_lakes <- unique(modis_already_run$lake_id)
pld_gt_1km2 <- readRDS("data/shps/pld_gt_1km2_v2.rds") #168,808 lakes
modis_rerun_lakes <- pld_gt_1km2[pld_gt_1km2$lake_id %notin% modis_already_run_lakes,] #39,134 lakes

#fix area in AS from m to ha
AS <- read_sf("F:/pld2020/Originals/PLD_AS.shp")
AS <- AS[AS$max_area > 1e6,] #greater than 1km2 (100 ha)
AS_lakes <- unique(AS$lake_id)
#saveRDS(AS_lakes, "data/AS_lakes_sa_in_m.rds")

#add surface area of lakes with correction: 
# divide m2 by 10000 to get ha
pld_gt_1km2$max_area_corrected <- ifelse(pld_gt_1km2$lake_id %in% AS_lakes, pld_gt_1km2$max_area/10000, pld_gt_1km2$max_area)

modis_rerun_lakes_warea <- modis_rerun_lakes %>% dplyr::left_join(pld_gt_1km2 %>% st_drop_geometry())
colnames(modis_rerun_lakes_warea)


landsat_wrs <- read_sf("data/shps/WRS2_descending_0/WRS2_descending.shp")

sf_use_s2(FALSE)
modis_rerun_lakes_warea_wWRS <- sf::st_join(x = modis_rerun_lakes_warea, y = landsat_wrs, join = st_overlaps, left = T) #going to be many y to x 

modis_rerun_lakes_warea_wWRS_2 <- modis_rerun_lakes_warea_wWRS[modis_rerun_lakes_warea_wWRS$lake_id %notin% unique(modis_already_run_2$lake_id),] #forgot that batch lakes were run

#write_sf(modis_rerun_lakes_warea_wWRS, "data/inputsGEE/modis_reruns_4-11-2025.shp")
write_sf(modis_rerun_lakes_warea_wWRS_2, "data/inputsGEE/modis_reruns_4-11-2025.shp", delete_layer = T)

## create another modis re-runs for batch1 (did not have clouds)
modis_already_run_2 <- read_sf("data/inputsGEE/pld_overlaps_10k_b1.shp")
modis_already_run_lakes <- unique(modis_already_run_2$lake_id)
pld_gt_1km2 <- readRDS("data/shps/pld_gt_1km2_v2.rds") #168,808 lakes
modis_rerun_lakes <- pld_gt_1km2[pld_gt_1km2$lake_id %in% modis_already_run_lakes,] #10878 lakes

#fix area in AS from m to ha
AS_lakes <- readRDS("data/AS_lakes_sa_in_m.rds")


#add surface area of lakes with correction: 
# divide m2 by 10000 to get ha
pld_gt_1km2$max_area_corrected <- ifelse(pld_gt_1km2$lake_id %in% AS_lakes, pld_gt_1km2$max_area/10000, pld_gt_1km2$max_area)

modis_rerun_lakes_warea <- modis_rerun_lakes %>% dplyr::left_join(pld_gt_1km2 %>% st_drop_geometry())
colnames(modis_rerun_lakes_warea)

#load in updated overlaps lakes 
ls_full <- readRDS("scripts/ls_full_lakes.rds")
ls_partial <- readRDS("scripts/ls_full_lakes_manyPaths.rds")

ls_wrs <- data.table::rbindlist(list(ls_full, ls_partial), fill = T) %>% dplyr::select(lake_id, PATH, ROW)

modis_rerun_lakes_b2_wWRS <- dplyr::left_join(modis_rerun_lakes_warea, ls_wrs)

write_sf(modis_rerun_lakes_b2_wWRS, "data/inputsGEE/modis_reruns_4-25-2025.shp", delete_layer = T)

## ned

quantile(modis_rerun_lakes_warea$max_area_corrected, .98) #>10,000 ha 

max(paths, na.rm = T)

sum(is.na(modis_rerun_lakes_warea_wWRS_2$PATH)) #there's a lot of NA's 


#check if there are non-overlapping polygons that did not get run
#determined that all overlapping lakes that have not been run are in "previously run"
{
overlapping_lakes <- read_sf("data/shps/pld_gt1km_lsoverlaps.shp")
#lakes that were run that were already overlapping = 3726
sum(unique(modis_already_run$lake_id) %in% unique(overlapping_lakes$lake_id))
contained_lakes <- unique(pld_gt_1km2[pld_gt_1km2$lake_id %notin% unique(overlapping_lakes$lake_id),]$lake_id)

sum(unique(modis_already_run$lake_id) %in% contained_lakes) #125948 lakes run of all containted lakes 
sum(unique(modis_already_run$lake_id) %notin% contained_lakes) #the remaining lakes are overlapping lakes 

length(unique(modis_already_run$lake_id)) #129674
length(unique(contained_lakes)) #143,560

sum(contained_lakes %notin% unique(modis_already_run$lake_id)) #17,512 lakes have not already been run -- 
}

#### read in netcdf example merra-2 & get grids ####
## ------------------------------------------
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

list.files(pattern = "*.nc4", recursive = T)

nc_data <- nc_open(list.files(pattern = "*.nc4", recursive = T)[3])
# Save the print(nc) dump to a text file
{
  sink('merra2meta.txt')
  print(nc_data)
  sink()
}


temp.array <- ncvar_get(nc_data, "TSURF") # store the data in a 3-dimensional array
dim(temp.array) 

fillvalue <- ncatt_get(nc_data, "TSURF", "_FillValue")
fillvalue

temp.array[temp.array == fillvalue$value] <- NA
temp.slice <- temp.array[,, 1] #one hour, one level 
dim(temp.slice)

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)

r <- raster(t(temp.slice), xmn=-180, xmx=180, ymn=-90, ymx=90, crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')

#cuts=c(100,150,160,170,180,190,200) #set breaks
pal <- colorRampPalette(c("yellow","blue"))

plot(r, col = pal(7)) 

#writeRaster(r, "merra_example3.tif", overwrite = T) #this is the correct one

rpoints <- rasterToPoints(r, spatial = T)
rpoints_sf <- st_as_sf(rpoints)

rpolys<- rasterToPolygons(r, dissolve = F)
rpolys_sf <- st_as_sf(rpolys)

## add mg_id 
rpoints_sf$mg_id <- seq(1:nrow(rpoints_sf))
rpolys_sf$mg_id <- seq(1:nrow(rpolys_sf))

#write_sf(rpoints_sf, "data/shps/merra_center_frData.shp", delete_layer = T)
#write_sf(rpolys_sf, "data/shps/merra_grid_frData.shp", delete_layer = T)


#### join pld by rpolys to get merra-2 IDs ####
## --------------------------------------------
rpoints_sf <- read_sf("data/shps/merra_center_frData.shp")
rpolys_sf <- read_sf("data/shps/merra_grid_frData.shp")

pld_merra_1 <- st_join(pld_wWRS_centers, rpolys_sf, join = st_within) 
length(unique(sort(pld_merra_1$mg_id))) #this is pld in rpolys sf (get unique tiles)
#pld_merra_1 <- read_sf("data/shps/pld_wWRS_centers_wMerra2ids_1.shp")

pld_merra_2 <- st_join(pld_overlaps_centers, rpolys_sf, join = st_within)
pld_merra_3 <- st_join(pld_ar_centers, rpolys_sf, join = st_within)

all_mg_ids <- c(pld_merra_1$mg_id, pld_merra_2$mg_id, pld_merra_3$mg_id)

rpoints_sf_filtered <- rpoints_sf %>% dplyr::filter(mg_id %in% unique(all_mg_ids)) #this is just the unique tiles (e.g. run on GEE)

#write_sf(rpoints_sf_filtered, "data/shps/merra2_centers_wPLD.shp", delete_layer = T)
#write_sf(pld_merra_1, "data/shps/pld_wWRS_centers_wMerra2ids_1.shp", delete_layer = T)

pld_merra_all <- data.table::rbindlist(list(pld_merra_1, pld_merra_2, pld_merra_3), fill = T) %>% dplyr::select(lake_id, mg_id) %>% dplyr::distinct()
pld_merra_all 

#saveRDS(pld_merra_all, "data/pld_lake-id_merra-id.rds")
