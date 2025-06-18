# load packages
library(readr)
library(lubridate)
library(tidyverse)
library(sf)
library(glue)
library(data.table)

s1fol <- "step1LandsatMODIS"
`%notin%` <- Negate(`%in%`)

#### STEP 1A: LANDSAT DATA ####
# -------------------------- #

# download new landsat data 
library(googledrive)
nasa_photos <- drive_get(path = "NASA PHOTOS")$id
gdrive_files <- drive_ls(as_id(nasa_photos), pattern = "Landsat")$name
#all google drive files are already downloaded for Landsat 

complete_paths <- readRDS(glue("data/{s1fol}/landsat_files_run.rds"))
complete_paths



# load landsat data from external harddrive 
landsat_paths <- list.files(glue("F:/LakeIce/{s1fol}"), pattern = "Landsat", 
                            full.names = T)
# do not re-run old paths 
landsat_paths_notrun <- landsat_paths[which(basename(landsat_paths) %notin% 
                                              complete_paths)]

landsat_byLake <- grep(x = landsat_paths_notrun, pattern = "pld", value = T)
landsat_byPath <- grep(x = landsat_paths_notrun, pattern = "Path", value = T)

landsat_byLake_list <- lapply(landsat_byLake, read_csv)
landsat_byPath_list <- lapply(landsat_byPath, read_csv)

#make big dataframe with Landsat loads 
SLIDE_manylakes_list <- c(landsat_byPath_list, landsat_byLake_list)
SLIDE_manylakes_df <- data.table::rbindlist(SLIDE_manylakes_list, fill = T)   

#add PATH and ROW if there was not a path and row 
SLIDE_manylakes_df$WRS_PATH <- 
  ifelse(is.na(SLIDE_manylakes_df$WRS_PATH), 
         str_extract(SLIDE_manylakes_df$LANDSAT_PRODUCT_ID,
                     pattern = "[0-9]{6}") %>%
           substr(start = 1, stop = 3) %>%
           as.numeric(),
         SLIDE_manylakes_df$WRS_PATH)

SLIDE_manylakes_df$WRS_ROW <-
  ifelse(is.na(SLIDE_manylakes_df$WRS_ROW),
         str_extract(SLIDE_manylakes_df$LANDSAT_PRODUCT_ID, 
                     pattern = "[0-9]{6}") %>%
           substr(start = 4, stop = 6) %>%
           as.numeric(), 
         SLIDE_manylakes_df$WRS_ROW)

#figure out how many images per collection 
SLIDE_manylakes_df %>%
  group_by(lake_id) %>%
  summarise(n = length(LANDSAT_PRODUCT_ID)) %>%
  summarise(mean = mean(n), 
            max = max(n), 
            min = min(n), 
            med = median(n)) #1-1,767 images per lake; most ~300 images 

#reformat data
SLIDE_manylakes <- SLIDE_manylakes_df %>% 
  select(-`system:index`) %>%
  rename(time_unix_ms = `system:time_start`) #index_landsat = `system:index`,
rm(SLIDE_manylakes_df)
SLIDE_manylakes$DOY <- yday(SLIDE_manylakes$date)
SLIDE_manylakes$satellite <- substr(SLIDE_manylakes$LANDSAT_SCENE_ID,start = 1, stop = 3)

#filter data by row x path based on overlaps, divide, and put back together 
#note: not getting thresholds for problem lakes (lakes with no full images )
overlap_log <- read_csv("data/logs/overlap_log.csv") %>% 
  select(lake_id, overlap_type) #read in current overlap log 

#read in files that tell you which row x path to use 
overlap_pathxrow_filtered <- data.table::rbindlist(lapply(c("scripts/ls_full_lakes_manyPaths.rds", "scripts/ls_full_lakes.rds"), readRDS), fill = T)

## No overlaps; remove date duplicates 
## -----------------------------------
SLIDE_manylakes_none <- SLIDE_manylakes[SLIDE_manylakes$lake_id %in% overlap_log[overlap_log$overlap_type == "full_none",]$lake_id]

dups <- setDT(SLIDE_manylakes_none)[,.(n_dups = .N), 
              by = .(lake_id, satellite, date)] %>% filter(n_dups >1)
true_duplicates <- unique(dups$lake_id)

## view some lakes to spot check (no overlapping outlines)
{

landsat_wrs <- read_sf("data/shps/WRS2_descending_0/WRS2_descending.shp")
pld_gt_1km2 <- readRDS("data/shps/pld_gt_1km2_v2.rds") 

pdf('VIEW_true_dups.pdf', height = 10, width = 10)
par(mfrow = c(3, 3))  ## set the layout to be 3 by 3
n_lakes <- length(true_duplicates)


lapply(true_duplicates[sample(x = 1:n_lakes,size = 90,replace = F)], function(x){
  #x = 7210980652 
  lake = pld_gt_1km2[pld_gt_1km2$lake_id == x,]
  #area = round(conv_unit(lake$max_area_corrected[1], "hectare", "km2"),1)
  plot(st_geometry(lake), col = "blue", bg = "gray")
  plot(st_geometry(landsat_wrs), add = T)
  sp::degAxis(side = 1)
  sp::degAxis(side = 2,las = 2)
  #polygonsLabel(landsat_wrs, landsat_wrs$PATH)
  title(lake$names[1], sub = paste0(lake$lake_id))
})
dev.off()
}

dups_filtered <- 
  setDT(SLIDE_manylakes_none[SLIDE_manylakes_none$lake_id %in% 
                               true_duplicates,])[, .SD[which.max(WRS_ROW)],
                                                  by = .(lake_id, 
                                                         satellite, 
                                                         date)]

#there are no more duplicates 
dups_check <- setDT(dups_filtered)[,.(n_dups = .N), 
                                    by = .(lake_id, satellite, date)] %>% 
  filter(n_dups >1)
SLIDE_manylakes_none_filtered <- 
    data.table::rbindlist(list(
      SLIDE_manylakes_none[SLIDE_manylakes_none$lake_id %notin% true_duplicates,],
      dups_filtered),
      use.names = T, fill = T)


## from overlaps; use only approved row and path 
## ---------------------------------------------

#filter slide many lakes by full lakes with horizontal or vertical duplicates 
SLIDE_manylakes_full_hv <- SLIDE_manylakes[SLIDE_manylakes$lake_id %in% overlap_log[overlap_log$overlap_type %in% c("full_horizontal", "full_vertical"),]$lake_id,]
SLIDE_manylakes_full_hv$lake_id <- as.factor(SLIDE_manylakes_full_hv$lake_id)

overlap_pathxrow_filtered2 <- overlap_pathxrow_filtered %>% 
  select(lake_id, PATH, ROW) %>% 
  rename(WRS_PATH=PATH, WRS_ROW=ROW)

overlap_pathxrow_filtered2$lake_id <- as.factor(overlap_pathxrow_filtered2$lake_id)

SLIDE_manylakes_full_hv_filtered <- merge.data.table(x = setDT(SLIDE_manylakes_full_hv), y = setDT(overlap_pathxrow_filtered2), all.y = T) %>% distinct()

nrow(SLIDE_manylakes_full_hv)
nrow(SLIDE_manylakes_full_hv_filtered)

#there are true duplicates (e.g. the same value for the same scene_id)
dups_check <- setDT(SLIDE_manylakes_full_hv_filtered)[,.(n_dups = .N), 
                                   by = .(lake_id, satellite, date)] %>% filter(n_dups >1)

SLIDE_manylakes_full_hv_filtered2 <- SLIDE_manylakes_full_hv_filtered %>% 
  group_by(LANDSAT_SCENE_ID) %>% 
  slice_sample(n = 1) #this picks one from each scene 

dups_check <- 
  setDT(SLIDE_manylakes_full_hv_filtered2)[,.(n_dups = .N),
                                           by = .(lake_id, satellite, date)] %>% 
  filter(n_dups >1)

#now that we have eliminated all duplicates from overlap lakes and from non-overlap lakes 
SLIDE_manylakes_full <- 
  data.table::rbindlist(list(SLIDE_manylakes_full_hv_filtered2,
                             SLIDE_manylakes_none_filtered), 
                        use.names = T, fill = T) 

#save file 
if (file.exists(glue("data/{s1fol}/SLIDE_fullLakes_v{Sys.Date()}"))){
  warning("file already exists with this name, you will need to change the filepath to save")
} else {
  saveRDS(SLIDE_manylakes_full, glue("data/{s1fol}/SLIDE_fullLakes_v{Sys.Date()}"))
}

## view 5 random lakes to spot check 
n_lakes <- length(unique(SLIDE_manylakes_full$lake_id)) #get n lakes
ggplot(data = SLIDE_manylakes_full[SLIDE_manylakes_full$lake_id %in% unique(SLIDE_manylakes_full$lake_id)[sample(x = 1:n_lakes,size = 5,replace = F)],]) + geom_point(aes(x = date, y = RFSnowIce, color = satellite)) + facet_wrap(~lake_id, ncol = 1) #view 5 

## write log files 
## ---------------

# id lakes that do not get ice cover or do not thaw
#two flags, no_ice, only_ice >gt 5 observations >75% ice, < 25% no ice 
#write_csv(append[1:10,], "ice_free_ice_only_log.csv") #creates first log

ice_log <- read_csv("data/logs/ice_free_ice_only_log.csv") #current log
ice_log$lake_id <- as.factor(ice_log$lake_id)

append <- SLIDE_manylakes_full %>% #to add 
  group_by(lake_id) %>% 
  summarise(n_ice = sum(RFSnowIce >= .80, na.rm=TRUE), 
            n_iceFree = sum(RFSnowIce <= .20, na.rm = TRUE)) %>%
  filter(n_ice < 5| n_iceFree < 5) %>%
  mutate(flag = ifelse(n_ice < 5, "no_ice", "only_ice")) %>% select(lake_id, flag)

temp <- full_join(x = ice_log, append) #temp join

#decides whether to append
if(nrow(ice_log) < nrow(temp)){
  write_csv(temp, "data/logs/ice_free_ice_only_log.csv")
  rm(temp)
} else{
  warning("writing this new csv results in a smaller ice log")
}

rm(ice_log, append)

# lake log (e.g which lakes are in what file)
lake_log <- data.frame(lake_id= as.factor(unique(SLIDE_manylakes$lake_id)), 
                       date_processed = Sys.Date())

lake_log$status <- ifelse(lake_log$lake_id %in% unique(SLIDE_manylakes_full$lake_id), 
                          "compiled", "not_compiled")

lake_log[lake_log$status == "not_compiled",]$lake_id %in% overlap_log[overlap_log$overlap_type == "partial",]$lake_id

if (file.exists(glue("data/logs/ls_s1_lake_log.csv"))){
  old_file <- read_csv(glue("data/logs/ls_s1_lake_log.csv"))
  new_file <- full_join(old_file, lake_log)
  write.csv(new_file, "data/logs/ls_s1_lake_log.csv")
  print("wrote new file")
} else {
  write.csv(lake_log, "data/logs/ls_s1_lake_log.csv")
  print("saved new file")
}


#end of script 
complete_paths <- c(basename(complete_paths), basename(landsat_paths_notrun))
saveRDS(complete_paths, file = glue("data/{s1fol}/landsat_files_run.rds"))

log_con <- file("data/logs/ls_paths_thresholds.log")
cat(paste0("on ", Sys.Date(), " read in ls files from these file paths: "), file = log_con) 
cat(complete_paths, file = "data/logs/ls_paths_thresholds.log", append = TRUE)


#write_csv(dups, "data/logs/two_rows_duplicates.csv")

# ------------------------------------------------------------------------------

#### STEP 1B: MODIS DATA ####
# -------------------------- #

#the following is also in threshold_modis_read_files_loop: 
{

modis_paths <- list.files(glue("F:/LakeIce/{s1fol}"), pattern = "MODIS", full.names = T)

complete_paths <- readRDS(glue("data/{s1fol}/modis_files_run.rds"))
modis_paths_notrun <- modis_paths[which(basename(modis_paths) %notin% complete_paths)]

modis_byLake <- grep(x = modis_paths_notrun, pattern = "pld", value = T)
modis_byPath <- grep(x = modis_paths_notrun, pattern = "Path", value = T)


modis_byLake_list <- lapply(modis_byLake, read_csv)
modis_byPath_list <- lapply(modis_byPath, read_csv)

modis_file_list <- c(modis_byLake_list, modis_byPath_list)

filtered_modis_list <- list()
for(i in 1:length(modis_file_list)){
  c <- colnames(modis_file_list[[i]])
  id <- paste0("l",i)
  if("propClear" %in% c){
    filtered_modis_list[[id]] <- modis_file_list[[i]]
  }else{
    print(paste0(i, " was skipped"))
  }
}

#modis_manylakes_df <- data.table::rbindlist(filtered_modis_list, fill = T, 
#                                            use.names = T)  
rm(modis_byLake_list, modis_byPath_list) 
rm(modis_file_list)
#how many lakes 
lakes <- lapply(filtered_modis_list, function(x) {unique(x$lake_id)})
length(unique(unlist(lakes))) #129389 lakes 

#saveRDS(filtered_modis_list, "F:/LakeIce/temp/fl_md_ls_TEMP.rds")

}

#### STEP 1c: COMBINE DATA ####
# -------------------------- #

## combining MODIS and Landsat data for many lakes: 
## filter out lakes that have no ice based on Landsat 

ice_log <- read_csv("data/logs/ice_free_ice_only_log.csv")
lakes_no_ice <- ice_log[ice_log$flag == "no_ice",]$lake_id #check unique here too 
length(unique(ice_log$lake_id)) #23,000 lakes have no ice or only ice (only lakes with flags are in ice_log)

# here replaced by threshold_modis_get_thresholds_loop.R
######################################################
# creates plots & writes log file for plot, saves rds file with threshold, but also a csv 
{

filtered_modis_list <- readRDS("F:/LakeIce/temp/fl_md_ls_TEMP.rds") #will take several mins to load 
#cleaning the data 
mod_cols = grep(x = colnames(filtered_modis_list[[1]]), pattern = "gt", value = T)

## COMBINE LANDSAT AND MODIS
#load in landsat data: 

ls_merge <- lapply(list.files(path = glue("data/{s1fol}/"), 
                              pattern = "fullLakes", 
                              full.names = T), 
                   function(x){readRDS(x)})

ls_merge_df <- rbindlist(ls_merge)
mod_cols2 <- c("lake_id","date","propClear", mod_cols)

rm(ls_merge)

nrow(filtered_modis_list[[2]])
md_thresholds_full <- list()
p <- list()

for(i in 1:length(filtered_modis_list)){
  x = filtered_modis_list[[i]]
  md_merge_df <- setDT(x[,mod_cols2])
  ls_merge_smaller <- ls_merge_df[ls_merge_df$lake_id %in% unique(md_merge_df$lake_id),]
  
  #do this by lake 
  md_merge_split <- split(md_merge_df, f = md_merge_df$lake_id)
  md_thresholds <- list()
  
  for(i in 1:length(md_merge_split)){
    i = 1
    md = md_merge_split[[i]]
    lake = as.character(md$lake_id[i])
    md$lake_id <- as.factor(md$lake_id)
    ls = ls_merge_df[ls_merge_df$lake_id == lake,]
    LanMOD <- merge(x = md, 
                    y = ls, 
                    all.x = TRUE)
    #filter out date mismatches & clouds > 70% in modis images
    LanMOD_filtered <- LanMOD[propClear > 0.7 & !is.na(LANDSAT_SCENE_ID) & cloud < 0.3,] 
    #figure out how to melt 
    LanMOD_melt <- melt(LanMOD_filtered, measure.vars =  mod_cols) 
    colnames(LanMOD_melt)[which(colnames(LanMOD_melt) == "variable")] <- "threshold"
    colnames(LanMOD_melt)[which(colnames(LanMOD_melt) == "value")] <- "mod_ice"
    
    LanMOD_summary <- LanMOD_melt %>%
      dplyr::mutate(diff = abs(RFSnowIce - mod_ice)) %>%
      group_by(threshold) %>%
      summarise(dif = mean(diff, na.rm = T), 
                sd =  sd(diff, na.rm = T),
                n_obs = length(!is.na(diff)), 
                n_ice = length(which(RFSnowIce > 0.8)), 
                n_water = length(which(RFSnowIce < 0.2))) 
    
    #thresholds
    md_thresholds[[lake]] <- LanMOD_summary %>% 
      filter(dif == min(dif, na.rm = T))
    
    #plot 
    max_threshold = max(LanMOD_summary$sd) 
    min_threshold = min(LanMOD_summary$dif) - max_threshold
    p[[lake]] <- ggplot() + 
      geom_point(data =  LanMOD_melt %>%
                   dplyr::mutate(diff = abs(RFSnowIce - mod_ice)), 
                 aes(x = threshold, y = diff), color = 'gray', alpha = 0.5) + 
      geom_errorbar(data = LanMOD_summary, 
                    aes(x = threshold, ymin = dif - 0, ymax = dif + sd), width = 0.25, color = "black") + 
      geom_point(data =  LanMOD_summary, 
                 aes(x = threshold, y = dif), color = 'red') + 
      geom_point(data =  md_thresholds[[lake]], 
                 aes(x = threshold, y = dif), shape = 1, size = 5) + 
      ylim(c(0, max_threshold)) + 
      theme_bw() + 
      labs(subtitle = paste0("pld_lake_id: ", lake)) + 
      scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE)]})
    
  }
  #export thresholds 
  md_thresholds_full[[i]] <- rbindlist(md_thresholds)
  
  }
  
saveRDS("scripts/md_thresholds_TEMP.rds")
}
###

## SANDBOX! 
{

#this creates threshold plot
LanMODsummary <- pivot_longer(LanMOD, cols = starts_with("gt"), names_to = "mod", values_to = "ice") %>%
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
}