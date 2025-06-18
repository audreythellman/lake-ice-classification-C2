# load packages
library(readr)
library(lubridate)
library(tidyverse)
library(sf)
library(glue)
library(data.table)

s1fol <- "step1LandsatMODIS"
`%notin%` <- Negate(`%in%`)

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
#cleaning the data 
mod_cols = grep(x = colnames(filtered_modis_list[[1]]), pattern = "gt", value = T)

#write temporary file
saveRDS(filtered_modis_list, "F:/LakeIce/temp/fl_md_ls_TEMP.rds")
#write modis paths already run 

#END OF SCRIPT
complete_paths <- c(basename(complete_paths), basename(modis_paths_notrun))
saveRDS(complete_paths, file = glue("data/{s1fol}/modis_files_run.rds"))

log_con <- file("data/logs/md_paths_thresholds.log")
cat(paste0("on ", Sys.Date(), " read in modis files from these file paths: "), file = log_con) 
cat(complete_paths, file = "data/logs/md_paths_thresholds.log", append = TRUE)