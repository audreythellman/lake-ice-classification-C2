# load packages

list.of.packages <- c("readr", "lubridate", "tidyverse", "sf", "glue", "data.table", "gridExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# intall packages if not loaded 

library(readr)
library(lubridate)
library(tidyverse)
library(sf)
library(glue)
library(data.table)
library(gridExtra)

s1fol <- "step1LandsatMODIS"
`%notin%` <- Negate(`%in%`)


## start scripts 

#filtered_modis_list <- readRDS("F:/LakeIce/temp/fl_md_ls_TEMP.rds") #will take several mins to load 
filtered_modis_list <- readRDS("data/step1LandsatMODIS/fl_md_ls_TEMP2.rds") #will take several mins to load; filtered modis list has the data from GEE 

#save new vesion of temp 
# saveRDS(filtered_modis_list[c(1:8,18)], "data/step1LandsatMODIS/fl_md_ls_TEMP2.rds")

#cleaning the data 
mod_cols = grep(x = colnames(filtered_modis_list[[1]]), pattern = "gt", value = T)

## COMBINE LANDSAT AND MODIS
#load in landsat data: 

ls_merge <- lapply(list.files(path = glue("data/{s1fol}/"), 
                              pattern = "fullLakes", 
                              full.names = T), 
                   function(x){readRDS(x)})
#this is the Landsat data ready to merge 

ls_merge_df <- rbindlist(ls_merge)
mod_cols2 <- c("lake_id","date","propClear", mod_cols)

rm(ls_merge)

print("done with external drive")

md_thresholds_full <- list()

for(i in 1:length(filtered_modis_list)){ #length(filtered_modis_list)
  #i = 1
  tryCatch({
    #i = 3
    x = filtered_modis_list[[i]]
    md_merge_df <- setDT(x[,mod_cols2])
    ls_merge_smaller <- ls_merge_df[ls_merge_df$lake_id %in% 
                                      unique(md_merge_df$lake_id),]
    
    #do this by lake 
    #md_merge_split <- split(md_merge_df, f = md_merge_df$lake_id)
    md_thresholds <- list()
    unique_lakes <- unique(md_merge_df$lake_id)
    
    #print(paste0("just finished split @", Sys.time(), " : ", i, " for ", length(md_merge_split), " lakes"))
    print(paste0("starting calc @", Sys.time(), " : ", i, " for ", 
                 length(unique_lakes), " lakes"))
    
    
    #create empty plot vector 
    p <- list()
    
    for(j in 1:length(unique_lakes)){ #length(unique_lakes)
     tryCatch({
     #j = 20
      md = md_merge_df[md_merge_df$lake_id == unique_lakes[j], ]
      lake = as.character(md$lake_id[1])
      md$lake_id <- as.factor(md$lake_id)
      ls = ls_merge_df[ls_merge_df$lake_id == lake,]
      LanMOD <- merge(x = md, 
                      y = ls, 
                      all.x = TRUE)
      #filter out date mismatches & clouds > 70% in modis images
      LanMOD_filtered <- LanMOD[propClear > 0.7 & 
                                  !is.na(LANDSAT_SCENE_ID) & cloud < 0.3,] 
      #figure out how to melt 
      LanMOD_melt <- melt(LanMOD_filtered, measure.vars =  mod_cols) 
      colnames(LanMOD_melt)[which(colnames(LanMOD_melt) == "variable")] <-
        "threshold"
      colnames(LanMOD_melt)[which(colnames(LanMOD_melt) == "value")] <- 
        "mod_ice"
      
      LanMOD_summary <- LanMOD_melt %>%
        dplyr::mutate(diff = abs(RFSnowIce - mod_ice)) %>%
        group_by(lake_id, threshold) %>%
        summarise(dif = mean(diff, na.rm = T), 
                  sd =  sd(diff, na.rm = T),
                  n_obs = length(!is.na(diff)), 
                  n_ice = length(which(RFSnowIce > 0.8)), 
                  n_water = length(which(RFSnowIce < 0.2)), 
                  .groups = 'drop') 
      
      #thresholds
      md_thresholds[[lake]] <- LanMOD_summary %>% 
        filter(dif == min(dif, na.rm = T))
      
      #write table 
      suppressWarnings({
      if (file.exists("data/step2Thresholds/thresholds_v2.csv")){
        write.table( md_thresholds[[lake]], 
                     file = "data/step2Thresholds/thresholds_v2.csv", 
                     #change back to v1
                     append = T, sep = ",", 
                     quote = F, col.names = F, row.names = F)
      }else{
        file.create("data/step2Thresholds/thresholds_v2.csv")
        write.table( md_thresholds[[lake]], 
                     file = "data/step2Thresholds/thresholds_v2.csv", 
                     #change back to v1
                     append = T, sep = ",", 
                     quote = F, col.names = T, row.names = F)
      }})
      
      #threshold flag
      if(md_thresholds[[lake]]$n_ice[1] | md_thresholds[[lake]]$n_water[1] > 
         5){flagcolor="black"}else{flagcolor = "red"}
      
      #plot 
      max_threshold = max(LanMOD_summary$dif) + max(LanMOD_summary$sd) 
      min_threshold = min(LanMOD_summary$dif) - max(LanMOD_summary$sd)
      p[[lake]] <- ggplot() + 
        geom_point(data =  LanMOD_melt %>%
                     dplyr::mutate(diff = abs(RFSnowIce - mod_ice)), 
                   aes(x = threshold, y = diff), color = 'gray', alpha = 0.5) + 
        geom_errorbar(data = LanMOD_summary, 
                      aes(x = threshold, ymin = dif - sd, ymax = dif + sd), 
                      width = 0.25, color = "black") + 
        geom_point(data =  LanMOD_summary, 
                   aes(x = threshold, y = dif), color = 'red') + 
        geom_point(data =  md_thresholds[[lake]], 
                   aes(x = threshold, y = dif), shape = 1, size = 5) + 
        ylim(c(min_threshold, max_threshold)) + 
        theme_bw() + 
        labs(subtitle = paste0("pld_lake_id: ", lake)) + 
        theme(plot.subtitle=element_text(color=flagcolor)) + 
        scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE)]}, limits = rev)
      
      print(paste0(j, " :complete from split"))
      
     }, error = function(msg){
       message("Error for list member:",i, "-" ,j)}
     ) 
    }
    #export thresholds 
    md_thresholds_full[[i]] <- rbindlist(md_thresholds)
    
    #export plot 
    time = format(Sys.time(), "%Y%m%d%H%M%S")
    fname = glue("figs/thresholds_{time}_{i}.pdf")
    ggsave(
      filename = fname, #add version
      plot = marrangeGrob(p, nrow=5, ncol=5), 
      width = 12, height = 12
    )
    #save record of which lakes are where 
    if (file.exists("data/logs/thresholdplots.csv")){
      threshlog <- read_csv("data/logs/thresholdplots.csv", 
                            col_types = list("c", "c", "d", "f"))
      print("adding to file log new threshold plots")
      newthresholog <- data.frame(file = rep(fname,length(p)),
                                  lake_id = names(p),
                                  num = 1:length(p),
                                  page = gl(ceiling(length(p)/25),
                                            25,
                                            length = length(p)))
      threshlog <- full_join(threshlog, newthresholog)
    }else{
      threshlog <- data.frame(file = rep(fname,length(p)), 
                              lake_id = names(p), 
                              num = 1:length(p), 
                              page = gl(ceiling(length(p)/25),
                                        25, 
                                        length = length(p)))
      
         }
    write_csv(threshlog, "data/logs/thresholdplots.csv")
  }, error = function(msg){
    message("Error for list member:", i)}
  )
}

saveRDS(md_thresholds_full, "scripts/md_thresholds_TEMP.rds")