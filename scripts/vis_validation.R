# plot validation 

library(readr)
library(ggplot2)
library(dplyr)
library(sf)
library(lubridate)
library(glue)

glrip_matches <- read_rds("data/validation/glrip_w_pldID_filtered.rds")
colnames(glrip_matches)
glrip_data <- read_csv("data/validation/liag_freeze_thaw_table.csv") %>% left_join(glrip_matches %>% select(lake_id, lakecode))
colnames(glrip_data)

#load in dataset with pld < 1km2 in area 
pld_gt_1km2 <- readRDS("data/shps/pld_gt_1km2_v2.rds")
pld_gt_1km2$lake_id <- as.numeric(pld_gt_1km2$lake_id)

## load in ts from glrip 

glrip_ts_0531 <- read_csv("data/step3ts/glrip_ts_0531.csv")
for(i in 1:24) {
  id = unique(glrip_ts_0531$lake_id)[i]
  gl = filter(glrip_data, lake_id == id)
  print(max(gl$iceoff_year))
}
gl


## make a plot with the data
p <- lapply(1:24, function(i) {

  id = unique(glrip_ts_0531$lake_id)[i]
  ts = glrip_ts_0531[glrip_ts_0531$lake_id == id, ]
  gl = filter(glrip_data, lake_id == id) 
  gl$iceoff_dt <- mdy(glue("{gl$iceoff_month}-{gl$iceoff_day}-{gl$iceoff_year}"))
  gl$iceon_dt <- mdy(glue("{gl$iceon_month}-{gl$iceon_day}-{gl$iceon_year}"))
  gl$season_dt <- as.numeric(stringr::str_extract(gl$season, pattern = "[0-9]{4}"))
  
  ggplot() + 
    geom_area(data = ts, aes(x = date, y = IceFracMOD), fill = "gray", alpha = 0.5) + 
    geom_point(data = filter(gl, gl$season_dt >= 1999), aes(x = iceoff_dt, y = 0), 
               color = "blue") + 
    facet_wrap(~lake_id) + theme_bw()

})
library(gridExtra)


ggsave(
  filename = glue("figs/init_glrip_0604.pdf"), #add version
  plot = marrangeGrob(p, nrow=3, ncol=1), 
  width = 12, height = 6
)

dev.off()

p <- lapply(1:24, function(i) {
  ## visualizing only breakup 
  #i = 6
  id = unique(glrip_ts_0531$lake_id)[i]
  ts = glrip_ts_0531[glrip_ts_0531$lake_id == id, ]
  gl = filter(glrip_data, lake_id == id) 
  
  #update data formats
  ts$season_dt <- year(ts$date)
  ts$yday <- yday(ts$date)
  
  gl$iceoff_dt <- yday(mdy(glue("{gl$iceoff_month}-{gl$iceoff_day}-{gl$iceoff_year}")))
  gl$iceon_dt <- yday(mdy(glue("{gl$iceon_month}-{gl$iceon_day}-{gl$iceon_year}")))
  gl$season_dt <- as.numeric(stringr::str_extract(gl$season, pattern = "[0-9]{4}")) + 1
  gl$tf_start <- gl$iceoff_dt - 15
  gl$tf_end <- gl$iceoff_dt + 15
  
  #plot properties 
  end = max(gl$tf_end)
  start = min(gl$tf_start)
  
  
  ggplot() + 
    geom_area(data = ts, aes(x = yday, y = IceFracMOD), fill = "gray", alpha = 0.5) + 
    geom_point(data = filter(gl, gl$season_dt >= 2000), aes(x = iceoff_dt, y = 0), 
               color = "blue") + 
    lims(x = c(start,end)) + 
    labs(title = id) + 
    facet_wrap(~season_dt) + theme_bw()
})

ggsave(
  filename = glue("figs/zoomed_glrip_0604.pdf"), #add version
  plot = marrangeGrob(p, nrow=1, ncol=1), 
  width = 12, height = 12
)


## make a plot with a 1:1 line 
