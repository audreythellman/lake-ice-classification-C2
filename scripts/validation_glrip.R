## validation 

#setup 
library(sf)
library(dplyr)
library(stringdist)
`%notin%` <- Negate(`%in%`)

#load data 

#GLRIP DATA
glrip_attr <- read.csv("data/validation/liag_physical_character_table.csv") %>% 
  replace(. == -999, NA) %>% tidyr::drop_na(lon_decimal, lat_decimal)
glrip_attr_sf <- st_as_sf(glrip_attr, coords = c("lon_decimal", "lat_decimal"), crs = 4326) %>% dplyr::filter(lakeorriver  == "L")


#PLD DATA

pld_gt1km2 <- readRDS("data/shps/pld_gt_1km2_v2.rds")

#JOIN GLRIP DATA TO PLD DATA
{
sf_use_s2(FALSE)

glrip_w_pldID <- st_join(glrip_attr_sf, pld_gt1km2, join = st_nearest_feature, left = TRUE)
lakenames <- glrip_w_pldID %>% select(names, lakename)
#check within 1000 m 
#glrip_w_pldID_withindist <- st_join(glrip_attr_sf, pld_gt1km2, join = st_is_within_distance, left = TRUE, dist = 1000)
}
saveRDS(glrip_w_pldID, "data/validation/glrip_w_pldID.rds")
glrip_w_pldID <- readRDS("data/validation/glrip_w_pldID.rds")

name_match <- function(a, b) {
  #a <- lakenames$names[18]
  #b <- lakenames$lakename[18]
  a_fmt <- unlist(strsplit(a, ";"))
  b_fmt <- unlist(strsplit(b, " "))
  a2 <- a_fmt[a_fmt != 'LAKE']
  b2 <- b_fmt[b_fmt != 'LAKE']
  
  test1 <- expand.grid(b2, a2)
  test2 <- expand.grid(b, a_fmt)
  test <- rbind(test1, test2)
  
  distances <- mapply(stringdist, a = test$Var1, b = test$Var2, method = "jw")
  any(distances <= 0.2) # 0 = exact match, 1 = complete mismatch
}

glrip_w_pldID$bool <- mapply(name_match, a = glrip_w_pldID$names, b = glrip_w_pldID$lakename)

true_matches <- glrip_w_pldID[which(glrip_w_pldID$bool == T),]

false_matches <- glrip_w_pldID[which(glrip_w_pldID$bool == F),]

library(measurements)
distance <- 1000
units(distance) <- "m"



saveRDS(true_matches, "data/validation/glrip_w_pldID_filtered.rds") #these are the true matches

