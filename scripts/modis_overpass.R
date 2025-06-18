## get modis approximate overpass times: 


#this is how to do it 

library(httr)
library(XML)
library(glue)
library(stringr)
library(rvest)

#this accesses modis geoMetadata 
################
yr = "2000"
#https://ladsweb.modaps.eosdis.nasa.gov/opendap/RemoteResources/laads/geoMeta/61/TERRA/2000/MOD03_2000-02-23.txt

mod_yrs <- as.character(seq(from = 2000, to = 2025, by =1))

for(j in 3:length(mod_yrs)){
  yr = mod_yrs[j]
  url <- glue(
    "https://ladsweb.modaps.eosdis.nasa.gov/opendap/RemoteResources/laads/geoMeta/61/TERRA/{yr}/contents.html") # Replace with the URL you want to scrape
  webpage <- read_html(url)
  links <- webpage %>% html_nodes("a") %>% html_attr("href") #get links to txts 
  detail_links <- grep(links, pattern = "MOD03", value = T) #get just the metadata files
    
  times_list <- list()
  #make the length list 
  
  #one year takes about two minutes
  for(i in 1:length(detail_links)){
  
    header <- glue("https://ladsweb.modaps.eosdis.nasa.gov/opendap/RemoteResources/laads/geoMeta/61/TERRA/{yr}/")
    
    onelink <- glue('{header}{detail_links[i]}')
   
    file <- content(GET(onelink), encoding = "UTF-8")
    lines <- strsplit(file, "\n")[[1]]
      
    # Separate metadata and data
    #write if lines are >0 
    metadata <- grep("^#", lines, value = TRUE)
    colnames <- unlist(strsplit(gsub("# ", "",  metadata[3]), ","))
    
    data_lines <- grep("^[^#]", lines, value = TRUE)
    #write csv
    tempfile <- tempfile()
    writeLines(data_lines, tempfile)
    
    times_list[[i]] <- read.csv(tempfile, header = F)
    colnames(times_list[[i]]) <- colnames
    
    print(glue('{detail_links[i]} completed at {Sys.time()}'))
  }
  
  write_rds(times_list, glue("data/overpassTimes/overpass_{yr}.rds"))
}




#this script processes the metadata file: 
#do this for another date: 
modis_meta <- read_csv(file = "C:/Users/athellma/Downloads/MOD03_2002-10-08.txt", skip = 2)

gringlong <- grep(colnames(modis_meta), pattern = "GRingLongitude", value = T)
gringlat <- grep(colnames(modis_meta), pattern = "GRingLatitude", value = T)
remove_coord <- grep(colnames(modis_meta), pattern = "Coord", value = T)

modis_lat <- modis_meta %>% 
  select(!all_of(c(gringlong, remove_coord))) %>% #remove longitude
  tidyr::pivot_longer(gringlat, names_to = "rep", values_to = "lat") %>%
  mutate(rep = stringr::str_extract(rep, pattern = "[0-9]{1}")) 

modis_lon <- modis_meta %>% 
  select(!all_of(c(gringlat, remove_coord))) %>% #remove longitude
  tidyr::pivot_longer(gringlong, names_to = "rep", values_to = "lon") %>%
  mutate(rep = stringr::str_extract(rep, pattern = "[0-9]{1}")) 

modis_sf <- st_as_sf(full_join(modis_lon, modis_lat), coords = c("lon", "lat"), crs = 4326)

crs(modis_sf)
write_sf(modis_sf, "data/shps/modis_overpass_2002281.gpkg", delete_layer = T)

## this function clicks on the link on a page and downloads the html text on it
##############

## write function 
# yr <- "2000"
# doy <- "055"
# 
# get_times_for_1day <- function(doy) {
#   url <- glue(
#     "https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/61/MOD09GQ/{yr}/{doy}") # Replace with the URL you want to scrape
#   webpage <- read_html(url)
#   links <- webpage %>% html_nodes("a") %>% html_attr("href")
#   detail_links <- grep(links, pattern = "/details/file/61/", value = T)
#   
#   times_list <- list()
#   #make the length list 
#   #each day should take 10 minutes 
#   for(i in 100:110){
#     df2 <- 
#       readHTMLTable(content(GET(glue(
#         'https://ladsweb.modaps.eosdis.nasa.gov{detail_links[i]}'
#       )), "text"))[[1]]
#     newnames <- df2$Layers
#     names(df2) <- NULL
#     df3 <- as.data.frame(t(df2[-1]))
#     colnames(df3) <- newnames
#     times_list[[i]] <- df3
#     #print(Sys.time())
#   }
#   perday_time_df <- data.table::rbindlist(times_list)
#   return(perday_time_df)
#   
# }

