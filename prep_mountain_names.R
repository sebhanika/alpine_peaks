
## ---------------------------
##
## Script name: Mountains
##
## Topic: Webmap German Mountain Names in the Alps
##
## Author: Sebastian Hanika
##
## Date Created: 12/08/2022
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


# Libraries and setup -----------------------------------------------------


#install.packages("osmdata")

library(tidyverse)
library(sf)
library(osmdata)
library(downloader)
library(leaflet)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# Load mountain areas -----------------------------------------------------

url_alps <- "https://www.eea.europa.eu/data-and-maps/data/european-mountain-areas/european-mountain-areas/european-mountain-areas/at_download/file"

download(url = url_alps, dest="mountain_areas.zip", mode="wb") # downloads zip folder into current directory
unzip("mountain_areas.zip", exdir = "data/mountain_areas") # unzips file into current working directory


alps.load <- read_sf(dsn = "C:/Users/hanik/Documents/Data_projects/Intern/misc/mountain_names/r_version/data/mountain_areas/m_massifs_v1.shp") %>% 
  filter(mm_code == 1)



alps.load <- read_sf(dsn ="data/mountain_areas/m_massifs_v1.shp") %>% 
  filter(mm_code == 1)  #filter only for alps

alps.reproj <- st_transform(alps.load, 4326)
alps <- st_make_valid(alps.reproj)

rm(alps.load, alps.reproj)
gc()

# Load OSM data -----------------------------------------------------------

# set extent of box
alp.ext <-  unname(st_bbox(alps, crs = st_crs(4326)))

# build query
osm.query <- opq(bbox = alp.ext,
                 nodes_only = TRUE,
                 timeout = 500) %>% 
  add_osm_feature(key = 'natural', value = 'peak') 


# call query, takes a long time
peaks.query <- osmdata_sf(osm.query)



# issue with OSM Encoding
encode_osm <- function(list){
  # For all data frames in query result
  for (df in (names(list)[map_lgl(list, is.data.frame)])) {
    last <- length(list[[df]])
    # For all columns except the last column ("geometry")
    for (col in names(list[[df]])[-last]){
      # Change the encoding to UTF8
      Encoding(list[[df]][[col]]) <- "UTF-8"
    }
  }
  return(list)
}

results_encoded <- encode_osm(peaks.query)

peaks.raw <- results_encoded$osm_points %>% 
  select(c(osm_id, name, geometry))


# filter only points in alps 
peaks.alps <- st_intersection(peaks.raw, alps)

rm(peaks.query)
gc()


# Clean name data ---------------------------------------------------------


# most typical endings
endings <- paste0(c("berg", "spitze", "spitz", "kogel", "horn", "joch", 
                    "stein", "eck", "ling", "stock", "kopf"), " ")

endings_filt <- paste(endings, collapse = "|")



peaks.ends <- peaks.alps %>% 
  mutate(name = str_replace_all(name, "\\-", " - "),
         name = str_replace_all(name, "\\/", " / "),
         name = str_replace_all(name, "  "," "),
         name = paste0(name, " ")) %>% #adding whitespace at the end
  mutate(type_end = str_extract(name, endings_filt)) %>%  #extract values if
  mutate(type_end = str_trim(if_else(type_end == "spitze ",
                                     "spitz ",
                                     type_end), "right"),
         type_end_lab = paste0("-", type_end),
         type_end_lab = ifelse(type_end_lab == "-spitz", "-spitz(e)", type_end_lab)) %>% 
  filter(type_end != is.na(type_end))



saveRDS(peaks.ends, "peaks_ends")

# testing stuff while building app, delete later --------------------------


peaks <- readRDS("peaks_ends")

peaks$ele <- as.numeric(peaks$ele)









