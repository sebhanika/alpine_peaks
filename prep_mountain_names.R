
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

library(tidyverse)
library(sf)
library(geojsonsf)
library(osmdata)
library(downloader)
library(leaflet)
library(nationalparkcolors)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load country data -------------------------------------------------------

url_countries <- "https://gisco-services.ec.europa.eu/distribution/v2/countries/download/ref-countries-2020-10m.geojson.zip"
download(url = url_countries, dest="countries.zip", mode="wb") # downloads zip folder into current directory
unzip("countries.zip", exdir = "data/europe_countries", files = "CNTR_RG_10M_2020_4326.geojson") # unzips file into current working directory

# loads geojson into R
countries_europe <- geojson_sf("data/europe_countries/CNTR_RG_10M_2020_4326.geojson") %>% 
  filter(ISO3_CODE %in% c("AUT", "FRA", "ITA", "DEU", "SVN", "CHE", "LIE"))


# Load mountain areas -----------------------------------------------------

url_alps <- "https://www.eea.europa.eu/data-and-maps/data/european-mountain-areas/european-mountain-areas/european-mountain-areas/at_download/file"

download(url = url_alps, dest="mountain_areas.zip", mode="wb") # downloads zip folder into current directory
unzip("mountain_areas.zip", exdir = "data/mountain_areas") # unzips file into current working directory

# load area of alps
alps.load <- read_sf(dsn ="data/mountain_areas/m_massifs_v1.shp") %>% 
  filter(mm_code == 1)  #filter only for alps

# reproject data
alps.reproj <- st_transform(alps.load, 4326) 

# fix geometries
alps <- st_make_valid(alps.reproj) # f

#remove unnecessary objects
rm(alps.load, alps.reproj)


# Load OSM data -----------------------------------------------------------

# set extent of box
alp.ext <-  unname(st_bbox(alps, crs = st_crs(4326)))

# build query
osm.query <- opq(bbox = alp.ext,
                 nodes_only = TRUE,
                 timeout = 500) %>% 
  add_osm_feature(key = 'natural', value = 'peak') 


# call query. This can take a long time
peaks.query <- osmdata_sf(osm.query)

# fix issue with OSM Encoding, regarding Umlaute Ä. Ö. Ü
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

# apply encoding
results_encoded <- encode_osm(peaks.query)

# select only relevant data
peaks.raw <- results_encoded$osm_points %>% 
  select(c("osm_id", "name", "geometry", "ele"))

# filter only points in polygon of 'Alps'
peaks.alps.filt <- st_intersection(peaks.raw, alps)

# remove unneccesary object
rm(peaks.query)
gc()


# Spatial join of country attribute to each point
peaks.alps <- st_join(
  peaks.alps.filt,
  select(countries_europe, c(NAME_ENGL, ISO3_CODE)),
  join = st_intersects,
  left = TRUE,
  largest = FALSE
)



# Clean name data ---------------------------------------------------------

# most typical endings
endings <- paste0(c("berg", "spitze", "spitz", "kogel", "horn", "joch", 
                    "stein", "eck", "ling", "stock", "kopf"), " ")

endings_filt <- paste(endings, collapse = "|")

# clean data, name variable
peaks.ends <- peaks.alps %>% 
  select(-c(m_massive, name_mm, mm_code, area_km2)) %>% #drop unnecessary columns
  mutate(ele = as.numeric(ele)) %>% 
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



#saveRDS(peaks.ends, "peaks_ends")



# testing stuff while building app, delete later --------------------------


peaks <- readRDS("peaks_ends")

peaks$ele <- as.numeric(peaks$ele)



