

library(shiny)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(sf)
library(nationalparkcolors)


peaks <- readRDS("peaks_ends")
peaks$ele <- as.numeric(peaks$ele)

endings <- sort(unique(peaks$type_end_lab))

pal <- colorFactor(c(park_palette("GeneralGrant"), "#8C9D57", "#E79498"),
                   domain = endings)

palgg <- c(park_palette("GeneralGrant"), "#8C9D57", "#E79498")

cntrs <- sort(unique(peaks$NAME_ENGL))