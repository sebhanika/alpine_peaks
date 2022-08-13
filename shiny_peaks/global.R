

library(shiny)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(sf)


peaks <- readRDS("peaks_ends")
peaks$ele <- as.numeric(peaks$ele)

endings <- sort(unique(peaks$type_end_lab))

pal <- colorFactor(c("red", "blue", "green", "yellow", "grey",
                     "lightgreen", "black", "navy", "brown", "darkred"),
                   domain = endings)

