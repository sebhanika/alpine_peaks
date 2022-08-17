
## ---------------------------
##
## Title: Global settings of Shiny app
##
## Topic: Shiny app about suffixes of mountain names
##
## Author: Sebastian Hanika
##
## Date Created: 2022-08-17
##
## ---------------------------


# Libraries ---------------------------------------------------------------

library(shiny)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(sf)
library(nationalparkcolors)


# Data and colors ---------------------------------------------------------

peaks <- readRDS("peaks_ends")
peaks$ele <- as.numeric(peaks$ele)

# tpyes of endings, sorted a-z
endings <- sort(unique(peaks$type_end_lab))

# countries sorted a-z
cntrs <- sort(unique(peaks$NAME_ENGL))

# colors based on the package 'nationalparkcolors' but adjusted for 
# more groups
custom_pal <- c("#FBE697", "#F3AE6D", "#8FC0CE", "#C9DACA", 
                "#14232A", "#E79498", "#1F304A", "#802729", "#8C9D57")


pal <- colorFactor(custom_pal, domain = endings)




