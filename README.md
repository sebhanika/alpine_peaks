# alpine_peaks

This is the repo for my first shiny app. Find the final result at https://sebhanika.shinyapps.io/shiny_peaks/

This project visualizes the spatial distribution of the most common German endings of mountain names in the Alps. Many mountains share common suffixes such as “-horn”, “-spitze” and “-kogel”, however are there any regional differences? Since German is spoken throughout many regions of the Alps, these distributions do not necessarily follow national borders. However, obviously, most mountain peaks with German names can be found in predominantly German speaking areas such as Austria, South-Tyrol, Germany and parts of Switzerland.

The analysis is based on the names of mountains as reported in the OpenStreetMap project and therefore might not include every regional variety or secondary summits. Furthermore not all peaks had elevation data included, hence some peaks are missing in the visualization in the plot tab. Additionally some other special names or name add-ons might not have been correctly handled in the data preparation.

The script 'prep_mountain_names.R' includes the data preparation and should also download/query all data requiered to reproduce the project.
