

library(shiny)
library(leaflet)

# 
# 
# #loadRDS
peaks <- readRDS("data/temp_peaks_ends")

endings <- sort(unique(peaks$type_end_lab))

pal <- colorFactor(c("red", "blue", "green", "yellow", "grey",
                     "lightgreen", "black", "navy", "brown", "darkred"),
                   domain = endings)

labs <- as.list(peaks$name)



ui <- fluidPage(
  titlePanel("title panel"),
  
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
               information from the 2010 US Census."),
      
      selectInput("end", 
                  label = "Choose a variable to display",
                  choices = as.list(endings),
                  selected = "-berg", multiple = T)),
    
    mainPanel("Main",
              leafletOutput("map"),
              p())
  ),
  
)

server <- function(input, output, session) {
  
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    peaks[peaks$type_end_lab == input$end,]
  })

  output$map <- renderLeaflet({
    leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
      fitBounds(4.7, 45, 17.0, 48.4)
    })

  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(color = ~pal(type_end_lab),
                 popup = ~name, 
                 label = lapply(labs, HTML))
      
  })
  
}

shinyApp(ui, server)