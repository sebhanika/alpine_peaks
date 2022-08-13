

library(shiny)
library(shinythemes)
library(leaflet)
library(tidyverse)


peaks <- readRDS("data/peaks_ends")
peaks$ele <- as.numeric(peaks$ele)

endings <- sort(unique(peaks$type_end_lab))

pal <- colorFactor(c("red", "blue", "green", "yellow", "grey",
                     "lightgreen", "black", "navy", "brown", "darkred"),
                   domain = endings)



ui <-navbarPage("Alpine Peaks Endings",
                
             # Tab with Map and explanations   
            tabPanel("Map", fluidPage(theme = shinytheme("flatly")),
                     
                     # Explanation about project
                     wellPanel(
                              h3("Project Explanations"),
                              p("This project visualizies the most common German endings of mountains
                                in the Alps. Since many regions of the alps are multilingual this does not
                                necessaryly follow national borders. However natrually most of the mountains with
                                German names can be found in predominatly German speaking areas such as Austria,
                                South-Tyrol, Germany and Switzerland. The analyis is based on Open Street Map 
                                data and the R code to recrate the project can be found here")),
                     
                     
                      # sidebar with user input definitions
                      sidebarLayout(
                        # side bar panel with text selction
                        sidebarPanel(
                          
                          
                          p("does this work?"),
                          br(),
                          
                          
                          # Explanation text
                          
                          helpText("Here you can select multiple types of endings for mountain names.
               Hover across points andclick on points to get more information about the mountain"),
                          
                          # input
                          selectInput("end", 
                                      label = "Mountain name Ending",
                                      choices = as.list(endings), 
                                      selected = "-berg",
                                      multiple = T)
                          
                          
                          ), 
                        
                        
                        # Main panel for displaying outputs
                        mainPanel(
                          leafletOutput("map"),
                          p()
                          )
                        )
                      ),
            
            # Tab with additonal plots
            tabPanel("Plot", fluidPage(theme = shinytheme("flatly")),
                     
                     # sidebar with user input definitions
                     sidebarLayout(
                       
                       # side bar panel with text selction
                       sidebarPanel(
                         # Explaination text
                         helpText("Here you can select the range of elevation in meters."),
                         
                         # define slider
                         sliderInput("elev",
                             "Elevation:", 
                             min = 0,
                             max = max(peaks$ele, na.rm = TRUE),
                             value = c(0, max(peaks$ele, na.rm = TRUE)), sep='')),
                       
                       # Main panel with plots
                       mainPanel(
                 
                 # plot 1 iwth total counts
                 fluidRow(plotOutput("plot1")),
                 # plot 2 with counts by ending
                 fluidRow(plotOutput("plot2"))
                 
                                )
                              )
                  )
  

# close navbarPage and UI
)





# define server function

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    
    # if nothing is selected because user deleted everything we display all points
    # not the initial setting however as it is cluttered
    if(is.null(input$end)){
      return(peaks)}
      
    # return selected input
    else 
      {return(peaks[peaks$type_end_lab == input$end,])
    }
  })

  # create Leaflet background map
  output$map <- renderLeaflet({
    leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
      fitBounds(4.7, 45, 17.0, 48.4) # preselects alpine areas
    })

  # add points based on filtered data by user
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(color = ~pal(type_end_lab),
                 popup = ~paste("<strong>", name,"</strong>", "</br>", 
                                "Elevation in m:", 
                                prettyNum(ele, big.mark = ",")), 
                 label = ~sapply(name, HTML))
      
  })
  
  
  
  
  ### - data for plot
  
  # Reactive expression for the data subsetted to what the user selected
  plotdata <- reactive({
    subset(peaks, peaks$ele %in% c(input$elev[1]:input$elev[2]))
    
  })
  
  
  output$plot1 <- renderPlot({
    
    plotdata() %>% 
      ggplot(aes(x = forcats::fct_infreq(type_end_lab))) +
      
      geom_bar(stat = "count", fill = "#2c3e50")+
      
      coord_flip()+
      
      theme_bw() +
      
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            panel.border = element_rect(colour = "grey30", fill=NA, size=0.5),
            strip.background =element_rect(fill="white"),
            strip.text = element_text(color = "grey30", size = 16),
            title = element_text(color = "grey30", size = 20)) +
      labs(title = "Most commom endings of Alpine Peaks in German",
           x = "Endings",
           y = "Count")
    })
  
  
  output$plot2 <- renderPlot({
    
    plotdata() %>% 
      ggplot(aes(x = ele, fill = type_end)) +
      
      geom_histogram()+
      
      facet_wrap(~type_end_lab, nrow = 2) +
      
      scale_x_continuous(breaks = seq(0, 4000, by = 1000),
                         labels = seq(0,4, by = 1)) +
      
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            panel.border = element_rect(colour = "grey30", fill=NA, size=0.5),
            axis.title = element_text(color = "grey30", size = 12),
            legend.position = "none",
            strip.background =element_rect(fill="white"),
            strip.text = element_text(color = "grey30", size = 14),
            title = element_text(color = "grey30", size = 20)) +
      labs(title = "Distribution of Alpine Peaks Elevation",
           x = "Elevation in 1000m",
           y = "Count")
  })
  
  
  
}

shinyApp(ui, server)