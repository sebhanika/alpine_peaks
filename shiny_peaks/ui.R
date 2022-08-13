

navbarPage("Alpine Peaks Endings",
                
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
                                data and the R code to recrate the project can be found here.")),
                     
                     wellPanel(
                       h4("Map Explanations"),
                       p("Below we can see the map in which we can dynamicaaly select the endings of mountain
                         names we want to visualize.")),
                     
                     
                      # sidebar with user input definitions
                      sidebarLayout(
                        # side bar panel with text selction
                        sidebarPanel(
                          
                          # Explanation text
                          
                          helpText("Here you can select multiple types of endings for mountain names.
               Hover across points andclick on points to get more information about the mountain"),
                          
                          # input
                          selectInput("end", 
                                      label = "Mountain name Ending",
                                      choices = as.list(endings), 
                                      selected = "-berg",
                                      multiple = T),
                          
                          # Legend Input
                          checkboxInput("legend", "Show legend", TRUE)
                          
                          
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
                     
                     # Plot explanations
                     wellPanel(
                       h4("Plot Explanations"),
                       p("Below we can see two plots regarding to see the data in a raw form. FIrst the total
                          counts by endings. We can adjust the elevation of the mountains by the slider panel. 
                          The second plot shows the histogram by ending and elevevaiton. It also reacts to the
                          slider panel.")),
                     
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


