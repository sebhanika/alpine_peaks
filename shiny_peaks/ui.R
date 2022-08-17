
#Set User Interface

navbarPage("Alpine Peaks Endings",
           
           # Map Tab -------------------------------
           
           tabPanel("Map", fluidPage(theme = shinytheme("flatly")),
                    
                    # sidebar with user input definitions
                    sidebarLayout(
                      # side bar panel with text selction
                      sidebarPanel(
                        
                        # Explanation text
                        h4("Spatial distribution of mountain name endings"),
                        
                        p("Select up to nine different suffixes of German mountain names in the Alps."),
                        
                        p("Hover across points or click them to get more information about the mountain peak.", br(), ),
                        
                        p("Change the basemap if you need more contrast."),
                        
                        # input/select mountain ending
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
                        leafletOutput("map", width = "98.5%")
                        )
                    )
           ),
           
           
           # Plot tab -------------------------------
           
           tabPanel("Plots", fluidPage(theme = shinytheme("flatly")),
                    
                    # sidebar with user input definitions
                    sidebarLayout(
                      
                      # side bar panel with text selction
                      sidebarPanel(
                        # Explaination text
                        
                        h4("Elevation and Countries"),
                        p("Here we see the data in a different form. The first graph shows the total
                          counts by suffix. The second plot shows histograms of elevation by each ending. 
                          You can filter the data by countries and elevation."),
                        


                        # define slider
                        sliderInput("elev",
                                    "Select range of elevation in meter:", 
                                    min = 0,
                                    max = max(peaks$ele, na.rm = TRUE),
                                    value = c(0, max(peaks$ele, na.rm = TRUE)), sep=''),
                        
                        br(),
                        
                        selectInput("country", 
                                    label = "Select countries:",
                                    choices = c("ALL", cntrs), 
                                    selected = "ALL",
                                    multiple = T)
                      
                      ),
                      
                      # Main panel with plots
                      mainPanel(
                        
                        # plot 1 iwth total counts
                        fluidRow(plotOutput("plot1", width = "97%")),
                        # plot 2 with counts by ending
                        fluidRow(plotOutput("plot2", width = "97%"))
                        
                      )
                    )
           ),
           
           
           
           # About -------------------------------
           tabPanel("About", fluidPage(theme = shinytheme("flatly")),
                    
                    # Explanation about project
                    wellPanel(
                      h3("Project explanations"),
                      p("This project visualizes the spatial distribution of the most common
                        German endings of mountain names in the Alps. Many mountains share common
                        suffixes such as “-horn”, “-spitze” and “-kogel”, however are there any
                        regional differences? Since German is spoken throughout many regeions
                        of the alps, these distributions do not necessarily follow national borders.
                        However, obviously, most mountain peaks with German names can be found in
                        predominantly German speaking areas such as Austria, South-Tyrol,
                        Germany and parts of Switzerland."),
                      
                      p("The analysis is based on the names of mountains as reported in the OpenStreetMap
                        project and therefore might not include every regional variety or secondary summits.
                        Furthermore not all peaks had elevation data included, hence some peaks are missing in
                        the visualization in the plot tab. Additionally some other special names or name add-ons
                        might not be fully excluded from the dataset."),
                      
                      p("The R code to recreate the project can be found", 
                        a("here.", href="https://github.com/sebhanika/alpine_peaks", target="_blank")
                      )
                    )
                      
           ),
           # Footer -------------------------------
           hr(style = "border-color: #cbcbcb;"),
           fluidRow(
             column(9,
                    # data sources
                    p('Data sources: ', 
                      tags$a(href = "https://www.openstreetmap.org", 'Openstreetmap', 
                             target = '_blank'), ", ", 
                      tags$a(href = "https://www.eea.europa.eu", 'EEA', 
                             target = '_blank'), "and ",
                      tags$a(href = "https://ec.europa.eu/eurostat/", 'eurostat', 
                             target = '_blank'), ".", style = "font-size: 85%"),
                    
                    #github info
                    p("Created by Sebastian Hanika in August 2022", HTML("&bull;"),
                      "Find the code on Github:", 
                      tags$a(href = "https://github.com/sebhanika/alpine_peaks", 
                             icon(name = "github", class = 'fa fa-github', style = 'color:#5000a5'),
                             target = '_blank'), style = "font-size: 85%"),
                    
                    #contact info
                    p("Connect with me on Twitter", 
                      tags$a(href = "https://twitter.com/SebHanika", 
                             icon(name = "twitter", class = 'fa fa-twitter', style = 'color:#00acee'), 
                             target = '_blank'),
                      
                      HTML("&bull;"), "Or send an email ",
                      
                      tags$a(href = "mailto:hanikasebastian@gmail.com",
                             icon(name = "Env", class = 'fa fa-envelope', style = 'color:#516888'),
                             target = '_blank'), 
                      style = 'font-size:85%' ),
                    
                    
                    p(tags$em("Last changes: August 2022"), style = 'font-size:75%'),
                    
                    
                    ),
             
             windowTitle = "Mountain Names")
           
           # close navbarPage and UI
)


