

navbarPage("Alpine Peaks Endings",
                
           
           # Map Tab -------------------------------
           
            tabPanel("Map", fluidPage(theme = shinytheme("flatly")),
                     

                      # sidebar with user input definitions
                      sidebarLayout(
                        # side bar panel with text selction
                        sidebarPanel(
                          
                          # Explanation text
                          
                          p("Here you can you can select multiple suffixes of mountain names.
                            Hover across points or click them to get more information about the mountain peak"),
                          
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
                          leafletOutput("map"),
                          p()
                          )
                        )
                      ),
            
           
            # Plot tab -------------------------------

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
                             value = c(0, max(peaks$ele, na.rm = TRUE)), sep=''),
                         
                         
                         selectInput("country", 
                                     label = "Country",
                                     choices = c("ALL", cntrs), 
                                     selected = NULL,
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
                           German suffixes of mountains in the Alps. Many mountains share common
                           suffixes such as “-horn”, “-spitze” and “-kogel”, however are there any
                           regional differences? Since German is spoken throughout many regeions
                           of the alps, these distrbtions do not follow national borders. However,
                           naturally most mountain peaks with German names can be found in
                           predominantly German speaking areas such as Austria, South-Tyrol,
                           Germany and parts of Switzerland."),
                         
                         h3("Methodology"),
                         p("The analysis is based on the names of mountains as reported in the OpenStreetMap
                           project and therefore might not include every regional variety or secondoary summits.
                           Furthermore not all peaks had elevation data included, hence some peaks are missing in
                           the visualization in the plot tab. 
                           The R code to recreate the project can be found", 
                           a("here.", href="https://github.com/sebhanika/alpine_peaks", target="_blank")
                           ),
                       
                       
                       
                       #link to website
                       p(a("Sebastian Hanika", href="https://github.com/sebhanika/alpine_peaks", 
                           target="_blank"), style = "font-size:25px"),
                       #email
                       p("e-mail: hanikasebastian@gmail.com",style = "font-size:20px"),
                       #github
                       
                       p(a("Link to Github Code", href="https://github.com/sebhanika/alpine_peaks", 
                           target="_blank"), style = "font-size:20px"),
                       #twitter
                       p(a("Connect at Twitter", href="https://github.com/sebhanika/alpine_peaks", 
                           target="_blank"), style = "font-size:25px")
                       
                  ),
  
          # Footer -------------------------------
          hr(style = "border-color: #cbcbcb;"),
          fluidRow(
            column(9,
                   p('All of the data used to generate this app were obtained from ', 
                     tags$a(href = "https://www.openstreetmap.org", 'Openstreetmap', 
                            target = '_blank'), '.', 
                     style = "font-size: 85%"),
                   
                   p("App created by Sebastian Hanika in January 2019", HTML("&bull;"),
                     "Find the code on Github:", tags$a(href = "https://github.com/sebhanika/alpine_peaks", "Github", target = '_blank'), style = "font-size: 85%"),
                   p("Have a question? Spot an error? Send an email ", tags$a(href = "mailto:hanikasebastian@gmail.com", "Mail", style = 'color:#990000', target = '_blank'), style = "font-size: 85%"),
                   p(tags$em("Last updated: August 2022"), style = 'font-size:75%')),
          windowTitle = "The Masters Data Viz" )
          
              )

# close navbarPage and UI
)


