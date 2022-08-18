
#Set User Interface

bootstrapPage('',

  navbarPage(
    title = icon(name = "mountain", class = "fa-solid fa-mountain"),
    windowTitle = "ShinyPeaks",
    fluid = T,
    theme = shinytheme("flatly"), 
    position = "fixed-top",

             # Map Tab -------------------------------
             
             tabPanel("Map", 
                      titlePanel(h2("Alpine Peaks Endings", style = "margin-bottom: 30px;
                                                                     font-size: 36px")),                 
                     # Introduction text 
                      wellPanel(
                      
                     #h2("Alpine Peaks Endings"),
                     #br(),
                      
                      em(p("Have you ever wondered if German suffixes of mountain names have regional patterns across the Alps?"),
                      p("No?"),
                      p("Well, I guess it is a bit niche, nevertheless I hope you can enjoy this Shiny App!"),
                      style = "text-indent:25px; "),
                      br(),
                     
                      p("If you don't speak German, I assume you might be a bit confused right now (maybe even if you speak it).
                        In German many mountains share common suffixes such as “-horn”, “-spitze” and “-kogel”. 
                        While hiking, I recently wondered if there are any detectable regional differences between 
                        these names, especially since German dialects vary widely across the Alps. 
                        
                        If you want to find you more, you can explore the map below to see differences in the spatial 
                        distribution. Additional information about the data can be found on the",
                        em("Plots"), "tab.")
                      
                        ),
                      
                      # sidebar with user input definitions
                      sidebarLayout(
                        # side bar panel with text selection
                        
                        sidebarPanel(
                          
                          # Explanation text
                          h4("Interactive map of mountain names"),
                          
                          #Bulletpoints
                          tags$ul(
                            tags$li("Select suffixes (up to 9)."), 
                            tags$li("Hover or click on points."), 
                            tags$li("Zoom in or out for better overview."), 
                            tags$li("Change basemap for better contrast.")
                            ), 
                          
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
             
             tabPanel("Plots",
                      
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
             tabPanel("About",
                      
                      # Explanation about project
                      wellPanel(
                        h3("About this project"),
                        p("This project visualizes the spatial distribution of the most common
                          German endings of mountain names in the Alps. Many mountains share common
                          suffixes such as “-horn”, “-spitze” and “-kogel”, however are there any
                          regional differences? Since German is spoken throughout many regions
                          of the Alps, these distributions do not necessarily follow national borders.
                          However, obviously, most mountain peaks with German names can be found in
                          predominantly German speaking areas such as Austria, South-Tyrol,
                          Germany and parts of Switzerland."),
                        
                        p("The analysis is based on the names of mountains as reported in the OpenStreetMap
                          project and therefore might not include every regional variety or secondary summits.
                          Furthermore not all peaks had elevation data included, hence some peaks are missing in
                          the visualization in the plot tab. Additionally some other special names or name add-ons
                          might not have been correctly handled in the data preparation."),
                        
                        p("The R code to recreate the project can be found", 
                          a("here.", href="https://github.com/sebhanika/alpine_peaks", target="_blank")
                        )
                      )
                        
             ),
             # Footer -------------------------------
      footer = tags$div(
        class = "footer", 
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
                          target = '_blank'), ".", style = "font-size: 80%"),
                 
                 #github info
                 p("Created by Sebastian Hanika in August 2022", HTML("&bull;"),
                   "Find the code on Github:", 
                   tags$a(href = "https://github.com/sebhanika/alpine_peaks", 
                          icon(name = "github", class = 'fa fa-github', style = 'color:#5000a5'),
                          target = '_blank'), style = "font-size: 80%"),
                 
                 #contact info
                 p("Connect with me on Twitter", 
                   tags$a(href = "https://twitter.com/SebHanika", 
                          icon(name = "twitter", class = 'fa fa-twitter', style = 'color:#00acee'),
                               target = '_blank'),
                   HTML("&bull;"), "Or send an email ",
                   tags$a(href = "mailto:hanikasebastian@gmail.com",
                          icon(name = "envelope", class = 'fa-solid fa-envelope', style = 'color:#516888'),
                               target = '_blank'), 
                   style = 'font-size:80%' ),
                 
                 # Last changes
                 p(tags$em("Last changes: August 2022"), style = 'font-size:70%')
                 )
          ),
        style = "margin-left: 15px;"
      )
             # close navbarPage and UI
  ),

tags$style("* { font-family: Helvetica; font-size: 101%}"),
tags$style(type="text/css", "body {padding-top: 70px;}")
)


