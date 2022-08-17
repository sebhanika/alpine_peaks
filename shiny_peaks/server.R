
# define server function

function(input, output, session) {
  

  # Map -----------------------------------------------------------

  
  # Reactive expression for the data subsetted by selectedInput
  filteredData <- reactive({
    
    # if nothing is selected because user deleted everything, all points are displayed
    if(is.null(input$end)){
      return(peaks)
      }
      
    # return selected input
    else {
      return(peaks[peaks$type_end_lab == input$end,])
      }
    
  })

  # create Leaflet background map
  output$map <- renderLeaflet({
    leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
      fitBounds(6, 46.5, 17.5, 47.5) %>%  # zooms into alpine areas
      addProviderTiles(providers$CartoDB.Positron, 
                       group = "Basemap - greyscale") %>% #adding basemaps
      addProviderTiles(providers$CartoDB.DarkMatter, 
                       group = "Basemap - dark") %>%
      addLayersControl(
        baseGroups = c("Basemap - greyscale", "Basemap - dark"), # adding control for base maps
        options = layersControlOptions(collapsed = TRUE))
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
  
  
    # Create reactive legend
    observe({
      proxy <- leafletProxy("map", data = filteredData())
      
      # Remove any existing legend, and only if the legend is enabled, create a new one.
      proxy %>% clearControls()
      if (input$legend) {
        proxy %>% addLegend(position = "bottomright",
                            title = "Ending",
                            pal = pal, values = ~type_end_lab)
      }
    })
  
  

  # Plots ---------------------------------------------------------


  # Reactive expression for the data subsetted by slider panels

  plotdata <- reactive({
    # if nothing is selected because user deleted everything we display all points
    # not the initial setting however as it is cluttered
    if(input$country == "ALL" || is.null(input$country)){
      return(subset(peaks, peaks$ele %in% c(input$elev[1]:input$elev[2])))
    }

    # return selected input
    else {
      return(subset(peaks,peaks$ele %in% c(input$elev[1]:input$elev[2]) & #condition 1
                      peaks$NAME_ENGL == input$country)) # condition 2
      }
    })


  ### Plot1 with Total counts by endings(sorted)
  # Plot 1 is a simple bar chart that changes based on country and elevation input
  output$plot1 <- renderPlot({
    
    plotdata() %>% 
      as_tibble() %>% 
      count(type_end_lab) %>% 
      ggplot(aes(x = reorder(type_end_lab, n))) +
      geom_point(aes(y = n), color = "#516888", size = 3)+
      geom_segment(aes(xend = type_end_lab, y = 0, yend = n),
                   color = "#516888") +
      coord_flip()+
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.title = element_text(color = "grey30", size = 12),
            legend.position = "none",
            title = element_text(color = "grey30", size = 16)) +
      labs(title = "Most commom endings of Alpine mountains in German",
           x = "Endings",
           y = "Count")
    })
  
  
  ### Plot two with facets of counts by endings
  # Plot 2 are histograms faceted by suffix that change based on country and elevation input
  
  output$plot2 <- renderPlot({
    
    plotdata() %>% 
      ggplot(aes(x = ele, fill = type_end)) +
      geom_histogram(bins = 40, na.rm = T)+
      facet_wrap(~type_end_lab, nrow = 3) +
      scale_fill_manual(values = custom_pal) +
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
            title = element_text(color = "grey30", size = 16)) +
      labs(title = "Distribution of Elevation of alpine mountains",
           x = "Elevation in 1000m",
           y = "Count")
  })
  
}