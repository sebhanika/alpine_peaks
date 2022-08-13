
# define server function

function(input, output, session) {
  
  #### Create Map 
  
  
  # Reactive expression for the data subsetted by selectedInput
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
      fitBounds(6, 46.5, 17.5, 47.5) # zooms into alpine areas
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
    
    
    
    
    # create legend
    
    # Use a separate observer to recreate the legend as needed.
    observe({
      proxy <- leafletProxy("map", data = filteredData())
      
      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()
      if (input$legend) {
        proxy %>% addLegend(position = "bottomright",
                            title = "Ending",
                            pal = pal, values = ~type_end_lab
        )
      }
    })
    
    
      
  })
  
  
  
  
  ##### - data for plot
  
  # Reactive expression for the data subsetted by slider panels
  plotdata <- reactive({
    subset(peaks, peaks$ele %in% c(input$elev[1]:input$elev[2]))
    
  })
  
  ### Plot1 with Total counts by endings(sorted)
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
  
  
  
  ### Plot two with facets of counts by endigs
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