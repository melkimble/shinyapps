library(shiny)
library(leaflet)
library(RColorBrewer)
library(spdplyr)
library(lsa)

# https://rstudio.github.io/leaflet/shiny.html


function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    ## input from the slider
    CosSim_rds[CosSim_rds$cor >= input$range[1] & CosSim_rds$cor <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, NCT$meanTenure)
  })
  
  
  bins <- as.vector(quantile(NCT$meanTenure, na.rm=TRUE))
  pal <- colorBin("YlOrRd", domain = NCT$meanTenure, bins = bins, na.color = "#808080")
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g NCT",
    NCT$Location_I, NCT$meanTenure) %>% 
    lapply(htmltools::HTML)
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% 
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      addPolygons(
        data=NCT,
        layerId=~Location_I,
        fillColor = ~pal(meanTenure),
        weight = 1,
        opacity = 1,
        color = "#444444",
        dashArray = "3",
        fillOpacity = 0.8,
        highlight = highlightOptions(
          color = "white", 
          weight = 2,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      setView(lng = -69.7314453, lat = 43.9143787, zoom = 11)
    

  })
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    

    

    
    
    click <- input$map_shape_click
    idx <- na.omit(CosSim_rds[CosSim_rds$row == click$id,])
    
    if(is.null(click))
      return()
    
    ## CosSim
    #(input$corrVars)
    if(is.null(input$corrVars)){
      print("A selection must be made")
      idx <- na.omit(CosSim_rds[CosSim_rds$row == click$id,])
      
      idx <- idx[(idx$Similarity >= input$range[1]) & (idx$Similarity <= input$range[2]),]
      
      selected_polygon <- NCT %>% filter(NCT@data[["Location_I"]] %in% unique(idx$Station))
      
      leafletProxy("map") %>% clearGroup("highlighted_polygon")
      
      #add a slightly thicker red polygon on top of the selected one
      leafletProxy("map") %>% addPolylines(stroke=TRUE, weight = 3,color="red",data=selected_polygon,group="highlighted_polygon")
      
      idx<-idx[c("Station","Similarity")]
      DT::datatable({idx <- idx[order(idx$Similarity,decreasing = TRUE),] 
      idx})
      
    } else{
      

      
      Combo_Sub<-Combo_rds[grep(paste(c(input$corrVars,"Station"), collapse="|"),colnames(Combo_rds))]
      
      Combo_Cossim<-CosSim_T(Combo_Sub, "Station")
      idx<-flattenCorrMatrix(Combo_Cossim)
      idx <- na.omit(idx[idx$row == click$id,])
      colnames(idx)[colnames(idx)=="column"] <- "Station"
      colnames(idx)[colnames(idx)=="cor"] <- "Similarity"
      
      idx <- idx[(idx$Similarity >= input$range[1]) & (idx$Similarity <= input$range[2]),]
      
      selected_polygon <- NCT %>% filter(NCT@data[["Location_I"]] %in% unique(idx$Station))
      
      leafletProxy("map") %>% clearGroup("highlighted_polygon")
      
      #add a slightly thicker red polygon on top of the selected one
      leafletProxy("map") %>% addPolylines(stroke=TRUE, weight = 3,color="red",data=selected_polygon,group="highlighted_polygon")
      
      idx<-idx[c("Station","Similarity")]
      DT::datatable({idx <- idx[order(idx$Similarity,decreasing = TRUE),] 
      idx})
      
    }
    

  })
  
  # Show the values in an HTML table ----
  output$theOutputTable <- DT::renderDataTable({
    sliderValues()
    #remove any previously highlighted polygon
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data=NCT)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend(pal = pal, values = ~meanTenure, opacity = 0.8, title = "Normalized Tenure",
                          position = "bottomleft")
    }
  })
}
