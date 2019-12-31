library(shiny)
library(leaflet)
library(RColorBrewer)
library(spdplyr)
library(rgdal)
library(lsa)
library(stringr)

# https://rstudio.github.io/leaflet/shiny.html


function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
#  filteredData <- reactive({
    ## input from the slider
#    CosSim_rds[CosSim_rds$Similarity >= input$range[1] & CosSim_rds$Similarity <= input$range[2],]
#  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, NCT$meanTenure)
  })
  
  
  bins <- as.vector(quantile(NCT$meanTenure[NCT$meanTenure>0], na.rm=TRUE))
  polyPal <- colorBin("YlOrRd", domain = NCT$meanTenure, bins = bins, na.color = "#808080")
  
  
  colorData <-as.factor(DMRDataMelt$speciesCategory)
  pointPal <- colorFactor("viridis", colorData)
  
  
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
        fillColor = ~polyPal(meanTenure),
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
      addCircles(data = DMRDataMelt, ~longitude, ~latitude, radius= 150, layerId=~unique(SITE_ID),
                 stroke=FALSE, fillOpacity=0.8, fillColor=pointPal(colorData)) %>%
      
      setView(lng = -69.7314453, lat = 43.9143787, zoom = 11)
    

  })
  

  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    ## input from the slider
    #CosSim_rds[CosSim_rds$Similarity >= input$range[1] & CosSim_rds$Similarity <= input$range[2],]
    
    click <- input$map_shape_click
    
    idx <- CosSim_rds[(CosSim_rds$row == click$id) & (CosSim_rds$Similarity > 0),]
    
    if(is.null(click))
      return()
    
    ## CosSim
    #(input$corrVars)
    if(is.null(input$corrVars)){
      output$printOutput<-renderPrint("A variable selection must be made.", quoted=FALSE)
      return()
      } else {
      
      Combo_Sub<-Combo_rds[grep(paste(c(input$corrVars,"Station"), collapse="|"),colnames(Combo_rds))]

      Combo_Cossim<-CosSim_T(Combo_Sub, "Station")
      idx<-flattenCorrMatrix(Combo_Cossim, "Station")
      
      idx <- idx[(idx$row == click$id) & (idx$Similarity > 0),]
      
      idx <- idx[(idx$Similarity >= input$range[1]) & (idx$Similarity <= input$range[2]),]
      idx<-idx %>% 
        filter(!str_detect(Station, 'NA'))      
      
      NumRows<-nrow(idx)
      
      if (NumRows == 0) {
        ## if there are no selections, then no polygons will be selected
        output$printOutput<-renderPrint("No areas within desired correlation ranges.", quoted=FALSE)
        leafletProxy("map") %>% clearGroup("highlighted_polygon")
      } else {
        
        output$printOutput<-renderPrint(sprintf("%s areas within desired correlation ranges.",NumRows), quoted=FALSE)
        
        
        ## if there are selections, then polygons will be selected
        selected_polygon <- NCT %>% filter(NCT@data[["Location_I"]] %in% unique(idx$Station))
        
        leafletProxy("map") %>% clearGroup("highlighted_polygon")
        
        #add a slightly thicker red polygon on top of the selected one
        leafletProxy("map") %>% addPolylines(stroke=TRUE, weight = 3,color="red",data=selected_polygon,group="highlighted_polygon")
        
      }
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
      proxy %>% addLegend(position = "bottomleft", pal=pointPal, values=colorData, title="Species Category") %>%
      addLegend(pal = polyPal, values = ~meanTenure, opacity = 0.8, title = "Normalized Tenure", position = "bottomleft") 
        
    }
  })
}
