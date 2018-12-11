library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
#set.seed(100)
#zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
#zipdata <- zipdata[order(zipdata$centile),]
# , session
function(input, output) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -67.709946, lat = 44.146299, zoom = 8)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  leasesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(DMRData[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(DMRData,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  tempBreaks <- hist(plot = FALSE, DMRData$SST, breaks = 20)$breaks

  output$histTemp <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(leasesInBounds()) == 0)
      return(NULL)
    
    TheTitle=paste("Sea Surface Temperature \n(Mean:",round(mean(leasesInBounds()$SST),digits=2),") at Aquaculture Sites",sep="")
    #gghistTemp<-
    ggplot(leasesInBounds(), aes(x=SST)) +
      theme(plot.title=element_text(hjust=0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            #panel.border = element_blank(),
            panel.background = element_blank()) +
      xlim(range(DMRData$SST)) +
      geom_histogram(binwidth=1, colour="white", fill="#00DD00") +
      geom_vline(aes(xintercept=mean(leasesInBounds()$SST)),
                 color="blue", linetype="dashed", size=1) +
      ggtitle(TheTitle) +
      xlab("Temperature (C)") +
      ylab("Frequency")
    
    #print(gghistTemp)
  })

  output$boxSpeciesTemp <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(leasesInBounds()) == 0)
      return(NULL)
    #ggboxTemp<-
    ggplot(leasesInBounds(), aes(x=species, y=SST, fill=species)) +
      geom_boxplot() +
      theme(legend.position="none",
            plot.title=element_text(hjust=0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            #panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x=element_blank()) +
      ggtitle("Temperature by Species") +
      ylab("Temperature (C)")
    #print(ggboxTemp)
    
  })

  output$boxSpeciesBathy <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(leasesInBounds()) == 0)
      return(NULL)
    #ggboxTemp<-
    ggplot(leasesInBounds(), aes(x=species, y=BATHY, fill=species)) +
      geom_boxplot() +
      theme(legend.position="none",
            plot.title=element_text(hjust=0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            #panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x=element_blank()) +
      ggtitle("Bathymetry by Species") +
      ylab("Bathymetry (m)")
    #print(ggboxTemp)
    
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    ## user inputs based on what is selected from the drop down menu
    colorBy <- input$color
    sizeBy <- input$size
    
    if (colorBy == "superzip") {
      # Color and palette are treated specially in the "SST" case, because
      # the values are categorical instead of continuous.
      ## this input$threshold is from the ui.R script, it grabs the value input by the user
      ## and adjusts the threshold on the size of the icons based on the threshold.
      colorData <- ifelse(DMRData$SST >= (100 - input$threshold), "yes", "no")
      pal <- colorFactor("viridis", colorData)
    } else {
      colorData <- DMRData[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    }
    
    if (sizeBy == "superzip") {
      # Radius is treated specially in the "species" case.
      radius <- ifelse(DMRData$SST >= (100 - input$threshold), 30000, 3000)
    } else {
      radius <- DMRData[[sizeBy]] / max(DMRData[[sizeBy]]) * 30000
    }
    
    leafletProxy("map", data = DMRData) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~ID,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })

  # Show a popup at the given location
  ## later grab SITE_ID from this group and calculate aggregates based on
#  showSitePopup <- function(ID, lat, lng) {
#    selectedSite <- DMRData[DMRData$ID == ID,]
#    content <- as.character(tagList(
#      tags$h4("Score:", selectedSite$ID),
#      tags$strong(HTML(sprintf("%s, %s %s",
#        selectedSite$species, selectedSite$equipment, selectedSite$ID
#      ))), tags$br(),
#      sprintf("Median household income: %s", selectedSite$BATHY), tags$br(),
#      sprintf("Percent of adults with BA: %s%%", as.integer(selectedSite$SeedDist)), tags$br(),
#      sprintf("Adult population: %s%%", as.integer(selectedSite$SST))
#    ))
#    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = ID)
#  }

  # When map is clicked, show a popup with city info
#  observe({
#    leafletProxy("map") %>% clearPopups()
#    event <- input$map_shape_click
 #   if (is.null(event))
#      return()

#    isolate({
#      showSitePopup(event$id, event$lat, event$lng)
#    })
#  })


  ## Data Explorer ###########################################

#  observe({
#    cities <- if (is.null(input$states)) character(0) else {
#      filter(cleantable, State %in% input$states) %>%
#        `$`('City') %>%
#        unique() %>%
#        sort()
#    }
#    stillSelected <- isolate(input$cities[input$cities %in% cities])
#    updateSelectInput(session, "cities", choices = cities,
#      selected = stillSelected)
#  })

#  observe({
#    zipcodes <- if (is.null(input$states)) character(0) else {
#      cleantable %>%
#        filter(State %in% input$states,
#          is.null(input$cities) | City %in% input$cities) %>%
#        `$`('Zipcode') %>%
#        unique() %>%
#        sort()
#    }
#    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
#    updateSelectInput(session, "zipcodes", choices = zipcodes,
#      selected = stillSelected)
#  })

#  observe({
#    if (is.null(input$goto))
#      return()
#    isolate({
#      map <- leafletProxy("map")
#      map %>% clearPopups()
#      dist <- 0.5
#      zip <- input$goto$zip
#      lat <- input$goto$lat
#      lng <- input$goto$lng
#      showSitePopup(zip, lat, lng)
#      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
#    })
#  })

#  output$ziptable <- DT::renderDataTable({
#    df <- cleantable %>%
#      filter(
#        Score >= input$minScore,
#        Score <= input$maxScore,
#        is.null(input$states) | State %in% input$states,
#        is.null(input$cities) | City %in% input$cities,
#        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
#      ) %>%
#      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
#    action <- DT::dataTableAjax(session, df)

#    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
#  })
}
