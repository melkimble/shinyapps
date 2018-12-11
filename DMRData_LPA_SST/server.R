library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

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
      setView(lng = -68.87, lat = 44.92, zoom = 8)
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
#  tempBreaks <- hist(plot = FALSE, DMRData$SST, breaks = 20)$breaks

#  output$histTemp <- renderPlot({
    # If no zipcodes are in view, don't plot
#    if (nrow(leasesInBounds()) == 0)
#      return(NULL)
    
#    hist(leasesInBounds()$SST,
#      breaks = tempBreaks,
#      main = "Sea Surface Temperature (visible sites)",
#      xlab = "Temperature (C)",
#      xlim = range(DMRData$SST),
#      col = '#00DD00',
#      border = 'white')
#  })

#  output$boxSpecies <- renderPlot({
#    # If no zipcodes are in view, don't plot
#    if (nrow(leasesInBounds()) == 0)
#      return(NULL)
    
#    boxplot(SST ~ species, data = leasesInBounds())
#  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size

    if (colorBy == "SST") {
      # Color and palette are treated specially in the "SST" case, because
      # the values are categorical instead of continuous.
      colorData <- cut(DMRData$SST, breaks=c(-Inf, 7.3600, 19.5675, Inf), labels=c("low","medium","high"))
      pal <- colorFactor("viridis", colorData)
    } else {
      colorData <- DMRData[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    }

    if (sizeBy == "species") {
      # Radius is treated specially in the "species" case.
      radius <- 10*(as.numeric(as.factor(DMRData$species)))
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
  showSitePopup <- function(SITE_ID, lat, lng) {
    selectedSite <- DMRData[DMRData$SITE_ID == SITE_ID,]
    content <- as.character(tagList(
      tags$h4("Score:", selectedSite$SITE_ID),
      tags$strong(HTML(sprintf("%s, %s %s",
        selectedSite$species, selectedSite$equipment, selectedSite$SITE_ID
      ))), tags$br(),
      sprintf("Median household income: %s", dollar(selectedSite$BATHY * 1000)), tags$br(),
      sprintf("Percent of adults with BA: %s%%", as.integer(selectedSite$SeedDist)), tags$br(),
      sprintf("Adult population: %s", selectedSite$SST)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = ID)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showSitePopup(event$id, event$lat, event$lng)
    })
  })


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
