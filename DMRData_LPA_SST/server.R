library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
LPAdata <- dmrLpaSST
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
#zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {

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
      return(LPAdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(LPAdata,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  tempBreaks <- hist(plot = FALSE, dmrLpaSST$SST, breaks = 20)$breaks

  output$histTemp <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(leasesInBounds()) == 0)
      return(NULL)

    hist(leasesInBounds()$SST,
      breaks = tempBreaks,
      main = "Temperature C (visible leases)",
      xlab = "Percentile",
      xlim = range(dmrLpaSST$SST),
      col = '#00DD00',
      border = 'white')
  })

  output$scatterBathy <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(leasesInBounds()) == 0)
      return(NULL)

    print(xyplot(species ~ BATHY, data = leasesInBounds(), xlim = range(dmrLpaSST$BATHY), ylim = range(dmrLpaSST$species)))
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size

    if (colorBy == "SST") {
      # OK, so in here I have to present special use-case catgeorical coloring rules based on what variable is selected.
      # equipment, species, and site_id are fine and do not require any special use-case.
      ## I picked these categorical breaks based on quantiles 25% and 75%.
      colorData <- cut(dmrLpaSST$SST, breaks=c(-Inf, 7.3600, 19.5675, Inf), labels=c("Low","Med","High"))
      #colorData <- ifelse(dmrLpaSST$SST >= (100 - input$threshold), "yes", "no")
      
      pal <- colorFactor("viridis", colorData)
    } else if (colorBy == "BATHY") {
      colorData <- cut(dmrLpaSST$BATHY, breaks=c(-Inf, -5.100, 4.795, Inf), labels=c("Low","Med","High"))
      #colorData <- ifelse(dmrLpaSST$BATHY >= (100 - input$threshold), "yes", "no")
      
      pal <- colorFactor("viridis", colorData)
    } else if (colorBy == "SeedDist") {
      colorData <- cut(dmrLpaSST$SeedDist, breaks=c(-Inf, 2475.165, 7516.225, Inf), labels=c("Low","Med","High"))
      #colorData <- ifelse(dmrLpaSST$SeedDist >= (100 - input$threshold), "yes", "no")
      
      pal <- colorFactor("viridis", colorData)
    } else {
      colorData <- dmrLpaSST[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    }

    if (sizeBy == "species") {
      # OK, so in here I have to present special use-case continuous sizing rules based on what variable is selected
      ## equipment, species, and site_id are categorical. Hmmm
      radius <- ifelse(dmrLpaSST$species >= (100 - input$threshold), 30000, 3000)
    } else if (sizeBy == "equipment") {
      radius <- ifelse(dmrLpaSST$equipment >= (100 - input$threshold), 30000, 3000)
    } else if (sizeBy == "SITE_ID") {
      radius <- ifelse(dmrLpaSST$SITE_ID >= (100 - input$threshold), 30000, 3000)
    } else {
      radius <- dmrLpaSST[[sizeBy]] / max(dmrLpaSST[[sizeBy]]) * 30000
    }

    leafletProxy("map", data = dmrLpaSST) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~species,
        stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
        layerId="colorLegend")
  })

  # Show a popup at the given location
  showSiteIDPopup <- function(SITE_ID, lat, lng) {
    selectedLeases <- dmrLpaSST[dmrLpaSST$SITE_ID == SITE_ID,]
    content <- as.character(tagList(
      tags$h4("Site ID:", as.integer(selectedLeases$SST)),
      ## The original is formatted as City, State Zipcode. It might be useful
      ## to put the gear in here or seed location.
      tags$strong(HTML(sprintf("%s, %s, %s",
        selectedLeases$SITE_ID, selectedLeases$species, selectedLeases$equipment
      ))), tags$br(),
      sprintf("Average Temperature: %s", aggregate(TestData["SST"], list(TestData$SITE_ID),na.rm=TRUE,mean)), tags$br(),
      sprintf("Average Bathymetry: %s%%", aggregate(TestData["BATHY"], list(TestData$SITE_ID),na.rm=TRUE,mean)), tags$br(),
      sprintf("Average Distance to Seed: %s", aggregate(TestData["SeedDist"], list(TestData$SITE_ID),na.rm=TRUE,mean)), tags$br()
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = SITE_ID)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showSiteIDPopup(event$id, event$lat, event$lng)
    })
  })


  ## Data Explorer ###########################################
  #Actual (ID,SITE_ID zipcode,latitude,longitude,species states,equipment cities,SST,BATHY,SeedDist)
  #cleantable (Id, SiteId, Lat, Long, Species, Equipment City, SST, Bathymetry, SeedDistance)
  
  observe({
    equipment <- if (is.null(input$species)) character(0) else {
      filter(cleantable, Species %in% input$species) %>%
        `$`('Equipment') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$equipment[input$equipment %in% equipment])
    updateSelectInput(session, "equipment", choices = equipment,
                      selected = stillSelected)
  })
  
  observe({
    SITE_ID <- if (is.null(input$species)) character(0) else {
      cleantable %>%
        filter(Species %in% input$species,
               is.null(input$equipment) | Equipment %in% input$equipment) %>%
        `$`('SiteId') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$SITE_ID[input$SITE_ID %in% SITE_ID])
    updateSelectInput(session, "SITE_ID", choices = SITE_ID,
                      selected = stillSelected)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      sid <- input$goto$sid
      lat <- input$goto$lat
      lng <- input$goto$lng
      showSiteIDPopup(sid, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$siteTable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$species) | Species %in% input$species,
        is.null(input$equipment) | Equipment %in% input$equipment,
        is.null(input$SITE_ID) | SiteId %in% input$SITE_ID
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-sid="', SiteId, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
