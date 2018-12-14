library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(htmlTable)


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
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  meltLeasesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(DMRDataMelt[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(DMRDataMelt,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  observe({
    # Precalculate the breaks we'll need for the two histograms
    # tempBreaks <- hist(plot = FALSE, DMRDataMelt$SST, breaks = 20)$breaks
    selectPlot<-input$selectedplot
    
    output$histTemp <- renderPlot({
      # If no zipcodes are in view, don't plot
      if (nrow(meltLeasesInBounds()) == 0)
        return(NULL)
      
      TheTitle=paste("Sea Surface Temperature \n(Mean:",round(mean(meltLeasesInBounds()$SST),digits=2),") at Aquaculture Sites",sep="")
      ggplot(meltLeasesInBounds(), aes(x=SST)) +
        theme(plot.title=element_text(hjust=0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank()) +
        xlim(range(DMRData$SST)) +
        geom_histogram(binwidth=1, colour="white", fill="#00DD00") +
        geom_vline(aes(xintercept=mean(meltLeasesInBounds()$SST)),
                   color="blue", linetype="dashed", size=1) +
        ggtitle(TheTitle) +
        xlab("Temperature (C)") +
        ylab("Frequency")
      
    })
  
    output$scatterspeciesTemp <- renderPlot({
      # If no zipcodes are in view, don't plot
      if (nrow(meltLeasesInBounds()) == 0)
        return(NULL)
  #    TheTitle=paste("Sea Surface Temperature at Aquaculture Sites",sep="")
      ggplot(meltLeasesInBounds(), aes(x=as.Date(Datef), y=SST, color=species, shape=species)) +
        theme(plot.title=element_text(hjust=0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(angle=45, hjust=1),
              legend.position="top",
              legend.title=element_blank()) +
        geom_point() +
        scale_x_date(date_labels = "%b %Y", date_breaks="3 month") +
  #      ggtitle(TheTitle) +
        xlab("Date") +
        ylab("Temperature (C)")
      
      #print(gghistTemp)
    })
  
    output$boxSpeciesTemp <- renderPlot({
      # If no zipcodes are in view, don't plot
      if (nrow(meltLeasesInBounds()) == 0)
        return(NULL)
      #ggboxTemp<-
      ggplot(meltLeasesInBounds(), aes(x=species, y=SST, fill=species)) +
        geom_boxplot() +
        theme(legend.position="none",
              plot.title=element_text(hjust=0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
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
      ggplot(leasesInBounds()[!is.na(leasesInBounds()$BATHY),], aes(x=species, y=BATHY, fill=species)) +
        geom_boxplot() +
        theme(legend.position="none",
              plot.title=element_text(hjust=0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.title.x=element_blank()) +
        ggtitle("Bathymetry by Species") +
        ylab("Bathymetry (m)")
      #print(ggboxTemp)
      
    })
    
    plotOutput(selectPlot, height = 250)
  })  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    ## user inputs based on what is selected from the drop down menu
    colorBy <- input$color
#    sizeBy <- input$size
    if (colorBy == "SST") {
      # Color and palette are treated specially in the "SST" case, because
      # the values are categorical instead of continuous.
      ## this input$threshold is from the ui.R script, it grabs the value input by the user
      ## and adjusts the threshold on the size of the icons based on the threshold.
      # Precalculate the breaks we'll need for the two histograms
      #colorBy="SST"
      
      colorData <- DMRDataMeltAgg[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    } else if (colorBy == "BATHY") {
      colorData <- DMRDataMeltAgg[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    } else {
      colorData <-as.factor(DMRDataMeltAgg[[colorBy]])
      pal <- colorFactor("viridis", colorData)
    }
    #sizeBy="species"
#    if (sizeBy == "species") {
      # Radius is treated specially in the "species" case.
#      radius <-  hist(plot = FALSE,as.numeric(as.factor(DMRDataMeltAgg[[sizeBy]])), breaks=7)$breaks*100
#    } else if (sizeBy == "equipment") {
#      radius <- hist(plot = FALSE,as.numeric(as.factor(DMRDataMeltAgg[[sizeBy]])), breaks=7)$breaks*10
#    } else {
#      radius <- hist(plot = FALSE,as.numeric(as.factor(DMRDataMeltAgg[[sizeBy]])), breaks=7)$breaks
     # radius <- DMRDataMeltAgg[[sizeBy]] / max(DMRDataMeltAgg[[sizeBy]]) * 10000
#    }
    
    leafletProxy("map", data = DMRDataMeltAgg) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius= 250, layerId=~unique(SITE_ID),
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })

  # Show a popup at the given location
  ## later grab SITE_ID from this group and calculate aggregates based on
  
  # DMRData DMRDataMelt DMRDataMeltAgg DMRDataMeltMonthAgg
  
  showSitePopup <- function(ID, lat, lng) {
    #print(ID)
    #ID="JYOU215"
    selectedSite <- DMRDataMeltMonthAgg[DMRDataMeltMonthAgg$SITE_ID == ID,]
    Months<-selectedSite$Month
    Temps<-selectedSite$SST
    SDTemps<-selectedSite$SST_StdDev
    
    TheTable<-selectedSite[,c("Month","SST","SST_StdDev", "BATHY")]
    MonthOrder<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug", "Sep", "Oct", "Nov", "Dec")
    TheTable<-TheTable[order(factor(TheTable$Month,levels=c(MonthOrder))),]
    
    names(TheTable)<-c("Month", "Mean Temp", "Std Dev", "Bathy")
    
    content <- paste("<h4> Site ID:", ID,"</h4>",
                     "<strong>", sprintf("%s, %s", selectedSite$species[1], 
                                         selectedSite$equipment[1]),"</strong>", 
                     htmlTable(TheTable, col.rgroup = c("none", "#F9FAF0"), 
                               col.columns = c("none", "#F1F0FA"),
                               align.header = "clcr",
                               align.cgroup = "lcr",
                               padding.tspanner = "&nbsp;&nbsp;"),
                     "*Sea Surface Temperature (SST, C)","</br>", "Bathymetry (m).")
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = ID)
  }

  # When map is clicked, show a popup with site info
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
