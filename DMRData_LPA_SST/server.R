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
  meltMonthLeasesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(DMRDataMeltMonthAgg[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(DMRDataMeltMonthAgg,
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
    output$plot <- renderPlot({
      if (selectPlot == "histTemp") {
        # If no zipcodes are in view, don't plot
        if (nrow(meltLeasesInBounds()) == 0)
          return(NULL)
        TheTitle=paste("Sea Surface Temperature (Mean:",round(mean(meltLeasesInBounds()$SST),digits=2),") at Aquaculture Sites",sep="")
        ggplot(meltLeasesInBounds(), aes(x=SST)) +
          theme(plot.title=element_text(hjust=0.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank()) +
          xlim(range(DMRDataMelt$SST)) +
          geom_histogram(binwidth=1, colour="white", fill="#00DD00") +
          geom_vline(aes(xintercept=mean(meltLeasesInBounds()$SST)),
                     color="blue", linetype="dashed", size=1) +
          ggtitle(TheTitle) +
          xlab("Temperature (C)") +
          ylab("Frequency")
        } else if (selectPlot == "histBathy") {
          TheTitle=paste("Bathymetry (Mean:",round(mean(meltLeasesInBounds()$BATHY),digits=2),") at Aquaculture Sites",sep="")
          ggplot(meltLeasesInBounds(), aes(x=BATHY)) +
            theme(plot.title=element_text(hjust=0.5),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
            xlim(range(DMRDataMelt$BATHY)) +
            geom_histogram(binwidth=1, colour="white", fill="#00DD00") +
            geom_vline(aes(xintercept=mean(meltLeasesInBounds()$BATHY)),
                       color="blue", linetype="dashed", size=1) +
            ggtitle(TheTitle) +
            xlab("Bathymetry (m)") +
            ylab("Frequency")
          
        } else if (selectPlot == "scatterspeciesTemp") {
          # If no zipcodes are in view, don't plot
          if (nrow(meltMonthLeasesInBounds()) == 0)
            return(NULL)
          TheTitle=paste("Sea Surface Temperature at Aquaculture Sites",sep="")
          ggplot(meltMonthLeasesInBounds(), aes(x=Month, y=SST, color=species, shape=species)) +
            theme(plot.title=element_text(hjust=0.5),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.text=element_text(size=12),
 #                 axis.text.x = element_text(angle=35, hjust=1),
                  axis.title.x = element_blank(),
                  legend.position="top",
                  legend.text=element_text(size=12),
                  legend.title=element_blank()) +
            scale_x_discrete(limits = month.abb) +
            geom_point(size = 3) +
#            scale_x_date(date_labels = "%b %Y", date_breaks="3 month") +
#            ggtitle(TheTitle) +
            ylab("Temperature (C)")
          } else if (selectPlot == "boxSpeciesTemp") {
            # If no zipcodes are in view, don't plot
            if (nrow(meltMonthLeasesInBounds()) == 0)
              return(NULL)
            #MonthOrder<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug", "Sep", "Oct", "Nov", "Dec")
            ggplot(meltMonthLeasesInBounds(), aes(x=Month, y=SST, fill=species)) +
              geom_boxplot() +
              theme(plot.title=element_text(hjust=0.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.text=element_text(size=12),
                    legend.text=element_text(size=12),
                    legend.title=element_blank(),
                    legend.position="top",
                    axis.title.x=element_blank()) +
              scale_x_discrete(limits = month.abb) +
#              scale_x_date(date_labels = "%b", date_breaks="1 month") +
#              ggtitle("Monthly Temperature by Species") +
              ylab("Temperature (C)")
            } else if (selectPlot == "boxSpeciesBathy") {
              # If no zipcodes are in view, don't plot
              if (nrow(meltMonthLeasesInBounds()) == 0)
                return(NULL)
              ggplot(meltMonthLeasesInBounds()[!is.na(meltMonthLeasesInBounds()$BATHY),], aes(x=species, y=BATHY, fill=species)) +
                geom_boxplot() +
                theme(legend.position="none",
                      plot.title=element_text(hjust=0.5),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.text=element_text(size=12),
                      axis.title.x=element_blank()) +
#                scale_x_date(date_labels = "%b", date_breaks="1 month") +
#                ggtitle("Bathymetry by Species") +
                ylab("Bathymetry (m)")
              } else return(NULL)
      }) 
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
      pal <- colorBin("RdYlBu", colorData, 7, pretty = FALSE)
    } else if (colorBy == "BATHY") {
      colorData <- DMRDataMeltAgg[[colorBy]]
      pal <- colorBin("Greys", colorData, 7, pretty = FALSE)
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
      addCircles(~longitude, ~latitude, radius= 150, layerId=~unique(SITE_ID),
                 stroke=FALSE, fillOpacity=0.8, fillColor=pal(colorData)) %>%
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
    TheID <- DMRDataMeltMonthAgg$SITE_ID[DMRDataMeltMonthAgg$SITE_ID == ID][1]
    lat <- DMRDataMeltMonthAgg$latitude[DMRDataMeltMonthAgg$SITE_ID == ID][1]
    lng <- DMRDataMeltMonthAgg$longitude[DMRDataMeltMonthAgg$SITE_ID == ID][1]

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
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = TheID)
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
 # DMRData
  observe({
    equips <- if (is.null(input$species)) character(0) else {
      filter(cleantable, Species %in% input$species) %>%
        `$`('Equipment') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$equipment[input$equipment %in% equips])
    updateSelectInput(session, "equips", choices = equips,
      selected = stillSelected)
  })
  observe({
    leasetypes <- if (is.null(input$species)) character(0) else {
      cleantable %>%
        filter(Species %in% input$species,
          is.null(input$equipment) | Equipment %in% input$equipment) %>%
        `$`('LeaseType') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$leasetypes[input$leasetypes %in% leasetypes])
    updateSelectInput(session, "leasetypes", choices = leasetypes,
      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      site <- input$goto$SiteId
      lat <- input$goto$lat
      lng <- input$goto$lng
      showSitePopup(site, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$dmrTable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Bathymetry >= input$minBathy,
        Bathymetry <= input$maxBathy,        
        is.null(input$species) | Species %in% input$species,
        is.null(input$equips) | Equipment %in% input$equips,
        is.null(input$leasetypes) | LeaseType %in% input$leasetypes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-site="', SiteId, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
