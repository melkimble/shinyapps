library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(htmlTable)

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
    ## user inputs based on what is selected from the timeSlider
    yearRange<-input$timeSlider
    
    subset(DMRDataMeltMonthAgg,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2]& 
             StartYr>=yearRange[1] & StartYr<=yearRange[2])
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  meltLeasesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(DMRDataMelt[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    ## user inputs based on what is selected from the timeSlider
    yearRange<-input$timeSlider
    
    subset(DMRDataMelt,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2] & 
             StartYr>=yearRange[1] & StartYr<=yearRange[2])
  })
  observe({
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
          TheTitle=paste("Bathymetry (Mean:",round(mean(na.omit(meltLeasesInBounds()$BATHY)),digits=2),") at Aquaculture Sites",sep="")
          ggplot(na.omit(meltLeasesInBounds()), aes(x=BATHY)) +
            theme(plot.title=element_text(hjust=0.5),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
            xlim(range(DMRDataMelt$BATHY)) +
            geom_histogram(binwidth=1, colour="white", fill="#00DD00") +
#            geom_vline(aes(xintercept=mean(meltLeasesInBounds()$BATHY)),
#                       color="blue", linetype="dashed", size=1) +
            ggtitle(TheTitle) +
            xlab("Bathymetry (m)") +
            ylab("Frequency")
          
        } else if (selectPlot == "scatterspeciesTemp") {
          # If no zipcodes are in view, don't plot
          if (nrow(meltMonthLeasesInBounds()) == 0)
            return(NULL)
          TheTitle=paste("Sea Surface Temperature at Aquaculture Sites",sep="")
          ggplot(meltMonthLeasesInBounds(), aes(x=Month, y=SST, color=speciesCategory, shape=speciesCategory)) +
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
            ggplot(meltMonthLeasesInBounds(), aes(x=Month, y=SST, fill=speciesCategory)) +
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
#              ggtitle("Monthly Temperature by SpeciesCategory") +
              ylab("Temperature (C)")
            } else if (selectPlot == "boxSpeciesBathy") {
              # If no zipcodes are in view, don't plot
              if (nrow(meltMonthLeasesInBounds()) == 0)
                return(NULL)
              ggplot(meltMonthLeasesInBounds()[!is.na(meltMonthLeasesInBounds()$BATHY),], aes(x=speciesCategory, y=BATHY, fill=speciesCategory)) +
                geom_boxplot() +
                theme(legend.position="none",
                      plot.title=element_text(hjust=0.5),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.text=element_text(size=12),
                      axis.title.x=element_blank()) +
#                scale_x_date(date_labels = "%b", date_breaks="1 month") +
#                ggtitle("Bathymetry by SpeciesCategory") +
                ylab("Bathymetry (m)")
              } else return(NULL)
      }) 
    })
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    ## user inputs based on what is selected from the timeSlider
    yearRange<-input$timeSlider
    TheStart<-yearRange[1]
    TheEnd<-yearRange[2]
    DMRDataMeltAggTimeSub<-DMRDataMeltAgg[DMRDataMeltAgg$StartYr>=TheStart & DMRDataMeltAgg$StartYr<=TheEnd,]
    
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
      
      colorData <- DMRDataMeltAggTimeSub[[colorBy]]
      pal <- colorBin("RdYlBu", colorData, 7, pretty = FALSE, reverse = TRUE)
    } else if (colorBy == "BATHY") {
      colorData <- DMRDataMeltAggTimeSub[[colorBy]]
      colorData <- DMRDataMeltAggTimeSub[["BATHY"]]
      pal <- colorBin("Greys", colorData, 7, pretty = FALSE, reverse = TRUE)
    } else {
      colorData <-as.factor(DMRDataMeltAggTimeSub[[colorBy]])
      pal <- colorFactor("viridis", colorData)
    }
    #sizeBy="speciesCategory"
#    if (sizeBy == "speciesCategory") {
      # Radius is treated specially in the "speciesCategory" case.
#      radius <-  hist(plot = FALSE,as.numeric(as.factor(DMRDataMeltAggTimeSub[[sizeBy]])), breaks=7)$breaks*100
#    } else if (sizeBy == "equipment") {
#      radius <- hist(plot = FALSE,as.numeric(as.factor(DMRDataMeltAggTimeSub[[sizeBy]])), breaks=7)$breaks*10
#    } else {
#      radius <- hist(plot = FALSE,as.numeric(as.factor(DMRDataMeltAggTimeSub[[sizeBy]])), breaks=7)$breaks
     # radius <- DMRDataMeltAggTimeSub[[sizeBy]] / max(DMRDataMeltAggTimeSub[[sizeBy]]) * 10000
#    }
    
    leafletProxy("map", data = DMRDataMeltAggTimeSub) %>%
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
                     "<strong>", sprintf("%s, %s", selectedSite$speciesCategory[1], 
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
  #sort(unique(cleantable$LeaseType))
  #names(cleantable)
  #?structure
  observe({
    equipment <- if (is.null(input$speciesCategory)) character(0) else {
      filter(cleantable, SpeciesCategory %in% input$speciesCategory) %>%
        `$`('Equipment') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$equipment[input$equipment %in% equipment])
    updateSelectInput(session, "equipment", choices = equipment,
      selected = stillSelected)
  })
  observe({
    leasetype <- if (is.null(input$speciesCategory)) character(0) else {
      cleantable %>%
        filter(SpeciesCategory %in% input$speciesCategory,
          is.null(input$equipment) | Equipment %in% input$equipment) %>%
        `$`('LeaseType') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$leasetype[input$leasetype %in% leasetype])
    updateSelectInput(session, "leasetype", choices = leasetype,
      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.05
      
      lat <- input$goto$lat
      lng <- input$goto$lng
      site <- as.character(cleantable$SiteId[cleantable$Lat == lat & cleantable$Long == lng])

#      print(paste(site, lat, lng, sep=" "))
      showSitePopup(site, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist*10)
    })
  })

  output$dmrTable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Bathymetry >= input$minBathy,
        Bathymetry <= input$maxBathy,  
        is.null(input$speciesCategory) | SpeciesCategory %in% input$speciesCategory,
        is.null(input$equipment) | Equipment %in% input$equipment,
        is.null(input$leasetype) | LeaseType %in% input$leasetype, 
        SAT010029_20130824 | SAT010029_20140115 | SAT010029_20140304 | SAT010029_20140507 | SAT010029_20140608
        | SAT010029_20140827 | SAT010029_20140912 | SAT010029_20140928 | SAT010029_20141030 | SAT010029_20150307
        | SAT010029_20150713 | SAT010029_20150729 | SAT010029_20150814 | SAT010029_20150915 | SAT010029_20151220
        | SAT011029_20160823 | SAT011030_20130714 | SAT011030_20130730 | SAT011030_20130815 | SAT011030_20131002
        | SAT011030_20131018 | SAT011030_20140327 | SAT011030_20140412 | SAT011030_20140530 | SAT011030_20140919 
        | SAT011030_20150415 | SAT011030_20150906 | SAT011030_20151008 | SAT011030_20151109 | SAT011030_20151125
        | SAT011030_20160128 | SAT011030_20160417 | SAT011030_20160620 | SAT011030_20160706 | SAT011030_20160722
        | SAT011030_20160823 >= input$minTemp,
        SAT010029_20130824 | SAT010029_20140115 | SAT010029_20140304 | SAT010029_20140507 | SAT010029_20140608
        | SAT010029_20140827 | SAT010029_20140912 | SAT010029_20140928 | SAT010029_20141030 | SAT010029_20150307
        | SAT010029_20150713 | SAT010029_20150729 | SAT010029_20150814 | SAT010029_20150915 | SAT010029_20151220
        | SAT011029_20160823 | SAT011030_20130714 | SAT011030_20130730 | SAT011030_20130815 | SAT011030_20131002
        | SAT011030_20131018 | SAT011030_20140327 | SAT011030_20140412 | SAT011030_20140530 | SAT011030_20140919 
        | SAT011030_20150415 | SAT011030_20150906 | SAT011030_20151008 | SAT011030_20151109 | SAT011030_20151125
        | SAT011030_20160128 | SAT011030_20160417 | SAT011030_20160620 | SAT011030_20160706 | SAT011030_20160722
        | SAT011030_20160823 <= input$maxTemp
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-site="', SiteId, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
