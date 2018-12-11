library(dplyr)

DMRData <- readRDS("data/TestData.rds")
DMRData$ID<-as.integer(DMRData$ID)
DMRData$SITE_ID <- as.character(DMRData$SITE_ID)
DMRData$latitude <- as.numeric(DMRData$latitude)
DMRData$longitude <- as.numeric(DMRData$longitude)
DMRData$species <- as.character(DMRData$species)
DMRData$equipment <- as.character(DMRData$equipment)
DMRData$SST<-round(DMRData$SST,2) 
DMRData$BATHY<-round(DMRData$BATHY,2)
DMRData$SeedDist<-round(DMRData$SeedDist,2)

#row.names(DMRData) <- DMRData$zipcode

#cleantable <- DMRData %>%
#  select(
#    SiteId = SITE_ID,
#    Lat = latitude,
#    Long = longitude,
#    Species = species,
#    Equipment = equipment,
#    SeaSurfaceTemp = SST,
#    Bathymetry = BATHY,
#    SeedDist = SeedDist
#  )
