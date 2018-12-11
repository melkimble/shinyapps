library(dplyr)

DMRData <- readRDS("data/TestData.rds")
DMRData$SITE_ID <- as.character(DMRData$SITE_ID)
DMRData$latitude 
DMRData$longitude 
DMRData$species <- as.character(DMRData$species )
DMRData$equipment <- as.character(DMRData$equipment )
DMRData$SST 
DMRData$BATHY
DMRData$SeedDist
#row.names(DMRData) <- DMRData$zipcode

cleantable <- DMRData %>%
  select(
    SiteId = SITE_ID,
    Species = species,
    Equipment = equipment,
    SeaSurfaceTemp = SST,
    Bathymetry = BATHY,
    SeedDist = SeedDist,
    Lat = latitude,
    Long = longitude
  )
