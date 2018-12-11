library(dplyr)

DMRData <- readRDS("data/TestData.rds")
DMRData$SITE_ID
DMRData$latitude 
DMRData$longitude 
DMRData$species 
DMRData$equipment 
DMRData$SST 
DMRData$BATHY
DMRData$SeedDist
#row.names(DMRData) <- DMRData$zipcode

cleantable <- allzips %>%
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
