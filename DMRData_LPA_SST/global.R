library(dplyr)

dmrLpaSST <- readRDS("data/TestData.rds")
#names(dmrLpaSST)
dmrLpaSST$ID 
dmrLpaSST$SITE_ID 
dmrLpaSST$latitude
dmrLpaSST$longitude
dmrLpaSST$species
dmrLpaSST$equipment
dmrLpaSST$SST
dmrLpaSST$BATHY
dmrLpaSST$SeedDist




row.names(dmrLpaSST) <- dmrLpaSST$ID 

cleantable <- dmrLpaSST %>%
  select(
    Id = ID,
    SiteId = SITE_ID,
    Lat = latitude,
    Long = longitude,
    Species = species,
    Equipment = equipment,
    SST = SST,
    Bathymetry = BATHY,
    SeedDistance = SeedDist
  )

