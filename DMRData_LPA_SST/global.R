library(dplyr)

DMRData <- readRDS("data/TestData.rds")
DMRDataMelt <- readRDS("data/TestDataMelt.rds")
DMRDataMeltAgg <- readRDS("data/TestDataMeltAgg.rds")

names(DMRData)<-c("ID", "SITE_ID", "latitude", "longitude", "species", "equipment", "LEASE_TYPE", "Status", "TheYr", "EndYr", "BATHY", 
                  "X010029_20130824", "X010029_20140115", "X010029_20140304", "X010029_20140507", "X010029_20140608", "X010029_20140827",
                  "X010029_20140912", "X010029_20140928", "X010029_20141030", "X010029_20150307", "X010029_20150713", "X010029_20150729",
                  "X010029_20150814", "X010029_20150915", "X010029_20151220", "X011029_20160823", "X011030_20130714", "X011030_20130730",
                  "X011030_20130815", "X011030_20131002", "X011030_20131018", "X011030_20140327", "X011030_20140412", "X011030_20140530",
                  "X011030_20140919", "X011030_20150415", "X011030_20150906", "X011030_20151008", "X011030_20151109", "X011030_20151125",
                  "X011030_20160128", "X011030_20160417", "X011030_20160620", "X011030_20160706", "X011030_20160722", "X011030_20160823")
names(DMRDataMelt)<-c("SITE_ID", "LSAT8_PPPRRR", "Date", "SST", "latitude", "longitude", "species")

#DMRData$ID<-as.integer(DMRData$ID)
#DMRData$SITE_ID <- as.character(DMRData$SITE_ID)
#DMRData$latitude <- as.numeric(DMRData$LATITUDE)
#DMRData$longitude <- as.numeric(DMRData$LONGITUDE)
#DMRData$species <- as.character(DMRData$species)
#DMRData$equipment <- as.character(DMRData$equipment)
#DMRData$SST<-round(DMRData$SST,2) 
DMRData$BATHY<-round(DMRData$BATHY,2)
#DMRData$SeedDist<-round(DMRData$SeedDist,2)

DMRDataMelt$SST <- round(DMRDataMelt$SST,2)
DMRDataMelt$Datef<-format(as.Date(as.character(DMRDataMelt$Date), "%Y%m%d"),"%Y-%m-%d")
DMRDataMelt$Month<-format(as.Date(as.character(DMRDataMelt$Date), "%Y%m%d"),"%b")

#row.names(DMRData) <- DMRData$ID

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

