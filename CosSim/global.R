library(spdplyr)
library(rgdal)
library(lsa)


# To install gdal: sudo apt install gdal-bin proj-bin libgdal-dev libproj-dev
# https://community.rstudio.com/t/can-not-install-rgdal/33971
# R -e "install.packages('rgdal',type='source')"

NCT <- readOGR("data/WQWL_VoronoiPolys_V2_NCT_NAD83.shp", layer = "WQWL_VoronoiPolys_V2_NCT_NAD83", GDAL1_integer64_policy = TRUE)
CosSim_rds<- readRDS("data/DMR_Lobo_Voronoi_CosSim_flat.rds")
Combo_rds<-readRDS("data/DMR_Lobo_Voronoi_Combined.rds")
NCT$meanTenure[NCT$meanTenure==0]<-NA
colnames(CosSim_rds)[colnames(CosSim_rds)=="column"] <- "Station"
colnames(CosSim_rds)[colnames(CosSim_rds)=="cor"] <- "Similarity"


