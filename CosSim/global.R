library(spdplyr)
library(rgdal)

# To install gdal: sudo apt install gdal-bin proj-bin libgdal-dev libproj-dev
# https://community.rstudio.com/t/can-not-install-rgdal/33971
# R -e "install.packages('rgdal',type='source')"

flattenCorrMatrix <- function(df) {
  df <- as.matrix(df)
  CosColNames<-colnames(df)
  rownames(df)<-CosColNames
  
  ut <- upper.tri(df)
  flat_df<-data.frame(
    row = rownames(df)[row(df)[ut]],
    column = rownames(df)[col(df)[ut]],
    cor  =(df)[ut]
  )
  
  flat_df <- flat_df[order(flat_df$row),]
  return(flat_df)
}

RemvNAColRow <- function(df){
  ## drop when column contains all NAs
  df <- df[,colSums(is.na(df))<nrow(df)]
  ## drop row if contains all NAs
  df<-df[rowSums(is.na(df)) != ncol(df), ]
  return(df)
}

CosSim_T <- function(df, groupby){
  df<-RemvNAColRow(df)
  
  df[[groupby]] <- as.character(df[[groupby]])
  rownames(df) <- df[[groupby]]
  theStations <- df[[groupby]]
  
  df_cos<-df[ , -which(names(df) %in% c(groupby))]
  
  df_cos_t <- t(df_cos)
  
  df_cos_t <- as.matrix(df_cos_t)
  df_CosSim<-cosine(df_cos_t)
  
  colnames(df_CosSim)<-theStations
  rownames(df_CosSim)<-theStations
  return(df_CosSim)
}

NCT <- readOGR("data/WQWL_VoronoiPolys_V2_NCT_NAD83.shp", layer = "WQWL_VoronoiPolys_V2_NCT_NAD83", GDAL1_integer64_policy = TRUE)
CosSim_rds<- readRDS("data/DMR_Lobo_Voronoi_CosSim_flat.rds")
Combo_rds<-readRDS("data/DMR_Lobo_Voronoi_Combined.rds")
NCT$meanTenure[NCT$meanTenure==0]<-NA



