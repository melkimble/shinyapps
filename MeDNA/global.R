library(dplyr)
library(googlesheets4)
library(data.table)
library(lubridate)
library(stringr)
library(ggplot2)
#library(scales)
#library(zoo)
library(tidyr)
library(gridExtra)
library(rgdal)
library(viridis)
library(shiny)
library(leaflet)
library(spdplyr)
library(raster)
library(sf)
#library(rgeos)
#library(rmapshaper)
library(DT)
library(shinythemes)
#library(shinyWidgets)

source("medna_survey123.R")
# LOAD PROJECT VARIABLES
################################################################################
overwrite = FALSE
inputFolder = "data/02_Working/PyOutput/"
originalFolder = "data/01_Original/"
outputFolder = "data/02_Working/"
outputPngFolder = "data/figures/"

todaysDateFn = format(Sys.Date(), "%Y%m%d")

# Projection
WGS84_SRID = 4326

# LOAD SHAPFILES
################################################################################
# Watershed Boundary Dataset (WBD), National Hydrography Dataset (NHD), United States Geological Survey (USGS)
WBDHU8SHP_simplified_filename = "WBDHU8_MENH_NEA_simplified"
WBDHU8SHP_simplified = paste0(outputFolder, WBDHU8SHP_simplified_filename,".shp")
WBD_spdf_simplified<-rgdal::readOGR(WBDHU8SHP_simplified)

#rgdal::ogrListLayers(WBD_spdf_simplified)
centers_wbd <- data.frame(rgeos::gCentroid(WBD_spdf_simplified, byid = TRUE))
centers_wbd$region_cod <- WBD_spdf_simplified$region_cod

boundingbox_wbd<-raster::extent(WBD_spdf_simplified@bbox)

## Set map colors
################################################################################
regionPal <- colorFactor("Set3", as.factor(WBD_spdf_simplified$region_cod))
existsPal <- colorFactor(c("antiquewhite", "forestgreen"), as.factor(c("Yes","No")))

# LOAD GSHEETs
################################################################################
# pre-select and grab oath

# designate project-specific cache
options(gargle_oauth_cache = ".secrets")

# check the value of the option, if you like
gargle::gargle_oauth_cache()

# trigger auth on purpose --> store a token in the specified cache
#googledrive::drive_auth(cache = ".secrets", email = "melissa.kimble@maine.edu")

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "melissa.kimble@maine.edu"
)

# load gsheet
s123_gsheet_url = "https://docs.google.com/spreadsheets/d/1IVn6Ui80VZZrFjjRCy6FaOwh_5fR9ZJj4OxNypYBduc/edit?usp=sharing"
site_ids_gsheet <- googlesheets4::read_sheet(s123_gsheet_url, "SiteIDs")
survey_sub<-googlesheets4::read_sheet(s123_gsheet_url, "eDNA_Sampling_v14_sub")
survey_crew_join<-googlesheets4::read_sheet(s123_gsheet_url, "survey_crew_join")
survey_envmeas_join<-googlesheets4::read_sheet(s123_gsheet_url, "survey_envmeas_join")
survey_collection_join<-googlesheets4::read_sheet(s123_gsheet_url, "survey_collection_join")
clean_filter_join<-googlesheets4::read_sheet(s123_gsheet_url, "clean_filter_join")
clean_subcore_join<-googlesheets4::read_sheet(s123_gsheet_url, "clean_subcore_join")

# FORMAT DATA
################################################################################
site_ids_spdf <- site_ids_gsheet %>%
  dplyr::select(SiteID, `General Location Name`, `Intended Purpose`, `Latitude`, `Longitude`, `System Type`, `Region Code`, 
                `Survey123 Filter Count`, `Survey123 Core Count`) %>%
  sf::st_as_sf(.,coords=c('Longitude', 'Latitude'), crs=WGS84_SRID) %>% # st_as_sf combines fields into geometry field to create sf object
  dplyr::mutate(geom = gsub(geometry, pattern="(\\))|(\\()|c",replacement = "")) %>% # remove geometry characters
  tidyr::separate(geom, into=c("lon","lat"),sep=",") %>% # reseparate out lat, long
  dplyr::rename(site_id = SiteID) %>%
  dplyr::rename(general_location_name = `General Location Name`) %>%
  dplyr::rename(projects = `Intended Purpose`) %>%
  dplyr::rename(system_type = `System Type`) %>%
  dplyr::rename(region_code = `Region Code`) %>%
  dplyr::rename(s123_filter_count = `Survey123 Filter Count`) %>%
  dplyr::rename(s123_core_count = `Survey123 Core Count`) %>%
  sf::as_Spatial()
# displays column names & number of rows - 65

site_id_prjs <- site_ids_gsheet %>%
  dplyr::select(SiteID, `Intended Purpose`)  %>%
  dplyr::rename(site_id = SiteID) %>%
  dplyr::rename(sid_prjs = `Intended Purpose`) %>%
  dplyr::add_row(site_id="other", sid_prjs="Other")

boundingbox_sids<-raster::extent(site_ids_spdf@bbox)
rm(site_ids_gsheet)

# add seasons to survey_sub
# add seasons to survey_sub
survey_sub <- add_seasons_df(survey_sub)
survey_sub$system_type <- plyr::mapvalues(survey_sub$system_type, from=c("P", "C", "E", "S", "L", "A", "other"), to=c("Pelagic", "Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))
# add seasons to survey_crew_join
survey_crew_join <- add_seasons_df(survey_crew_join)
survey_crew_join$system_type <- plyr::mapvalues(survey_crew_join$system_type, from=c("P", "C", "E", "S", "L", "A", "other"), to=c("Pelagic", "Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))
# add seasons to survey_collection_join
survey_collection_join <- add_seasons_df(survey_collection_join)
survey_collection_join$system_type <- plyr::mapvalues(survey_collection_join$system_type, from=c("P", "C", "E", "S", "L", "A", "other"), to=c("Pelagic", "Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))
# add seasons to survey_envmeas_join
survey_envmeas_join <- add_seasons_df(survey_envmeas_join)
survey_envmeas_join$system_type <- plyr::mapvalues(survey_envmeas_join$system_type, from=c("P", "C", "E", "S", "L", "A", "other"), to=c("Pelagic", "Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))
# add seasons to clean_filter_join
clean_filter_join <- add_seasons_df(clean_filter_join)
clean_filter_join$system_type <- plyr::mapvalues(clean_filter_join$system_type, from=c("P", "C", "E", "S", "L", "A", "other"), to=c("Pelagic", "Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))
# add seasons to clean_subcore_join
clean_subcore_join <- add_seasons_df(clean_subcore_join)
clean_subcore_join$system_type <- plyr::mapvalues(clean_subcore_join$system_type, from=c("P", "C", "E", "S", "L", "A", "other"), to=c("Pelagic", "Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))

################################################################################