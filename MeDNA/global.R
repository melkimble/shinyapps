library(leaflet)
library(dplyr)
library(googlesheets4)
library(data.table)
library(tidyr)
library(rgdal)
library(shiny)
library(spdplyr)
library(raster)
library(sf)

add_seasons_df <- function(df) {
  df<-df %>%
    mutate(season = case_when(survey_month==12 | survey_month==1 | survey_month==2 ~ "Winter", 
                              survey_month==3 | survey_month==4 | survey_month==5 ~ "Spring",
                              survey_month==6 | survey_month==7 | survey_month==8 ~ "Summer",
                              survey_month==9 | survey_month==10 | survey_month==11 ~ "Fall")) %>%
    mutate(season_year = paste0(season," ",survey_year))
  return(df)
}

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

var_fmt=data.table(var=c("env_measurements", "envmeas_depth", "water_temp",
                         "salinity", "turbidity",
                         "conductivity", "do",
                         "par1", "par2",
                         "water_depth", "depth_core_collected"),
                   var_fmt=c("Environmental Measurements", "Measurement Depth (M)", "Water Temperature (°C)",
                             "Salinity (Practical Salinity Unit)", "Turbidity (Formazin Nephelometric Unit)",
                             "Conductivity (μS/cm)", "Dissolved Oxygen (mg/L)",
                             "PAR1 (μmoles/sec/m²)", "PAR2 (μmoles/sec/m²)",
                             "Water Collection Depth (M)", "Core Collection Depth (M)"))

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
# Authorize googlesheets4
# https://googlesheets4.tidyverse.org/reference/gs4_auth.html
# https://googlesheets4.tidyverse.org/reference/gs4_auth_configure.html
# https://googlesheets4.tidyverse.org/reference/index.html#section-auth

googlesheets4::gs4_auth(email="melissa.kimble@maine.edu", path="/srv/shiny-server/.secrets/survey123-fefda012b951.json")

# load gsheet
sid_gsheet_url = "https://docs.google.com/spreadsheets/d/1TzlvUuaedzW0Q8JgPepYvmos0M4y1f-M_TMvLy6ZaJI/edit?usp=sharing"
#s123_gsheet_url = "https://docs.google.com/spreadsheets/d/1IVn6Ui80VZZrFjjRCy6FaOwh_5fR9ZJj4OxNypYBduc/edit?usp=sharing"
site_ids_gsheet <- googlesheets4::read_sheet(sid_gsheet_url, "lyr_view_fieldapp")
#survey_sub<-googlesheets4::read_sheet(s123_gsheet_url, "eDNA_Sampling_v14_sub")
#survey_crew_join<-googlesheets4::read_sheet(s123_gsheet_url, "survey_crew_join")
#survey_envmeas_join<-googlesheets4::read_sheet(s123_gsheet_url, "survey_envmeas_join")
#survey_collection_join<-googlesheets4::read_sheet(s123_gsheet_url, "survey_collection_join")
#clean_filter_join<-googlesheets4::read_sheet(s123_gsheet_url, "clean_filter_join")
#clean_subcore_join<-googlesheets4::read_sheet(s123_gsheet_url, "clean_subcore_join")

survey_sub <- data.table::fread(paste0(inputFolder,'eDNA_Sampling_v14_sub.csv'))
survey_crew_join <- data.table::fread(paste0(inputFolder,'survey_crew_join.csv'))
survey_envmeas_join <- data.table::fread(paste0(inputFolder,'survey_envmeas_join.csv'))
survey_collection_join <- data.table::fread(paste0(inputFolder,'survey_collection_join.csv'))
clean_filter_join <- data.table::fread(paste0(inputFolder,'clean_filter_join.csv'))
clean_subcore_join <- data.table::fread(paste0(inputFolder,'clean_subcore_join.csv'))

# FORMAT DATA
################################################################################
site_ids_spdf <- site_ids_gsheet %>%
  dplyr::select(SiteID, `General Location Name`, `Intended Purpose`, `Latitude`, `Longitude`, 
                `System Type`, `Watershed Code`, `Region Name`, 
                `Survey123 Filter Count`, `Survey123 Core Count`) %>%
  sf::st_as_sf(.,coords=c('Longitude', 'Latitude'), crs=WGS84_SRID) %>% # st_as_sf combines fields into geometry field to create sf object
  dplyr::mutate(geom = gsub(geometry, pattern="(\\))|(\\()|c",replacement = "")) %>% # remove geometry characters
  tidyr::separate(geom, into=c("lon","lat"),sep=",") %>% # reseparate out lat, long
  dplyr::rename(site_id = SiteID) %>%
  dplyr::rename(general_location_name = `General Location Name`) %>%
  dplyr::rename(project_ids = `Intended Purpose`) %>%
  dplyr::rename(system_type = `System Type`) %>%
  dplyr::rename(watershed_code = `Watershed Code`) %>%
  dplyr::rename(region_name = `Region Name`) %>%
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
survey_sub <- add_seasons_df(survey_sub)
survey_sub$system_type <- plyr::mapvalues(survey_sub$system_type, from=c("P", "C", "E", "S", "L", "A", "other"), to=c("Pelagic", "Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))
survey_sub <- survey_sub %>% dplyr::rename(gid=survey_global_id)
# add seasons to survey_crew_join
survey_crew_join <- add_seasons_df(survey_crew_join)
survey_crew_join$system_type <- plyr::mapvalues(survey_crew_join$system_type, from=c("P", "C", "E", "S", "L", "A", "other"), to=c("Pelagic", "Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))
survey_crew_join <- survey_crew_join %>% dplyr::rename(gid=crew_global_id)
# add seasons to survey_collection_join
survey_collection_join <- add_seasons_df(survey_collection_join)
survey_collection_join$system_type <- plyr::mapvalues(survey_collection_join$system_type, from=c("P", "C", "E", "S", "L", "A", "other"), to=c("Pelagic", "Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))
survey_collection_join <- survey_collection_join %>% dplyr::rename(gid=collection_global_id)
# add seasons to survey_envmeas_join
survey_envmeas_join <- add_seasons_df(survey_envmeas_join)
survey_envmeas_join$system_type <- plyr::mapvalues(survey_envmeas_join$system_type, from=c("P", "C", "E", "S", "L", "A", "other"), to=c("Pelagic", "Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))
survey_envmeas_join <- survey_envmeas_join %>% dplyr::rename(gid=envmeas_global_id)
# add seasons to clean_filter_join
clean_filter_join <- add_seasons_df(clean_filter_join)
clean_filter_join$system_type <- plyr::mapvalues(clean_filter_join$system_type, from=c("P", "C", "E", "S", "L", "A", "other"), to=c("Pelagic", "Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))
clean_filter_join <- clean_filter_join %>% dplyr::rename(gid=filter_global_id)
# remove any filter_labels with "delete"
clean_filter_join <- clean_filter_join %>% 
  dplyr::mutate(filter_label_lower=tolower(filter_label)) %>%
  dplyr::filter(!grepl("delete", filter_label_lower)) %>%
  dplyr::select(-filter_label_lower)
# add seasons to clean_subcore_join
clean_subcore_join <- add_seasons_df(clean_subcore_join)
clean_subcore_join$system_type <- plyr::mapvalues(clean_subcore_join$system_type, from=c("P", "C", "E", "S", "L", "A", "other"), to=c("Pelagic", "Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))
clean_subcore_join <- clean_subcore_join %>% dplyr::rename(gid=sample_global_id)
################################################################################
