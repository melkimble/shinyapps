library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(rgdal)
library(viridis)
library(shiny)
library(leaflet)
library(spdplyr)
library(raster)
library(sf)
library(DT)
library(formattable)

function(input, output, session) {
  theme_set(theme_bw())
  observe({
    if (input$navbar == "Maps") {
      # only render if the tab is "maps"
      output$map_survey <-output$map_crew <- output$map_envmeas <- output$map_col <- output$map_filters <- output$map_subcores <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          setView(lng=-68.5, lat=44.0, zoom=7)
      })
      if (input$tab_maps == "Surveys") {
        perc_var_txt<-input$tab_maps
        proxy <- leafletProxy("map_survey")
        df_map <- survey_sub
      }
      if (input$tab_maps == "Crew") {
        perc_var_txt<-input$tab_maps
        proxy <- leafletProxy("map_crew")
        df_map <- survey_crew_join %>%
          dplyr::filter(if_all(c(crew_fname, crew_lname), ~ !is.na(.)))
      }
      if (input$tab_maps == "Env Measurements") {
        select_var_maps<-input$map_selectedvar_envmeas
        perc_var_txt<-tools::toTitleCase(gsub("_"," ",select_var_maps))
        proxy <- leafletProxy("map_envmeas")
        df_map <- survey_envmeas_join  %>%
          dplyr::mutate(env_measurements=na_if(env_measurements, "")) %>%
          dplyr::filter(!if_any(c(select_var_maps, site_id, env_measurements), is.na))
      }
      if (input$tab_maps == "Collections") {
        proxy <- leafletProxy("map_col")
        if (input$map_selected_coltype == "collection_type"){
          perc_var_txt<-input$tab_maps
          df_map = survey_collection_join %>%
            dplyr::mutate(collection_type=na_if(collection_type, "")) %>%
            dplyr::filter(!if_any(c(collection_type, site_id), is.na))
        } else {
          perc_var_txt<-tools::toTitleCase(gsub("_"," ",input$map_selected_coltype))
          df_map <- survey_collection_join %>%
            dplyr::filter(!if_any(c(collection_type, site_id), is.na) & collection_type==input$map_selected_coltype)
        }
      }
      if (input$tab_maps == "Filters") {
        proxy <- leafletProxy("map_filters")
        if (input$map_selected_filtertype == "filter_type"){
          perc_var_txt<-input$tab_maps
          df_map <- clean_filter_join %>%
            dplyr::mutate(filter_type=na_if(filter_type, "")) %>%
            dplyr::filter(!if_any(c(survey_GlobalID, filter_GlobalID, filter_type, filter_label), is.na))
        } else {
          perc_var_txt<-tools::toTitleCase(gsub("_"," ",input$map_selected_filtertype))
          df_map = clean_filter_join %>%
            dplyr::mutate(filter_type=na_if(filter_type, "")) %>%
            dplyr::filter(!if_any(c(survey_GlobalID, filter_GlobalID, filter_type, filter_label), is.na) & filter_type==input$map_selected_filtertype)
        }
      }
      if (input$tab_maps == "SubCores") {
        perc_var_txt<-input$tab_maps
        proxy <- leafletProxy("map_subcores")
        df_map <- clean_subcore_join %>%
          dplyr::filter(!if_any(c(survey_GlobalID, collection_GlobalID), is.na))
      }
      
      ## SURVEY SUMMARY MAP SETUP
      rcdext_sids_all <- df_map %>%
        dplyr::filter(!if_any(c(site_id), is.na)) %>%
        dplyr::select(site_id) %>%
        dplyr::filter(site_id!="other") %>%
        dplyr::distinct() %>%
        dplyr::left_join(as.data.frame(site_ids_spdf)) %>%
        dplyr::mutate(RecordsExist="Yes") %>%
        dplyr::select(site_id, RecordsExist) %>%
        dplyr::full_join(as.data.frame(site_ids_spdf)) %>%
        dplyr::mutate(id = row_number()) %>%
        dplyr::mutate(id = paste0("s",id)) %>%
        dplyr::mutate(RecordsExist = ifelse(is.na(RecordsExist), "No", RecordsExist)) %>%
        dplyr::select(id, site_id, general_location_name, RecordsExist, 
                      projects, system_type, watershed_code, lon, lat) %>%
        dplyr::arrange(site_id) 
      
      rcdext_sids_other <- df_map %>%
        dplyr::filter(!if_any(c(site_id), is.na)) %>%
        dplyr::filter(site_id == "other") %>%
        dplyr::filter(lat_manual > 0 | long_manual > 0) %>%
        dplyr::select(site_id, general_location_name, projects, lat_manual, long_manual) %>%
        dplyr::distinct() %>%
        dplyr::rename(lat=lat_manual) %>%
        dplyr::rename(lon=long_manual) %>%
        dplyr::mutate(RecordsExist="Yes") %>%
        dplyr::mutate(watershed_code="other") %>%
        dplyr::mutate(system_type="other") %>%
        dplyr::mutate(id = row_number()) %>%
        dplyr::mutate(id = paste0("o",id)) %>%
        dplyr::select(id, everything()) %>%
        dplyr::arrange(general_location_name)
      
      rcdext_sids_all_spdf <- rcdext_sids_all %>%
        sf::st_as_sf(., coords=c('lon', 'lat'), crs=WGS84_SRID) %>% # st_as_sf combines fields into geometry field to create sf object
        dplyr::mutate(geom=gsub(geometry, pattern="(\\))|(\\()|c", replacement="")) %>% # remove geometry characters
        tidyr::separate(geom, into=c("lon","lat"), sep=",") %>% # reseparate out lat, long
        sf::as_Spatial()
      
      if (nrow(rcdext_sids_other) > 0) {
        rcdext_sids_other_spdf <- rcdext_sids_other %>% 
          sf::st_as_sf(., coords=c('lon', 'lat'), crs=WGS84_SRID) %>% # st_as_sf combines fields into geometry field to create sf object
          dplyr::mutate(geom=gsub(geometry, pattern="(\\))|(\\()|c", replacement="")) %>% # remove geometry characters
          tidyr::separate(geom, into=c("lon", "lat"), sep=",") %>% # reseparate out lat, long
          sf::as_Spatial()
      }
      
      unique_survey_gids <- unique(survey_sub$survey_GlobalID)
      unique_tab_gids <- unique(df_map$survey_GlobalID) 
      perc_gids <- (length(unique_tab_gids)/length(unique_survey_gids))*100
      perc_sids <- rcdext_sids_all %>%
        dplyr::group_by(RecordsExist) %>%
        dplyr::summarise(count=n()) %>%
        dplyr::mutate(perc = formattable::percent(count / sum(count)))
      
      output$maps_text_survey <- output$maps_text_crew <-output$maps_text_envmeas <-output$maps_text_col <-output$maps_text_filters <-output$maps_text_subcores <-renderUI({
        str1 <- sprintf("Percentage of Survey Records with %s: %g %%", perc_var_txt, perc_gids)
        str2 <- sprintf("Percentage of Site IDs with %s: %s", perc_var_txt, perc_sids$perc[perc_sids$RecordsExist=="Yes"])
        HTML(paste(str1, str2, sep = '<br/>'))
      })
      
      if (nrow(rcdext_sids_other)>0) {
        rcdext_df <- rbind(data.table(rcdext_sids_all), data.table(rcdext_sids_other), fill=TRUE)
      } else {
        rcdext_df <- as.data.frame(rcdext_sids_all)
      }
      
      # SURVEY SUMMARY MAP TABLE
      output$table_map_survey <- output$table_map_envmeas <- output$table_map_crew <- output$table_map_col <- output$table_map_filters <- output$table_map_subcores <-DT::renderDataTable({
        df <- rcdext_df %>% 
          dplyr::mutate(across(c(lat, lon), as.numeric)) %>%
          dplyr::mutate(ZoomTo = paste('<a class="go-map" href="" data-lat="', lat, '" data-lon="', lon, '" data-id="', id, '"><i class="fa fa-crosshairs"></i></a>', sep="")) %>%
          dplyr::select(ZoomTo, everything())
        zoomto <- DT::dataTableAjax(session, df)
        #DT::datatable(rcdext_df, options = list(scrollX = TRUE, pageLength = 5, ajax = list(url = zoomto)), escape = FALSE, rownames = FALSE)
        DT::datatable(df, options = list(scrollX = TRUE, pageLength = 5, ajax = list(url = zoomto)), escape = FALSE)
        
      })
      
      # SURVEY SUMMARY MAP
      sids_popup <- sprintf(
        "<strong>%s: %s</strong><br/>Has %s records: %s<br/>%s",
        rcdext_sids_all_spdf$site_id, rcdext_sids_all_spdf$general_location_name,
        input$tab_maps,
        rcdext_sids_all_spdf$RecordsExist, rcdext_sids_all_spdf$projects) %>% 
        lapply(htmltools::HTML)
      
      proxy %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data=WBD_spdf_simplified, layerId=~region_cod,
                    fillColor="grey", fillOpacity=0.5, 
                    color="#444444", opacity=1,
                    weight=1,  dashArray="3",
                    group="Watersheds") %>%
        addLabelOnlyMarkers(data=centers_wbd, lng=~x, lat=~y, label=~region_cod,
                            labelOptions = labelOptions(noHide = TRUE, direction = 'middle', textOnly = TRUE),
                            group="Watersheds") %>%
        hideGroup("Watersheds") %>%
        addCircleMarkers(data=rcdext_sids_all_spdf, layerId=~id, 
                         fillColor=~existsPal(RecordsExist), fillOpacity=0.8, 
                         color="#444444", opacity=1, popup=sids_popup,
                         weight=1, radius=5, stroke=TRUE,
                         group="Site IDs") %>%
        addLegend(data=rcdext_sids_all_spdf, pal=existsPal, values=~RecordsExist, opacity = 1)
      if (nrow(rcdext_sids_other)>0) {
       osids_popup <- sprintf(
          "<strong>%s: %s</strong><br/>Has %s records: %s<br/>%s",
          rcdext_sids_other_spdf$site_id, rcdext_sids_other_spdf$general_location_name,
          input$tab_maps,
          rcdext_sids_other_spdf$RecordsExist, rcdext_sids_other_spdf$projects) %>% 
          lapply(htmltools::HTML)
        proxy %>% addCircleMarkers(data=rcdext_sids_other_spdf, layerId=~id, 
                                   fillColor=~existsPal(RecordsExist), fillOpacity=0.8, 
                                   color="#444444", opacity=1, popup=osids_popup,
                                   weight=1, radius=5, stroke=TRUE,
                                   group="Other Sites") %>%
          addLayersControl(
            overlayGroups = c("Watersheds", "Site IDs", "Other Sites"),
            options = layersControlOptions(collapsed = FALSE))
      } else {
        proxy %>%
          addLayersControl(
            overlayGroups = c("Watersheds", "Site IDs"),
            options = layersControlOptions(collapsed = FALSE))
      }
      
      showSitePopup <- function(ID, lat, lng, proxy) {
        selected_site_id <- rcdext_df$site_id[rcdext_df$id == ID][1]
        selected_site_name <- rcdext_df$general_location_name[rcdext_df$id == ID][1]
        selected_exists <- rcdext_df$RecordsExist[rcdext_df$id == ID][1]
        selected_prj <- rcdext_df$projects[rcdext_df$id == ID][1]
        content<-sprintf(
          "<strong>%s: %s</strong><br/>Has %s records: %s<br/>%s",
          selected_site_id, selected_site_name,
          input$tab_maps,
          selected_exists, selected_prj) %>% 
          lapply(htmltools::HTML)
        #print(proxy$id)
        #addPopups(lng, lat, content, layerId=ID)
        proxy %>% addPopups(lng, lat, content, layerId=ID)
      }
      
      # if click in table, show popup
      if (is.null(input$goto))
        return()
      isolate({
        proxy %>% clearPopups()
        dist <- 0.05
        
        lat <- input$goto$lat
        lng <- input$goto$lng
        ID <- input$goto$id
        
        showSitePopup(ID, lat, lng, proxy)
        proxy %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist*10)
      })
      
      
    }
    
    if (input$navbar == "Barplot Counts") {
      if (input$tab_barplots == "Surveys") {
        selected_barplot <- input$barplot_selected_survey
        selected_projects <- input$barplot_selprj_survey
        df_bplot <- survey_sub
      }
      if (input$tab_barplots == "Crew") {
        selected_barplot <- input$barplot_selected_crew
        selected_projects <- input$barplot_selprj_crew
        df_bplot <- survey_crew_join %>%
          dplyr::filter(!if_all(c(crew_fname, crew_lname), is.na))
      }
      if (input$tab_barplots == "Env Measurements") {
        selected_barplot<-input$barplot_selected_envmeas
        selected_projects<-input$barplot_selprj_envmeas
        df_bplot <- survey_envmeas_join  %>%
          dplyr::mutate(env_measurements=na_if(env_measurements, "")) %>%
          dplyr::filter(!if_any(c(site_id, env_measurements), is.na))
      }
      if (input$tab_barplots == "Collections") {
        selected_barplot<-input$barplot_selected_col
        selected_projects<-input$barplot_selprj_col
        selected_coltype<-input$barplot_selected_coltype
        if (selected_coltype == "collection_type") {
          df_bplot <- survey_collection_join %>%
            dplyr::mutate(collection_type=na_if(collection_type, "")) %>%
            dplyr::filter(!if_any(c(collection_type, site_id), is.na))
        } else {
          df_bplot <- survey_collection_join %>%
            dplyr::mutate(collection_type=na_if(collection_type, "")) %>%
            dplyr::filter(!if_any(c(collection_type, site_id), is.na) & collection_type==selected_coltype)
        }
      }
      if (input$tab_barplots == "Filters") {
        selected_barplot<-input$barplot_selected_filters
        selected_projects<-input$barplot_selprj_filters
        selected_filtertype<-input$barplot_selected_filtertype
        selected_filtertype_fmt<-tools::toTitleCase(gsub("_"," ",selected_filtertype))
        if (selected_filtertype=="filter_type") {
          df_bplot <- clean_filter_join %>%
            dplyr::mutate(filter_type=na_if(filter_type, "")) %>%
            filter(!if_any(c(collection_GlobalID, filter_GlobalID, filter_type, filter_label), is.na))
        } else {
          df_bplot <- clean_filter_join %>%
            dplyr::mutate(filter_type=na_if(filter_type, "")) %>%
            filter(!if_any(c(collection_GlobalID, filter_GlobalID, filter_type, filter_label), is.na) & filter_type==selected_filtertype)
        }
        cids <- df_bplot %>%
          distinct(collection_GlobalID)
        
        cids_filter <- df_bplot %>%
          dplyr::select(collection_GlobalID, filter_type) %>% 
          dplyr::mutate(is_nitex=factor(ifelse(filter_type=="nitex", "nitex", "not_nitex"))) %>%
          dplyr::distinct(collection_GlobalID, is_nitex) %>%
          dplyr::group_by(collection_GlobalID) %>%
          dplyr::count(collection_GlobalID) %>%
          dplyr::group_by(n) %>%
          dplyr::count(n) %>%
          dplyr::mutate(perc_prefilter=(nn/nrow(cids))*100)
        
        output$barplot_perc_filters <- renderUI({
          str1 <- sprintf("Percentage of Water Collections with prefilters & filters: %g %%", round(cids_filter$perc_prefilter[cids_filter$n==2],2))
          str2 <- sprintf("Percentage of Water Collections with only prefilters or filters (not both): %g %%", round(cids_filter$perc_prefilter[cids_filter$n==1],2))
          HTML(paste(str1, str2,  sep = '<br/>'))
        })
        
      }
      if (input$tab_barplots == "SubCores") {
        selected_barplot <- input$barplot_selected_subcores
        selected_projects <- input$barplot_selprj_subcores
        df_bplot <- clean_subcore_join %>%
          dplyr::filter(!if_any(c(survey_GlobalID, collection_GlobalID), is.na))
      }
      selected_projects <- gsub("[()]", "", selected_projects)
      selected_projects <- tolower(selected_projects)
      
      # Subset df_bplot based on selected projects in plot view
      df_sub_prj <- df_bplot %>%
        dplyr::mutate(projects=na_if(projects,"")) %>%
        dplyr::mutate(projects = replace_na(projects, "Maine eDNA")) %>%
        dplyr::left_join(site_id_prjs) %>%
        dplyr::mutate(sid_prjs_sub = gsub(",", " ", sid_prjs)) %>%
        dplyr::mutate(sid_prjs_sub = gsub("[()]", "",sid_prjs_sub)) %>%
        dplyr::mutate(sid_prjs_sub = tolower(sid_prjs_sub)) %>%
        dplyr::filter(grepl(paste(selected_projects, collapse="|"), sid_prjs_sub)) %>%
        dplyr::select(-sid_prjs_sub)
      
      
      ## RENDER TEXT: NROWS
      output$barplot_sum_survey <- output$barplot_sum_crew <- output$barplot_sum_envmeas <- output$barplot_sum_col <- output$barplot_sum_filters <- output$barplot_sum_subcores <- renderText({
        sprintf("Total number of rows: %g", nrow(df_sub_prj))
      })
      
      # only render if on barplot counts tab
      # SURVEY SUMMARY TABLE
      output$table_barplot_survey <- output$table_barplot_crew <- output$table_barplot_envmeas <- output$table_barplot_col <- output$table_barplot_filters <- output$table_barplot_subcores <- DT::renderDataTable({
        if (selected_barplot == "barSRSeason") {
          DT::datatable(df_sub_prj %>%
                          dplyr::filter(!if_any(c(survey_GlobalID, season), is.na)) %>%
                          dplyr::select(survey_GlobalID, season) %>% 
                          dplyr::group_by(season) %>%
                          dplyr::count(season) %>%
                          dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                          dplyr::mutate(season=factor(season, levels= c("Winter", "Spring", "Summer", "Fall"))) %>%
                          dplyr::arrange(season), options = list(scrollX = TRUE, pageLength = 5))
        } else if (selected_barplot == "barSRSystem") {
          DT::datatable(df_sub_prj %>%
                          dplyr::filter(!if_any(c(survey_GlobalID, system_type), is.na)) %>%
                          dplyr::select(survey_GlobalID, system_type) %>% 
                          dplyr::group_by(system_type) %>%
                          dplyr::count(system_type) %>%
                          dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                          dplyr::mutate(system_type=factor(system_type, levels= c("Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))) %>%
                          dplyr::arrange(system_type), options = list(scrollX = TRUE, pageLength = 5))
        } else if (selected_barplot == "barSRSeasonYear") {
          DT::datatable(df_sub_prj %>%
                          dplyr::filter(!if_any(c(survey_GlobalID, season_year), is.na)) %>%
                          dplyr::select(survey_GlobalID, season_year) %>% 
                          dplyr::group_by(season_year) %>%
                          dplyr::count(season_year) %>%
                          dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                          tidyr::separate(season_year, into=c("season","year"),sep=" ") %>%
                          dplyr::mutate(season=factor(season, levels= c("Winter", "Spring", "Summer", "Fall"))) %>%
                          dplyr::arrange(season, year) %>%
                          dplyr::left_join(df_sub_prj %>%
                                             dplyr::filter(!if_any(c(survey_GlobalID, season_year), is.na)) %>%
                                             dplyr::select(survey_GlobalID, season_year) %>% 
                                             dplyr::group_by(season_year) %>%
                                             dplyr::count(season_year) %>% 
                                             tidyr::separate(season_year, into=c("season","year"),sep=" ") %>%
                                             dplyr::group_by(season) %>%
                                             dplyr::summarize(
                                               avg_n_season = mean(n, na.rm=TRUE),
                                               sd_n_season = sd(n, na.rm=TRUE)
                                             ) 
                          ) %>% 
                          mutate_if(is.numeric, round, 2), options = list(scrollX = TRUE, pageLength = 5))
        } else if (selected_barplot == "barSRSystemYear") {
          DT::datatable(df_sub_prj %>%
                          dplyr::filter(!if_any(c(survey_GlobalID, survey_year, system_type), is.na)) %>%
                          dplyr::select(survey_GlobalID, survey_year, system_type) %>% 
                          dplyr::mutate(sys_year=paste0(system_type, " ", survey_year)) %>%
                          dplyr::group_by(sys_year) %>%
                          dplyr::count(sys_year) %>%
                          dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                          tidyr::separate(sys_year, into=c("system_type","year"),sep=" ") %>%
                          dplyr::mutate(system_type=factor(system_type, levels= c("Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))) %>%
                          dplyr::arrange(system_type, year) %>%
                          dplyr::left_join(df_sub_prj %>%
                                             dplyr::filter(!if_any(c(survey_GlobalID, survey_year, system_type), is.na)) %>%
                                             dplyr::select(survey_GlobalID, survey_year, system_type) %>% 
                                             dplyr::mutate(sys_year=paste0(system_type, " ", survey_year)) %>%
                                             dplyr::group_by(sys_year) %>%
                                             dplyr::count(sys_year) %>%
                                             tidyr::separate(sys_year, into=c("system_type","year"),sep=" ") %>%
                                             dplyr::group_by(system_type) %>%
                                             dplyr::summarize(
                                               avg_n_system = mean(n, na.rm=TRUE),
                                               sd_n_system = sd(n, na.rm=TRUE)
                                             ) 
                          ) %>% 
                          mutate_if(is.numeric, round, 2), options = list(scrollX = TRUE, pageLength = 5))
          
        } else if (selected_barplot == "barSRMonthYear") {
          DT::datatable(df_sub_prj %>%
                          dplyr::filter(!if_any(c(survey_GlobalID, survey_date), is.na)) %>%
                          dplyr::select(survey_GlobalID, survey_date) %>% 
                          dplyr::mutate(survey_date = lubridate::ymd(survey_date, tz="UTC")) %>%
                          dplyr::mutate(survey_yrmo = format(survey_date, format="%b %Y")) %>%
                          dplyr::group_by(survey_yrmo) %>%
                          dplyr::count(survey_yrmo) %>%
                          dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                          tidyr::separate(survey_yrmo, into=c("month","year"),sep=" ") %>%
                          dplyr::mutate(month=factor(month, levels= c("Jan", "Feb", "Mar", 
                                                                      "Apr", "May", "Jun", 
                                                                      "Jul", "Aug", "Sep", 
                                                                      "Oct", "Nov", "Dec"))) %>%
                          dplyr::arrange(month, year) %>%
                          dplyr::left_join(df_sub_prj %>%
                                             dplyr::filter(!if_any(c(survey_GlobalID, survey_date), is.na)) %>%
                                             dplyr::select(survey_GlobalID, survey_date) %>% 
                                             dplyr::mutate(survey_date = lubridate::ymd(survey_date, tz="UTC")) %>%
                                             dplyr::mutate(survey_yrmo = format(survey_date, format="%b %Y")) %>%
                                             dplyr::group_by(survey_yrmo) %>%
                                             dplyr::count(survey_yrmo) %>%
                                             tidyr::separate(survey_yrmo, into=c("month","year"),sep=" ") %>%
                                             dplyr::group_by(month) %>%
                                             dplyr::summarize(
                                               avg_n_month = mean(n, na.rm=TRUE),
                                               sd_n_month = sd(n, na.rm=TRUE)
                                             ) 
                          ) %>% 
                          dplyr::mutate_if(is.numeric, round, 2), options = list(scrollX = TRUE, pageLength = 5))
        } else if (selected_barplot == "barSRSiteYear") {
          DT::datatable(df_sub_prj %>%
                          dplyr::filter(!if_any(c(survey_GlobalID, survey_date), is.na)) %>%
                          dplyr::select(survey_GlobalID, site_id, survey_year) %>% 
                          dplyr::mutate(sid_year=paste0(site_id, " ", survey_year)) %>%
                          dplyr::group_by(sid_year) %>%
                          dplyr::count(sid_year) %>%
                          dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                          tidyr::separate(sid_year, into=c("site_id","year"),sep=" ") %>%
                          dplyr::left_join(as.data.frame(site_ids_spdf)) %>%
                          dplyr::select(site_id, general_location_name, year, n, perc) %>%
                          dplyr::arrange(site_id, year) %>%
                          dplyr::left_join(df_sub_prj %>%
                                             dplyr::filter(!if_any(c(survey_GlobalID, survey_date), is.na)) %>%
                                             dplyr::select(survey_GlobalID, site_id, survey_year) %>% 
                                             dplyr::mutate(sid_year=paste0(site_id, " ", survey_year)) %>%
                                             dplyr::group_by(sid_year) %>%
                                             dplyr::count(sid_year) %>%
                                             dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                                             tidyr::separate(sid_year, into=c("site_id","year"),sep=" ") %>%
                                             dplyr::group_by(site_id) %>%
                                             dplyr::summarize(
                                               avg_n_site = mean(n, na.rm=TRUE),
                                               sd_n_site = sd(n, na.rm=TRUE)
                                             ) 
                          ) %>% 
                          dplyr::mutate_if(is.numeric, round, 2), options = list(scrollX = TRUE, pageLength = 5))
        } else if (selected_barplot == "barFTSeason") {
          DT::datatable(df_sub_prj %>%
                          dplyr::mutate(filter_type=na_if(filter_type, "")) %>%
                          tidyr::drop_na(survey_GlobalID, filter_GlobalID, season, filter_type, filter_label) %>%
                          dplyr::select(filter_GlobalID, filter_type, season) %>% 
                          dplyr::mutate(ft_st=paste0(filter_type, " ", season)) %>%
                          dplyr::group_by(ft_st) %>%
                          dplyr::count(ft_st) %>%
                          dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                          tidyr::separate(ft_st, into=c("filter_type","season"),sep=" ") %>%
                          dplyr::mutate(filter_type=factor(filter_type, levels= c("nitex", "gff", "supor", "cn", "other"))) %>%
                          dplyr::mutate(season=factor(season, levels= c("Winter", "Spring", "Summer", "Fall"))) %>%
                          dplyr::arrange(filter_type, season) %>%
                          dplyr::left_join(df_sub_prj %>%
                                             dplyr::mutate(filter_type=na_if(filter_type, "")) %>%
                                             tidyr::drop_na(survey_GlobalID, filter_GlobalID, season, filter_type, filter_label) %>%
                                             dplyr::select(filter_GlobalID, filter_type, season) %>% 
                                             dplyr::mutate(ft_st=paste0(filter_type, " ", season)) %>%
                                             dplyr::group_by(ft_st) %>%
                                             dplyr::count(ft_st) %>%
                                             tidyr::separate(ft_st, into=c("filter_type","season"),sep=" ") %>%
                                             dplyr::group_by(season) %>%
                                             dplyr::summarize(
                                               avg_n_season = mean(n, na.rm=TRUE),
                                               sd_n_season = sd(n, na.rm=TRUE)
                                             ) 
                          ) %>% 
                          dplyr::mutate_if(is.numeric, round, 2), options = list(scrollX = TRUE, pageLength = 5))
        } else if (selected_barplot == "barFTSystem") {
          DT::datatable(df_sub_prj %>%
                          dplyr::mutate(filter_type=na_if(filter_type, "")) %>%
                          tidyr::drop_na(survey_GlobalID, filter_GlobalID, season, filter_type, filter_label) %>%
                          dplyr::select(filter_GlobalID, filter_type, system_type) %>% 
                          dplyr::mutate(ft_st=paste0(filter_type, " ", system_type)) %>%
                          dplyr::group_by(ft_st) %>%
                          dplyr::count(ft_st) %>%
                          dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                          tidyr::separate(ft_st, into=c("filter_type","system_type"),sep=" ") %>%
                          dplyr::mutate(filter_type=factor(filter_type, levels= c("nitex", "gff", "supor", "cn", "other"))) %>%
                          dplyr::mutate(system_type=factor(system_type, levels= c("Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))) %>%
                          dplyr::arrange(filter_type, system_type) %>%
                          dplyr::left_join(df_sub_prj %>%
                                             dplyr::mutate(filter_type=na_if(filter_type, "")) %>%
                                             tidyr::drop_na(survey_GlobalID, filter_GlobalID, season, filter_type, filter_label) %>%
                                             dplyr::select(filter_GlobalID, filter_type, system_type) %>% 
                                             dplyr::mutate(ft_st=paste0(filter_type, " ", system_type)) %>%
                                             dplyr::group_by(ft_st) %>%
                                             dplyr::count(ft_st) %>%
                                             tidyr::separate(ft_st, into=c("filter_type","system_type"),sep=" ") %>%
                                             dplyr::group_by(system_type) %>%
                                             dplyr::summarize(
                                               avg_n_system = mean(n, na.rm=TRUE),
                                               sd_n_system = sd(n, na.rm=TRUE)
                                             ) 
                          ) %>% 
                          dplyr::mutate_if(is.numeric, round, 2), options = list(scrollX = TRUE, pageLength = 5))
        } else if (selected_barplot == "barFTYear") {
          DT::datatable(df_sub_prj %>%
                          dplyr::mutate(filter_type=na_if(filter_type, "")) %>%
                          tidyr::drop_na(survey_GlobalID, filter_GlobalID, season, filter_type, filter_label) %>%
                          dplyr::select(filter_GlobalID, filter_type, survey_year) %>% 
                          dplyr::mutate(ft_year=paste0(filter_type, " ", survey_year)) %>%
                          dplyr::group_by(ft_year) %>%
                          dplyr::count(ft_year) %>%
                          dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                          tidyr::separate(ft_year, into=c("filter_type","year"),sep=" ") %>%
                          dplyr::mutate(filter_type=factor(filter_type, levels= c("nitex", "gff", "supor", "cn", "other"))) %>%
                          dplyr::arrange(filter_type, year) %>%
                          dplyr::left_join(df_sub_prj %>%
                                             dplyr::mutate(filter_type=na_if(filter_type, "")) %>%
                                             tidyr::drop_na(survey_GlobalID, filter_GlobalID, season, filter_type, filter_label) %>%
                                             dplyr::select(filter_GlobalID, filter_type, survey_year) %>% 
                                             dplyr::mutate(ft_year=paste0(filter_type, " ", survey_year)) %>%
                                             dplyr::group_by(ft_year) %>%
                                             dplyr::count(ft_year) %>%
                                             tidyr::separate(ft_year, into=c("filter_type","year"),sep=" ") %>%
                                             dplyr::group_by(year) %>%
                                             dplyr::summarize(
                                               avg_n_year = mean(n, na.rm=TRUE),
                                               sd_n_year = sd(n, na.rm=TRUE)
                                             ) 
                          ) %>% 
                          dplyr::mutate_if(is.numeric, round, 2), options = list(scrollX = TRUE, pageLength = 5))
        } else return(NULL)
      })
      
      # SURVEY BARPLOT COUNTS
      output$barplot_survey <-output$barplot_crew <- output$barplot_envmeas <- output$barplot_col <- output$barplot_filters <- output$barplot_subcores <- renderPlot({
        if (selected_barplot == "barSRSeason") {
          #outputPngFileName <- file.path(outputPngFolder,paste0("s123_survey_all_season_count_barplot.png")) 
          plotTitle = sprintf("Count of %s Records by Season", tools::toTitleCase(input$tab_barplots))
          ggplot(df_sub_prj %>%
                   dplyr::filter(!if_any(c(survey_GlobalID, season), is.na)) %>%
                   dplyr::select(survey_GlobalID, season) %>% 
                   dplyr::group_by(season) %>%
                   dplyr::count(season) %>%
                   dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                   dplyr::mutate(season=factor(season, levels= c("Winter", "Spring", "Summer", "Fall"))), 
                 aes(season, n, fill=season)) +
            geom_bar(stat="identity") +
            xlab("Season") +
            ylab("Count") +
            ggtitle(plotTitle) +
            viridis::scale_fill_viridis(discrete = T) +
            theme(plot.title = element_text(hjust = 0.5)) +
            #theme(axis.text.x = element_text(angle=50, hjust=1)) +
            theme(legend.position = "none") 
        } else if (selected_barplot == "barSRSystem") {
          #outputPngFileName <- file.path(outputPngFolder,paste0("s123_survey_all_season_count_barplot.png")) 
          plotTitle = sprintf("Count of %s Records by System", tools::toTitleCase(input$tab_barplots))
          ggplot(df_sub_prj %>%
                   dplyr::filter(!if_any(c(survey_GlobalID, system_type), is.na)) %>%
                   dplyr::select(survey_GlobalID, system_type) %>% 
                   dplyr::group_by(system_type) %>%
                   dplyr::count(system_type) %>%
                   dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                   dplyr::mutate(system_type=factor(system_type, levels= c("Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))), 
                 aes(system_type, n, fill=system_type)) +
            geom_bar(stat="identity") +
            xlab("System") +
            ylab("Count") +
            ggtitle(plotTitle) +
            viridis::scale_fill_viridis(discrete = T) +
            theme(plot.title = element_text(hjust = 0.5)) +
            #theme(axis.text.x = element_text(angle=50, hjust=1)) +
            theme(legend.position = "none") 
        } else if (selected_barplot == "barSRSeasonYear") {
          #outputPngFileName <- file.path(outputPngFolder,paste0("s123_survey_all_season_year_count_barplot.png"))
          plotTitle = sprintf("Count of %s Records by Season and Year", tools::toTitleCase(input$tab_barplots))
          ggplot(df_sub_prj %>%
                   dplyr::filter(!if_any(c(survey_GlobalID, season_year), is.na)) %>%
                   dplyr::select(survey_GlobalID, season_year) %>% 
                   dplyr::group_by(season_year) %>%
                   dplyr::count(season_year) %>%
                   dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                   tidyr::separate(season_year, into=c("season","year"),sep=" ") %>%
                   dplyr::mutate(season=factor(season, levels= c("Winter", "Spring", "Summer", "Fall"))),
                 aes(season, n)) +
            geom_bar(aes(fill=year), position="stack", stat="identity") +
            viridis::scale_fill_viridis(discrete = T) +
            xlab("Season") +
            ylab("Count") +
            ggtitle(plotTitle) +
            #theme(axis.text.x = element_text(angle=50, hjust=1)) +
            theme(plot.title = element_text(hjust = 0.5)) 
        } else if (selected_barplot == "barSRSystemYear") {
          #outputPngFileName <- file.path(outputPngFolder,paste0("s123_survey_all_season_year_count_barplot.png"))
          plotTitle = sprintf("Count of %s Records by System and Year", tools::toTitleCase(input$tab_barplots))
          ggplot(df_sub_prj %>%
                   dplyr::filter(!if_any(c(survey_GlobalID, survey_year, system_type), is.na)) %>%
                   dplyr::select(survey_GlobalID, survey_year, system_type) %>% 
                   dplyr::mutate(sys_year=paste0(system_type, " ", survey_year)) %>%
                   dplyr::group_by(sys_year) %>%
                   dplyr::count(sys_year) %>%
                   dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                   tidyr::separate(sys_year, into=c("system_type","year"),sep=" ") %>%
                   dplyr::mutate(system_type=factor(system_type, levels= c("Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))),
                 aes(system_type, n)) +
            geom_bar(aes(fill=year), position="stack", stat="identity") +
            viridis::scale_fill_viridis(discrete = T) +
            xlab("System") +
            ylab("Count") +
            ggtitle(plotTitle) +
            #theme(axis.text.x = element_text(angle=50, hjust=1)) +
            theme(plot.title = element_text(hjust = 0.5)) 
        } else if (selected_barplot == "barSRMonthYear") {
          #outputPngFileName <- file.path(outputPngFolder,paste0("s123_survey_all_month_year_count_barplot.png"))
          plotTitle = sprintf("Count of %s Records by Month and Year", tools::toTitleCase(input$tab_barplots))
          ggplot(df_sub_prj %>%
                   dplyr::filter(!if_any(c(survey_GlobalID, survey_date), is.na)) %>%
                   dplyr::select(survey_GlobalID, survey_date) %>% 
                   dplyr::mutate(survey_date = lubridate::ymd(survey_date, tz="UTC")) %>%
                   dplyr::mutate(survey_yrmo = format(survey_date, format="%b %Y")) %>%
                   dplyr::group_by(survey_yrmo) %>%
                   dplyr::count(survey_yrmo) %>%
                   dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                   tidyr::separate(survey_yrmo, into=c("month","year"),sep=" ") %>%
                   dplyr::mutate(month=factor(month, levels= c("Jan", "Feb", "Mar", 
                                                               "Apr", "May", "Jun", 
                                                               "Jul", "Aug", "Sep", 
                                                               "Oct", "Nov", "Dec"))),
                 aes(month, n)) +
            geom_bar(aes(fill=year), position="stack", stat="identity") +
            viridis::scale_fill_viridis(discrete = T) +
            xlab("Month") +
            ylab("Count") +
            ggtitle(plotTitle) +
            #theme(axis.text.x = element_text(angle=50, hjust=1)) +
            theme(plot.title = element_text(hjust = 0.5)) 
        } else if (selected_barplot == "barSRSiteYear") {
          #outputPngFileName <- file.path(outputPngFolder,paste0("s123_survey_all_sids_year_count_barplot.png"))
          plotTitle = sprintf("Count of %s Records by Site and Year", tools::toTitleCase(input$tab_barplots))
          ggplot(df_sub_prj %>%
                   dplyr::filter(!if_any(c(survey_GlobalID, survey_date), is.na)) %>%
                   dplyr::select(survey_GlobalID, site_id, survey_year) %>% 
                   dplyr::mutate(sid_year=paste0(site_id, " ", survey_year)) %>%
                   dplyr::group_by(sid_year) %>%
                   dplyr::count(sid_year) %>%
                   dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                   tidyr::separate(sid_year, into=c("site_id","year"),sep=" "),
                 aes(site_id, n)) +
            geom_bar(aes(fill=year), position="stack", stat="identity") +
            viridis::scale_fill_viridis(discrete = T) +
            xlab("Site ID") +
            ylab("Count") +
            ggtitle(plotTitle) +
            theme(axis.text.x = element_text(angle=50, hjust=1)) +
            theme(plot.title = element_text(hjust = 0.5))
        } else if (selected_barplot == "barFTSeason") {
          plotTitle = sprintf("Count of %s by Season", selected_filtertype_fmt)
          ggplot(df_sub_prj %>%
                   dplyr::mutate(filter_type=na_if(filter_type, "")) %>%
                   tidyr::drop_na(survey_GlobalID, filter_GlobalID, season, filter_type, filter_label) %>%
                   dplyr::select(filter_GlobalID, filter_type, season) %>% 
                   dplyr::mutate(ft_st=paste0(filter_type, " ", season)) %>%
                   dplyr::group_by(ft_st) %>%
                   dplyr::count(ft_st) %>%
                   dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                   tidyr::separate(ft_st, into=c("filter_type","season"),sep=" ") %>%
                   dplyr::mutate(filter_type=factor(filter_type, levels= c("nitex", "gff", "supor", "cn", "other"))) %>%
                   dplyr::mutate(season=factor(season, levels= c("Winter", "Spring", "Summer", "Fall"))),
                 aes(season, n)) +
            geom_bar(aes(fill=filter_type), position="stack", stat="identity") +
            viridis::scale_fill_viridis(discrete = T) +
            xlab("Season") +
            ylab("Count") +
            ggtitle(plotTitle) +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(fill=selected_filtertype_fmt)
        } else if (selected_barplot == "barFTSystem") {
          plotTitle = sprintf("Count of %s by System", selected_filtertype_fmt)
          ggplot(df_sub_prj %>%
                   dplyr::mutate(filter_type=na_if(filter_type, "")) %>%
                   tidyr::drop_na(survey_GlobalID, filter_GlobalID, season, filter_type, filter_label) %>%
                   dplyr::select(filter_GlobalID, filter_type, system_type) %>% 
                   dplyr::mutate(ft_st=paste0(filter_type, " ", system_type)) %>%
                   dplyr::group_by(ft_st) %>%
                   dplyr::count(ft_st) %>%
                   dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                   tidyr::separate(ft_st, into=c("filter_type","system_type"),sep=" ") %>%
                   dplyr::mutate(filter_type=factor(filter_type, levels= c("nitex", "gff", "supor", "cn", "other"))) %>%
                   dplyr::mutate(system_type=factor(system_type, levels= c("Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))),
                 aes(system_type, n)) +
            geom_bar(aes(fill=filter_type), position="stack", stat="identity") +
            viridis::scale_fill_viridis(discrete = T) +
            xlab("System") +
            ylab("Count") +
            ggtitle(plotTitle) +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(fill=selected_filtertype_fmt)
        } else if (selected_barplot == "barFTYear") {
          plotTitle = sprintf("Count of %s by Year", selected_filtertype_fmt)
          ggplot(df_sub_prj %>%
                   dplyr::mutate(filter_type=na_if(filter_type, "")) %>%
                   tidyr::drop_na(survey_GlobalID, filter_GlobalID, season, filter_type, filter_label) %>%
                   dplyr::select(filter_GlobalID, filter_type, survey_year) %>% 
                   dplyr::mutate(ft_year=paste0(filter_type, " ", survey_year)) %>%
                   dplyr::group_by(ft_year) %>%
                   dplyr::count(ft_year) %>%
                   dplyr::mutate(perc=round((n/nrow(df_sub_prj))*100, 2)) %>%
                   tidyr::separate(ft_year, into=c("filter_type","year"),sep=" ") %>%
                   dplyr::mutate(filter_type=factor(filter_type, levels= c("nitex", "gff", "supor", "cn", "other"))),
                 aes(filter_type, n)) +
            geom_bar(aes(fill=year), position="stack", stat="identity") +
            viridis::scale_fill_viridis(discrete = T) +
            xlab(selected_filtertype_fmt) +
            ylab("Count") +
            ggtitle(plotTitle) +
            theme(plot.title = element_text(hjust = 0.5))
        } else return(NULL)
      }) 
    }
    if (input$navbar == "Summary Plots") {
      if (input$tab_plots == "Env Measurements") {
        custom_height=function() input$height_envmeas
        depth_var<-"envmeas_depth"
        selected_var_plots<-input$plots_selectedvar_envmeas
        selected_var_plots_fmt<-tools::toTitleCase(gsub("_", " ",selected_var_plots))
        selected_plot<-input$plots_selected_envmeas
        start_range<-input$range_start_envmeas
        end_range<-input$range_end_envmeas
        df_summaryplot <- survey_envmeas_join %>%
          dplyr::filter(!if_any(c(selected_var_plots, site_id), is.na)) %>%
          dplyr::select(envmeas_GlobalID, survey_date, projects, system_type, 
                        site_id, other_site_id, general_location_name, 
                        supervisor, username, recorder_first_name, recorder_last_name, 
                        envmeas_date, envmeas_depth, envmeas_instrument, 
                        ctd_filename, ctd_notes, ysi_filename, ysi_model, ysi_serial_number, ysi_notes, secchi_depth,
                        secchi_notes, niskin_number, niskin_notes, other_instruments, env_measurements, flow_rate,
                        water_temp, salinity, ph, par1, par2, turbidity,
                        conductivity, do, pheophytin, chla, no3no2, no2,
                        nh4, phosphate, bottom_substrate, lab_date, envmeas_notes,
                        lat_manual, long_manual, 
                        survey_month, survey_year, season, season_year, survey_GlobalID)
      }
      if (input$tab_plots == "Collections") {
        custom_height=function() input$height_col
        selected_plot_coltype<-input$plots_selected_coltype
        selected_plot<-input$plots_selected_col
        if (selected_plot_coltype == "water_sample"){
          depth_var<-selected_var_plots<-"water_depth"
          selected_var_plots_fmt<-tools::toTitleCase(gsub("_", " ",selected_var_plots))
          start_range<-input$range_start_col
          end_range<-input$range_end_col
          df_summaryplot <- survey_collection_join %>%
            dplyr::mutate(collection_type=na_if(collection_type, "")) %>%
            dplyr::filter(!if_any(c(survey_GlobalID, collection_GlobalID, collection_type, selected_var_plots), is.na)) %>%
            dplyr::filter(collection_type==selected_plot_coltype) %>%
            dplyr::select(collection_GlobalID, collection_type, survey_date, projects, 
                          system_type, site_id, other_site_id, general_location_name, 
                          supervisor, username, recorder_first_name, recorder_last_name,
                          water_collect_date, water_control, water_control_type, water_depth,
                          water_vessel_material, water_vessel_color, water_vessel_label, 
                          water_collect_notes, was_filtered, lat_manual, long_manual,
                          survey_month, survey_year, season, season_year, 
                          survey_GlobalID)
        } else if (selected_plot_coltype == "sed_sample"){
          depth_var<-selected_var_plots<-"depth_core_collected"
          selected_var_plots_fmt<-tools::toTitleCase(gsub("_", " ",selected_var_plots))
          start_range<-input$range_start_col
          end_range<-input$range_end_col
          df_summaryplot <- survey_collection_join %>%
            dplyr::mutate(collection_type=na_if(collection_type, "")) %>%
            filter(!if_any(c(survey_GlobalID, collection_GlobalID, collection_type, selected_var_plots), is.na)) %>%
            dplyr::filter(collection_type==selected_plot_coltype) %>%
            dplyr::select(collection_GlobalID, collection_type, survey_date, projects, 
                          system_type, site_id, other_site_id, general_location_name, 
                          supervisor, username, recorder_first_name, recorder_last_name, 
                          core_datetime_start, core_datetime_end,core_label,core_control,
                          core_method,depth_core_collected,core_length,core_diameter,
                          core_notes,subcores_taken,purpose_other_cores, lat_manual, long_manual,
                          survey_month, survey_year, season, season_year,
                          survey_GlobalID)
        }
      }
      df_inrange <- reactive({
        df_summaryplot %>%
          dplyr::filter(!!as.symbol(selected_var_plots)>=start_range & !!as.symbol(selected_var_plots)<=end_range)
      })
      
      unique_survey_sids <- unique(survey_sub$site_id)
      unique_sids <- unique(df_summaryplot$site_id)
      unique_sids_vars <- df_summaryplot %>%
        dplyr::select(site_id) %>%
        dplyr::distinct()
      
      perc_sids<-round((nrow(unique_sids_vars)/length(unique_survey_sids))*100,2)
      perc_sids_var<-round((nrow(unique_sids_vars)/length(unique_sids))*100,2)
      
      quant_str<-quantile(df_summaryplot[[selected_var_plots]], na.rm=TRUE)
      min_str<-min(df_summaryplot[[selected_var_plots]], na.rm=TRUE)
      max_str<-max(df_summaryplot[[selected_var_plots]], na.rm=TRUE)
      
      quant_str[2]
      
      output$plots_text_envmeas <- output$plots_text_col <-renderUI({
        str1 <- sprintf("Percentage of Site IDs with %s measurements: %g %%", selected_var_plots_fmt, perc_sids)
        str2 <- sprintf("Percentage of Site IDs with %s that also have %s: %g %%", input$tab_plots, selected_var_plots_fmt, perc_sids_var)
        str3 <- sprintf("Min (%s): %g", selected_var_plots_fmt, min_str)
        str4 <- sprintf("Max (%s): %g", selected_var_plots_fmt, max_str)
        str5 <- sprintf("Quantiles (%s): 0%% %s, 25%% %s, 50%% %s, 75%% %s, 100%% %s", selected_var_plots_fmt, quant_str[1], quant_str[2], quant_str[3], quant_str[4], quant_str[5])
        HTML(paste(str1, str2, str3, str4, str5,  sep = '<br/>'))
      })
      output$plots_envmeas <- output$plots_col <- renderPlot(
        height = custom_height,
        {
          if (selected_plot == "histVar") {
            plotTitle=sprintf("%s Distribution", selected_var_plots_fmt)
            # check distribution
            ggplot(df_inrange(),
                   aes_string(x=selected_var_plots)) + 
              ylab("Density") +
              xlab(selected_var_plots_fmt) +
              ggtitle(plotTitle) +
              geom_histogram(aes(y=..density..), colour="black", fill="white") +
              geom_density(alpha=.2, fill="#FF6666") 
          } else if (selected_plot == "boxSiteMonth") {
            plotTitle=sprintf("%s by Site and Month", selected_var_plots_fmt)
            ggplot(df_inrange() %>%
                     dplyr::filter(!if_any(c(selected_var_plots, site_id), is.na)) %>% 
                     dplyr::mutate(survey_date = lubridate::ymd(survey_date, tz="UTC")) %>%
                     dplyr::mutate(month = format(survey_date, format="%b")) %>%
                     dplyr::mutate(month=factor(month, levels= c("Jan", "Feb", "Mar", 
                                                                 "Apr", "May", "Jun", 
                                                                 "Jul", "Aug", "Sep", 
                                                                 "Oct", "Nov", "Dec"))) %>%
                     dplyr::mutate(system_type=factor(system_type, levels= c("Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))),
                   aes_string(x="site_id", y=selected_var_plots, fill="system_type")) + 
              geom_boxplot() +
              ylab(selected_var_plots_fmt) +
              xlab("Site ID") +
              labs(fill="") +
              facet_grid(month ~ .) +
              ggtitle(plotTitle) +
              theme(legend.position="top", legend.box = "horizontal") +
              theme(axis.text.x = element_text(angle=50, hjust=1)) +
              theme(plot.title = element_text(hjust = 0.5))
          } else if (selected_plot == "boxSystem") {
            plotTitle=sprintf("%s by System", selected_var_plots_fmt)
            ggplot(df_inrange() %>%
                     dplyr::mutate(system_type=factor(system_type, levels= c("Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))), 
                   aes_string(x="system_type", y=selected_var_plots, fill="system_type")) + 
              geom_boxplot() +
              ylab(selected_var_plots_fmt) +
              xlab("System") +
              ggtitle(plotTitle) +
              #theme(axis.text.x = element_text(angle=50, hjust=1)) +
              theme(plot.title = element_text(hjust = 0.5)) +
              theme(legend.position = "none") 
          } else if (selected_plot == "boxSite") {
            plotTitle=sprintf("%s by Site", selected_var_plots_fmt)
            ggplot(df_inrange() %>%
                     dplyr::mutate(system_type=factor(system_type, levels= c("Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))), 
                   aes_string(x="site_id", y=selected_var_plots, fill="system_type")) + 
              geom_boxplot() +
              ylab(selected_var_plots_fmt) +
              xlab("Site ID") +
              labs(fill="") +
              ggtitle(plotTitle) +
              theme(legend.position="top", legend.box = "horizontal") +
              theme(axis.text.x = element_text(angle=50, hjust=1)) +
              theme(plot.title = element_text(hjust = 0.5))
          } else if (selected_plot == "splotDepthMoSys") {
            #fileName="s123_envmeas_wtemp_system_month_depth_splot"
            plotTitle=sprintf("%s by System, Depth, and Month", selected_var_plots_fmt)
            ggplot(df_inrange() %>%
                     dplyr::mutate(system_type=factor(system_type, levels= c("Coast", "Estuary", "Stream", "Lake", "Aquarium", "other"))), 
                   aes_string(x="survey_month", y=depth_var, color=selected_var_plots)) +
              geom_point(size=2) +
              geom_jitter(width = 0.25, height = 2) +
              scale_x_discrete(name ="Month", limits=factor(seq(1, 12, by=1))) +
              scale_y_continuous(name=selected_var_plots_fmt) +
              labs(color=selected_var_plots_fmt) +
              ggtitle(plotTitle) +
              theme(plot.title = element_text(hjust = 0.5)) +
              theme(legend.position="top", legend.box = "horizontal") +
              facet_grid(system_type ~ .)
          } else return(NULL)
        })
      output$table_plots_envmeas  <- output$table_plots_col  <- DT::renderDataTable({
        DT::datatable(df_inrange(), options = list(scrollX = TRUE, pageLength = 5))
      })
      
    }
  }) 
}
