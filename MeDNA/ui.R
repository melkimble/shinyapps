library(shiny)
library(leaflet)
library(raster)
library(sf)
library(DT)
library(shinythemes)
library(shinyjs)

envmeas_choices <- c("Measurement Depth" = "envmeas_depth",
                     "Water Temperature" = "water_temp",
                     "Salinity" = "salinity",
                     "Turbidity" = "turbidity",
                     "Conductivity" = "conductivity",
                     "Dissolved Oxygen" = "do",
                     "PAR1" = "par1",
                     "PAR2" = "par2")

envmeas_any_choices <- append(c("Any" = "env_measurements"), envmeas_choices)

project_choices <- c("Larval Black Box (T1)" = "Larval Black Box (T1)",
                     "Alewife (T1)" = "Alewife (T1)",
                     "Fisheries eDNA (T1)" = "Fisheries eDNA (T1)",
                     "Harmful algal blooms (T2)" = "Harmful algal blooms (T2)",
                     "Species on the move (T2)" = "Species on the move (T2)",
                     "Index Sites (T3)" = "Index Sites (T3)",
                     "Macrosystem Integration (T3)" = "Macrosystem Integration (T3)",
                     "Microbial biosensors (T3)" = "Microbial biosensors (T3)",
                     "Community Science (T3)" = "Community Science (T3)",
                     "Other" = "Other")

barplot_choices <-c("Season" = "barSRSeason",
                    "System" = "barSRSystem",
                    "Season and Year" = "barSRSeasonYear",
                    "System and Year" = "barSRSystemYear",
                    "Month and Year" = "barSRMonthYear",
                    "Site and Year" = "barSRSiteYear")

filter_type_barplot_choices <- append(barplot_choices, c("Filter Type and Season" = "barFTSeason",
                                                         "Filter Type and System" = "barFTSystem",
                                                         "Filter Type and Year" = "barFTYear"))


var_plot_choices <- c("Histogram" = "histVar",
                      "Boxplot by Site and Month" = "boxSiteMonth",
                      "Boxplot by System" = "boxSystem",
                      "Boxplot by Site" = "boxSite",
                      "Scatterplot by Depth, Month, and System"= "splotDepthMoSys")

col_type_choices <- c("Water Collection" = "water_sample",
                      "Sediment Collection" = "sed_sample")

col_type_any_choices <- append(c("Any" = "collection_type"), col_type_choices) 

filter_type_choices<-c("Nitex" = "nitex",
                       "Supor" = "supor",
                       "Glass Fiber Filter (GF/F)" = "gff",
                       "Cellulose Nitrate (CN)" = "cn",
                       "Other" = "other")

filter_type_any_choices<-append(c("Any" = "filter_type"), filter_type_choices) 

htmlpage_css <- ".multicol {height: 175px;-webkit-column-count: 2;
                -moz-column-count: 2;column-count: 2;
                -moz-column-fill: auto;-column-fill: auto;}"
hr_css <- "border-top: 1px solid #000000;width: 100%"

navbarPage(
  id="navbar",
  title="Maine-eDNA Survey123 Summary",
  theme = "style/style.css",
  footer = includeHTML("footer.html"),
  fluid = TRUE, 
  collapsible = TRUE,
  # ----------------------------------
  # tab panel 1 - Home
  tabPanel("Home",
           includeHTML("home.html"),
           tags$head(
             tags$link(rel = "stylesheet", 
                       type = "text/css", 
                       href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
             tags$link(rel = "icon", 
                       type = "image/png", 
                       href = "images/eDNA-logo-Regular-L_square-notext.png")
           )
  ),
  # ----------------------------------
  # tab panel 2 - Maps
  tabPanel("Maps",
           tags$head(tags$script(src = "plugins/gomap.js")),
           tabsetPanel(id="tab_maps",
                       tabPanel("Surveys",
                                bootstrapPage(
                                  column(12, leafletOutput("map_survey")),
                                  column(12, htmlOutput("maps_text_survey")),
                                  hr(style = hr_css),
                                  column(12, div(style = "margin-top: 30px; font-size: 1em;",
                                                 DT::dataTableOutput('table_map_survey')))
                                )
                       ),
                       tabPanel("Crew", 
                                bootstrapPage(
                                  column(12, leafletOutput("map_crew")),
                                  column(12, htmlOutput("maps_text_crew")),
                                  hr(style = hr_css),
                                  column(12, div(style = "margin-top: 30px; font-size: 1em;",
                                                 DT::dataTableOutput('table_map_crew')))
                                )                          
                       ),
                       tabPanel("Env Measurements", 
                                bootstrapPage(
                                  column(12, leafletOutput("map_envmeas")),
                                  fluidRow(
                                    column(3, selectInput("map_selectedvar_envmeas", 
                                                          label="Variable", 
                                                          choices=envmeas_any_choices,
                                                          selected="env_measurements")
                                    ),
                                    column(9, htmlOutput("maps_text_envmeas")),
                                  ),
                                  hr(style = hr_css),
                                  column(12, div(style = "margin-top: 30px; font-size: 1em;",
                                                 DT::dataTableOutput('table_map_envmeas')))
                                )
                       ),
                       tabPanel("Collections", 
                                bootstrapPage(
                                  column(12, leafletOutput("map_col")),
                                  fluidRow(
                                    column(3, selectInput("map_selected_coltype", 
                                                          label="Variable", 
                                                          choices=col_type_any_choices,
                                                          selected="collection_type")
                                    ),
                                    column(9, htmlOutput("maps_text_col")),
                                  ),
                                  hr(style = hr_css),
                                  column(12, div(style = "margin-top: 30px; font-size: 1em;",
                                                 DT::dataTableOutput('table_map_col')))
                                )
                       ),
                       tabPanel("Filters", 
                                bootstrapPage(
                                  column(12, leafletOutput("map_filters")),
                                  fluidRow(
                                    column(3, selectInput("map_selected_filtertype", 
                                                          label="Variable", 
                                                          choices=filter_type_any_choices,
                                                          selected="filter_type")
                                    ),
                                    column(9, htmlOutput("maps_text_filters")),
                                  ),
                                  hr(style = hr_css),
                                  column(12, div(style = "margin-top: 30px; font-size: 1em;",
                                                 DT::dataTableOutput('table_map_filters')))
                                )                          
                       ),
                       tabPanel("SubCores", 
                                bootstrapPage(
                                  column(12, leafletOutput("map_subcores")),
                                  column(12, htmlOutput("maps_text_subcores")),
                                  hr(style = hr_css),
                                  column(12, div(style = "margin-top: 30px; font-size: 1em;",
                                                 DT::dataTableOutput('table_map_subcores')))
                                )                          
                       )
           )
  ),
  # ----------------------------------
  # tab panel 3 - Barplots
  tabPanel("Barplot Counts",
           tabsetPanel(id="tab_barplots",
                       tabPanel("Surveys",
                                bootstrapPage(tags$head(tags$style(HTML(htmlpage_css))),
                                              column(12, plotOutput("barplot_survey")),
                                              hr(style = hr_css),
                                              fluidRow(
                                                column(3, selectInput("barplot_selected_survey", 
                                                                      label="Plot", 
                                                                      choices=barplot_choices, 
                                                                      selected="barSRSeason")),
                                                column(9, tags$div(align = 'left', class = 'multicol',
                                                                   checkboxGroupInput("barplot_selprj_survey",
                                                                                      label="Projects",
                                                                                      inline = FALSE, 
                                                                                      choices=project_choices,
                                                                                      selected="Index Sites (T3)"))),
                                              ),
                                              hr(style = hr_css),
                                              textOutput("barplot_sum_survey"),
                                              hr(style = hr_css),
                                              column(12, div(style = "margin-top: 30px; font-size: 1em;",
                                                             DT::dataTableOutput('table_barplot_survey')))
                                )
                       ),
                       tabPanel("Crew", 
                                bootstrapPage(tags$head(tags$style(HTML(htmlpage_css))),
                                              column(12, plotOutput("barplot_crew")),
                                              hr(style = hr_css),
                                              fluidRow(
                                                column(3, selectInput("barplot_selected_crew", 
                                                                      label="Plot", 
                                                                      choices=barplot_choices, 
                                                                      selected="barSRSeason")),
                                                column(9, tags$div(align = 'left', class = 'multicol',
                                                                   checkboxGroupInput("barplot_selprj_crew",
                                                                                      label="Projects",
                                                                                      inline = FALSE, 
                                                                                      choices=project_choices,
                                                                                      selected="Index Sites (T3)"))),
                                              ),
                                              hr(style = hr_css),
                                              textOutput("barplot_sum_crew"),
                                              hr(style = hr_css),
                                              column(12, div(style = "margin-top: 30px; font-size: 1em;",
                                                             DT::dataTableOutput('table_barplot_crew')))
                                )
                       ),
                       tabPanel("Env Measurements",
                                bootstrapPage(tags$head(tags$style(HTML(htmlpage_css))),
                                              column(12, plotOutput("barplot_envmeas")),
                                              hr(style = hr_css),
                                              fluidRow(
                                                column(3, selectInput("barplot_selected_envmeas", 
                                                                      label="Plot", 
                                                                      choices=barplot_choices, 
                                                                      selected="barSRSeason")),
                                                column(9, tags$div(align = 'left', class = 'multicol',
                                                                   checkboxGroupInput("barplot_selprj_envmeas",
                                                                                      label="Projects",
                                                                                      inline = FALSE, 
                                                                                      choices=project_choices,
                                                                                      selected="Index Sites (T3)"))),
                                              ),
                                              hr(style = hr_css),
                                              textOutput("barplot_sum_envmeas"),
                                              hr(style = hr_css),
                                              column(12, div(style = "margin-top: 30px; font-size: 1em;",
                                                             DT::dataTableOutput('table_barplot_envmeas')))
                                )
                       ),
                       tabPanel("Collections", 
                                bootstrapPage(tags$head(tags$style(HTML(htmlpage_css))),
                                              column(12, plotOutput("barplot_col")),
                                              hr(style = hr_css),
                                              fluidRow(
                                                column(6, selectInput("barplot_selected_col", 
                                                                      label="Plot", 
                                                                      choices=barplot_choices, 
                                                                      selected="barSRSeason")),
                                                column(6, selectInput("barplot_selected_coltype",
                                                                      label="Collection Type",
                                                                      choices=col_type_any_choices,
                                                                      selected="collection_type"))
                                                
                                              ),
                                              fluidRow(
                                                column(12, tags$div(align = 'left', class = 'multicol',
                                                                    checkboxGroupInput("barplot_selprj_col",
                                                                                       label="Projects",
                                                                                       inline = FALSE, 
                                                                                       choices=project_choices,
                                                                                       selected="Index Sites (T3)")))
                                              ),
                                              hr(style = hr_css),
                                              textOutput("barplot_sum_col"),
                                              hr(style = hr_css),
                                              column(12, div(style = "margin-top: 30px; font-size: 1em;",
                                                             DT::dataTableOutput('table_barplot_col')))
                                )
                       ),
                       tabPanel("Filters", 
                                bootstrapPage(tags$head(tags$style(HTML(htmlpage_css))),
                                              column(12, plotOutput("barplot_filters")),
                                              htmlOutput("barplot_perc_filters"),
                                              hr(style = hr_css),
                                              fluidRow(
                                                column(6, selectInput("barplot_selected_filters", 
                                                                      label="Plot", 
                                                                      choices=filter_type_barplot_choices, 
                                                                      selected="barSRSeason")),
                                                column(6, selectInput("barplot_selected_filtertype",
                                                                      label="Filter Type",
                                                                      choices=filter_type_any_choices,
                                                                      selected="filter_type"))
                                                
                                              ),
                                              fluidRow(
                                                column(12, tags$div(align = 'left', class = 'multicol',
                                                                    checkboxGroupInput("barplot_selprj_filters",
                                                                                       label="Projects",
                                                                                       inline = FALSE, 
                                                                                       choices=project_choices,
                                                                                       selected="Index Sites (T3)")))
                                              ),
                                              hr(style = hr_css),
                                              textOutput("barplot_sum_filters"),
                                              hr(style = hr_css),
                                              column(12, div(style = "margin-top: 30px; font-size: 1em;",
                                                             DT::dataTableOutput('table_barplot_filters')))
                                )                          
                       ),
                       tabPanel("SubCores", 
                                bootstrapPage(tags$head(tags$style(HTML(htmlpage_css))),
                                              column(12, plotOutput("barplot_subcores")),
                                              hr(style = hr_css),
                                              fluidRow(
                                                column(3, selectInput("barplot_selected_subcores", 
                                                                      label="Plot", 
                                                                      choices=barplot_choices, 
                                                                      selected="barSRSeason")),
                                                column(9, tags$div(align = 'left', class = 'multicol',
                                                                   checkboxGroupInput("barplot_selprj_subcores",
                                                                                      label="Projects",
                                                                                      inline = FALSE, 
                                                                                      choices=project_choices,
                                                                                      selected="Index Sites (T3)"))),
                                              ),
                                              hr(style = hr_css),
                                              textOutput("barplot_sum_subcores"),
                                              hr(style = hr_css),
                                              column(12, div(style = "margin-top: 30px; font-size: 1em;",
                                                             DT::dataTableOutput('table_barplot_subcores')))
                                )
                       )
           )
  ),
  # ----------------------------------
  # tab panel 3 - Plots
  tabPanel("Summary Plots",
           tabsetPanel(id="tab_plots",
                       tabPanel("Env Measurements",
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput("plots_selectedvar_envmeas", 
                                                label="Variable", 
                                                choices=envmeas_choices,
                                                selected="envmeas_depth"),
                                    selectInput("plots_selected_envmeas", 
                                                label="Plot", 
                                                choices=var_plot_choices, 
                                                selected="histVar"),
                                    sliderInput("height_envmeas", label="Plot Height", min = 250, max = 2000, value = 500),
                                    numericInput("range_start_envmeas","Variable Start", value = 0),
                                    numericInput("range_end_envmeas", "Variable End", value = 100),
                                    htmlOutput("plots_text_envmeas")
                                  ),
                                  mainPanel(
                                    plotOutput("plots_envmeas", height = "auto"),
                                    hr(style = hr_css),
                                    div(style = "margin-top: 30px; font-size: 1em;",
                                        DT::dataTableOutput('table_plots_envmeas'))
                                  )
                                )
                       ),
                       tabPanel("Collections", 
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput("plots_selected_coltype", 
                                                label="Variable", 
                                                choices=col_type_choices,
                                                selected="water_sample"),
                                    selectInput("plots_selected_col", 
                                                label="Plot", 
                                                choices=var_plot_choices, 
                                                selected="histVar"),
                                    sliderInput("height_col", label="Plot Height", min = 250, max = 2000, value = 500),
                                    numericInput("range_start_col","Variable Start", value = 0),
                                    numericInput("range_end_col", "Variable End", value = 100),
                                    htmlOutput("plots_text_col")
                                  ),
                                  mainPanel(
                                    plotOutput("plots_col", height = "auto"),
                                    hr(style = hr_css),
                                    div(style = "margin-top: 30px; font-size: 1em;",
                                        DT::dataTableOutput('table_plots_col'))
                                  )
                                )
                       )
           )
  ),
  tabPanel("About",
           includeHTML("about.html"),
           shinyjs::useShinyjs(),
           tags$head(
             tags$link(rel = "stylesheet", 
                       type = "text/css", 
                       href = "plugins/carousel.css"),
             tags$script(src = "plugins/holder.js")
           ),
           tags$style(type="text/css",
                      ".shiny-output-error { visibility: hidden; }",
                      ".shiny-output-error:before { visibility: hidden; }"
           )
  )
)