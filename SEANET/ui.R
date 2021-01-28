# LibraryList<-c("leaflet","dplyr","RColorBrewer", "scales", "lattice","ggplot2","htmlTable")
# for (TheLibrary in LibraryList)
# {
#   if(TheLibrary %in% rownames(installed.packages()) == FALSE) install.packages(TheLibrary)
# }
library(leaflet)
library(DT)

# Choices for drop-downs
vars <- c(
  "Species Category" = "speciesCategory",
#  "Equipment" = "equipment", ## add equipment category someday
  "Sea Surface Temperature" = "SST",
  "Bathymetry" = "BATHY"
)
# Choices for plots
plotVars <- c(
  "Scatterplot: Monthly SST by Species Cat." = "scatterspeciesTemp",
  "Boxplot: Monthly SST by Species Cat." = "boxSpeciesTemp",
  "Boxplot: Bathymetry by Species Cat." = "boxSpeciesBathy",
  "Histogram: Sea Surface Temperature" = "histTemp",
  "Histogram: Bathymetry" = "histBathy"
)
navbarPage("DMR Lease Data", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      bootstrapPage(absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = "auto", height = "auto",style = "opacity: 0.92", 
        HTML('<button data-toggle="collapse" data-target="#MapOptions">Map Options and Plots</button>'),
        tags$div(id = 'MapOptions',  class="collapse",
                 h4("Site Explorer"),
                 plotOutput("plot", height = 250),
                 hr(),
                 wellPanel(
                   fixedRow(
                     column(12, 
                            sliderInput("timeSlider", label = "Year Range", min = 2004, max = 2017, value = c(2004, 2017), sep=""),
                            fixedRow(
                              column(4, selectInput("color", "Color", vars)),
                              column(8, selectInput("selectedplot","Plot", plotVars, selected = "scatterspeciesTemp"))
                              )
                            )
                     )
                   )
                 )
        )),

      tags$div(id="cite",
        'Data compiled for ', tags$em('Maine Department of Marine Resources Lease Site Profiles'), ' by Melissa Kimble (SEANET, 2018).',
        'Bathymetric data obtained from the University of New Hampshire (UNH) Joint Hydrographic Center/Center for Coastal and Ocean Mapping (JHC/CCOM).',
        'Sea Surface Temperature data obtained from the Coastal Satellite Oceanography team at the University of Maine',
        'Aquaculture lease data obtained from the Maine Department of Marine Resources.'
      )
    )
  ),
#######################################################
#DMRData
#names(DMRData)
  tabPanel("Data Explorer",
    fluidRow(
      column(3, selectInput("speciesCategory", "SpeciesCategory", c("All speciesCategory"="", sort(unique(cleantable$SpeciesCategory))), multiple=TRUE)),
      column(3, conditionalPanel("input.speciesCategory", selectInput("equipment", "Equipment", c("All equipment"=""), multiple=TRUE))),
      column(3, conditionalPanel("input.speciesCategory", selectInput("leasetype", "Lease Type", c("All leasetype"=""), multiple=TRUE)))
      ),
    fluidRow(
      column(1, numericInput("minBathy", "Min Bathy", min=-100, max=0, value=-100)),
      column(1, numericInput("maxBathy", "Max Bathy", min=-100, max=0, value=0)),
      column(1, numericInput("minTemp", "Min Temp", min=-50, max=50, value=-50)),
      column(1, numericInput("maxTemp", "Max Temp", min=-50, max=50, value=50))
      ),
    hr(),
    DT::dataTableOutput("dmrTable")
  ),

  conditionalPanel("false", icon("crosshair"))
)
