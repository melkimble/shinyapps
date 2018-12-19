library(leaflet)

# Choices for drop-downs
vars <- c(
  "Species Category" = "speciesCategory",
  "Equipment" = "equipment",
  "Sea Surface Temperature" = "SST",
  "Bathymetry" = "BATHY"
)
# Choices for plots
plotVars <- c(
  "Scatterplot: Monthly SST by Species Category" = "scatterspeciesTemp",
  "Boxplot: Monthly SST by Species Category" = "boxSpeciesTemp",
  "Boxplot: Bathymetry by Species Category" = "boxSpeciesBathy",
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
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 750 , height = 350,

        h3("Site Explorer"),
        plotOutput("plot", height = 225),
        tags$div(style="display:inline-block",selectInput("color", "Color", vars)),
        tags$div(style="display:inline-block",selectInput("selectedplot","Plot", plotVars, selected = "scatterspeciesTemp"))

#        selectInput("color", "Color", vars),
#        selectInput("selectedplot","Plot", plotVars, selected = "scatterspeciesTemp")

#        selectInput("size", "Size", vars, selected = "SST"),
#        plotOutput("histTemp", height = 175),
#        plotOutput("boxSpeciesTemp", height = 200),
#        plotOutput("boxSpeciesBathy", height=200)
      ),

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
