library(leaflet)

# Choices for drop-downs
vars <- c(
  "Species" = "species",
  "Equipment" = "equipment",
  "Sea Surface Temperature" = "SST",
  "Bathymetry" = "BATHY"
)
# Choices for plots
plotVars <- c(
  "Scatterplot: Monthly SST by Species" = "scatterspeciesTemp",
  "Boxplot: Monthly SST by Species" = "boxSpeciesTemp",
  "Boxplot: Bathymetry by Species" = "boxSpeciesBathy",
  "Histogram: Sea Surface Temperature" = "histTemp"
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
        draggable = TRUE, top = 60, left = 20, right = 20, bottom = "auto",
        width = "auto", height = 350,

        h2("Site Explorer"),
        
         selectInput("color", "Color", vars),
#        selectInput("size", "Size", vars, selected = "SST"),
         selectInput("selectedplot", "Plot",plotVars, selected = "scatterspeciesTemp")

#        plotOutput("histTemp", height = 175),
#        plotOutput("boxSpeciesTemp", height = 200),
#        plotOutput("boxSpeciesBathy", height=200)
      ),
      tags$div(id="cite",
        'Data compiled for ', tags$em('Maine Department of Marine Resources Lease Site Profiles'), ' by Melissa Kimble (SEANET, 2018).',
        'Bathymetric data obtained from the University of New Hampshire (UNH) Joint Hydrographic Center/Center for Coastal and Ocean Mapping (JHC/CCOM)',
        'Sea Surface Temperature data obtained from the Coastal Satellite Oceanography team at the University of Maine',
        'Aquaculture lease data obtained from the Maine Department of Marine Resources.'
      )
    )
  )#,

#  tabPanel("Data explorer",
#    fluidRow(
#      column(3,
#        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
#      ),
#      column(3,
#        conditionalPanel("input.states",
#          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
#        )
#      ),
#      column(3,
#        conditionalPanel("input.states",
#          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
#        )
#      )
#    ),
#    fluidRow(
#      column(1,
#        numericInput("minScore", "Min score", min=0, max=100, value=0)
#      ),
#      column(1,
#        numericInput("maxScore", "Max score", min=0, max=100, value=100)
#      )
#    ),
#    hr(),
#    DT::dataTableOutput("ziptable")
#  ),

#  conditionalPanel("false", icon("crosshair"))
)
