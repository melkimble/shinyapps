library(leaflet)

# Choices for drop-downs
vars <- c(
  "What species is it?" = "species",
  "What equipment is used?" = "equipment",
  "Sea Surface Temperature" = "SST",
  "Bathymetry" = "BATHY",
  "Seed Distance" = "SeedDist"
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
        width = 330, height = "auto",

        h2("Site Explorer"),

        selectInput("color", "Color", vars),
        selectInput("size", "Size", vars, selected = "SST"),

        plotOutput("histTemp", height = 200)
#        plotOutput("boxSpecies", height = 250)
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('Maine Department of Marine Resources Lease Site Profiles'), ' by Melissa Kimble (SEANET, 2018).'
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
