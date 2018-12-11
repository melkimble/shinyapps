library(leaflet)

# Choices for drop-downs
vars <- c(
  "What species?" = "species",
  "What site?" = "SITE_ID",
  "What Equipment?" = "equipment",
  "Sea Surface Temperature (C)" = "SST",
  "Bathymetry (m)" = "BATHY",
  "Seed Distance" = "SeedDist"
)


navbarPage("DMR Aquaculture Leases", id="nav",

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

        h2("Lease Site Explorer"),

        selectInput("color", "Color", vars),
        selectInput("size", "Size", vars, selected = "SST"),
        conditionalPanel("input.color == 'species' || input.size == 'SST'",
          # Only prompt for threshold when coloring or sizing by species
          numericInput("threshold", "Species threshold (top n percentile)", 5)
        ),

        plotOutput("histTemp", height = 200),
        plotOutput("boxBathy", height = 250)
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('Maine Department of Marine Resources Lease Site Profiles'), ' by Melissa Kimble (SEANET, 2018).'
      )
    )
  ),
  
  ## Data Explorer ###########################################
  #Actual (ID,SITE_ID zipcode,latitude,longitude,species states,equipment cities,SST,BATHY,SeedDist)
  #cleantable (Id, SiteId, Lat, Long, Species, Equipment City, SST, Bathymetry, SeedDistance)
  
  tabPanel("Data explorer",
    fluidRow(
      column(3,
        selectInput("species", "Species", c("All species"="", structure(state.abb, names=state.name), "ShellFish, Floating trays"="Floating trays"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.species",
          selectInput("equipment", "Equipment", c("All equipment"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.species",
          selectInput("SITE_ID", "SiteId", c("All SITE_ID"=""), multiple=TRUE)
        )
      )
    ),
    fluidRow(
      column(1,
        numericInput("minScore", "Min score", min=0, max=100, value=0)
      ),
      column(1,
        numericInput("maxScore", "Max score", min=0, max=100, value=100)
      )
    ),
    hr(),
    DT::dataTableOutput("siteTable")
  ),

  conditionalPanel("false", icon("crosshair"))
)
