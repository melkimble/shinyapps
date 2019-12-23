library(shiny)
library(leaflet)



bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 5,
                # Input: Simple integer interval ----
                sliderInput("range", "Range:",
                            min = 0, max = 1,
                            value = c(0.7,0.90), step = 0.1),
                tableOutput('values'),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

