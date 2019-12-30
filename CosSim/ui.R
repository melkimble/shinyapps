library(shiny)
library(leaflet)



bootstrapPage(
  titlePanel(h1("Aquaculture Lease Tenure", align = "center"), windowTitle="Aquaculture Lease Tenure"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", height = "65%"),
  absolutePanel(top = 05, right = 5,
                # Input: Simple integer interval ----
                checkboxInput("legend", "Show legend", TRUE)
                

  ),
  wellPanel(
    sliderInput("range", "Similarity Range:",
                min = 0, max = 1,
                value = c(0.7,0.90), step = 0.1),
    DT::dataTableOutput('theOutputTable')
  )
  )


