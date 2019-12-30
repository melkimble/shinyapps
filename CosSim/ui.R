library(shiny)
library(leaflet)

# Choices for drop-downs
corrVarsOptions <- c(
  "Fecal Coliform" = "fscore",
  "Sea Surface Temperature" = "temp",
  "Salinity" = "sal"
)

bootstrapPage(
  
  titlePanel(h1(id="title-header","Aquaculture Lease Tenure", align = "center"), windowTitle="Aquaculture Lease Tenure"),
  tags$style(type="text/css", "#title-header {color: black;
             font-size: 30px;
             font-style: bold;}"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", height = "65%"),
  wellPanel(
    fixedRow(
      column(4, checkboxInput("legend", "Show legend", TRUE))
    ),
    fixedRow(
      column(8, checkboxGroupInput("corrVars", label= h4("Correlation Variables"), choices=corrVarsOptions, inline = TRUE, selected=c("fscore","sal","temp"))),
      ),

    sliderInput("range", h4("Similarity Range"),
                min = 0, max = 1,
                value = c(0.7,0.90), step = 0.1),
    DT::dataTableOutput('theOutputTable')
  )
  )


