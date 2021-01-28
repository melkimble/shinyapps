library(shiny)

ui <- shinyUI(bootstrapPage(
  absolutePanel(
    id = "controls", class = "panel panel-default", fixed = TRUE,
    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
    width = 330, height = "auto",
    HTML('<button data-toggle="collapse" data-target="#demo">Collapsible</button>'),
    tags$div(id = 'demo',  class="collapse",
             checkboxInput('input_draw_point', 'Draw point', FALSE ),
             verbatimTextOutput('summary')))
))

server <- shinyServer(function(input, output, session) {
  output$summary <- renderPrint(print(cars))
  
})

shinyApp(ui = ui, server = server)
