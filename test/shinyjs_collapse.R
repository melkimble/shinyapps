library(shiny)
library(shinyjs)

ui = fillPage(
  useShinyjs(),  # Set up shinyjs
  
  hidden( #start as hidden
    absolutePanel(
      id = 'thePanel', style='background-color: #eaf2f8',
      top = 90, left = 10,
      checkboxInput('input_draw_point', 'Draw point', FALSE ),
      verbatimTextOutput('summary')),
    fixedPanel(top = 10, left = 165,
           actionButton('plotBtn', 'Show Panel',
                        style="opacity: .80; color: #fff; background-color: #a662e3; border-color: #a153e5"))))

server <- shinyServer(function(input, output, session) {
  output$summary <- renderPrint(print(cars))
  
  observeEvent(input$plotBtn, {
    toggle('thePanel')
  })
  
})

shinyApp(ui = ui, server = server)