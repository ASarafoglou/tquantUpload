library(shiny)

ui <- shinyUI(fluidPage(
  textInput(inputId="t.input", label="Eingabe der Wissenstruktur"),
  actionButton(inputId="go", label="Eingabe bestätigen"),
  verbatimTextOutput(outputId="kstruc")
))

server <- shinyServer(function(input, output){
   dat <- eventReactive(input$go,{
      dat <- list()
      dat[[input$go]] <- input$t.input
      dat
  })             

  output$kstruc <- renderPrint(dat())
})

shinyApp(ui=ui, server=server)
