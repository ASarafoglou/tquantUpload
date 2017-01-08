### User input.R
#
# utilizes the custom_input_control example from Joe Cheng <joe@rstudio.com>
#

library(shiny)
# setwd("B:/Dateien/Forschungspraktikum Heller/Wissensraum-Shiny/Beispiele/Selbstgebautes/KS-Input")
source("chooser.R")
get_set <- function(x, sep=",") {
  as.set(unlist(strsplit(x, split=sep)))
}
x <- c("a,b,c")

ui <- shinyUI(fluidPage(
  p(h1("Auswahl der Wissenstruktur"), br(),
    "Bitte die Menge als a,b,c,d eingeben", br(),
       textInput(inputId="get_item", label="Eingabe der Grundmenge",value=""),
    actionButton(inputId="start_item", label="Eingabe bestätigen"),
    verbatimTextOutput("test"),
    uiOutput(outputId="p_chooser"),
    chooserInput(inputId="dum",
                 "Verfügbare Elemente",
                 "Gewählte Elemente",
                 row.names(USArrests)#set_power(get_set(x))
                 , c(), size = 10, multiple = TRUE)
     )
  ))

server <- shinyServer(function(input, output, session){
  dat <- reactiveValues()
  
  # Trigger fuer start_item
  observeEvent(input$start_item, {
    dat$items <- input$get_item
    dat$pitems <- set_power(get_set(dat$items))
    output$test <- renderPrint({dat$pitems})
    output$p_chooser <- renderUI({
      chooserInput(inputId="dum",
                   "Verfügbare Elemente",
                   "Gewählte Elemente",
                   dat$pitems, c(), size = 10, multiple = TRUE)
    })
  })
})

shinyApp(ui=ui, server=server)
