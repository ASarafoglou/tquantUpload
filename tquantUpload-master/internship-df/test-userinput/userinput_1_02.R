library(shiny)
library(kst)
# extract <- function(){
#   if(input$go==1){
#     dat <- list()
#     dat[[input$go]] <- input$t.input
#     dat
#   }
#   else{dat[[input$go]] <- input$t.input
#   dat}
# }

ui <- shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Eingabe", 
             p("Die Eingabe von AB bitte ueber  A,B",
               textInput(inputId="t.input", label="Eingabe der Wissenstruktur")),
             actionButton(inputId="go", label="Eingabe bestaetigen"),
             actionButton(inputId="reset", label="Liste loeschen"),
             actionButton(inputId="save", label="Liste speichern"),
             verbatimTextOutput(outputId="klist")),
    tabPanel("Ausgabe der Struktur",
             p("Benoetigt eine abgespeicherte Liste",
               verbatimTextOutput(outputId="kstruc")),
             actionButton(inputId="kber", "Operationen der Darstellung beginnen"),
             p("Wissensraum:", verbatimTextOutput(outputId="ksp")),
             p("Atome der Struktur:", verbatimTextOutput(outputId="kbs")),
             p("Hassediagramm der Struktur", plotOutput(outputId="ksp_plot")),
             p("Inner/Outer-Fringe", 
               renderUI(outputId="slct_frng"),
               verbatimTextOutput(outputId="k_fringe"))) #Ende tabPanel Strukturausgabe
  )# Ende tabsetPanel
))# Ende fluidPage #Ende Ui

server <- shinyServer(function(input, output){
  dat <- reactiveValues(count=0, ls=list(), kstr=NULL, skst=NULL, kst=NULL,
                        strc=NULL)
  
  # zeigt das was im Eingabefeld angegeben ist an
  observeEvent(input$go,{
    dat$count <- dat$count+1
    dat$ls[dat$count] <- input$t.input
        }
      )  
  # Erstellt eine Liste mit der Leeren Menge
  observeEvent(input$reset, {
    dat$ls <- list()
    dat$count <- 0
        }
               )
  
  # Speichert die Eingabe ab
  observeEvent(input$save,{
    dat$kstr <- dat$ls
    
    # Extrahiert die Mengenvereinigungen, indem kstr elementweise durch gegangen wird
    # und die "A,B"s aufgesplittet werden auf "A" "B"
     d.kstr <- list(length(dat$kstr))
     for(i in 1:length(dat$kstr)){
       d.kstr[[i]] <- as.set(unlist(strsplit(dat$kstr[[i]], split=",")))
     }
     dat$skstr <- as.set(d.kstr)
    }
     )
  
  output$klist <- renderPrint({
    dat$ls})
  
  output$kstruc <- renderPrint({
    dat$skstr
  })
  
  observeEvent(input$kber, {
    dat$strc <- kstructure(dat$skstr)
    dat$kst <- kspace(dat$strc)
  }
  )        
  
  output$kbs <- renderPrint({
    if(input$kber==0){
      return()
    }
    kbase(dat$kst)
  })
  
  output$ksp <- renderPrint({
    if(input$kber==0){
      return()
    }
    dat$kst
  })
  
  output$ksp_plot <- renderPlot({
#     if(input$kber==0){
#       return()
#     }
    plot(dat$strc)})
  
#   output$slct_frng <- uiOutput(selectInput(
#     inputId="slct_frng",
#     label="Item für Fringeberechnung",
#     choices=)
#   output$k_fringe <- renderPrint({})
#   
}) # Ende serverfunction

shinyApp(ui=ui, server=server)


# conditionalPanel() erlaubt es Panels sichtbar zu machen!
# lapply(unlist(strsplit(unlist(y), ",")), set)
