### User input.R
#
# utilizes the custom_input_control example from Joe Cheng <joe@rstudio.com>
#

library(shiny)
library(kst)
library(CDM)
library(relations)
library(pks)
# setwd("B:/Dateien/Forschungspraktikum Heller/Wissensraum-Shiny/Beispiele/Selbstgebautes/KS-Input")
source("chooser.R")

# Selbstgeschriebene Funktionen, Platzhalter und Datensatzladen
get_set <- function(x, sep=",") {
  as.set(unlist(strsplit(x, split=sep)))
}
#name_list <- function(x) {
#  
#}

x <- c("a,b,c")
data(DoignonFalmagne7)
preset <- list(D = DoignonFalmagne7)

server <- shinyServer(function(input, output, session){
  dat <- reactiveValues()
  
  # Trigger fuer start_item
  observeEvent(input$start_item, {
    dat$items <- input$get_item
    dat$pitems <- set_power(get_set(dat$items))
    #output$test <- renderPrint({dat$pitems})
    output$p_chooser <- renderUI({
      chooserInput(inputId="p_chooser",
                   "Verfuegbare Elemente",
                   "Gewaehlte Elemente",
                   dat$pitems, c(), size = 10, multiple = TRUE)
    })
  })
  
  # Trigger fuer get_chooser (auswahl finalisieren)
  observeEvent(input$get_chooser, {
    dat$fitems <- isolate({input$p_chooser$right})
    
    #output$test <- renderText({dat$fitems})
     dat$kstr <- kstructure(as.set(
       lapply(strsplit(dat$fitems, split=" "), as.set)))
     
     dat$ksp <- kspace(dat$kstr)
     
     dat$patksp <- as.binmat(dat$ksp)
     
     dat$nmksp <- as.pattern(dat$patksp, as.letters=TRUE)
     
     dimnames(dat$patksp)[[1]] <- dat$nmksp
     
     names(dat$nmksp) <- dat$nmksp
     
     #output$test <- renderPrint({dat$patksp})
     updateSelectInput(session, "item_choose",
                       choices=as.list(dat$nmksp))
  })
  
  # Trigger fuer preset Unterauswahl
  observeEvent(input$slct_data, {
    if(isolate(class(preset[[input$slct_data]])) == "list" ){
      output$slct_chooser <- renderUI(
        selectInput(inputId="slct_chooser",
                    label="Auswahl der Struktur in Datensatz",
        choices=c(isolate(names(preset[[input$slct_data]]))))
        )
    }
#     output$k_hasse <- renderPlot({isolate(plot(preset[[input$slct_data]]))})
#     output$k_base <- renderPrint({isolate(kbase(dat$ksp))})
  })
  
  # Trigger fuer preset verwendung
  observeEvent(input$go_slct, {
    if(!is.null(input$slct_data) 
       & class(preset[[input$slct_data]][[input$slct_chooser]])=="matrix"){
      
      dat$kstr <- kstructure(as.pattern(
        isolate(preset[[input$slct_data]][[input$slct_chooser]]),
        as.set=TRUE))
      dat$ksp <- kspace(dat$kstr)
      dat$patksp <- as.binmat(dat$ksp)
      
      dat$nmksp <- as.pattern(dat$patksp, as.letters=TRUE)
      
      dimnames(dat$patksp)[[1]] <- dat$nmksp
      
      names(dat$nmksp) <- dat$nmksp
      
      updateSelectInput(session, "item_choose",
                        choices=as.list(dat$nmksp))
      
    }
    else{
      output$slct_error <- renderPrint("Der ausgewaehlte Datensatz ist keine binaere Matrix")
    }
   
  })
  
  # Trigger fuer reset
#   observeEvent(input$reset, {
#     
#   })
  
  # Trigger fuer Darstellung
  # Updatet noch automatisch, vermutlich weil nach erstem Druecken der Wert =/=
  # 0
  observeEvent(input$go_plot, {
    output$k_hasse <- renderPlot({isolate(plot(dat$ksp))})
    output$k_base <- renderPrint({isolate(kbase(dat$ksp))})
  })
  
  # Trigger fuer atome
  # Platzhalter bisher
  observeEvent(input$go_atom, {
    output$k_atom <- renderPrint({
      katoms(dat$ksp,
             items=get_set(isolate(input$item_choose), sep=""))})
  })
  
  
})

