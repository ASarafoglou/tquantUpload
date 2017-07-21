library(shiny)
library(sets)
library(proxy)
library(relations)
library(kst)

ui <- shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Eingabe", 
             p("Die Eingabe von AB bitte über  A,B",
               textInput(inputId="t.input", label="Eingabe der Wissenstruktur")),
             actionButton(inputId="go", label="Eingabe bestätigen"),
             actionButton(inputId="reset", label="Liste löschen"),
             actionButton(inputId="save", label="Liste speichern"),
             verbatimTextOutput(outputId="klist"),
             verbatimTextOutput(outputId="pset"),
             verbatimTextOutput(outputId="pset1")),
    tabPanel("Ausgabe der Struktur",
             p("Benötigt eine abgespeicherte Liste",
               verbatimTextOutput(outputId="kstruc")),
             p(actionButton(inputId="kber", "Operationen der Darstellung beginnen"),
               actionButton(inputId="p_kber", "Darstellung mittels Potenzmenge")),
             p("Wissensraum:", verbatimTextOutput(outputId="ksp")),
             p("Atome der Struktur:", verbatimTextOutput(outputId="kbs")),
             p("Hassediagramm der Struktur", plotOutput(outputId="ksp_plot")),
             p("Inner/Outer-Fringe", 
               #renderUI(outputId="slct_frng"),
               verbatimTextOutput(outputId="k_fringe"))) #Ende tabPanel Strukturausgabe
  )# Ende tabsetPanel
))# Ende fluidPage #Ende Ui

server <- shinyServer(function(input, output, session){
  dat <- reactiveValues(count=0, ls=list(), kstr=NULL, skst=NULL, kst=NULL,
                        strc=NULL)
  
  # zeigt das was im Eingabefeld angegeben ist an
  observeEvent(input$go,{
    dat$count <- dat$count+1
    dat$ls[dat$count] <- input$t.input
    updateTextInput(session, "t.input", value="")
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
     dat$pkstr <- set_power(dat$kstr)
     dat$pskstr <- set_power(dat$skstr)
    }
     )
  
  output$klist <- renderPrint({
    dat$ls})
  
  ## Output der Potenzmenge
  output$pset <- renderPrint({
    dat$pkstr
  })
  output$pset1 <- renderPrint({
    dat$pskstr
  })
  
  ## Darstellung von dat$ls als Menge
  output$kstruc <- renderPrint({
    dat$skstr
  })
  
  # Generiert die Wissensstruktur/-Raum
  observeEvent(input$kber, {
    dat$strc <- kstructure(dat$skstr)
    dat$kst <- kspace(dat$strc)
    
  }
  )
  observeEvent(input$p_kber, {
    dat$pstrc <- kstructure(dat$pkstr)
    dat$pkst <- kspace(dat$pstrc)
  })
  
  # Funktioniert nicht mehr fuer kber
  output$kbs <- renderPrint({
    if(input$kber==0 & input$p_kber==0){
      return()
    }
    else{
      if(input$kber!=0){
        kbase(dat$kst)
      }
      #if(input$p_kber!=0){
      else{
        kbase(dat$pkst)
      }
    }
  })
  
  output$ksp <- renderPrint({
    if(input$kber==0 & input$p_kber==0){
      return()
    }
    else{
      if(input$kber!=0){
        dat$kst
      }
      if(input$p_kber!=0){
        dat$pkst
      }
    }
  })
  
  output$ksp_plot <- renderPlot({
    plot(dat$strc)
    })
  
  
}) # Ende serverfunction

shinyApp(ui=ui, server=server)


# conditionalPanel() erlaubt es Panels sichtbar zu machen!
# lapply(unlist(strsplit(unlist(y), ",")), set)