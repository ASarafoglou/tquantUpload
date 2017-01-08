### User input.R
#
# utilizes the custom_input_control example from Joe Cheng <joe@rstudio.com>
#

library(shiny)

ui <- shinyUI(fluidPage(
  splitLayout(
    p(
  p(h2("Auswahl der Wissenstruktur"), br(),
    "Bitte die Menge als a,b,c,d eingeben", br(),
    textInput(inputId="get_item", label="Eingabe der Grundmenge",value=""),
    actionButton(inputId="start_item", label="Eingabe bestätigen")),
    #textOutput("test"),
    #verbatimTextOutput("test"),
  p(uiOutput(outputId="p_chooser"), 
    actionButton("get_chooser", label="Auswahl bestätigen"))),
  p(p(h2("Bestehenden Datensatz verwenden"), br(),
      selectInput(inputId="slct_data",
                  "Bitte Datensatz wählen",
                  choices=c("none"=" ", "Falmangne" = "D")),
      br(), actionButton(inputId="go_slct", "Diesen Datensatz verwenden"),
      uiOutput(outputId="slct_chooser"),
      uiOutput(outputId="slct_error")))
  ),
  #actionButton(inputId="reset", label="Auswahl zurücksetzen"),
  p(h2("Darstellung der Wissenstruktur")),
  br(),
  splitLayout(p(actionButton("go_plot", "Darstellung anzeigen"), br(),
                   plotOutput("k_hasse"), br(),
                "Basis des Wissensraumes",
              verbatimTextOutput("k_base")),
              p("Atome der Items", br(), 
                selectInput(inputId="item_choose",
                            label="Auswahl der Elemente",
                            choices=""),
                actionButton("go_atom", "Atome anzeigen"),
                verbatimTextOutput("k_atom")))

  )
)
