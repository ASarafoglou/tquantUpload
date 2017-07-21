## Binary-Input ui.R
#
# mostly based on J. Hellers Code tatsuoka_KST_ita
#
# Upload a .txt-file (or similar) to the code (!)
# select the threshhold on which data will be dichtomised (?)
# select the amount of items (!)
# select the displaynames of items (!)
# get feedback on what this dataframe might look like (!)
# get feedback on response patterns (!)
# select threshhold for precedence (!)
# display precedence (!)
# check on axioms:
#       (1)   transitive closure              ()
#       (2)   Forward/backward graded         ()
#       (3)   Vereinigungs/Durchschnittstabil ()
# Get knowledge structure from precedence (!)
# display knowledge structure ()
#       (1)   Display the item combination ()


library(shiny)
library(CDM)
library(pks)
library(relations)

shinyUI(fluidPage(
  h3("Datensatz auswaehlen"),
  sidebarLayout(sidebarPanel(
  fileInput(inputId="get_data", label="Datensatz auswaehlen"
            #,
            #accept=c("text/csv", 
            #         "text/comma-separated-values,text/plain", 
            #         ".csv", ".txt")
            ),
  checkboxInput('header', 'Header', TRUE),
  radioButtons("sep", "Trennzeichen",
               c(Comma=',',
                 Semicolon=';',
                 Tab='\t'),
               ','),
  radioButtons('quote', "Anfuehrungszeichen",
               c(None='',
                 'Double Quote'='"',
                 'Single Quote'="'"),
               '"'),
  actionButton("go_data", "Datensatz einlesen")),
  mainPanel(
  tableOutput("show_data"),
  p("Anzahl der Spalten:"),
  verbatimTextOutput("ncols"),
  p("Spaltennamen:"),
  verbatimTextOutput("ncol_names"))),
  h3("Einstellungen fuer die Precedence"),
  sidebarLayout(sidebarPanel(
    uiOutput("choose_sub"),
    actionButton("go_sub", "Diese Spalten verwenden"),
    uiOutput("choose_thres"),
    uiOutput("choose_names"),
    uiOutput("go_setting")
  ), mainPanel(
    p("Nachstehende Tabelle beschreibt das Loesungsverhalten. Dabei benennt das Paar (x,y), dass x nicht geloest wurde, dafuer aber y. Grosse Werte in einer Zelle deuten eine Verletzung einer moeglichen Precedence-relation an."),
    tableOutput("b_konti"),
    p("Frequenzen der Antwortmuster:"),
    verbatimTextOutput("pattern.response")
  )),
  h3("PrecendenceRelation mit korrespondierender Wissenstruktur"),
  verbatimTextOutput("precedence"), 
  "Korrespondierende Wissensstruktur",
  verbatimTextOutput("print_K"),
  #verbatimTextOutput("hasse_prec")
  h4("Hasse-Diagramme der Precedence und korrespondierender Wissenstruktur"),
  plotOutput("hasse_prec"),
  plotOutput("hasse_K.rel"),
  h5("Induzierte Wissensstruktur"),
  verbatimTextOutput("K.ind")
  
))



