library(shiny)
library(pks)

shinyUI(fluidPage(
  splitLayout(verticalLayout(fileInput("file_input", "Upload file"),
              column(width=8,
                     actionButton("go_file", "Use uploaded file"),
                     actionButton("go_manual", "Use manual input")),
                     # Generated UI after clicking Use manual input
                     #
                     #
                     #
                     #
                     # Blim arguments
              # Knowledge structure
              selectInput("blim_K", "Binary stateby-problem matrix, representing the knowledge structure",
                          choices="Placeholder"),
              
              # absolute frequencies
              selectInput("blim_N.R", 
                          "(named) vector of absolute frequencies of response patterns",
                          choices="Placeholder"),
              
              # Method
              selectInput("blim_method",
                          "Choose method",
                          choices=c("Minimum discrepancy"="MD",
                                    "Maximum likelihood"="ML",
                                    "Min. Discrepancy, Max. likelihood"="MDML"),
                          selected="MDML"),
              
              # Probability of knowledge states
              numericInput("blim_P.K", 
                           "Probability of initial knowledge states",
                           value=0.5),
              
              # Probability of careless error
              numericInput("blim_beta",
                           "Initial Probability of careless errors",
                           value=0.1),
              
              # Probability of lucky guesses
              numericInput("blim_eta", "Inital Probability of lucky guesses",
                           value=0.1
                           ),
              
              # Errortype
              selectInput("blim_errtype",
                          "Choose Type of occurring response error",
                          choices=c("Just careless errors"="error",
                                    "Just lucky guesses"="lucky",
                                    "Both types"="both"),
                          selected="both"),
              
              # Equal error
              checkboxInput("blim_erreq",
                            "Are equal error rates to be assumed?", value=TRUE),
              
#               selectInput("blim_erreq",
#                           "Are equal error rates to be assumed?",
#                           choices=c("Yes"=TRUE, "No"=FALSE),
#                           selected=FALSE),
              
              # Uniformly sampled initial parameters
              checkboxInput("blim_randit",
                           "Are initial parameter values to be sampled uniformly with constarints?",
                           TRUE),
#               selectInput("blim_randit",
#                           "Are initial parameter values to be sampled uniformly with constarints?",
#                           choices=c("Yes"=TRUE, "No"=FALSE),
#                           selected=FALSE),
              
              # incradius
              numericInput("blim_incradius",
                          "Inclusion radius of knowledge states from minimum discrepant states (beta)",
                          min=0, max=1, value=0, step=0.1),
              
              # Stopping criterion iteration
              numericInput("blim_tol", "Stopping criterion for iteration, 1e^-x",
                           min=1, max=20, value=7, step=1),
              
              # Number of iterations
              sliderInput("blim_maxiter",
                          "Maximum number of iterations",
                          min=1000, max=100000, step=1000, value=10000),
              
              # Maximum number of items which are to be autocompleted
              sliderInput("blim_zeropad",
                          label="Number of items for which an 
                          incomplete vector of absolute frequencies 
                          is to be padded with zeroes",
                          min=1, max=20, value=12)
              
                     ),
              verticalLayout(
                # Outputs, like summary and maybe a graph.
                
              ))
))






