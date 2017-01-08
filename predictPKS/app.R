shinyApp(
  ui = tagList(
    navbarPage(
      theme = "united",  # <--- To use a theme, uncomment this
      "Probabilistic Knowledge Structures",
      tabPanel("Estimation",
               sidebarPanel(
                 tags$h4("Model specification"), br(),
                 tags$h5("Change initial values:"), 
                 checkboxInput("initCareless", "Careless errors", value = FALSE),
                 checkboxInput("initLuckyguess", "Lucky guesses", value = FALSE),
                 checkboxInput("initResponses", "Response frequencies", value = FALSE),
                 selectInput("analysis", "Analysis types:",
                             choices = c("Bayesian analysis"  = "bayesian",
                                         "Classical analysis" = "classic"),
                             selected = "classic"
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Analysis",
                            h4("Initialize your model:"),
                            uiOutput("initializeCareless"),
                            uiOutput("initializeLuckyguess"),
                            textOutput("initializeResponses"),
                            uiOutput("Blim")
                   ),
                   tabPanel("Additional information",
                            h4("Convergence diagnostic for Bayesian parameter estimation:"),
                            dataTableOutput("convergenceTable")
                            ),
                   tabPanel("See code",
                            h4("This functionality will be available soon"))
                 )
               )
      ),
      tabPanel("About",
               h4("This functionality will be available soon")),
      tabPanel("The Team",
               h4("This functionality will be available soon"))
    )
  ),
  
  server = function(input, output) {
    ## source all necessary scripts
    source("functions.R")
    source("PlotFunctions.R")
    
    # initialize parameter values
    output$initializeResponses <- renderText(
      if(input$initResponses){
        "The functionality to initialize response frequencies will be available soon"
      }
      )
    output$initializeCareless <- renderUI({
      if(input$initCareless){
        fluidPage(
          h5("Define the probability that a person makes a careless error on the following items:"),
          fluidRow(
            column(3,
              numericInput("beta1", "Item a", 0, 1, value = 0.1, step = 0.1)),
            column(3,
              numericInput("beta2", "Item b", 0, 1, value = 0.1, step = 0.1)),
            column(3,
              numericInput("beta3", "Item c", 0, 1, value = 0.1, step = 0.1)),
            column(3,
              numericInput("beta4", "Item d", 0, 1, value = 0.1,step = 0.1))
          )
        )
      }
    })
    output$initializeLuckyguess <- renderUI({
      if(input$initLuckyguess){
        fluidPage(
          h5("Define the probability that a person makes a lucky guess on the following items:"),
          fluidRow(
            column(3,
             numericInput("eta1", "Item a", 0, 1, value = 0.1, step = 0.1)),
            column(3,
             numericInput("eta2", "Item b", 0, 1, value = 0.1, step = 0.1)),
            column(3,
              numericInput("eta3", "Item c", 0, 1, value = 0.1, step = 0.1)),
            column(3,
             numericInput("eta4", "Item d", 0, 1, value = 0.1, step = 0.1))  
          )
        )
      }
    })
    # Frequentist Analysis
    goFrequentistAnalysis <- eventReactive(input$runFrequentistAnalysis, {
      
      if(input$initLuckyguess){
        etaInput <- c(input$eta1, input$eta2, input$eta3, input$eta4)
      } else {
        etaInput <- rep(0.1, 4)
      }
      if(input$initCareless){
        betaInput <- c(input$beta1, input$beta2, input$beta3, input$beta4)
      } else {
        betaInput <- rep(0.1, 4)
      } 
      
      list(method = input$estimationMethod,
           eta  = etaInput,
           beta = betaInput,
           NR   = round(runif(7, 0, 200))
      )
    })
    output$freqBlimPlot <- renderPlot({
      print(goFrequentistAnalysis())
      blim1 <- frequentistParameterEstimation(N.R = goFrequentistAnalysis()$NR, 
                                               estimation.method = goFrequentistAnalysis()$method, 
                                               beta.parameters   = goFrequentistAnalysis()$beta,
                                               eta.parameters    = goFrequentistAnalysis()$eta)
      print(blim1)
      matrix <- matrix(c(1, 1, 3, 3, 3, 3, 3, 3, 3, 
                         1, 1, 3, 3, 3, 3, 3, 3, 3, 
                         1, 1, 3, 3, 3, 3, 3, 3, 3, 
                         2, 2, 3, 3, 3, 3, 3, 3, 3, 
                         2, 2, 3, 3, 3, 3, 3, 3, 3, 
                         2, 2, 3, 3, 3, 3, 3, 3, 3), 6, 9, byrow = TRUE)
      layout(matrix)
      betaPlot(blim1$beta)
      etaPlot(blim1$eta)
      ksPlot(blim1$P.K)
    })
    # Bayesian Analysis
    goBayesianAnalysis <- eventReactive(input$runBayesianAnalysis, {
      
      if(input$initLuckyguess){
        eta.percentages <- c(input$eta1, input$eta2, input$eta3, input$eta4)
      } else {
        eta.percentages <- rep(0.5, 4)
      }
      if(input$initCareless){
        beta.percentages <- c(input$beta1, input$beta2, input$beta3, input$beta4)
      } else {
        beta.percentages <- rep(0.5, 4)
      }
      
      list(samples     = bayesianParameterEstimation(N.R = round(runif(7, 0, 200)),
                                                     beta.percentages = beta.percentages,
                                                     eta.percentages = eta.percentages),
           item.number = as.numeric(input$plotPosterior))
    })
    output$bayesPriorPosteriorPlot <- renderPlot({
      item.number <- goBayesianAnalysis()$item.number
      print(item.number)
      
      if(input$initLuckyguess){
        etaInput <- c(input$eta1, input$eta2, input$eta3, input$eta4)
      } else {
        etaInput <- rep(0.5, 4)
      }
      if(input$initCareless){
        betaInput <- c(input$beta1, input$beta2, input$beta3, input$beta4)
      } else {
        betaInput <- rep(0.5, 4)
      } 
      
      bayesHist(goBayesianAnalysis()$samples, item.number = item.number,
                beta.a = betaInput*10, eta.a = etaInput*10)
    }, height = 1000, width = 600 )
    output$bayesBlimPlot <- renderPlot({
      matrix <- matrix(c(1, 1, 3, 3, 3, 3, 3, 3, 3, 
                         1, 1, 3, 3, 3, 3, 3, 3, 3, 
                         1, 1, 3, 3, 3, 3, 3, 3, 3, 
                         2, 2, 3, 3, 3, 3, 3, 3, 3, 
                         2, 2, 3, 3, 3, 3, 3, 3, 3, 
                         2, 2, 3, 3, 3, 3, 3, 3, 3), 6, 9, byrow = TRUE)
      layout(matrix)
      betaPlot(goBayesianAnalysis()$samples$BUGSoutput$mean$beta)
      etaPlot(goBayesianAnalysis()$samples$BUGSoutput$mean$eta)
      ksPlot(goBayesianAnalysis()$samples$BUGSoutput$mean$P.K)
    })
    # Bayesian convergence diagnostics
    output$convergenceTable <- renderDataTable({
      convergenceDiagnostics(goBayesianAnalysis()$samples)
    })
    #  Output
    output$Blim <- renderUI({
      if(input$analysis == "classic"){
        fluidPage(
          selectInput("estimationMethod", label = "Choose a parameter estimation method:",
                      choices = c("Maximum likelihood method (ML)" = "ML",
                                  "Minimum discrepancy method (MD)" = "MD",
                                  "Mixed (MDML)" = "MDML"),
                      selected = "ML"),
          actionButton("runFrequentistAnalysis", "Estimate model", class = "btn-primary"),
          plotOutput("freqBlimPlot")
        )
      } else {
        fluidPage(
          checkboxGroupInput("plotPosterior", "Plot the prior and posterior distribution for the following items:",
                             c("Item a" = 1,
                               "Item b" = 2,
                               "Item c" = 3,
                               "Item d" = 4),
                             selected = c(1:4),
                             inline = TRUE),
          actionButton("runBayesianAnalysis", "Estimate model", class = "btn-primary"),
          plotOutput("bayesBlimPlot"),
          plotOutput("bayesPriorPosteriorPlot")
        )        
      }
    })
# end of the app
  }
)


