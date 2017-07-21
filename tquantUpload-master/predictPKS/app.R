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
                             selected = "bayesian"
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Analysis",
                            h4("Initialize your model:"),
                            uiOutput("initializeCareless"),
                            uiOutput("initializeLuckyguess"),
                            uiOutput("initializeResponses"),
                            uiOutput("Blim")
                   ),
                   tabPanel("Additional information",
                            h4("Convergence diagnostic for Bayesian parameter estimation"),
                            p("This panel provides information about the quality of the obtained MCMC samples, 
                              by means of Gelman and Rubins (1992) scale reduction factor and traceplots for each 
                              parameter of interest. Note that a scale reduction factor close to 
                              1 indicates good convergence. In addition, a good overlap between the chains in the traceplot 
                              indicates that the chains have converged from their initial starting values to
                              their stationary distribution. If convergence cannot be assumed, try out a larger number
                              of samples, a longer burn-in period or more thinning. Details about Bayesian cognitive modelling
                              and MCMC methods can be found, e.g., in Lee and Wagenmakers (2013)."),
                            dataTableOutput("convergenceTable"),
                            plotOutput("bayesTrace")
                            )
                 )
               )
      )
    )
  ),
  
  server = function(input, output) {
    ## source all necessary scripts
    source("functions.R")
    source("PlotFunctions.R")
    
    # initialize parameter values
    output$initializeResponses <- renderUI({
      if(input$initResponses){
        fluidPage(
          h5("Insert the number of observed response patterns:"),
          fluidRow(
            column(3,
                   numericInput("NR1", "0", 0, 1e5, value = 10)),
            column(3,
                   numericInput("NR2", "bc", 0, 1e5, value = 10)),
            column(3,
                   numericInput("NR3", "bd", 0, 1e5, value = 10)),
            column(3,
                   numericInput("NR4", "abc", 0, 1e5, value = 10))
          ),
          fluidRow(
            column(3,
                   numericInput("NR5", "acd", 0, 1e5, value = 10)),
            column(3,
                   numericInput("NR6", "abd", 0, 1e5, value = 10)),
            column(3,
                   numericInput("NR7", "abcd", 0, 1e5, value = 10))
          )
        )
      }
      })
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
      if(input$initResponses){
        NRInput <- c(input$NR1, input$NR2, input$NR3, input$NR4, input$NR5, input$NR6, input$NR7)
      } else {
        NRInput <- rep(10, 7)
      } 
      
      list(method = input$estimationMethod,
           eta  = etaInput,
           beta = betaInput,
           NR   = NRInput)
    })
    output$freqBlimPlot <- renderPlot({
      print(goFrequentistAnalysis())
      blim1 <- frequentistParameterEstimation(N.R = goFrequentistAnalysis()$NR, 
                                               estimation.method = goFrequentistAnalysis()$method, 
                                               beta.parameters   = goFrequentistAnalysis()$beta,
                                               eta.parameters    = goFrequentistAnalysis()$eta)
      print(blim1)
      blimPlot(blim1$beta, blim1$eta, blim1$P.K)
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
      if(input$initResponses){
        NR.Input <- c(input$NR1, input$NR2, input$NR3, input$NR4, input$NR5, input$NR6, input$NR7)
      } else {
        NR.Input <- rep(10, 7)
      } 
      
      list(out = bayesianParameterEstimation(N.R = NR.Input,
                                             beta.percentages = beta.percentages,
                                             eta.percentages = eta.percentages,
                                             n.iter = input$niter,
                                             n.thin = input$nthin,
                                             n.burnin = input$nburnin),
           item.number = as.numeric(input$plotPosterior))
    })
    output$bayesSummaryPlotPK <- renderPlot({
      plotBayesSummary(goBayesianAnalysis()$out$PK$info, param = "PK")}, height= 450, width = 600) 
    output$bayesSummaryPlotBeta <- renderPlot({
      plotBayesSummary(goBayesianAnalysis()$out$beta$info, param = "beta")}, height= 250, width = 600)  
    output$bayesSummaryPlotEta <- renderPlot({
      plotBayesSummary(goBayesianAnalysis()$out$eta$info, param = "eta")}, height = 250, width = 600)  
    output$bayesPriorPosteriorPlot <- renderPlot({
      item.number <- goBayesianAnalysis()$item.number
      print(item.number)
      bayesHist(goBayesianAnalysis()$out, item.number = item.number)
       }, height = 1000, width = 600 )
    output$bayesBlimPlot <- renderPlot({
      blimPlot(goBayesianAnalysis()$out$beta$info$median, 
               goBayesianAnalysis()$out$eta$info$median, 
               goBayesianAnalysis()$out$PK$info$median)
    })
    
    # Bayesian convergence diagnostics
    output$convergenceTable <- renderDataTable({
      convergenceDiagnostics(goBayesianAnalysis()$out$samples)
    })
    output$bayesTrace <- renderPlot({
      bayesTrace(goBayesianAnalysis()$out$samples)
    }, height = 1000, width = 600)
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
          numericInput("niter", "Specify number of iterations", 15e4, min = 6000, max = 1e6),
          numericInput("nburnin", "Specify the burn-in period", 5000, min = 0, max = 15000),
          numericInput("nthin", "Specify the thinning", 10, min = 0, max = 200),
          checkboxGroupInput("plotPosterior", "Plot the prior and posterior distribution for the following items:",
                             c("Item a" = 1,
                               "Item b" = 2,
                               "Item c" = 3,
                               "Item d" = 4),
                             selected = c(1:4),
                             inline = TRUE),
          actionButton("runBayesianAnalysis", "Estimate model", class = "btn-primary"),
          plotOutput("bayesBlimPlot"),
          plotOutput("bayesSummaryPlotPK", height = "600px"),
          plotOutput("bayesSummaryPlotBeta"), plotOutput("bayesSummaryPlotEta"), 
          plotOutput("bayesPriorPosteriorPlot")
        )        
      }
    })
# end of the app
  }
)


