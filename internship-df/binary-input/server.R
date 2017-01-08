## Binary-Input server.R
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

server <- shinyServer(function(input, output, session){
  
  # Dataframe eingelesen
  dat <- reactiveValues()
  observeEvent(input$go_data,{
    if(is.null(input$get_data)){return(NULL)}
    else{
    inFile <- input$get_data
    dat$file <- read.table(inFile$datapath, header=input$header, sep=input$sep,
                           quote=input$quote)
    dat$file_nrows <- nrow(dat$file)
    dat$file_ncols <- ncol(dat$file)
    dat$file_names <- names(dat$file)}
    })
  
  # Dataframe kurz visualisiert
  output$show_data <- renderTable({
    if(is.null(dat$file))
      return(NULL)
    
    else{
    if(dat$file_ncols > 8){head(dat$file[, 1:8])}
    else{head(dat$file)}}
    })
  
  output$ncols <- renderPrint({
    if(is.null(dat$file))
    return(NULL)
    
    else{dat$file_ncols}})
  
  output$ncol_names <- renderPrint({
    if(is.null(dat$file))
    return(NULL)
    
    else{dat$file_names}})
  
  # Dataframe beschneiden
  output$choose_sub <- renderUI({if(is.null(dat$file))
    return(NULL)
    
    else
    sliderInput("choose_sub", "Welche Spalten?",
                            min=0, max=dat$file_ncols,
                            value=c(0,dat$file_ncols), step=1)})
  
  # Kontingenztabellen erstellen,
  # Response patterns
  observeEvent(input$go_sub, {
    dat$file_sub <- dat$file[,input$choose_sub[1]:input$choose_sub[2]]
    
    dat$file_sub.NR <- as.pattern(dat$file_sub, freq=TRUE)
    
    dat$file_sub.nitems <- ncol(dat$file_sub)
    dat$file_sub.nsubj <- nrow(dat$file_sub)
    
    dat$file_sub.a <- t(apply(dat$file_sub, 2, function(p) apply(dat$file_sub, 2, function(q) sum((1 - p) * (1 - q)))))
    
    dat$file_sub.b <- t(apply(dat$file_sub, 2, function(p) apply(dat$file_sub, 2, function(q) sum((1 - p) * q))))
    
    dat$file_sub.c <- t(apply(dat$file_sub, 2, function(p) apply(dat$file_sub, 2, function(q) sum(p * (1 - q)))))
    
    dat$file_sub.d <- t(apply(dat$file_sub, 2, function(p) apply(dat$file_sub, 2, function(q) sum(p * q))))
    ## contingency table entries
    ## (b contains the frequency of p not solved, while q solved)
    ##
    ##                q
    ##              0   1
    ##            --------
    ##          0 | a   b
    ##        p   |
    ##          1 | c   d
    ##
    
    # multiplies columns wth each other, 1-q checks if there are 1 unsolved
    # or 0 solved then via cellvise multiplication checks if a 1 in column p 
    # is met
    # with 1 in column q, then sums those cases up
    # Same goes for everything else here, but the states changes
    
    output$pattern.response <- renderPrint({if(is.null(dat$file_sub))
      return(NULL)
      
      dat$file_sub.NR})
    
    
    output$b_konti <- renderTable({if(is.null(dat$file_sub))
      return(NULL)
      
      dat$file_sub.b}) # Rundung?
    output$choose_thres <- renderUI({
      numericInput("choose_thres",
                   "Grenze fuer Verletzung waehlen, Buggy wenn zu gross",
                   min=min(dat$file_sub.b),
                   max=max(dat$file_sub.b),
                   value=(min(dat$file_sub.b)+max(dat$file_sub.b))/4
                   
                   )})
    output$choose_names <- renderUI({
      selectInput("choose_names",
                  "Itemnamen waehlen",
                  choices=c("letters",
                            "LETTERS",
                            #"Numeric"=paste("i", dat$file_sub),
                            "As.is"))})
    
    output$go_setting <- renderUI({actionButton("go_setting", "Einstellungen verwenden")})
  })
  
  # Precedence-relation nach Absolutem Wert
  observeEvent(input$go_setting, {
    if(input$choose_names!="As.is"){
      if(input$choose_names!="letters"){
        dat$file_sub.names <- LETTERS[1:dat$file_sub.nitems]
      }
      else{dat$file_sub.names <- letters[1:dat$file_sub.nitems]}
    }
    else{dat$file_sub.names <- colnames(dat$file_sub)}
    
    dat$values <- data.frame(i=rep(1:dat$file_sub.nitems,
                                   each=dat$file_sub.nitems),
                             j=rep(1:dat$file_sub.nitems,
                                   dat$file_sub.nitems))
    
    dat$values$b <- dat$file_sub.b[as.matrix(
      dat$values[sample(
        dat$file_sub.nitems^2, dat$file_sub.nitems^2), c("i", "j")])]
    # samples nitems^2 times from nitems^2 (a way to get the indices of values)
    # then extracts i,j, which then gives the indices for values extracted from b
    # then assigns them to values and orders them decreasingly.
    dat$values <- dat$values[order(dat$values$b, decreasing=TRUE), ]
    rownames(dat$values) <- NULL
    
    # Everything smaller than 20 appears after the ii. index
    dat$crit.val <- input$choose_thres
    dat$ii <- min(which(dat$values$b <= dat$crit.val))
    
    ## precedence relation as matrix
    (dat$precedence <- (dat$file_sub.b <= dat$values$b[dat$ii]) + 0)
    # Row indicates the requirement of the column
    # looking at b, high value in (i,j) indicates that it is unlikely that
    # i is a prequisite for j, low values speak for being a requirement
    # + 0 eliminates TRUE/FALSE
    rownames(dat$precedence) <- colnames(dat$precedence) <- dat$file_sub.names
    
    dat$prec_rel <- relation(graph=as.data.frame(
      which(dat$precedence == 1, arr.ind=T)))

    # WIP, auswahl was verletzt sein soll, dann anzeige des funktions-outputs
    #
    # forward, backward, transitive, durchschnitts? aber ist das nicht irgendwo
    # enthalten?
#     output$test_vio <- renderUI({
#       selectInput("testvio",
#                   "Verletzt die Precedence ausgewaehlte Eigenschaft?",
#                   choices=)})
    
    dat$prec_trans <- transitive_closure(
      relation(graph=as.data.frame(
        which(dat$precedence == 1, arr.ind=T))))
    
    output$precedence <- renderPrint({dat$precedence})
    output$hasse_prec <- renderPlot({
      plot(dat$prec_trans, labels=list(
        dat$file_sub.names, dat$file_sub.names), main="Precedence-Relation")
    })
    
    # Knowledge space from precedence
    dat$K.0 <- expand.grid(rep(list(0:1), dat$file_sub.nitems))
    # initially all response patterns
    dat$retain <- rep(TRUE, nrow(dat$K.0))
    # will be used later to see if elements of K.0 (response patterns)
    # can be found in precedence
    ## for-loop
    for (i in 1:nrow(dat$K.0)) {
      for (j in 1:dat$file_sub.nitems) {
        if (dat$K.0[i, j] == 1) {# If there is a 1 in cell i,j
          if (any(dat$precedence[, j] > dat$K.0[i, ])) { 
            # Any states if there are values true
            # in this case: if there is a value in prec 
            # but not in K.0 then i. index
            # is set to FALSE (initially true)
            dat$retain[i] <- FALSE
            break
          }
        }
      }
    }
    ## Ende for-loop
    dat$K <- dat$K.0[dat$retain, 1:dat$file_sub.nitems] 
    # Extracts the rows for which retain==TRUE
    colnames(dat$K) <- dat$file_sub.names
    dat$K.rowname <- as.pattern(dat$K)
    
    output$print_K <- renderPrint({
      rownames(dat$K) <- as.pattern(dat$K)
      dat$K})
    
    ## set inclusion as order relation on knowledge structure
    dat$K.subseteq <- t(sapply(1:nrow(dat$K), function(i){
      sapply(1:nrow(dat$K), function(j){
        all(dat$K[i, ] * dat$K[j, ] == dat$K[i, ]) 
        # If all of K[i,] is in K[j,] then TRUE, else FALSE
      } )
    } ))
    dat$K.rel <- relation(graph=as.data.frame(
      which(dat$K.subseteq == TRUE, arr.ind=T)))
    
    dat$names.states <- apply(dat$K, 1, function(x){
      paste(colnames(dat$K)[x == 1], collapse=",")})
    
    dat$states <- paste("{", dat$names.states, "}", sep="")
    
    output$hasse_K.rel <- renderPlot({
      plot(dat$K.rel, labels=list(dat$states, dat$states), main="Korrespondierende Wissenstruktur")})
    
    output$K.ind <- renderPrint({
      print(as.set(dat$states), quote=FALSE)})
  })

})



