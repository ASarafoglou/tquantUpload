## Block 1
# what happens after block 1
output$block1 <- renderUI({
            # all correct from block b
            if(input$b1 == "correct" & input$b2 == "correct" & 
               input$b3 == "correct" & input$b4 == "correct"){
                state[1:8] <<- TRUE
                fluidPage(
       selectInput('c1', label = questions$c1,     # present all questions from c 
                   choices =  c('  '  = 'empty',
                                '16%' = 'choice1',
                                '48%' = 'correct',
                                '12%' = 'choice2',
                                '2%'  = 'choice3'), 
                   width = '100%'),
       selectInput('c2', label = questions$c2, 
            choices =  c('  '  = 'empty',
                         '2%'  = 'choice1',
                         '30%' = 'choice2',
                         '12%' = 'correct',
                         '50%' = 'choice3'), 
            width = '100%'),
                textOutput("block3.1"))
           # only three correct from block b
      } else if(input$b1 == "correct" & input$b2 == "correct" & 
                input$b3 == "correct" & input$b4 != "correct" &
                input$b1 != "empty"   & input$b4 != "empty") {
                state[1:7] <<- TRUE
                fluidPage(
       selectInput('c1', label = questions$c1,     # present one question from block c
                   choices =  c('  '  = 'empty',
                                '16%' = 'choice1',
                                '48%' = 'correct',
                                '12%' = 'choice2',
                                '2%'  = 'choice3'), 
                   width = '100%'),               
                textOutput("block3.2"))
      } else if(input$b1 == "correct" & input$b2 == "correct" & 
                input$b3 != "correct" & input$b4 == "correct") {
                state[c(1:4, 5, 6, 8)] <<- TRUE
                fluidPage(
       selectInput('c1', label = questions$c1,    
                   choices =  c('  '  = 'empty',
                                '16%' = 'choice1',
                                '48%' = 'correct',
                                '12%' = 'choice2',
                                '2%'  = 'choice3'), 
                   width = '100%'),              
                textOutput("block3.2"))
      } else if(input$b1 == "correct" & input$b2 != "correct" & 
                input$b3 == "correct" & input$b4 == "correct") {
                state[c(1:4, 5, 7, 8)] <<- TRUE
                fluidPage(
       selectInput('c2', label = questions$c2, 
            choices =  c('  '  = 'empty',
                         '2%'  = 'choice1',
                         '30%' = 'choice2',
                         '12%' = 'correct',
                         '50%' = 'choice3'), 
            width = '100%'),          
                textOutput("block3.3")) 
      } else if(input$b1 != "correct" & input$b2 == "correct" & 
                input$b3 == "correct" & input$b4 == "correct" &
                input$b1 != "empty"   & input$b4 != "empty") {
                state[c(2:4, 6, 7, 8)] <<- TRUE
                fluidPage(
      selectInput('a1', label = questions$a1, # present 1 question from block a
            choices =  c('  '  = 'empty',
                         '.75' = 'choice1',
                         '.65' = 'choice2',
                         '.40' = 'choice3',
                         '.55' = 'correct'),
            width = '100%'),               
                uiOutput("block2.1"))           
          # only two correct from block b
      } else if(input$b1 == "correct" & input$b2 == "correct" & 
                input$b3 != "correct" & input$b4 != "correct" &
                input$b3 != "empty"   & input$b4 != "empty") {
                state[c(1:3, 5, 6)] <<- TRUE
                fluidPage(
       selectInput('a4', label = questions$a4,  
            choices =  c('  '  = 'empty', 
                         '.75' = 'choice2',
                         '.35' = 'choice2',
                         '.40' = 'choice3',
                         '.65' = 'correct'), 
            width = '100%'),
                uiOutput("block2.2"))
      } else if(input$b1 != "correct" & input$b2 == "correct" & 
                input$b3 == "correct" & input$b4 != "correct" &
                input$b1 != "empty"   & input$b4 != "empty") {
                state[c(2:4, 6, 7)] <<- TRUE
                fluidPage(
     selectInput('a1', label = questions$a1, 
            choices =  c('  '  = 'empty',
                         '.75' = 'choice1',
                         '.65' = 'choice2',
                         '.40' = 'choice3',
                         '.55' = 'correct'),
            width = '100%'),              
                uiOutput("block2.1"))    
      } else if(input$b1 != "correct" & input$b2 == "correct" & 
                input$b3 != "correct" & input$b4 == "correct" &
                input$b1 != "empty"   & input$b4 != "empty") {
                state[c(2:4, 6, 8)] <<- TRUE
                fluidPage(
    selectInput('a1', label = questions$a1, 
            choices =  c('  '  = 'empty',
                         '.75' = 'choice1',
                         '.65' = 'choice2',
                         '.40' = 'choice3',
                         '.55' = 'correct'),
            width = '100%'),               
                uiOutput("block2.1"))    
      } else if(input$b1 == "correct" & input$b2 != "correct" & 
                input$b3 == "correct" & input$b4 != "correct" &
                input$b1 != "empty"   & input$b4 != "empty") {
                state[c(1, 2, 4, 5, 7)] <<- TRUE
                fluidPage(
      selectInput('a3', label = questions$a3, 
            choices =  c('  '  = 'empty',
                         '.40' = 'choice1',
                         '.32' = 'correct',
                         '1.20'= 'choice2',
                         '.50' = 'choice3'),
            width = '100%'),              
                textOutput("block2.3")) 
      } else if(input$b1 != "correct" & input$b2 != "correct" & 
                input$b3 == "correct" & input$b4 == "correct" &
                input$b1 != "empty"   & input$b4 != "empty") {
                state[c(2:4, 7, 8)] <<- TRUE
                fluidPage(
      selectInput('a1', label = questions$a1, 
            choices =  c('  '  = 'empty',
                         '.75' = 'choice1',
                         '.65' = 'choice2',
                         '.40' = 'choice3',
                         '.55' = 'correct'),
            width = '100%'),            
                uiOutput("block2.1"))    
      } else if(input$b1 == "correct" & input$b2 != "correct" & 
                input$b3 != "correct" & input$b4 == "correct") {
                state[c(1:4, 5, 8)] <<- TRUE
                fluidPage(                                 # finished K(b1, b4)
            textOutput("block0.1")
            ) 
         # only one correct from block b
      } else if(input$b1 != "correct" & input$b2 != "correct" & 
                input$b3 != "correct" & input$b4 == "correct" &
                input$b1 != "empty"   & input$b4 != "empty") {
                state[c(3, 4, 8)] <<- TRUE
                fluidPage(
      selectInput('a1', label = questions$a1, # present 2 questions from block a
            choices =  c('  '  = 'empty',
                         '.75' = 'choice1',
                         '.65' = 'choice2',
                         '.40' = 'choice3',
                         '.55' = 'correct'),
            width = '100%'), 
     selectInput('a2', label = questions$a2,
            choices = c('  '    = 'empty',
                        '1/3'   = 'choice1',
                        '12/30' = 'correct',
                        '1/2'   = 'choice2',
                        '8/30'  = 'choice3'), 
            width = '100%'),
                textOutput("block2.4"))    
      } else if(input$b1 != "correct" & input$b2 != "correct" & 
                input$b3 == "correct" & input$b4 != "correct" &
                input$b1 != "empty"   & input$b4 != "empty") {
                state[c(2, 4, 7)] <<- TRUE
                fluidPage(
     selectInput('a1', label = questions$a1, 
            choices =  c('  '  = 'empty',
                         '.75' = 'choice1',
                         '.65' = 'choice2',
                         '.40' = 'choice3',
                         '.55' = 'correct'),
            width = '100%'),                
     selectInput('a3', label = questions$a3, 
            choices =  c('  '  = 'empty',
                         '.40' = 'choice1',
                         '.32' = 'correct',
                         '1.20'= 'choice2',
                         '.50' = 'choice3'),
            width = '100%'),               
                textOutput("block2.5")) 
      } else if(input$b1 != "correct" & input$b2 == "correct" & 
                input$b3 != "correct" & input$b4 != "correct" &
                input$b1 != "empty"   & input$b4 != "empty") {
                state[c(2, 3, 6)] <<- TRUE
                fluidPage(
      selectInput('a1', label = questions$a1, 
            choices =  c('  '  = 'empty',
                         '.75' = 'choice1',
                         '.65' = 'choice2',
                         '.40' = 'choice3',
                         '.55' = 'correct'),
            width = '100%'),  
      selectInput('a4', label = questions$a4, 
            choices =  c('  '  = 'empty', 
                         '.75' = 'choice2',
                         '.35' = 'choice2',
                         '.40' = 'choice3',
                         '.65' = 'correct'), 
            width = '100%'),
                textOutput("block2.6"))    
      } else if(input$b1 == "correct" & input$b2 != "correct" & 
                input$b3 != "correct" & input$b4 != "correct" &
                input$b1 != "empty"   & input$b4 != "empty") {
                state[c(1, 2, 5)] <<- TRUE
                fluidPage(                                 
       selectInput('a3', label = questions$a3, 
            choices =  c('  '  = 'empty',
                         '.40' = 'choice1',
                         '.32' = 'correct',
                         '1.20'= 'choice2',
                         '.50' = 'choice3'),
            width = '100%'),   
       selectInput('a4', label = questions$a4, 
            choices =  c('  '  = 'empty', 
                         '.75' = 'choice2',
                         '.35' = 'choice2',
                         '.40' = 'choice3',
                         '.65' = 'correct'), 
            width = '100%'), 
                textOutput("block2.7"))        
         # none correct from block b but none empty
            } else if(input$b1 != "correct" & input$b2 != "correct" & 
                      input$b3 != "correct" & input$b4 != "correct" &
                      input$b1 != "empty"   & input$b4 != "empty") {
                fluidPage(
    selectInput('a1', label = questions$a1, # present all questions from block a
            choices =  c('  '  = 'empty',
                         '.75' = 'choice1',
                         '.65' = 'choice2',
                         '.40' = 'choice3',
                         '.55' = 'correct'),
            width = '100%'),                
     selectInput('a2', label = questions$a2,
            choices = c('  '    = 'empty',
                        '1/3'   = 'choice1',
                        '12/30' = 'correct',
                        '1/2'   = 'choice2',
                        '8/30'  = 'choice3'), 
            width = '100%'),
      selectInput('a3', label = questions$a3, 
            choices =  c('  '  = 'empty',
                         '.40' = 'choice1',
                         '.32' = 'correct',
                         '1.20'= 'choice2',
                         '.50' = 'choice3'),
            width = '100%'),
       selectInput('a4', label = questions$a4, 
            choices =  c('  '  = 'empty', 
                         '.75' = 'choice1',
                         '.35' = 'choice2',
                         '.40' = 'choice3',
                         '.65' = 'correct'), 
            width = '100%'),
                textOutput("block2.8"))
            } else {
                fluidPage(
                textOutput("block0.0"))
            } 
    })
