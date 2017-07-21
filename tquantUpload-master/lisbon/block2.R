## Block 2
# what happens when we ask questions in block a
# ask one question
# ask a1
output$block2.1 <- renderUI({
         if(input$a1 == "correct"){
                  state[1] <<- TRUE}
          fluidPage(
     selectInput('c2', label = questions$c2, 
            choices =  c('  '  = 'empty',
                         '2%'  = 'choice1',
                         '30%' = 'choice2',
                         '12%' = 'correct',
                         '50%' = 'choice3'), 
            width = '100%'),
                textOutput("block3.3"))
})
# ask a4
output$block2.2 <- renderUI({
         if(input$a4 == "correct"){
                  state[4] <<- TRUE}
          fluidPage(
     selectInput('c1', label = questions$c1, 
            choices =  c('  '  = 'empty',
                         '16%' = 'choice1',
                         '48%' = 'correct',
                         '12%' = 'choice2',
                         '2%'  = 'choice3'), 
            width = '100%'),
                textOutput("block3.2"))
})
# ask a3
output$block2.3 <- renderText({
         if(input$a3 == "correct"){
                  state[3] <<- TRUE}
        # print knowledge state
        c("You can master problem(s):", 
                       problem[which(state == TRUE)])
})
# ask two questions
# ask a1 and a2
output$block2.4 <- renderText({
         if(input$a1 == "correct"){
                  state[1] <<- TRUE}
         if(input$a2 == "correct"){
                  state[2] <- TRUE} 
        # print knowledge state
        c("You can master problem(s):", 
                       problem[which(state == TRUE)])
})
# ask a1 and a3
output$block2.5 <- renderText({
         if(input$a1 == "correct"){
                  state[1] <<- TRUE}
         if(input$a3 == "correct"){
                  state[3] <<- TRUE} 
        # print knowledge state
        c("You can master problem(s):", 
                       problem[which(state == TRUE)])
})
# ask a1 and a4
output$block2.6 <- renderText({
         if(input$a1 == "correct"){
                  state[1] <<- TRUE}
         if(input$a4 == "correct"){
                  state[4] <<- TRUE} 
        # print knowledge state
        c("You can master problem(s):", 
                       problem[which(state == TRUE)])
})
# ask a3 and a4
output$block2.7 <- renderText({
         if(input$a3 == "correct"){
                  state[3] <<- TRUE}
         if(input$a4 == "correct"){
                  state[4] <<- TRUE} 
        # print knowledge state
        c("You can master problem(s):", 
                       problem[which(state == TRUE)])
})
# ask a1, a2, a3 and a4
output$block2.8 <- renderText({
         if(input$a1 == "correct"){
                  state[1] <<- TRUE}
         if(input$a2 == "correct"){
                  state[2] <<- TRUE}
         if(input$a3 == "correct"){
                  state[3] <<- TRUE}
         if(input$a4 == "correct"){
                  state[4] <<- TRUE} 
        # print knowledge state
         if(sum(state) == 0){
                       c("You can master 0 problems.")
                       } else {
        c("You can master problem(s):", 
                       problem[which(state == TRUE)])}
})
