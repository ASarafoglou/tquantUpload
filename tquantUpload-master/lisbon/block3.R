## Block 3
# what happens after block 3
output$block3.1 <- renderText({
        if(input$c1 == "correct"){ 
                state[9] <<- TRUE
        } 
       if(input$c2 == "correct"){
                state[10] <<- TRUE}
        # print knowledge state
        c("You can master problem(s):", 
                       problem[which(state == TRUE)])
})
output$block3.2 <- renderText({
        if(input$c1 == "correct"){
                state[9] <<- TRUE}
        # print knowledge state
        c("You can master problem(s):", 
                       problem[which(state == TRUE)])
})
output$block3.3 <- renderText({
        if(input$c2 == "correct"){
                state[10] <<- TRUE}
        # print knowledge state
        c("You can master problem(s):", 
                       problem[which(state == TRUE)])
})
