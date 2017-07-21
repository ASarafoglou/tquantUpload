# Block 0
# what happens after Block 0
output$block0.0 <- renderText({
        # print knowledge state
        c("You can master 0 problems.")
})
output$block0.1 <- renderText({
        # print knowledge state
        c("You can master problem(s):", 
                       problem[which(state == TRUE)])
})

