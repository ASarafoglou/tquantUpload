library(shiny)

source("helpers.R")

shinyServer(function(input, output) {
  # set fixed parameters
  pi.ab.0 <- 0
  pi.empty <- .4
  beta.a <- .2
  beta.b <-  .2
  beta.c.default <- .33
  pi.ab.default <- .51

  output$ui_slider_beta_c <- renderUI({
    pi.ab <- ifelse(is.null(input$pi_ab), pi.ab.default, input$pi_ab)
    beta.c.0 <- input$beta_c_0
    beta.c.start <- calc_beta_c(pi.ab, pi.ab.0, beta.c.0, pi.empty)
    sliderInput("beta_c", label="", min=0, max=beta.c.0,
                value=beta.c.start, step=.01, animate=animationOptions(200))
  })

  output$ui_slider_pi_ab <- renderUI({
    beta.c <- ifelse(is.null(input$beta_c), beta.c.default, input$beta_c)
    beta.c.0 <- input$beta_c_0
    max <- round(beta.c.0 * (1 - pi.empty) + (1 - beta.c.0) * pi.ab.0,
                 digits=3)
    pi.ab.start <- calc_pi_ab(beta.c, pi.ab.0, beta.c.0, pi.empty)
    sliderInput("pi_ab", label="", min=0, max=max, step=.01,
                value=pi.ab.start)
  })

  parInput <- reactive({
    beta.c.0 <- input$beta_c_0
    if (input$tabs == "beta_c") {
        beta.c <- ifelse(is.null(input$beta_c),
                         beta.c.default,
                         input$beta_c)
        pi.ab <- round(calc_pi_ab(beta.c, pi.ab.0, beta.c.0, pi.empty),
                       digits=2)
    } else if (input$tabs == "pi_ab") {
        pi.ab <- ifelse(is.null(input$pi_ab),
                        calc_pi_ab(input$beta_c, pi.ab.0, beta.c.0, pi.empty),
                        input$pi_ab)
        beta.c <- round(calc_beta_c(pi.ab, pi.ab.0, beta.c.0, pi.empty),
                        digits=2)
    }
    list(beta.c=beta.c, pi.ab=pi.ab, beta.c.0=beta.c.0)
  })

  output$text_beta_pi <- renderUI({
    HTML("<i>&beta;<sub>c</sub></i> = ", parInput()$beta.c, " and ",
         "<i>&pi;<sub>ab</sub></i> = ", parInput()$pi.ab)
  })

  output$note <- renderUI({
    if (parInput()$beta.c >= .5) {
        note <- "Note that <i>&beta;<sub>c</sub></i> &le; 0.5 is not reasonable."
    } else note <- ""
    HTML(note)
  })

  # set parameters for plotting indifference curve
  ngp <- c(10, 10)
  xoffset <- 0.05
  yoffset <- 0.05 * (1 - pi.empty)
  xstep <- (1 - 2 * xoffset) / (ngp[1] - 1)

  output$plot <- renderPlot({
    par(cex.lab=1.2, mar=c(4.1, 4.0, 0, 0), pty="s")
    plotVectorField(
                    vecfun = function(x1, x2) {
                      c(-(1 - x1) / (1 - pi.empty - x2), 1 + 0 * x2)
                    },
                    xlim = c(0 + xoffset, 1 - xoffset),
                    ylim = c(0 + yoffset, 1 - pi.empty - yoffset),
                    grid.points = ngp
                    )
    axis(1, seq(0.0, 1.0, 0.1))
    axis(2, seq(0.0, 1 - pi.empty, 0.1))
    # insert curves into plot
    beta.c.0 <- parInput()$beta.c.0
    curve(calc_pi_ab(x, pi.ab.0, beta.c.0, pi.empty),
            xlim=c(0, (pi.ab.0 + beta.c.0 * (1 - pi.empty - pi.ab.0)) /
            (1 - pi.empty)),
            col="blue", add=TRUE)

    # Draw point
    points(parInput()$beta.c, parInput()$pi.ab, pch=16, cex=2, col="blue")
  })

  output$predictions_plot <- renderPlot({
    pred <- predictionFunction(beta.a, beta.b, parInput()$beta.c, pi.empty,
                               parInput()$pi.ab)
    pred <- c(pred, phi.abc = 1 - sum(pred)) * 100
    barplot(pred,
            horiz = TRUE,
            names.arg = c("0", "a", "b", "c", "ab", "ac", "bc", "abc"),
            cex.names = 1.3,
            las = 2,
            axes = FALSE,
            col = "lightblue",
            xlim = c(0, max(pred)),
            border = NA
            )
    axis(side = 3, cex.axis = 1.3)
  })

})

# TO DOs
