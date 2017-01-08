server <- function(input, output) {
  output$aonPlot <- renderPlot({
    g      <- input$g
    alpha  <- input$alpha
    t      <- input$t
    pguess <- function(t, alpha) (1 - alpha)^(t - 1)

    par(mai=c(.8, .8, .1, 0), mgp=c(2, .7, 0))
    plot(1, type="n", axes=FALSE, xlim=c(0, 30), ylim=c(0, 1),
         xlab="Trial (t)", ylab=expression(P(E[t])), cex.lab=1.5)
    abline(h=0, col="gray")
    points((1 - g)*pguess(1:t, alpha) ~ I(1:t), pch=21, bg="white",
           col="darkblue")
    axis(1)
    axis(2)
    box(bty="L")

    par(plt=c(0.75, 0.95, 0.6, 0.98))
    par(new=TRUE)
    plot(1, type="n", axes=FALSE, xlim=c(0.5, 2.5), ylim=c(0, 1),
         xlab="State", ylab=expression(P(S[t])))
    abline(h=0:1, col="gray")
    points(c(pguess(t, alpha), 1 - pguess(t, alpha)) ~ I(1:2), type="h",
           col="darkblue")
    points(c(pguess(t, alpha), 1 - pguess(t, alpha)) ~ I(1:2), pch=21,
           bg="white", col="darkblue")
    axis(1, 1:2, c("G", "L"))
    axis(2)
    box()
  })
}

ui <- fluidPage(
  titlePanel("All-or-none (AON) model: predicted learning curve"),
  withMathJax(),
  p("The probability of an error on trial \\(t\\) is given by
    \\(P(E_t) = (1 - g)(1 - \\alpha)^{t - 1}\\), where \\(g\\) is the guessing
    rate and \\(\\alpha\\) is the learning rate."),
  sidebarLayout(
    sidebarPanel(
      sliderInput("g", "Guessing rate (g)",
                  min=0, max=.5, value=.5, step=.1),
      sliderInput("alpha", "Learning rate (\\(\\alpha\\))",
                  min=0, max=1, value=.2,
                  animate=animationOptions(interval = 100)),
      sliderInput("t", "Trial (t)",
                  min=1, max=30, value=1,
                  animate=animationOptions(interval = 500))
    ),
    mainPanel(
      plotOutput("aonPlot")
    )
  )
)

shinyApp(ui, server)

