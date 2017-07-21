library(shiny)
library(mvtnorm)

ui <- fluidPage(
  titlePanel("Marginal and conditional correlation"),
  p(HTML("The marginal correlation (r<sub>xy</sub>) is different from the
          conditional or partial correlation (r<sub>xy.z</sub>).")),
  sidebarLayout(
    sidebarPanel(
      sliderInput("rho", HTML("&rho;<sub>xy</sub> within groups"),
                  min=-1, max=1, value=0, step=.01,
                  animate=animationOptions(interval = 100)),
      sliderInput("n", "n per group", min=1, max=200, value=100,
                  animate=animationOptions(interval = 50)),
      sliderInput("by", "z overlap", min=0, max=4, value=2.5, step=.5),
      actionButton("action", "Resample")
    ),
    mainPanel(
      plotOutput("scatterPlot"),
      HTML("Correlation table"),
      tableOutput("corTable")
    )
  )
)

server <- function(input, output) {
  r <- reactive({
    input$action
    rho    <- input$rho
    mu     <- seq(0, by=input$by, length.out=4)
    x      <- do.call(rbind, lapply(mu, function(m)
                      rmvnorm(input$n, c(m, m), cbind(c(1, rho), c(rho, 1)))))
    dat    <- data.frame(x = x[, 1], y = x[, 2], z = rep(mu, each=input$n))
    lmlist <- lapply(mu, function(m) lm(y ~ x, dat[dat$z == m, ]))
    list( rho = rho,
           mu = mu,
          dat = dat,
       lmlist = lmlist,
       cortab = cor(dat),
         pcor = cor(resid(lm(y ~ z, dat)), resid(lm(x ~ z, dat))) )
  })

  output$scatterPlot <- renderPlot({
    zidx <- as.numeric(factor(r()$dat$z))
    par(mai=c(.5, .5, .5, .5), mgp=c(1, 0, 0), pty="s")
    plot(y ~ x, r()$dat, pch=c(1, 2, 7, 8)[zidx], axes=FALSE, cex.lab=1.5,
         col=colorRampPalette(c("#9090EE", "#80EE80"))(4)[zidx])
    abline(lm(y ~ x, r()$dat), lty=2)
    for (l in r()$lmlist) lines(l$fitted ~ l$model$x, col="darkblue")
    text(r()$mu, r()$mu, c(expression(z[1]), expression(z[2]),
                           expression(z[3]), expression(z[4])), cex=1.5)
    box()
    mtext(as.expression(substitute(r[xy] == rval*", "~r[xy.z] == pcorval,
                                   list(    rval = round(r()$cortab[1, 2], 2),
                                         pcorval = round(r()$pcor, 2) ))),
          side=3, line=.5, cex=1.5)
  })
  output$corTable <- renderTable(r()$cortab)
}

shinyApp(ui, server)

