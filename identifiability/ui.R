library(shiny)

shinyUI(fluidPage(
  titlePanel("Identifiability of a BLIM: Trade-off between parameters
             \\(\\beta_c\\) and \\(\\pi_{ab}\\)"),
  withMathJax(),

  p("The item set consists of three items, \\(Q = \\{a, b, c\\}\\), and the
    knowledge structure is \\(\\mathcal{K}^{02} = \\{\\emptyset, \\{a, b\\},
    Q\\}\\). The parameter vector is \\(\\theta' = (\\beta_a, \\beta_b,
    \\beta_c, \\pi_\\emptyset, \\pi_{ab})\\), not allowing for guessing."),

  p("For the BLIM with knowledge structure \\(\\mathcal{K}^{02}\\) and
    parameter vector \\(\\theta\\) the parameters \\(\\beta_c\\) and
    \\(\\pi_{ab}\\) are not identifiable.  Therefore, different pairs of
    values for these parameters predict the same response frequencies
    (plot on the right).  The relation of \\(\\beta_c\\) and \\(\\pi_{ab}\\)
    is described by the indifference curve in the plot below."),

  p("The values for the identifiable parameters are
    \\(\\beta_a         = 0.2\\),
    \\(\\beta_b         = 0.2\\), and
    \\(\\pi_{\\emptyset}= 0.4\\)."
    ),

  fluidRow(
    column(3,
          h4("Choose indifference curve"),
          sliderInput("beta_c_0", label="",
                      min=0, max=.99, value=.9, step=.05),
          h4("Change parameter value"),
          tabsetPanel(id="tabs",
                      tabPanel(title=HTML("$$\\beta_c$$"),
                               value="beta_c",
                               uiOutput("ui_slider_beta_c")
                      ),
                      tabPanel(title=HTML("$$\\pi_{ab}$$"),
                               value="pi_ab",
                               uiOutput("ui_slider_pi_ab")
                      )),
	  htmlOutput("note"),
	  tags$head(tags$style("#note{color: gray; font-size: 12px;
			       font-style: italic;}"))
    ),
    column(4,
          h4("Relation of \\(\\beta_c\\) and \\(\\pi_{ab}\\)"),
	  htmlOutput("text_beta_pi"),
          plotOutput("plot")
    ),
    column(4,
           h4("Predicted response frequencies (N = 100)"),
           plotOutput("predictions_plot")
    )
  )
))
