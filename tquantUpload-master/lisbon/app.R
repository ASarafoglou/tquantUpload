source("questions.R", local = TRUE) # load questions
# Load packages
library(shiny)
library(shinydashboard)
library("Rgraphviz")      # for plot
library("paircompviz")
library("pks")            # for results

##### UI PAGE #####
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Knowledge Space Structure Test", titleWidth = 350),
  dashboardSidebar(
      # Sidebar Menu Names and Icons
    sidebarMenu(
      menuItem("Welcome!", tabName = "welcome",icon = icon("fort-awesome")),
      menuItem("Introduction", tabName = "introduction",icon = icon("gear")),
      menuItem("Examples", tabName = "examples",icon = icon("eye")),
      menuItem("Test", tabName = "test", icon = icon("graduation-cap")),
      menuItem("Results", tabName = "result", icon = icon("line-chart")))),
 dashboardBody(
     tabItems(
         # Welcome Page
     tabItem(tabName = "welcome",
            h2("Teaching Basics of the Knowledge Space Theory"),
            br(),
            fluidPage(
                fluidRow(
                    column(7,
                     img(src="idea.png", height = 250),
                     br(),
                     br(),
                     img(src="tquant.png", height = "100")
                    ),
                    column(5,
                     box(title=h2("Development Team:"), 
                         solidHeader = TRUE,
                         status = "primary",
                         width = '100%',
                         h2(img(src="oldenburg.png", width = "40"),
                            "   Nadine Jacobsen"), 
                         h2(img(src="oldenburg.png", width = "40"),
                            "   Franziska Klein"),      
                         h2(img(src="lisboa.png", width = "40"),
                            "   Ana Lapa"),                         
                           h2(img(src="amsterdam.png", width="40"),
                                  "   Alexandra Sarafoglou"),
                           h2(img(src="padova.png", width = "40"),
                                  "   Alessandro Silvestri")
                           ))
                
                )
            )),
        # Introduction Page
     tabItem(tabName = "introduction",
    box(title ="Theoretical Background: What are knowledge spaces?", status = "primary", solidHeader = TRUE, width = 12,
     p('So you may wonder what this knowledge space you just saw might be?'),
     img(src = "knowledgeState.png", height = 300, align = "right"),
     p('Knowledge spaces are a formal (mathematical) theory started in the eighties for the 
       assessment of knowledge in human beings. The initial objective was to construct "an 
       efficient machine for the assessment of knowledge". So what distinguishes the style from 
       the Classical test theory and others? In KST no attempt is made to compute a numerical 
       score for representing how much a student has learned.Rather, the goal of the assessment 
       is to provide a precise description of what the student knows at a given moment.Assume that 
       the collection', span('Q', style= 'color:blue'),'of all possible questions that can be asked in 
       probabilistic theory is available. This set Q contains the ten questions asked 
       above and  is called a', span(' domain of knowledge.', style = 'color:blue')),
     p('The knowledge of a student is represented by 
       the subset of all those questions in Q that the student is capable of answering correctly 
       (excluding lucky guesses and careless errors). This subset is named knowledge state.The ' , 
       span('knowledge state K', style = 'color:blue'),' is a subset of Q. So what you will see in the example is a', span('Knowledge Structure', style = 'color:blue'), 'which is a pair', 
       span('(Q;K)', style= 'color:blue'),', where K is a collection of subsets of Q, containing at least the empty set and Q. 
       Since some of the questions depend on knowledge, also relevant for other questions, a 
       quasi-order on a set Q of questions can be established, this is called', span('surmise relation.', style= 'color:blue'),
       'In our example, people who answer Item xx correct, will also have knowledge about the 
       Items xx and xx.'),
     a('I want to learn more!', href ='https://drive.google.com/file/d/0B-lgUbL4jfs8S0hUaXBmOTNoLW8/view?pli=1'),
           br(),br()),
           img(src = "kst1.png", height = 375, width = 600, align = "center")),
    # Example Page
  tabItem(tabName = "examples",
          box(title = 'A knowledge Structure of elementary algebra', status = "primary", solidHeader = TRUE, width = 8,
          p('Suppose that after a careful examination of the following 6 problems in a toy domain of knowledge.'),
          img(src="problems.png",height = 200, align = "center"),
          p('A teacher establishes the following relations:'),
          img(src="surmiseRelation.png", height = 40, align = "center"),
          br(), br(),
          p('That means, that a person able to solve problem e is also able to solve problem b, since this is a',
            span('predecessor', style = 'color:blue'), 'of problem e.'),
          p('If you want to display this surmise relations you can use a', span('Hasse Diagram', style = 'color:blue'), 'as shown below.'),
          img(src="knowledgeStructureExample.png", height = 150, align = "center"))),
    # Testing Page
  tabItem(tabName = "test", 
          fluidPage(
              fluidRow(
                  column(8,
                  h3("Testing your Knowledge State in Basic Probability Theory"),
                         selectInput("b1", questions$b1,        # question b1
                                     choices =  c('  '   = 'empty',
                                                  '7/10' = 'correct',
                                                  '1/3'  = 'choice1',
                                                  '2/7'  = 'choice2',
                                                  '1/5'  = 'choice3'), 
                                     width = '100%'),
                         selectInput("b2", questions$b2,        # question b2
                                     choices =  c('  '   = 'empty',
                                                  '9/52' = 'choice1',
                                                  '2/52' = 'correct',
                                                  '2/9'  = 'choice2',
                                                  '9/52' = 'choice3'),
                                     width = '100%'), 
                         selectInput("b3", questions$b3,        # question b3
                                     choices =  c('  '  = 'empty', 
                                                  '1/4' = 'choice', 
                                                  '3/4' = 'correct', 
                                                  '1/2' = 'choice2', 
                                                  '1/8' = 'choice3'),
                                     width = '100%'),
                         selectInput("b4", questions$b4,        # question b4
                                     choices =  c('  '  = 'empty',
                                                  '40%' = 'choice1',
                                                  '70%' = 'choice2',
                                                  '50%' = 'choice3',
                                                  '12%' = 'correct'),
                                     width = '100%'),
                         uiOutput("block1")), 
                  column(4, 
                         box(title  = "Here you see your knowledge state:",
                             status = 'primary',
                             solidHeader = TRUE,
                             width       = '100%',
                             plotOutput("KST")),
                         actionButton("KS.plot",
                                      "Plot my Results", icon = icon("refresh"))
                         ))
              ),
              br(), br(), br(), br() # add empty space
              ),
  tabItem(tabName = "result",
          h1('Look at your Test results'),
          fluidPage(
              fluidRow(
                column(6,
                textOutput('knowledgeState'), 
                br(), br(),
                textOutput('descriptiveYourState'),
                br(), br(), 
                textOutput('descriptiveMostCommonState'),
                br(), br(),
                wellPanel(
                    textInput('Name', placeholder = "Enter your name",""),
                    actionButton("submit","Save Your Results"),
                    uiOutput('table'),
                    uiOutput('table2'))
                ),
                column(6,
                       box(title  = "This is the most common knowledge state:",
                           status = 'primary',
                           solidHeader = TRUE,
                           width       = '100%',
                           plotOutput("KST2")),
                       h4("Compared to students that completed a statistics
                       course the occurence of this knowledge state has
                       a probability of 23 percent.")
              )
          )
          )))
))
###### SERVER #######
server <- function(input, output){
    
#     problem <- c("a1", "a2", "a3", "a4", 
#                  "b1", "b2", "b3", "b4", 
#                  "c1", "c2")
#     state   <<- rep(FALSE, 10) # will be updated
#     data(probability)
#     dat  <- probability
#     dat  <- as.pattern(cbind(dat$b202, dat$b201, dat$b204, 
#                              dat$b202, dat$b207, dat$b209, 
#                              dat$b205, dat$b210, dat$b212, dat$b211))
#     # your knowledge state
#     state.pattern <<- paste(as.character(as.numeric(state)), 
#                             collapse = "")
#     index <- which(names(table(dat)) %in% state.pattern)
#     freq  <- round(table(dat)[index]/504 * 100, 3)
#     
#     # questions
#     source("block1.R", local = TRUE) # first round of questions; block b
#     source("block0.R", local = TRUE) # second round of questions
#     source("block2.R", local = TRUE) # second round of questions; block a
#     source("block3.R", local = TRUE) # thrid round of questions; block c
#     # plot 
#     source("KSplot.R", local = TRUE) 
#     go.plot <- eventReactive(input$KS.plot,{ # observe action Button
#         hasse_Diag(state)
#     })
#     output$KST <- renderPlot({               # plot diagram
#         go.plot()
#     })
#     output$KST2 <- renderPlot({
#         stateMostCommon <- rep(TRUE, 10)
#         hasse_Diag(stateMostCommon)
#     })
#     ################
#     # save results #
#     ################
# #     Data <- reactive({
# #         
# #         if (input$submit > 0) {
# #             write.table(as.numeric(state), 
# #                         file=paste("KStest_",input$Name, ".txt", sep =""))}
# #     })
#     
#     output$table <- renderText({
#         if (input$submit > 0){
#             c("Your Data has been saved! Thank you for your participation.")
#                         write.table(as.numeric(state), 
#                                     file=paste("KStest_",input$Name, ".txt", sep =""))
#         }
#     })
#     output$table2 <- renderText({
#         if (input$submit > 0){
#             c("Your Data has been saved! Thank you for your participation.")
#         }
#     })    
#     # Look at results
#     output$knowledgeState <- renderText({
#         if(sum(state) == 0){
#             c("You can master 0 problems.")
#             } else {
#             # print knowledge state
#             c("You can master the following problem(s):", 
#               problem[which(state == TRUE)])
#             }
#         })
#     output$descriptiveYourState <- renderText({
#         if(length(index) == 0){
#             c("You are the first to display this knowledge state.")
#         } else {
#         # find your knowledge state
#         paste("Compared to students that completed a statistics
#               course the occurence of your knowledge state has
#               a probability of", freq, "percent", sep=" ")
#         }
#    })

#     output$descriptiveMostCommonState <- "Hi"
}
shinyApp(ui = ui, server = server)














