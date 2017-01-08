library(shiny)

#==========================ui==========================
#======Select expression for the stated empirical interpretations=======
# 1)
ui <- fluidPage(
  h3(strong("Local Stochastic Independence")),
  h4("In the basic local independence model (BLIM), the conditional
     probability of the response pattern R given the knowledge state K is
     based on the assumption of local stochastic independence between
     the responses given this knowledge state.
     The following app illustrates the building blocks of the probability
     P(R|K)."),
 verticalLayout(
# Lucky guess
   h4(strong("Select the appropriate expression for each of the stated
              empirical interpretations.")),
   h5(
     column(4, 
       strong("Lucky guess"), br(),
         HTML("<select id='RW'>Lucky guess</option>
           <option value='so'>Select one</option>
           <option value='beta'>&#946;</option>		# beta
           <option value='Nbeta'>1 - &#946;</option>	# 1 - beta
	   <option value='eta'>&#951;</option>		# eta
	   <option value='Neta'>1 - &#951;</option>	# 1 - eta
           </select>"
	 ),
         uiOutput("RW"), 
# Careless error
         strong("Careless Error"),
	 br(),	
  	 HTML("<select id='LF'>Careless Error</option>
           <option value='so'>Select one</option>
           <option value='beta'>&#946;</option>
           <option value='Nbeta'>1 - &#946;</option>
	   <option value='eta'>&#951;</option>
	   <option value='Neta'>1 - &#951;</option>
           </select>"
	 ),
         uiOutput("LF")
     ),

# Correct! (know it)	     
     column(6,
       strong("Correct! (know it)"), br(),
         HTML("<select id='R'>Correct! (know it)</option>
           <option value='so'>Select one</option>
           <option value='beta'>&#946;</option>
           <option value='Nbeta'>1 - &#946;</option>
	   <option value='eta'>&#951;</option>
	   <option value='Neta'>1 - &#951;</option>
           </select>"
	 ),
	 uiOutput("R"),

# Wrong! (don't know it)	 
       strong("Wrong! (don't know it)"), br(),
	 HTML("<select id='F'>Wrong! (don't know it)</option>
           <option value='so'>Select one</option>
           <option value='beta'>&#946;</option>
           <option value='Nbeta'>1 - &#946;</option>
	   <option value='eta'>&#951;</option>
	   <option value='Neta'>1 - &#951;</option>
           </select>"
	 ),
	 uiOutput("F")
     )
   ),

#==========Venn diagram, where the items should be placed
# 2)
   h5(hr(),
      h4(strong("Sarah takes part in a test with five items: a, b, c, d, and e.
		She only masters items a and b. But she answers
		items b, c, and e correctly."), 
	strong("The response pattern R contains Sarah's correct
	       responses, the knowledge state K contains the items
	       she masters."),
	br(),
	"Select the subset in which each item is to be placed."),
     column(6, plotOutput("Venn"), br()),	# plotOutput
     column(3, " ", uiOutput("Mengen")),	# K and R as a set 
     column(3,
# Item a is element of set K\R	    
       strong("Item a is element of"), br(),
         HTML("<select id='A'>Item a</option>
           <option value='so'>Select one</option>
           <option value='k'>K \\ R</option>		
           <option value='r'>R \\ K</option>	
	   <option value='kr'>R &#8745; K</option>	
	   <option value='nkr'>&#172; R &#8745; &#172; K</option>	
           </select>"
	 ),	    
       uiOutput("Itema"),  # right or wrong?
# Item b is element of set K and R       
       strong("Item b is element of"), br(),
         HTML("<select id='B'>Item b is element of</option>
           <option value='so'>Select one</option>
           <option value='k'>K \\ R</option>
           <option value='r'>R \\ K</option>
	   <option value='kr'>R &#8745; K</option>
	   <option value='nkr'>&#172; R &#8745; &#172; K</option>
           </select>"
	 ),	    
       uiOutput("Itemb"), # right or wrong?
# Item c is element of R\K       
       strong("Item c is element of"), br(),
         HTML("<select id='C'>Item c</option>
           <option value='so'>Select one</option>
           <option value='k'>K \\ R</option>
           <option value='r'>R \\ K</option>
	   <option value='kr'>R &#8745; K</option>
	   <option value='nkr'>&#172; R &#8745; &#172; K</option>
           </select>"
	 ),	    
       uiOutput("Itemc"),  # right or wrong?
# Item d is not an element of R or K       
       strong("Item d is element of"), br(),
         HTML("<select id='D'>Item d</option>
           <option value='so'>Select one</option>
           <option value='k'>K \\ R</option>
           <option value='r'>R \\ K</option>
	   <option value='kr'>R &#8745; K</option>
	   <option value='nkr'>&#172; R &#8745; &#172; K</option>
           </select>"
	 ),	    
       uiOutput("Itemd"), # right or wrong?
# Item e is element of R\K        
       strong("Item e is element of"), br(),
         HTML("<select id='E'>Item e</option>
           <option value='so'>Select one</option>
           <option value='k'>K \\ R</option>
           <option value='r'>R \\ K</option>
	   <option value='kr'>R &#8745; K</option>
	   <option value='nkr'>&#172; R &#8745; &#172; K</option>
           </select>"
	 ),
       uiOutput("Iteme") # right or wrong?
      	      
     )
   ),
#========
# 3)	 
   h5(hr(),
       h4(strong("Now classify Sarah's answers to each item."), br(),
	  "If your choice is correct, the corresponding expression will appear
          in the formula below. If your choice is wrong, a question mark will
          appear instead."),
      br(),
# Classify Sarah's answers to each item
# item a      
     column(2, radioButtons("a", "Item a", 
       c("Correct! (know it)" = 'r',
         "Lucky guess!" = 'RW',
         "Careless error!" = 'LF', 
         "Wrong! (don't know it)" = 'f') 
       ),
     actionButton("aa", "Submit"), br(),  # action button
     uiOutput("Formel1")                  # wrong or right?
     ),
# item b
     column(2, radioButtons("b", "Item b", 
       c("Correct! (know it)" = 'r',
	 "Lucky guess!" = 'RW',
	 "Careless error!" = 'LF', 
         "Wrong! (don't know it)" = 'f')
       ),
     actionButton("ab", "Submit"), br(),  # action button
     uiOutput("Formel2")                  # wrong or right?
     ),
# item c
     column(2, radioButtons("c", "Item c", 
       c("Correct! (know it)" = 'r',
         "Lucky guess!" = 'RW',
	 "Careless error!" = 'LF', 
         "Wrong! (don't know it)" = 'f')
       ),
       actionButton("ac", "Submit"), br(), # action button
       uiOutput("Formel3")                 # wrong or right?
     ),
# item d		
     column(2, radioButtons("d", "Item d", 
       c("Correct! (know it)" = 'r',
         "Lucky guess!" = 'RW',
         "Careless error!" = 'LF', 
         "Wrong! (don't know it)" = 'f')
        ),
       	actionButton("ad", "Submit"), br(), # action button
	uiOutput("Formel4")                 # wrong or right?
      ),
# item e
      column(2,radioButtons("e", "Item e", 
        c("Correct! (know it)" = 'r',
          "Lucky guess!" = 'RW',
	  "Careless error!" = 'LF', 
          "Wrong! (don't know it)" = 'f')
        ),
       	actionButton("ae", "Submit"), br(), # action button
        uiOutput("Formel5")                 # wrong or right?
     )
   ),

#======================= P(R|K) formula
# 4)      
   h6(
      h4(strong("The probability of the response pattern R given the knowledge 
      state K is:"), br(), "R = {b,c,e} and K = {a,b}") ,		
# formula
     column(1, h4(HTML("<b>P(R|K) = </b>"))),
     column(1, uiOutput("Formela")),			# parameter of item a
     column(1, h4(HTML("<strong>&sdot;</strong>"))),	# dot
     column(1, uiOutput("Formelb")),			# parameter of item b
     column(1, h4(HTML("<strong>&sdot;</strong>"))),	# dot
     column(1, uiOutput("Formelc")),			# parameter of item c
     column(1, h4(HTML("<strong>&sdot;</strong>"))),	# dot
     column(1, uiOutput("Formeld")),			# parameter of item d
     column(1, h4(HTML("<strong>&sdot;</strong>"))),	# dot
     column(1, uiOutput("Formele")), br(), br()		# parameter of item e
   ),
   h4(br(), strong("Select parameter values and see how P(R|K) changes.")),	
# Select parametervalue to calculate P(R|K)
   h5(column(5,
       sliderInput("sa", HTML("Select a value for &#946;<sub>a</sub>"), # beta a
		 min = 0, max = 0.49, step = 0.01, value = 0.1),
       sliderInput("sc",  HTML("Select a value for &#951;<sub>c</sub>"), # eta c
		  min = 0, max = 0.49, step = 0.01, value = 0.25),
       sliderInput("se",  HTML("Select a value for &#951;<sub>e</sub>"), # eta e
		  min = 0, max = 0.49, step = 0.01, value = 0.45)     
     ),
     column(1,paste("")),
     column(6,
       sliderInput("sb", HTML("Select a value for  &#946;<sub>b</sub>"),# beta b
		  min = 0, max = 0.49, step = 0.01, value = 0.15), 
       sliderInput("sd",  HTML("Select a value for &#951;<sub>d</sub>"), # eta d
		  min = 0, max = 0.49, step = 0.01, value = 0.35),
       h4(strong("The probability of the response pattern R given the
	    knowledge state K is: ")),
       h4(textOutput("Ergebnis"), br(), br())			# result
     )    
   )
 )
)

#=========================server================================
server <- function(input, output){
# Output zu 1) 
# lucky guess	
  output$RW <- renderText({
    if(input$RW == "eta"){
      HTML("<h5 style='color:green' align='left'><b>Well done!</b></h5> ")}
    # right
    else if(input$RW == 'so'){HTML("<br>")}
    # nothing chosen
    else{HTML("<h5 style ='color:red' align='left'>
	        <b>Wrong! Try again!</b></h5>")}
    # wrong		
  })	
# careless error
  output$LF <- renderText({
    if(input$LF == "beta"){
      HTML("<h5 style='color:green' align='left'><b>Well done!</b></h5>")}
    else if(input$LF == 'so'){HTML("<br>")}
    else{HTML("<h5 style ='color:red' align='left'>
	         <b>Wrong! Try again!</b></h5>")}	
  })
# correct
  output$R<- renderText({
    if(input$R == "Nbeta"){
      HTML("<h5 style='color:green' align='left'><b>Well done!</b></h5>")}
    else if(input$R == 'so'){HTML("<br>")}
    else{HTML("<h5 style ='color:red' align='left'>
	       <b>Wrong! Try again!</b></h5>")}	
  })
# wrong
  output$F <- renderText({
    if(input$F == "Neta"){
      HTML("<h5 style='color:green' align='left'><b>Well done!</b></h5>")}
    else if(input$F == 'so'){HTML("<br>")}
    else{HTML("<h5 style ='color:red' align='left'><b>Wrong! Try again!</b></h5>")}	
  })

#==================Venn diagram
  output$Venn <- renderPlot({
# drawing circles	  
    par(pty="s", mai = c(0,0,0,0))
    plot(10, 10, type="n", axes=FALSE, xlab="", ylab="", xlim=c(7.9, 13), 
	 ylim=c(8.5,11.75))
    symbols(c(9.5, 11.5), c(10, 10), circles=c(1.7, 1.7), add=TRUE,
            inches=FALSE, fg=c("orange", "blue"), lwd = 3)
# description for the circles 
    text(x = 8.2, y = 11, labels = "R", col = "orange", cex = 1.5)
    text(x = 12.8, y = 11, labels = "K", col = "blue", cex = 1.5)
# item a
    if(input$A == "r"){
      text(x = 9.1, y = 10.8, labels = "a", col = "red", cex = 1.5)
    }else if(input$A == "k"){
      text(x = 11.9, y = 10.8, labels = "a", col = "darkgreen", cex = 1.5)
    }else if(input$A == "kr"){
      text(x = 10.5, y = 10.6, labels = "a", col = "red", cex = 1.5)
    }else if(input$A == "nkr"){
      text(x = 10.9, y = 11.2, labels = "a", col = "red", cex = 1.5)
    }else{}	
# item b
    if(input$B == "r"){
      text(x = 8.9, y = 10.4, labels = "b", col = "red", cex = 1.5)
    }else if(input$B == "k"){
      text(x = 12.2, y = 10.4, labels = "b", col = "red", cex = 1.5)
    }else if(input$B == "kr"){
      text(x = 10.3, y = 10.4, labels = "b", col = "darkgreen", cex = 1.5)
    }else if(input$B == "nkr"){
      text(x = 10.7, y = 11.3, labels = "b", col = "red", cex = 1.5)
    }else{}
# item c
    if(input$C == "r"){
      text(x = 9, y = 10, labels = "c", col = "darkgreen", cex = 1.5)
    }else if(input$C == "k"){
      text(x = 12.1, y = 10, labels = "c", col = "red", cex = 1.5)
    }else if(input$C == "kr"){
      text(x = 10.4, y = 10, labels = "c", col = "red", cex = 1.5)
    }else if(input$C == "nkr"){
      text(x = 10.5, y = 11.1, labels = "c", col = "red", cex = 1.5)
    }else{}
# item d
    if(input$D == "r"){
      text(x = 9, y = 9.6, labels = "d", col = "red", cex = 1.5)
    }else if(input$D == "k"){
      text(x = 12, y = 9.6, labels = "d", col = "red", cex = 1.5)
    }else if(input$D == "kr"){
      text(x = 10.5, y = 9.6, labels = "d", col = "red", cex = 1.5)
    }else if(input$D == "nkr"){
      text(x = 10.3, y = 11.2, labels = "d", col = "darkgreen", cex = 1.5)
    }else{}
# item e
    if(input$E == "r"){
      text(x = 9.3, y = 9.2, labels = "e", col = "darkgreen", cex = 1.5)
    }else if(input$E == "k"){
      text(x = 11.7, y = 9.2, labels = "e", col = "red", cex = 1.5)
    }else if(input$E == "kr"){
      text(x = 10.6, y = 9.3, labels = "e", col = "red", cex = 1.5)
    }else if(input$E == "nkr"){
      text(x = 10.1, y = 11.2, labels = "e", col = "red", cex = 1.5)
    }else{}
  })

#==================== sets (K,R)
 output$Mengen <- renderUI({  
   MeR1 <- MeR2 <- MeR3 <- MeR4 <- MeR5 <- ""
   MeK1 <- MeK2 <- MeK3 <- MeK4 <- MeK5 <- ""
# Item a   
   if(input$A == "k"){
      MeK1 <- HTML("<span style='color:green'>a</span style>")
   }else if(input$A == "kr"){
      MeK1 <- HTML("<span style='color:green'>a</span style>")
      MeR1 <- HTML("<span style='color:red'>a</span style>")
   }else if(input$A == "r"){
      MeR1 <- HTML("<span style='color:red'>a</span style>")
   }else{}
# Item b   
   if(input$B == "k"){
     MeK2 <- HTML("<span style='color:green'>b</span style> ") 
   }else if(input$B == "kr"){
     MeK2 <- HTML("<span style='color:green'>b</span style>") 
     MeR2 <- HTML("<span style='color:green'>b</span style>")
   }else if(input$B == "r"){
     MeR2 <- HTML("<span style='color:green'>b</span style>")
   }else{}
# Item c   
   if(input$C == "k"){
     MeK3 <- HTML("<span style='color:red'>c</span style>") 
   }else if(input$C == "kr"){
     MeK3 <- HTML("<span style='color:red'>c</span style>") 
     MeR3 <- HTML("<span style='color:green'>c</span style>")
   }else if(input$C == "r"){
     MeR3 <- HTML("<span style='color:green'>c</span style>")
   }else{}
# Item d   
   if(input$D == "k"){
     MeK4 <- HTML("<span style='color:red'>d</span style>") 
   }else if(input$D == "kr"){
     MeK4 <- HTML("<span style='color:red'>d</span style>") 
     MeR4 <- HTML("<span style='color:red'>d</span style>")
   }else if(input$D == "r"){
     MeR4 <- HTML("<span style='color:red'>d</span style>")
   }else{}
# Item e   
   if(input$E == "k"){
     MeK5 <- HTML("<span style='color:red'>e</span style>") 
   }else if(input$E == "kr"){
     MeK5 <- HTML("<span style='color:red'>e</span style>") 
     MeR5 <- HTML("<span style='color:green'>e</span style>")
   }else if(input$E == "r"){
     MeR5 <- HTML("<span style='color:green'>e</span style>")
   }else{}

#========================= comma in K and R

   if(MeK1 != "" & MeK2 != ""|MeK1 != "" & MeK3 != ""|MeK1 != "" & MeK4 !=
      ""|MeK1 != "" & MeK5 != ""){
     k1 <- ","
   }else{k1 <- ""}
   if(MeK2 != "" & MeK3 != ""|MeK2 != "" & MeK4 != ""|
      MeK2 != "" & MeK5 != ""){
     k2 <- ","
   }else{k2 <- ""}
   if(MeK3 != "" & MeK4 != ""|MeK3 != "" & MeK5 != ""){
     k3 <- ","
   }else{k3 <- ""}
   if(MeK4 != "" & MeK5 != ""){
     k4 <- ","
   }else{k4 <- ""}

   if(MeR1 != "" & MeR2 != ""|MeR1 != "" & MeR3 != ""|MeR1 != "" & MeR4 !=
      ""|MeR1 != "" & MeR5 != ""){
     kR1 <- ","
   }else{kR1 <- ""}
   if(MeR2 != "" & MeR3 != ""|MeR2 != "" & MeR4 != ""|
      MeR2 != "" & MeR5 != ""){
     kR2 <- ","
   }else{kR2 <- ""}
   if(MeR3 != "" & MeR4 != ""|MeR3 != "" & MeR5 != ""){
     kR3 <- ","
   }else{kR3 <- ""}
   if(MeR4 != "" & MeR5 != ""){
     kR4 <- ","
   }else{kR4 <- ""}

# what will be shown
   h5(HTML("<b>K = {", MeK1, k1, MeK2, k2, MeK3, k3, MeK4, k4, MeK5), 
      HTML("<span style='color:black'>}</span style>"), br(), br(),
      HTML("R = {", MeR1, kR1, MeR2, kR2, MeR3, kR3, MeR4, kR4, MeR5),
      HTML("<span style='color:black'>}</span style></b>"))
  })
     
# answer to selectInput 
# item a  
  output$Itema <- renderText({
    if(input$A == "k"){
      HTML("<h5 style='color:green' align='left'><b>Good!</b></h5>")
    }else if(input$A == "so"){
      HTML("<br>")
    }else{HTML("<h5 style ='color:red' align='left'><b>False!</b></h5>")}
  })
# item b
   output$Itemb <- renderText({
    if(input$B == "kr"){
      HTML("<h5 style='color:green' align='left'><b>Good!</b></h5>")
    }else if(input$B == "so"){
      HTML("<br>")
    }else{HTML("<h5 style ='color:red' align='left'><b>False!</b></h5>")}
  })
# item c
   output$Itemc <- renderText({
    if(input$C == "r"){
      HTML("<h5 style='color:green' align='left'><b>Good!</b></h5>")
    }else if(input$C == "so"){
      HTML("<br>")
    }else{HTML("<h5 style ='color:red' align='left'><b>False!</b></h5>")}
  })
# item d
   output$Itemd <- renderText({
    if(input$D == "nkr"){
      HTML("<h5 style='color:green' align='left'><b>Good!</b></h5>")
    }else if(input$D == "so"){
      HTML("<br>")
    }else{HTML("<h5 style ='color:red' align='left'><b>False!</b></h5>")}
  })
# item e
 output$Iteme <- renderText({
    if(input$E == "r"){
      HTML("<h5 style='color:green' align='left'><b>Good!</b></h5>")
    }else if(input$E == "so"){
      HTML("<br>")
    }else{HTML("<h5 style ='color:red' align='left'><b>False!</b></h5>")}
  })
# calculate result
 output$Ergebnis <- renderText({
    X <- input$sa*(1-input$sb)*input$sc*(1-input$sd)*input$se
    paste("P(R|K) = ", round(X, digits = 4) )
    })

#=============================Action button answer
# Item a
  observeEvent(input$aa, {
    output$Formel1 <- renderUI({
      isolate(
        Fa <- if(input$a == "LF"){HTML(
		"<h5 style='color:green' align='left'><b>Great!</b></h5>")}
	else{HTML(
		"<h5 style ='color:red' align='left'><b>Wrong!</b></h5>")}
      )
    })
  })

# Item b
  observeEvent(input$ab, {
    output$Formel2 <- renderText({
      isolate(
	Fb <- if(input$b == "r"){HTML(
		"<h5 style='color:green' align='left'><b>Great!</b></h5>")}
	else{HTML(
		"<h5 style ='color:red' align='left'><b>Wrong!</b></h5>")}
      )
    })
  })
# Item c
  observeEvent(input$ac, {
    output$Formel3 <- renderText({
      isolate(
	Fc <- 	if(input$c == "RW"){HTML(
		"<h5 style='color:green' align='left'><b>Great!</b></h5>")}
	else{HTML(
		"<h5 style ='color:red' align='left'><b>Wrong!</b></h5>")}
      )
    })
  })
# Item d
  observeEvent(input$ad, {
    output$Formel4 <- renderText({
      isolate(
	Fd <- if(input$d == "f"){HTML(
		"<h5 style='color:green' align='left'><b>Great!</b></h5>")}
	else{HTML(
		"<h5 style ='color:red' align='left'><b>Wrong!</b></h5>")}
      )
    })
  })
# Item e
  observeEvent(input$ae, {
    output$Formel5 <- renderText({
      isolate(
	Fe <- if(input$e == "RW"){HTML(
		"<h5 style='color:green' align='left'><b>Great!</b></h5>")}
	else{HTML(
		"<h5 style ='color:red' align='left'><b>Wrong!</b></h5>")}
      )
    })
  })
#==================================== P(R|K)
# Item a
  observeEvent(input$aa, {				       
    output$Formela <- renderText({
      isolate(
	Fa <- if(input$a == "LF"){
	  HTML("<h4 style='color:green' align='left'>
	        <b>&#946;<sub>a</sub></b></h4>")}
	else{HTML("<h4 style='color:red' align='left'><b> ? </b></h4>")}
      )
    })
  })
# Item b
  observeEvent(input$ab, {				       
    output$Formelb <- renderText({
      isolate(
	Fb <- if(input$b == "r"){
	  HTML("<h4 style='color:green' align='left'>
	       <b>(1 - &#946;<sub>b</sub>)</b></h4>")}
	else{HTML("<h4 style='color:red' align='left'><b> ? </b></h4>")}
      )
    })
  })
# Item c
  observeEvent(input$ac, {				       
    output$Formelc <- renderText({
      isolate(
	Fc <- if(input$c == "RW"){
	  HTML("<h4 style='color:green' align='left'>
	        <b>&#951;<sub>c</sub></b></h4>")}
	else{HTML("<h4 style='color:red' align='left'><b> ? </b></h4>")}
      )
    })
  })
# Item d
  observeEvent(input$ad, {				       
    output$Formeld <- renderText({
      isolate(
	Fd <- if(input$d == "f"){
	  HTML("<h4 style='color:green' align='left'>
	        <b>(1 - &#951;<sub>d</sub>)</b></h4>")}
	else{HTML("<h4 style='color:red' align='left'><b> ? </b></h4>")}
      )
    })
  })
# Item e
  observeEvent(input$ae, {				       
    output$Formele <- renderText({
      isolate(
	Fe <- if(input$e == "RW"){
	  HTML("<h4 style='color:green' align='left'>
	        <b>&#951;<sub>e</sub></b></h4>")}
	else{HTML("<h4 style='color:red' align='left'><b> ? </b></h4>")}
      )
    })
  })
     

}

shinyApp(ui = ui, server = server)

