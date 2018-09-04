library(shiny)

shinyUI(fluidPage(
  titlePanel("ChugakuEigo"),
  sidebarLayout(
    sidebarPanel(
      selectInput("select.user", "select your name", choices = c("guest"), selected = "guest"),
      actionButton("action.start", label = "Start Learning"), 
      actionButton("action.save", label = "Save Score"),
      p(), 
      textOutput("welcome"),
      p(),
      helpText("----How to use----"),
      helpText("1. Select username and Start"),
      helpText("2. On the main tab, see Japanese sentence and translate it."),
      helpText("3. If you could do instantly, push 'OK'. Otherwise 'NG'"),
      helpText("4. Repeat 2 and 3."),
      helpText("5. When you finish learning, save your score by the button.")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("main",
          h3("Main View"),
          textOutput("about"),
          tableOutput("qanda"),
          actionButton("action.answer", label = "Show Answer"),
          br(),
          actionButton("action.ok", label = "OK"),
          actionButton("action.ng", label = "NG"),
          p()
        ),
        tabPanel("score",
          h3("Score"),
          textOutput("score.total"),
          h4("Your low scored questions"),
          tableOutput("score.weak")
        ),
        tabPanel("setting",
          h3("Setting"),
          htmlOutput("html.slider.qrange"),
	  numericInput("zeronum", label = h4("step-by-step mode"), value = 10, step = 1, min = 1),
          numericInput("prob.base", label = h4("bias to your weakness (1 means unbiased)"), value = 1.7, step = 0.1, min = 1)
        ),
        tabPanel("questions",
          h3("Questions"),
          DT::dataTableOutput("dt.questions")
        ),
        tabPanel("manage",
          h3("Manage"),
          textInput("textinp.useradd", label = "useradd", value = ""),
          actionButton("action.useradd", label = "Submit")
        ),
        tabPanel("history", 
          h3("History"), 
	  HTML("
	    v0.1 launch(2018-08-28)<br>
            v0.1.1 small layout change and bug fixes(2018-08-29)<br>
            v0.1.2 craete data folder, dynamic selection, modify variable names, automatic gap filling between score and questions and replace all tabs to spaces(2018-08-30) <br>
	    v0.1.3 bug fixes(2018-09-03)<br>
	    v0.1.4 step-by-step mode(2018-09-04)
          ")
        )
      )
    )
  )
))
