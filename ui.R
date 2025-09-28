library(shiny)
source("constants.R")

shinyUI(fluidPage(
  titlePanel("ChugakuEigo"),
  sidebarLayout(
    sidebarPanel(
      selectInput("select.user", "select your name", choices = c(APP$DEFAULTS$USER), selected = APP$DEFAULTS$USER),
      actionButton("action.start", label = "Start Learning"), 
      actionButton("action.save", label = "Save Score"),
      p(), 
      textOutput("welcome"),
      p(),
      helpText("----How to use----"),
      helpText("1. Select username and Start"),
      helpText("2. On the main tab, see Japanese sentence and translate it."),
      helpText("3. If you could do instantly, press 'OK'. Otherwise 'NG'"),
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
	  numericInput("zero.score.limit", label = h4("Maximum zero-score questions to include"), value = LEARNING$DEFAULTS$MAX_ZERO_SCORE_QUESTIONS, step = 1, min = 1),
          numericInput("prob.base", label = h4("bias to your weakness (1 means unbiased)"), value = LEARNING$DEFAULTS$PROBABILITY_BASE, step = LEARNING$PROBABILITY$STEP, min = LEARNING$PROBABILITY$MIN)
        ),
        tabPanel("questions",
          h3("Questions"),
          DT::dataTableOutput("dt.questions")
        ),
        tabPanel("manage",
          h3("Manage"),
          textInput("textinp.useradd", label = "Add User", value = ""),
          actionButton("action.useradd", label = "Add User"),
	  br(), br(),

	  selectInput("select.userdelete", label = "Select User to Delete", choices = NULL),
	  actionButton("action.userdelete", label = "Delete User", class = "btn-danger")
        ),
        tabPanel("history", 
          h3("History"), 
	  HTML("
	    v0.1 launch(2018-08-28)<br>
            v0.1.1 small layout change and bug fixes(2018-08-29)<br>
            v0.1.2 craete data folder, dynamic selection, modify variable names, automatic gap filling between score and questions and replace all tabs to spaces(2018-08-30) <br>
	    v0.1.3 bug fixes(2018-09-03)<br>
	    v0.1.4 step-by-step mode(2018-09-04)<br>
	    v0.1.5 lightweight(2018-09-06)
          ")
        )
      )
    )
  )
))
