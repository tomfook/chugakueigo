library(shiny)
source("constants.R")

shinyUI(fluidPage(
  titlePanel("ChugakuEigo"),
  sidebarLayout(
    sidebarPanel(
      selectInput("select.user", "select your name", choices = c(APP$DEFAULTS$USER), selected = APP$DEFAULTS$USER),
      uiOutput("html.action.start", inline = TRUE),
      uiOutput("html.action.save", inline = TRUE),
      p(), 
      textOutput("welcome"),
      p(),
      helpText("----How to use----"),
      helpText("1. Select username (or add new user in 'manage' tab)"),
      helpText("2. Press 'Start Learning' button"),
      helpText("3. See Japanese sentence -> try to translate mentally"),
      helpText("4. Press 'Show Answer' to check the correct English answer"),
      helpText("5. Press 'OK' if correct, 'NG' if incorrect"),
      helpText("6. Repeat - the app prioritizes your weak questions"),
      helpText("7. Press 'Save Score' when done to save your progress")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("main",
          h3("Main View"),
          textOutput("about"),
          tableOutput("qanda"),
          actionButton("action.answer",
	    label = "Show Answer",
	    title = "Click to reveal the correct English translation"
	    ),
          br(),
          actionButton("action.ok",
	    label = "OK",
	    title = "Mark as correct - this question will appear less often"
	  ),
          actionButton("action.ng",
	    label = "NG",
	    title = "Mark as incorrect - this question will appear more often"
	  ),
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
	  uiOutput("html.action.delete")
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
