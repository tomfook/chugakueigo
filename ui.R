library(shiny)

shinyUI(fluidPage(
	titlePanel("ChugakuEigo"),
	sidebarLayout(
		sidebarPanel(
			htmlOutput("html.select.user"),
			actionButton("action.start", "Start Learning"), 
			actionButton("action.save", label = "Save Score"),
			p(), 
			textOutput("welcome"),
			p(),
			helpText("----How to use----"),
			helpText("1. Select username and Start"),
			helpText("2. On main tab, see Japanese sentence and translate"),
			helpText("3. If you could do instantly, push 'OK'. Otherwise 'NG'")
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
					h3("score"),
					textOutput("score.total"),
					h4("Your low scored questions"),
					tableOutput("score.weak")
					),
				tabPanel("setting",
					h3("setting"),
					htmlOutput("html.slider.qrange"),
					numericInput("prob.base", label = h4("bias to your weakness"), value = 1.7, step = 0.1, min = 1)
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
					helpText("v0.1 launch(2018-08-28)"),
					helpText("v0.1.1 small layout change and bug fixes(2018-08-29)")
					)
				)
			)
		)
	)
)
