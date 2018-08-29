library(shiny)

shinyUI(fluidPage(
	titlePanel("ChugakuEigo"),
	sidebarLayout(
		sidebarPanel(
			htmlOutput("html.select.user"),
			actionButton("action.start", "Start Learning"), 
			actionButton("action.save", label = "save your score"),
			p(), 
			textOutput("welcome"),
			p(),
			actionButton("action.answer", label = "Show Answer"),
			br(),
			actionButton("action.ok", label = "OK"),
			actionButton("action.ng", label = "NG"),
			p(),
			helpText("----How to use----"),
			helpText("1. Select username and start"),
			helpText("2. Confirm answer"),
			helpText("3. Push 'OK' or 'NG' to score")
			),
		mainPanel(
			tabsetPanel(type = "tabs",
				tabPanel("main",
					h3("Main View"),
					textOutput("about"),
					tableOutput("qanda")
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
					)
				)
			)
		)
	)
)
