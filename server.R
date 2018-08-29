library(shiny) 
library(dplyr)

#param

shinyServer(function(input, output){ 
#external data
	main <- read.csv("qlist.csv", comment = "#", stringsAsFactor = FALSE)
	score.global <- read.csv("score.csv") %>%
										mutate(guest = 0L) %>%
										select(guest, everything())


		qa <- reactiveValues() 
		qa$start <- FALSE
		qa$score.all <- rep(0L, nrow(main))
		qa$namelist <- names(score.global)


#render UI
		output$html.select.user <- renderUI({
						selectInput("select.user", "select your name", choices = qa$namelist, selected = "guest")
		})
		output$html.slider.qrange <- renderUI({ 
						sliderInput("slider.qrange", label = h4("Range of Question"), min = 1L, max = nrow(main), value = c(0,nrow(main)))
		}) 

# useradd
		observeEvent(input$action.useradd,{
			if(!(input$textinp.useradd %in% qa$namelist)){
				qa$namelist <- c(qa$namelist, input$textinp.useradd)
				score.global[[input$textinp.useradd]] <<- rep(0L, nrow(main))
				showNotification(paste("User", input$textinp.useradd, "was added."))
			}else{
				showNotification("Your name is resistered.")
			}
		})

#user account
		observeEvent(input$select.user,{
			qa$trial <- 0L
			qa$start <- FALSE
			qa$user <- input$select.user
			qa$score.all <- score.global[[input$select.user]]
			qa$index <- NULL
			qa$score.this <- NULL
		})


#learning logic
		newQuestion <- function(){
			if(!is.null(input$slider.qrange[1])){
							range.min <- input$slider.qrange[1]
							range.max <- input$slider.qrange[2]
			}else{
							range.min <- 1L
							range.max <- nrow(main)
			}
			a <- sample(
									range.max - range.min + 1L,
									1,
									prob = input$prob.base^(-qa$score.all[seq(range.min, range.max)])
									) + (range.min- 1L)
			qa$index <- a
			qa$score.this <- qa$score.all[a]
			qa$question <- main$question[a]
			qa$answer.remember <- main$answer[a]
			qa$answer <- "" 
			qa$trial <- qa$trial + 1L
		}

		observeEvent(input$action.start,{ 
			qa$trial <- 0L
			qa$start <- TRUE
			newQuestion()
		})
		observeEvent(input$action.answer,{
			qa$answer <- qa$answer.remember
		}) 
		observeEvent(input$action.ok,{
			if(qa$start){
				if(qa$answer != ""){
					qa$score.all[qa$index] <- qa$score.this + 1L
					newQuestion()
				}else if(qa$answer == ""){
					showNotification("Confirm Answer!")
				}
			}else{
				showNotification("You have to start learning")
			}
		}) 
		observeEvent(input$action.ng,{
			if(qa$start){
				if(qa$answer != ""){
					newQuestion()
				}else if(qa$answer == ""){
					showNotification("Confirm Answer!")
				}
			}else{
				showNotification("You have to start learning.")
			}
		}) 
		output$about <- renderText({
				paste0("Question Number:", qa$index, " / Your Score:", qa$score.this)
		}) 
		output$qanda <- renderTable({
						tibble::tibble(` `= c("Q.", "A."), sentence = paste0((c(qa$question, qa$answer)), ""))
		})


#save
		observeEvent(input$action.save,{ 
			if(qa$user == input$select.user){
				score.global <<- read.csv("score.csv")
				score.global[[input$select.user]] <<- qa$score.all
				write.table(score.global, "score.csv", row.names=FALSE, sep = ",")
				score.global <<- score.global %>%
										mutate(guest = 0L) %>%
										select(guest, everything())
			}else{ 
				showNotification("You switched user account.")
			}
		})


#current status
		output$welcome <- renderText({
						if(qa$start){
							trial.prefix <- dplyr::case_when(
																	qa$trial %in% 11:13 ~ "th",
																	qa$trial %% 10 == 1 ~ "st",
																	qa$trial %% 10 == 2 ~ "nd",
																	qa$trial %% 10 == 3 ~ "rd",
																	TRUE ~ "th"
																	) 
							paste0(input$select.user, "'s ", qa$trial, trial.prefix, " Trial")
						}else{
							paste("Not started. Push the start button")
						}
		})

#score
		output$score.total <- renderText({
				paste("Total score:", sum(qa$score.all))
		})
		output$score.weak <- renderTable({ 
				main %>%
					mutate(score = qa$score.all) %>%
					arrange(score) %>%
					head(5)
		})



#data
		output$dt.questions <- DT::renderDataTable({
			dplyr::mutate(main, score = score.global[[input$select.user]])
		}) 

		
		
}
)

#To-do
# fill a gap between main and score
