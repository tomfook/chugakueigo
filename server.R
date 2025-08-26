library(shiny) 
library(dplyr)
source("modules/data_manager.R")
source("modules/user_manager.R")
source("modules/learning_engine.R")
source("modules/ui_helpers.R")
source("constants.R")

shinyServer(function(input, output, session){ 
  init_data <- initialize_data()
  main <- init_data$main
  score_global <- init_data$score_global

  qa <- reactiveValues() 
  qa$start <- FALSE
  qa$score <- rep(0L, nrow(main))
  qa$namelist <- names(score_global)
  qa$score.all <- score_global

#render UI
  output$html.slider.qrange <- renderUI({ 
    sliderInput(
      "slider.qrange",
      label = h4("Range of Question"),
      min = 1L,
      max = nrow(main),
      value = c(1,nrow(main)))
  }) 
  output$html.action.start <- renderUI({ 
    actionButton("action.start", label = "Start Learning",
      style = if_else(qa$start, STYLES$INACTIVE, STYLES$ACTIVE)
    )
  })
  output$html.action.save <- renderUI({ 
    actionButton("action.save", label = "Save Score",
      style = if_else(identical(qa$score.all[[input$select.user]], qa$score), STYLES$INACTIVE, STYLES$ACTIVE)
      )
  })

# user selection update
  observe({
    updateSelectInput(session, "select.user", choices = qa$namelist)
  })

# useradd
  observeEvent(input$action.useradd,{
    result <- add_new_user(input$textinp.useradd, qa, main)
    showNotification(result$message, type = if(result$success) "message" else "error")
    if (result$success) {
      updateTextInput(session, "textinp.useradd", value = "")
    }
  })

#remove user
  observe({
    delete_choices <- qa$namelist[qa$namelist != "guest"]
    updateSelectInput(
      session,
      "select.userdelete", 
      choices = if(length(delete_choices) > 0) delete_choices else "No users to delete"
    )
  })

  observeEvent(input$action.userdelete, {
    selected_user <- input$select.userdelete

    if (is.null(selected_user) || selected_user == "No users to delete") {
      showNotification("No user selected for deletion.", type = "warning")
      return()
    }

    if (selected_user == input$select.user) {
      showNotification("Cannot delete currently selected use. Please switch to another user first.", type = "warning")
      return()
    }

    result <- remove_user(selected_user, qa)
    showNotification(result$message, type = if(result$success) "message" else "error")
  })


#user account
  observeEvent(input$select.user,{
    qa$trial <- 0L
    qa$ok <- 0L
    qa$start <- FALSE
    qa$user <- input$select.user
    qa$score <- qa$score.all[[input$select.user]]
    qa$index <- NULL
    qa$question <- ""
    qa$answer <- ""
  }) 

#learning
  observe({
    if(is.null(input$slider.qrange[1])){
      qa$range.min <- 1L
    } else {
      qa$range.min <- input$slider.qrange[1]
    }
    if(is.null(input$slider.qrange[2])){
      qa$range.max <- nrow(main)
    } else {
      qa$range.max <- input$slider.qrange[2]
    }

    qa$prob <- calculate_question_probability(
      score_range = qa$score[seq(qa$range.min, qa$range.max)],
      prob_base = input$prob.base,
      ok_count = qa$ok,
      zero_limit = input$zeronum
      )

  })

  observeEvent(input$action.start,{ 
    qa$trial <- 0L
    qa$ok <- 0L
    qa$start <- TRUE
    qa <- newQuestion(main, qa)
  })
  observeEvent(input$action.answer,{
    qa$answer <- qa$answer.remember
  }) 
  observeEvent(input$action.ok,{
    if(qa$start){
      if(qa$answer != ""){
        qa$score[qa$index] <- qa$score[qa$index] + 1L
	qa$ok <- qa$ok + 1L
        qa <- newQuestion(main, qa)
      }else if(qa$answer == ""){
        showNotification("Confirm Answer!")
      }
    }
  }) 
  observeEvent(input$action.ng,{
    if(qa$start){
      if(qa$answer != ""){
        qa <- newQuestion(main, qa)
      }else if(qa$answer == ""){
        showNotification("Confirm Answer!")
      }
    }
  }) 
  output$qanda <- create_qanda_renderer(qa)

#save score
  observeEvent(input$action.save,{ 
    if(qa$user == input$select.user){
      qa$score.all <- read_score(qa = main, path = PATHS$SCORES)
      qa$score.all[[input$select.user]] <- qa$score
      write.table(qa$score.all, PATHS$SCORES, row.names=FALSE, sep = ",")
      qa$score.all <- qa$score.all %>%
        mutate(guest = 0L) %>%
        select(guest, everything())
    }else{ 
      showNotification("You switched user account.")
    }
  }) 

#current status
  output$welcome <- create_welcome_renderer(input, qa)

#score
  output$score.total <- create_score_total_renderer(qa)
  output$score.weak <- create_score_weak_renderer(main, qa)

#questions
  output$dt.questions <- create_questions_datatable_renderer(main, qa)

})

