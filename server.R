library(shiny) 
library(dplyr)
source("modules/data_manager.R")
source("modules/user_manager.R")
source("modules/learning_engine.R")
source("modules/ui_helpers.R")
source("constants.R")

shinyServer(function(input, output, session){ 
  init_data <- data_initialize()
  main <- init_data$main
  score_global <- init_data$score_global

  qa <- reactiveValues() 
  qa$start <- FALSE
  qa$score <- rep(0L, nrow(main))
  qa$namelist <- names(score_global)
  qa$score.all <- score_global

#render UI
  output$html.slider.qrange <- ui_render_slider_qrange(nrow(main))
  output$html.action.start <- ui_render_action_start(qa$start)
  output$html.action.save <- ui_render_action_save(identical(qa$score.all[[input$select.user]], qa$score))

# user selection update
  ui_observe_user_selection(session, qa)

# useradd
  observeEvent(input$action.useradd,{
    result <- user_add_new(input$textinp.useradd, qa, main)
    showNotification(result$message, type = if(result$success) "message" else "error")
    if (result$success) {
      updateTextInput(session, "textinp.useradd", value = "")
    }
  })

#remove user
  ui_observe_delete_choices(session, qa)

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

    result <- user_remove(selected_user, qa)
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

    qa$prob <- learning_calculate_probability(
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
    qa <- learning_new_question(main, qa)
  })
  observeEvent(input$action.answer,{
    qa$answer <- qa$answer.remember
  }) 
  observeEvent(input$action.ok,{
    if(qa$start){
      if(qa$answer != ""){
        qa$score[qa$index] <- qa$score[qa$index] + 1L
	qa$ok <- qa$ok + 1L
        qa <- learning_new_question(main, qa)
      }else if(qa$answer == ""){
        showNotification("Confirm Answer!")
      }
    }
  }) 
  observeEvent(input$action.ng,{
    if(qa$start){
      if(qa$answer != ""){
        qa <- learning_new_question(main, qa)
      }else if(qa$answer == ""){
        showNotification("Confirm Answer!")
      }
    }
  }) 
  output$qanda <- ui_render_qanda(qa)

#save score
  observeEvent(input$action.save,{ 
    if(qa$user == input$select.user){
      qa$score.all <- data_read_score(qa = main, path = PATHS$SCORES)
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
  output$welcome <- ui_render_welcome(input, qa)

#score
  output$score.total <- ui_render_score_total(qa)
  output$score.weak <- ui_render_score_weak(main, qa)

#questions
  output$dt.questions <- ui_render_questions_table(main, qa)

})

