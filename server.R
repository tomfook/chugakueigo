library(shiny) 
library(dplyr)
source("modules/data_manager.R")
source("modules/user_manager.R")
source("modules/learning_engine.R")
source("modules/ui_helpers.R")
source("constants.R")

shinyServer(function(input, output, session){ 
  init_result <- data_initialize()
  app_error <- !init_result$success

  if (app_error) {
    main <- data.frame(question = "", answer = "")
    score_global <- data.frame(guest = 0L)
    showNotification(
      paste("Important: Data initialization failed: ", init_result$message, "- restart app"),
      type = "error", duration = NULL
      )
  } else{
    main <- init_result$data$main
    score_global <- init_result$data$score_global
  }

  qa <- reactiveValues() 
  qa$app_error <- app_error
  qa$start <- FALSE
  qa$score <- rep(0L, nrow(main))
  qa$namelist <- names(score_global)
  qa$score.all <- score_global

#render UI
  output$html.slider.qrange <- ui_render_slider_qrange(nrow(main))
  output$html.action.start <- ui_render_action_start(qa$start, qa$app_error)
  output$html.action.save <- ui_render_action_save(identical(qa$score.all[[input$select.user]], qa$score), qa$app_error)

# user selection update
  ui_observe_user_selection(session, qa)

# useradd
  observeEvent(input$action.useradd,{
    if (app_error) {
      showNotification("Data error: Cannot add user", type = "error")
      return()
    }
    result <- user_add_new(input$textinp.useradd, qa, main)
    showNotification(result$message, type = if(result$success) "message" else "error")
    if (result$success) {
      updateTextInput(session, "textinp.useradd", value = "")
    }
  })

#remove user
  ui_observe_delete_choices(session, qa)

  observeEvent(input$action.userdelete, {
    if (app_error) {
      showNotification("Data error: Cannot delete user", type = "error")
      return()
    }
    selected_user <- input$select.userdelete
    validation <- user_validate_deletion(selected_user, input$select.user)
    if (!validation$valid) {
      showNotification(validation$message, type = validation$type)
      return()
    }
    result <- user_remove(selected_user, qa)
    showNotification(result$message, type = if(result$success) "message" else "error")
  })


#user account
  observeEvent(input$select.user,{
    qa <- user_switch_reset_state(qa, input$select.user)
  }) 

#learning
  observe({
    qa <- learning_update_range_and_probability(
      qa = qa,
      slider_range = input$slider.qrange,
      prob_base = input$prob.base,
      zero_limit = input$zeronum,
      main_data = main
    )
  })

  observeEvent(input$action.start,{ 
    if (app_error) {
      showNotification("Data error: Cannot start learning", type = "error")
      return()
    }
    qa <- learning_start_session(qa, main)
  })
  observeEvent(input$action.answer,{
    qa$answer <- qa$answer.remember
  }) 
  observeEvent(input$action.ok,{
    result <- learning_handle_ok_feedback(qa, main)
    qa <- result$updated_qa
    if (!result$success) {
      showNotification(result$message)
    }
  }) 
  observeEvent(input$action.ng,{
    result <- learning_handle_ng_feedback(qa, main)
    qa <- result$updated_qa
    if (!result$success) {
      showNotification(result$message)
    }
  }) 
  output$qanda <- ui_render_qanda(qa)

#save score
  observeEvent(input$action.save,{ 
    if (app_error) {
      showNotification("Data error: Cannot save score", type = "error")
      return()
    }
    result <- data_save_user_score(
      username = input$select.user,
      current_user = qa$user,
      user_scores = qa$score,
      qa_data = main
      )

    if (result$success) {
      qa$score.all <- result$updated_scores
    }

    showNotification(result$message, type = if(result$success) "message" else "error")
  }) 

#current status
  output$welcome <- ui_render_welcome(input, qa)

#score
  output$score.total <- ui_render_score_total(qa)
  output$score.weak <- ui_render_score_weak(main, qa)

#questions
  output$dt.questions <- ui_render_questions_table(main, qa)

})

