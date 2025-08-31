library(shiny) 
library(dplyr)
source("modules/data_manager.R")
source("modules/user_manager.R")
source("modules/learning_engine.R")
source("modules/ui_helpers.R")
source("modules/state_manager.R")
source("constants.R")

shinyServer(function(input, output, session){ 
  init_result <- data_initialize()
  app_error <- !init_result$success

  if (app_error) {
    qa_data <- data.frame(question = "", answer = "")
    score_global <- data.frame(guest = 0L)
    showNotification(
      paste("Important: Data initialization failed: ", init_result$message, "- restart app"),
      type = "error", duration = NULL
      )
  } else{
    qa_data <- init_result$data$qa_data
    score_global <- init_result$data$score_global
  }

  user_state_init <- state_initialize_user(qa_data, score_global, app_error)
  user_state <- reactiveValues() 
  for(name in names(user_state_init)) {
    user_state[[name]] <- user_state_init[[name]]
  }

  config_state_init <- state_initialize_config(qa_data)
  config_state <- reactiveValues()
  for(name in names(config_state_init)) {
    config_state[[name]] <- config_state_init[[name]]
  }

  learning_state_init <- state_initialize_learning()
  learning_state <- reactiveValues()
  for(name in names(learning_state_init)) {
    learning_state[[name]] <- learning_state_init[[name]]
  }

#render UI
  output$html.slider.qrange <- ui_render_slider_qrange(nrow(qa_data))
  output$html.action.start <- ui_render_action_start(learning_state$start, app_error)
  output$html.action.save <- ui_render_action_save(identical(user_state$all_user_scores[[input$select.user]], user_state$score), user_state$app_error)

# user selection update
  ui_observe_user_selection(session, user_state)

# useradd
  observeEvent(input$action.useradd,{
    if (app_error) {
      ui_show_data_error("add user")
      return()
    }
    result <- user_add_new(input$textinp.useradd, user_state, qa_data)
    ui_show_result(result)
    if (result$success) {
      updateTextInput(session, "textinp.useradd", value = "")
    }
  })

#remove user
  ui_observe_delete_choices(session, user_state)

  observeEvent(input$action.userdelete, {
    if (app_error) {
      ui_show_data_error("delete user")
      return()
    }
    selected_user <- input$select.userdelete
    validation <- user_validate_deletion(selected_user, input$select.user)
    if (!validation$valid) {
      showNotification(validation$message, type = validation$type)
      return()
    }

    showModal(modalDialog(
      title = "Confirmation",
      paste("Are you sure you want to delete user '", selected_user, "'?", sep = ""),
      footer = tagList(
	modalButton("Cancel"),
	actionButton("confirm_delete", "Delete", class = "btn-danger")
      )
    ))
  })
  observeEvent(input$confirm_delete, {
    selected_user <- input$select.userdelete
    result <- user_remove(selected_user, user_state)
    ui_show_result(result)
    removeModal()
  })


#user account
  observeEvent(input$select.user,{
    user_state <- user_switch_reset_state(user_state, input$select.user)
  }) 

#learning
  observe({
    config_state$prob_base <- input$prob.base
    config_state$zero_limit <- input$zeronum
    config_state <- learning_update_range(config_state, input$slider.qrange, qa_data)
    config_state <- learning_update_probability(config_state, user_state, learning_state)
  })

  observeEvent(input$action.start,{ 
    if (app_error) {
      ui_show_data_error("start learning")
      return()
    }
    learning_state <- learning_start_session(qa_data, config_state, learning_state)
  })
  observeEvent(input$action.answer,{
    learning_state$answer <- learning_state$correct_answer
  }) 
  observeEvent(input$action.ok,{
    result <- learning_handle_feedback(user_state, qa_data, config_state, learning_state, is_correct = TRUE)
    user_state <- result$updated_user_state
    learning_state <- result$updated_learning_state
    if (!result$success) {
      ui_show_result(result)
    }
  }) 
  observeEvent(input$action.ng,{
    result <- learning_handle_feedback(user_state, qa_data, config_state, learning_state, is_correct = FALSE)
    user_state <- result$updated_user_state
    learning_state <- result$updated_learning_state
    if (!result$success) {
      ui_show_result(result)
    }
  }) 
  output$qanda <- ui_render_qanda(learning_state)

#save score
  observeEvent(input$action.save,{ 
    if (app_error) {
      ui_show_data_error("save score")
      return()
    }
    result <- data_save_user_score(
      username = input$select.user,
      current_user = user_state$user,
      user_scores = user_state$score,
      qa_data = qa_data
      )

    if (result$success) {
      user_state$all_user_scores <- result$updated_scores
    }

    ui_show_result(result)
  }) 

#current status
  output$welcome <- ui_render_welcome(input, user_state, learning_state)

#score
  output$score.total <- ui_render_score_total(user_state)
  output$score.weak <- ui_render_score_weak(qa_data, user_state)

#questions
  output$dt.questions <- ui_render_questions_table(qa_data, user_state)

})

