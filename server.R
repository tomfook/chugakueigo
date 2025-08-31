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

  qa_init <- state_initialize(main, score_global, app_error)
  qa <- reactiveValues() 
  for(name in names(qa_init)) {
    qa[[name]] <- qa_init[[name]]
  }

  config_state_init <- state_initialize_config(main)
  config_state <- reactiveValues()
  for(name in names(config_state_init)) {
    config_state[[name]] <- config_state_init[[name]]
  }

  learning_session_state_init <- state_initialize_learning_session()
  learning_session_state <- reactiveValues()
  for(name in names(learning_session_state_init)) {
    learning_session_state[[name]] <- learning_session_state_init[[name]]
  }

#render UI
  output$html.slider.qrange <- ui_render_slider_qrange(nrow(main))
  output$html.action.start <- ui_render_action_start(learning_session_state$start, app_error)
  output$html.action.save <- ui_render_action_save(identical(qa$score.all[[input$select.user]], qa$score), qa$app_error)

# user selection update
  ui_observe_user_selection(session, qa)

# useradd
  observeEvent(input$action.useradd,{
    if (app_error) {
      ui_show_data_error("add user")
      return()
    }
    result <- user_add_new(input$textinp.useradd, qa, main)
    ui_show_result(result)
    if (result$success) {
      updateTextInput(session, "textinp.useradd", value = "")
    }
  })

#remove user
  ui_observe_delete_choices(session, qa)

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
    result <- user_remove(selected_user, qa)
    ui_show_result(result)
    removeModal()
  })


#user account
  observeEvent(input$select.user,{
    qa <- user_switch_reset_state(qa, input$select.user)
  }) 

#learning
  observe({
    config_state <- learning_update_range_and_probability(
      config_state = config_state,
      qa = qa,
      slider_range = input$slider.qrange,
      prob_base = input$prob.base,
      zero_limit = input$zeronum,
      main_data = main,
      learning_session_state = learning_session_state
    )
  })

  observeEvent(input$action.start,{ 
    if (app_error) {
      ui_show_data_error("start learning")
      return()
    }
    learning_session_state <- learning_start_session(main, config_state, learning_session_state)
  })
  observeEvent(input$action.answer,{
    learning_session_state$answer <- learning_session_state$answer.remember
  }) 
  observeEvent(input$action.ok,{
    result <- learning_handle_ok_feedback(qa, main, config_state, learning_session_state)
    qa <- result$updated_qa
    learning_session_state <- result$updated_learning_session_state
    if (!result$success) {
      ui_show_result(result)
    }
  }) 
  observeEvent(input$action.ng,{
    result <- learning_handle_ng_feedback(main, config_state, learning_session_state)
    learning_session_state <- result$updated_learning_session_state
    if (!result$success) {
      ui_show_result(result)
    }
  }) 
  output$qanda <- ui_render_qanda(learning_session_state)

#save score
  observeEvent(input$action.save,{ 
    if (app_error) {
      ui_show_data_error("save score")
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

    ui_show_result(result)
  }) 

#current status
  output$welcome <- ui_render_welcome(input, qa, learning_session_state)

#score
  output$score.total <- ui_render_score_total(qa)
  output$score.weak <- ui_render_score_weak(main, qa)

#questions
  output$dt.questions <- ui_render_questions_table(main, qa)

})

