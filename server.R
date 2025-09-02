library(shiny) 
library(dplyr)
source("modules/data_manager.R")
source("modules/user_manager.R")
source("modules/learning_engine.R")
source("modules/ui_helpers.R")
source("modules/state_manager.R")
source("constants.R")

shinyServer(function(input, output, session){ 
  # =============== INITIALIZATION ==================
  # Data initialization, error handling, and state setup
  # =================================================

  # Initialize core data
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

  # Initialize reactive states
  user_state <- state_create_reactive(state_initialize_user(qa_data, score_global, app_error))
  config_state <- state_create_reactive(state_initialize_config(qa_data))
  learning_state <- state_create_reactive(state_initialize_learning(qa_data))

  save_needed <- reactive({
    sum(learning_state$current_score) > 0
  })

  # =============== UI RENDERING ====================
  # Static UI element rendering
  # =================================================

  # Render dynamic UI elements
  output$html.slider.qrange <- ui_render_slider_qrange(nrow(qa_data))
  output$html.action.start <- ui_render_action_start(learning_state$start, app_error)
  output$html.action.save <- ui_render_action_save(!save_needed(), user_state$app_error)

  # User selection observer
  ui_observe_user_selection(session, user_state)

  # =============== USER MANAGEMENT =================
  # User selection, addition, deletion handling
  # ================================================

  # User account switching
  observeEvent(input$select.user,{
    user_state <- user_switch_reset_state(user_state, input$select.user)
    learning_state <- state_reset_learning(learning_state)
  }) 

  # User addition
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

  # User deletion
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

  # =============== LEARNING ENGINE =================
  # Learning session, question display, feedback processing
  # =================================================

  # Configuration updates
  observe({
    config_state$prob_base <- input$prob.base
    config_state$zero_limit <- input$zeronum
    config_state <- learning_update_range(config_state, input$slider.qrange, qa_data)
    config_state <- learning_update_probability(config_state, user_state, learning_state)
  })

  # Learning session controls
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

  # Feedback handling
  observeEvent(input$action.ok,{
    result <- learning_handle_feedback(qa_data, config_state, learning_state, is_correct = TRUE)
    learning_state <- result$updated_learning_state
    if (!result$success) {
      ui_show_result(result)
    }
  }) 
  observeEvent(input$action.ng,{
    result <- learning_handle_feedback(qa_data, config_state, learning_state, is_correct = FALSE)
    learning_state <- result$updated_learning_state
    if (!result$success) {
      ui_show_result(result)
    }
  }) 

  # =============== DATA PERSISTENCE ================
  # Score saving and data persistence
  # =================================================

  # Save user score
  observeEvent(input$action.save,{ 
    if (app_error) {
      ui_show_data_error("save score")
      return()
    }
    effective_scores <- user_state$score + learning_state$current_score

    result <- data_save_user_score(
      username = input$select.user,
      current_user = user_state$user,
      user_scores = effective_scores,
      qa_data = qa_data
      )

    if (result$success) {
      user_state$all_user_scores <- result$data
      user_state$score <- effective_scores
      learning_state$current_score <- rep(0L, nrow(qa_data))
    }

    ui_show_result(result)
  }) 

  # =============== OUTPUT RENDERING ================
  # Dynamic content output rendering
  # =================================================

  # Learning interface
  output$qanda <- ui_render_qanda(learning_state)

  # Status displays
  output$welcome <- ui_render_welcome(input, user_state, learning_state)

  # Score displays
  output$score.total <- ui_render_score_total(user_state, learning_state)
  output$score.weak <- ui_render_score_weak(qa_data, user_state, learning_state)

  # Questions table
  output$dt.questions <- ui_render_questions_table(qa_data, user_state, learning_state)

})

