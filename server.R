library(shiny) 
library(dplyr)
source("modules/data_manager.R")
source("modules/user_manager.R")
source("modules/learning_engine.R")
source("modules/ui_helpers.R")
source("modules/state_manager.R")
source("modules/security_manager.R")
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

  qa_count <- nrow(qa_data)

  # Initialize reactive states
  session$userData$user_state <- state_create_reactive(state_initialize_user(qa_count, score_global, app_error))
  session$userData$config_state <- state_create_reactive(state_initialize_config(qa_count))
  session$userData$learning_state <- state_create_reactive(state_initialize_learning(qa_count))

  effective_score <- reactive({
    session$userData$user_state$score + session$userData$learning_state$current_score
  })
  save_needed <- reactive({
    sum(session$userData$learning_state$current_score) > 0
  })

  # =============== UI RENDERING ====================
  # Static UI element rendering
  # =================================================

  # Render dynamic UI elements
  output$html.slider.qrange <- ui_render_slider_qrange(qa_count)
  output$html.action.start <- ui_render_action_start(session$userData$learning_state$start, app_error)
  output$html.action.save <- ui_render_action_save(!save_needed(), session$userData$user_state$app_error)

  # User selection observer
  ui_observe_user_selection(session, session$userData$user_state)

  # =============== USER MANAGEMENT =================
  # User selection, addition, deletion handling
  # ================================================

  # User account switching
  observeEvent(input$select.user,{
    session$userData$user_state <- user_switch_reset_state(session$userData$user_state, input$select.user)
    session$userData$learning_state <- state_reset_learning(session$userData$learning_state)
  }) 

  # User addition
  observeEvent(input$action.useradd,{
    if (app_error) {
      ui_show_data_error("add user")
      return()
    }
    result <- user_add_new(session$userData$user_state, input$textinp.useradd, qa_count)
    ui_show_result(result)
    if (result$success) {
      updateTextInput(session, "textinp.useradd", value = "")
    }
  })

  # User deletion
  ui_observe_delete_choices(session, session$userData$user_state)

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
      title = "Delete User - Password Required",
      div(
	p(paste("Are you sure you want to delete user '", selected_user, "'?", sep = "")),
	br(),
	p("Enter administrator password to confirm deletion:", style = UI$STYLES$WARNING),
	passwordInput("delete_password", label = NULL, placeholder = "Administrator password")
	),
      footer = tagList(
	modalButton("Cancel"),
	actionButton("confirm_delete", "Delete", class = "btn-danger")
      )
    ))
  })
  observeEvent(input$confirm_delete, {
    if (is.null(input$delete_password) || !security_verify_password(input$delete_password, ADMIN$PASSWORD_HASH, ADMIN$SALT)) {
      showNotification(
	"Incorrect administrator password. User deletion cancelled.",
	type = "error",
	duration = 5
      )
      return()
    }
    selected_user <- input$select.userdelete
    result <- user_remove(selected_user, session$userData$user_state)
    ui_show_result(result)
    removeModal()
  })

  # =============== LEARNING ENGINE =================
  # Learning session, question display, feedback processing
  # =================================================

  # Configuration updates
  observe({
    session$userData$config_state$prob_base <- input$prob.base
    session$userData$config_state$zero_limit <- input$zeronum
    session$userData$config_state <- learning_update_range(session$userData$config_state, input$slider.qrange, qa_count)
    session$userData$learning_state <- learning_update_probability(session$userData$learning_state, session$userData$config_state, effective_score())
  })

  # Learning session controls
  observeEvent(input$action.start,{ 
    if (app_error) {
      ui_show_data_error("start learning")
      return()
    }
    session$userData$learning_state <- learning_start_session(session$userData$learning_state, qa_data, session$userData$config_state)
  })
  observeEvent(input$action.answer,{
    session$userData$learning_state$answer <- session$userData$learning_state$correct_answer
  }) 

  # Feedback handling
  observeEvent(input$action.ok,{
    result <- learning_handle_feedback(session$userData$learning_state, qa_data, session$userData$config_state, is_correct = TRUE)
    session$userData$learning_state <- result$updated_learning_state
    if (!result$success) {
      ui_show_result(result)
    }
  }) 
  observeEvent(input$action.ng,{
    result <- learning_handle_feedback(session$userData$learning_state, qa_data, session$userData$config_state, is_correct = FALSE)
    session$userData$learning_state <- result$updated_learning_state
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
    effective_scores <- session$userData$user_state$score + session$userData$learning_state$current_score

    result <- data_save_user_score(
      username = input$select.user,
      current_user = session$userData$user_state$user,
      user_scores = effective_scores,
      qa_count = qa_count
      )

    if (result$success) {
      session$userData$user_state$all_user_scores <- result$data
      session$userData$user_state$score <- effective_scores
      session$userData$learning_state$current_score <- rep(0L, qa_count)
    }

    ui_show_result(result)
  }) 

  # =============== OUTPUT RENDERING ================
  # Dynamic content output rendering
  # =================================================

  # Learning interface
  output$qanda <- ui_render_qanda(session$userData$learning_state)

  # Status displays
  output$welcome <- ui_render_welcome(input, session$userData$user_state, session$userData$learning_state)

  # Score displays
  output$score.total <- ui_render_score_total(effective_score)
  output$score.weak <- ui_render_score_weak(qa_data, effective_score)

  # Questions table
  output$dt.questions <- ui_render_questions_table(qa_data, effective_score)

})

