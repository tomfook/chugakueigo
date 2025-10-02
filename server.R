library(shiny) 
library(dplyr)
source("modules/data_manager.R")
source("modules/learning_engine.R")
source("modules/ui_helpers.R")
source("modules/state_manager.R")
source("modules/security_manager.R")
source("constants.R")

shinyServer(function(input, output, session){ 
  # =============== INITIALIZATION ==================
  # Data initialization, error handling, and state setup
  # =================================================
  
  # Global cache initialization 
  if (!exists("app_global_cache", envir = .GlobalEnv)) {
    assign("app_global_cache", list(
      sheet_names = list(
	data = NULL,
	timestamp = NULL,
	is_valid = FALSE
      ),
      users_meta = list(
	data = NULL,
	timestamp = NULL,
	is_valid = FALSE
      )
    ), envir = .GlobalEnv)
  }

  # Initialize core data
  init_result <- ui_with_initialization_progress(function() {data_initialize()})
  app_error <- !init_result$success

  if (app_error) {
    qa_data <- data.frame(
      question_id = 1L,
      question = "",
      answer = "",
      created_at = Sys.time(),
      updated_at = Sys.time()
    )
    user_score <- setNames(rep(0L, 1), "1")
    showNotification(
      paste("Important: Data initialization failed: ", init_result$message, "- restart app"),
      type = "error", duration = NULL
      )
  } else{
    qa_data <- init_result$data$qa_data
    user_score <- init_result$data$user_score
  }

  qa_count <- nrow(qa_data)
  user_names <- init_result$data$user_names

  # Initialize reactive states
  session$userData$user_state <- state_create_reactive(state_initialize_user(qa_count, user_score, user_names, app_error))
  session$userData$config_state <- state_create_reactive(state_initialize_config(qa_count))
  session$userData$learning_state <- state_create_reactive(state_initialize_learning(qa_data))

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
    user_score_result <- data_read_user_score(input$select.user, qa_data)
    session$userData$user_state$user <- input$select.user
    session$userData$user_state$score <- user_score_result$data

    session$userData$learning_state <- state_reset_learning(session$userData$learning_state)
    session$userData$learning_state <- learning_update_probability(session$userData$learning_state, session$userData$config_state, effective_score())
  }) 

  # User addition
  observeEvent(input$action.useradd,{
    if (session$userData$user_state$app_error) {
      showNotification("Data error: Cannot add user - restart app", type = "error", duration = NULL)
      return()
    }

    username <- input$textinp.useradd

    if (is.null(username) || username == "" || trimws(username) == "") {
      showNotification("Username cannot be empty.", type = "error")
      return()
    }
    if (nchar(username) > APP$USERNAME$MAX_LENGTH) {
      showNotification(paste0("Username too long (max ", APP$USERNAME$MAX_LENGTH, " characters)."), type = "error")
      return()
    }
    if (!grepl(APP$USERNAME$PATTERN, username)) {
      showNotification(paste0("Username can only contain ", APP$USERNAME$ALLOWED_CHARS_DESC, "."), type = "error")
      return()
    }
    if (username == APP$DEFAULTS$USER) {
      showNotification(paste0("Username '", APP$DEFAULTS$USER, "' is reserved."), type = "error")
      return()
    }
    if (username %in% session$userData$user_state$user_names) {
      showNotification("Your name has been already registered.", type = "warning")
      return()
    }

    results <- ui_with_user_add_progress(
      username,
      list(
	meta_operation = function() data_add_user_to_meta(username),
	worksheet_operation = function() data_ensure_user_worksheet(username, qa_data$question_id)
	)
    )

    if (results$meta$success) {
      session$userData$user_state$user_names <- c(session$userData$user_state$user_names, username)

      if (!is.null(results$worksheet) && !results$worksheet$success) {
	showNotification(
	  paste("Warning: User added but failed to create worksheet:", results$worksheet$message),
	  type = "warning",
	  duration = 10
	)
      }
      updateTextInput(session, "textinp.useradd", value = "")
    } 
    ui_show_result(results$meta)
  })

  # User deletion
  ui_observe_delete_choices(session, session$userData$user_state)

  observeEvent(input$action.userdelete, {
    if (session$userData$user_state$app_error) {
      showNotification("Data error: Cannot delete user - restart app", type = "error", duration = NULL)
      return()
    }

    selected_user <- input$select.userdelete
    if (is.null(selected_user) || selected_user == "No users to delete"){
      showNotification("No user selected for deletion.", type = "warning")
      return()
    }
    if (selected_user == input$select.user) {
      showNotification("Cannot delete currently selected user. Please switch to another user first.", type = "warning")
      return()
    }

    showModal(modalDialog(
      title = "Delete User - Password Required",
      div(
	p(paste("Are you sure you want to delete user '", selected_user, "'?", sep = "")),
	br(),
	p("Enter administrator password to confirm deletion:", class = "text-danger font-weight-bold"),
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
    meta_result <- data_remove_user_from_meta(selected_user)
    if (meta_result$success) {
      worksheet_result <- data_delete_user_worksheet(selected_user)

      session$userData$user_state$user_names <- session$userData$user_state$user_names[session$userData$user_state$user_names != selected_user]

      if (!worksheet_result$success) {
	showNotification(
	  paste("Warning: User removed but failed to delete worksheet:", worksheet_result$message),
	  type = "warning",
	  duration = 10
	)
      }
    }

    ui_show_result(meta_result)
    removeModal()
  })

  # =============== LEARNING ENGINE =================
  # Learning session, question display, feedback processing
  # =================================================

  # Configuration updates
  observe({
    session$userData$config_state$prob_base <- input$prob.base
    session$userData$config_state$zero_limit <- input$zero.score.limit
    session$userData$config_state <- state_update_config_range(session$userData$config_state, input$slider.qrange, qa_count)
    session$userData$learning_state <- learning_update_probability(session$userData$learning_state, session$userData$config_state, effective_score())
  })

  # Learning session controls
  observeEvent(input$action.start,{ 
    if (session$userData$user_state$app_error) {
      showNotification("Data error: Cannot start learning - restart app", type = "error", duration = NULL)
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
    if (session$userData$user_state$app_error) {
      showNotification("Data error: Cannot save score - restart app", type = "error", duration = NULL)
      return()
    }

    effective_scores <- session$userData$user_state$score + session$userData$learning_state$current_score

    result <- ui_with_save_progress(function() {
      data_save_user_score(
        username = input$select.user,
        current_user = session$userData$user_state$user,
        user_scores = effective_scores
        )
      })

    if (result$success) {
      session$userData$user_state$score <- effective_scores
      session$userData$learning_state$current_score[] <- 0L
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

