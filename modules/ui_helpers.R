# ================================
# UI HELPERS MODULE
# Handles all UI rendering, observers, and notification functions
# ================================

library(dplyr)
source("constants.R")

# ================================
# Content Rendering Functions
# Data display and content generation functions
# ================================

ui_render_welcome <- function(input, user_state, learning_state) {
  renderText({
    validate(
      need(input$select.user, "Please select a user")
    )
    if(learning_state$start) {
      ordinal_suffix <- dplyr::case_when(
       learning_state$question_count %in% 11:13 ~ "th",
       learning_state$question_count %% 10 == 1 ~ "st",
       learning_state$question_count %% 10 == 2 ~ "nd",
       learning_state$question_count %% 10 == 3 ~ "rd",
       TRUE ~ "th"
     )
      paste0(
	     input$select.user, "'s ", learning_state$question_count, ordinal_suffix, " Trial", ", (OK: ", learning_state$correct_count, ")"
	     )
    } else {
      paste("Not started. Press the start button")
    }
  })
}

ui_render_score_total<- function(effective_score_reactive) {
  renderText({
    validate(
      need(effective_score_reactive(), "Loading scores...")
    )
    total <- sum(effective_score_reactive())
    paste("Total score:", total)
  })
}

ui_render_score_weak <- function(qa_data, effective_score_reactive, limit = 5) {
  renderTable({
    validate(
      need(effective_score_reactive(), "Loading scores..."),
      need(nrow(qa_data) > 0, "Loading questions...")
    )
    effective_score <- effective_score_reactive()

    score_df <- data.frame(
      question_id = as.integer(names(effective_score)),
      score = as.integer(effective_score)
    )

    qa_data %>%
      left_join(score_df, by = "question_id") %>%
      select(question, answer, score) %>%
      arrange(score) %>%
      head(limit)
  })
}

ui_render_qanda <- function(learning_state) {
  renderTable({
    validate(
      need(learning_state$start, "Press 'Start Learning' to begin")
    )
    validate(
      need(learning_state$question != "", "Loading question...")
    )
    tibble::tibble(
      ` ` = c("Q.", "A."),
      sentence = paste0(c(learning_state$question, learning_state$answer), "")
    )
  })
}

ui_render_questions_table <- function(qa_data, effective_score_reactive) {
  DT::renderDataTable({
    validate(
      need(effective_score_reactive(), "Loading scores..."),
      need(nrow(qa_data) > 0, "Loading questions...")
    )
    effective_score <- effective_score_reactive()

    score_df <- data.frame(
      question_id = as.integer(names(effective_score)),
      score = as.integer(effective_score)
    )
    qa_data %>%
      left_join(score_df, by = "question_id") %>%
      select(question, answer, score)
  })
}

ui_render_slider_qrange <- function(max_questions) {
  renderUI({
    sliderInput(
      "slider.qrange",
      label = h4("Range of Question"),
      min = 1L,
      max = max_questions,
      value = c(1, max_questions)
    )
  })
}

# ================================
# Control Element Functions
# Interactive UI element generation functions
# ================================

ui_render_action_start <- function(is_started, has_error = FALSE) {
  renderUI({
    if (has_error) {
      actionButton("action.start", label = "Start Learning (Disabled)",
	class = "text-muted", disabled = TRUE
	)
    } else {
      actionButton("action.start", label = "Start Learning",
        class = if_else(is_started, "text-muted", "text-dark")
    )
    }
  })
}

ui_render_action_save <- function(scores_match, has_error = FALSE) {
  renderUI({
    if (has_error) {
      actionButton("action.save", label = "Save Score (Disabled)",
		   class = "text-muted", disabled = TRUE
		   )
    } else {
      actionButton("action.save", label = "Save Score", class = if_else(scores_match, "text-muted", "text-dark")
      )
    }
  })
}

# ================================
# Observer Functions
# Reactive observer functions for UI updates
# ================================

ui_observe_user_selection <- function(session, user_state){
  observe({
    updateSelectInput(session, "select.user", choices = user_state$user_names)
  })
}

ui_observe_delete_choices <- function(session, user_state) {
  observe({
    delete_choices <- user_state$user_names[user_state$user_names != APP$DEFAULTS$USER]
    updateSelectInput(
      session,
      "select.userdelete",
      choices = if(length(delete_choices) > 0) delete_choices else "No users to delete"
      )
  })
}

# ================================
# Notification Functions
# User feedback and error notification functions
# ================================

ui_show_result <- function(result) {
  showNotification(
    result$message,
    type = if(result$success) "message" else "error"
  )
}

# ================================
# Loading Indicator Functions
# Progress indicators for long-running operations
# ================================

ui_with_initialization_progress <- function(operation) {
  withProgress(message = "Initializing ChugakuEigo...", value = 0, {
    incProgress(0.2, detail = "Loading question database...")
    incProgress(0.5, detail = "Connecting to Google Sheets...")
    incProgress(0.8, detail = "Loading user accounts...")
    result <- operation()
    incProgress(1.0, detail = "Initialization complete!")
    return(result)
  })
}

ui_with_save_progress <- function(operation) {
  withProgress(message = "Saving your score...", value = 0, {
    incProgress(0.3, detail = "Preparing score data...")
    incProgress(0.7, detail = "Uploading to Google Sheets...")
    result <- operation()
    incProgress(1.0, detail = if(result$success) "Score saved successfully!" else "Save completed")
    return(result)
  })
}

ui_with_user_add_progress <- function(username, add_operations) {
  withProgress(message = paste("Adding user:", username), value = 0, {
    incProgress(0.3, detail = "Adding to user registry...")
    meta_result <- add_operations$meta_operation()

    if (meta_result$success) {
      incProgress(0.7, detail = "Creating userworksheet...")
      worksheet_result <- add_operations$worksheet_operation()
      incProgress(1.0, detail = "User added successfully!")
      return(list(meta = meta_result, worksheet = worksheet_result))
    } else {
      incProgress(1.0, detail = "User addition failed")
      return(list(meta = meta_result, worksheet = NULL))
    }
  })
}
