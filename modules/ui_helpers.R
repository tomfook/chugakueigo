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

ui_render_score_total<- function(user_state) {
  renderText({
    total <- sum(user_state$score)
    paste("Total score:", total)
  })
}

ui_render_score_weak <- function(qa_data, user_state, limit = 5) {
  renderTable({
    qa_data %>%
      mutate(score = user_state$score) %>%
      arrange(score) %>%
      head(limit)
  })
}

ui_render_qanda <- function(learning_state) {
  renderTable({
    tibble::tibble(
      ` ` = c("Q.", "A."),
      sentence = paste0(c(learning_state$question, learning_state$answer), "")
    )
  })
}

ui_render_questions_table <- function(qa_data, user_state) {
  DT::renderDataTable({
    qa_data %>%
      mutate(score = user_state$score) %>%
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
	style = STYLES$INACTIVE, disabled = TRUE
	)
    } else {
      actionButton("action.start", label = "Start Learning",
        style = if_else(is_started, STYLES$INACTIVE, STYLES$ACTIVE)
    )
    }
  })
}

ui_render_action_save <- function(scores_match, has_error = FALSE) {
  renderUI({
    if (has_error) {
      actionButton("action.save", label = "Save Score (Disabled)",
		   style = STYLES$INACTIVE, disabled = TRUE
		   )
    } else {
      actionButton("action.save", label = "Save Score", style = if_else(scores_match, STYLES$INACTIVE, STYLES$ACTIVE)
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
    delete_choices <- user_state$user_names[user_state$user_names != DEFAULTS$USER]
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

ui_show_data_error <- function(operation) {
  showNotification(
    paste("Data error: Cannot", operation), type = "error"
  )
}
