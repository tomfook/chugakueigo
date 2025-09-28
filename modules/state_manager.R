# ================================
# STATE MANAGER MODULE
# Handles reactive state initialization for user, configuration, and learning states
# ================================
source("constants.R")

state_initialize_user <- function(qa_count, user_score, user_names, app_error) {
  list(
    app_error = app_error,
    user = UI$DEFAULTS$USER,
    user_names = user_names,
    score = user_score
    )
}

state_initialize_config <- function(qa_count) {
  list(
    range_min = 1L,
    range_max = qa_count,
    prob_base = UI$DEFAULTS$PROBABILITY_BASE,
    zero_limit = UI$DEFAULTS$ZERO_LIMIT
  )
}

state_initialize_learning <- function(qa_data) {
  qa_count <- nrow(qa_data)
  list(
    question_count = 0L,
    correct_count = 0L,
    current_score = setNames(rep(0L, qa_count), qa_data$question_id),
    probabilities = setNames(rep(1, qa_count), qa_data$question_id),
    start = FALSE,
    question_id = NULL,
    question = "",
    answer = "",
    correct_answer = ""
  )
}

state_create_reactive <- function(init_data) {
  rv <- reactiveValues()
  for(name in names(init_data)) {
    rv[[name]] <- init_data[[name]]
  }
  return(rv)
}

state_reset_learning <- function(learning_state) {
  learning_state$question_count <- 0L
  learning_state$correct_count <- 0L
  learning_state$current_score[] <- 0L
  learning_state$probabilities[] <- 1
  learning_state$start <- FALSE
  learning_state$question_id <- NULL
  learning_state$question <- ""
  learning_state$answer <- ""
  return(learning_state)
}

state_update_config_range <- function(config_state, slider_range, qa_count) {
  if(is.null(slider_range[1])) {
    config_state$range_min <- 1L
  } else {
    config_state$range_min <- slider_range[1]
  }

  if(is.null(slider_range[2])) {
    config_state$range_max <- qa_count
  } else {
    config_state$range_max <- slider_range[2]
  }

  return(config_state)
}

