# ================================
# STATE MANAGER MODULE
# Handles reactive state initialization for user, configuration, and learning states
# ================================
source("constants.R")

state_initialize_user <- function(qa_data, score_global, app_error) {
  list(
    app_error = app_error,
    user = UI$DEFAULTS$USER,
    user_names = names(score_global),
    all_user_scores = score_global,
    score = if(UI$DEFAULTS$USER %in% names(score_global)) score_global[[UI$DEFAULTS$USER]] else rep(0L, nrow(qa_data))
    )
}

state_initialize_config <- function(qa_data) {
  list(
    range_min = 1L,
    range_max = nrow(qa_data),
    prob_base = UI$DEFAULTS$PROBABILITY_BASE,
    zero_limit = UI$DEFAULTS$ZERO_LIMIT
  )
}

state_initialize_learning <- function(qa_data) {
  list(
    question_count = 0L,
    correct_count = 0L,
    current_score = rep(0L, nrow(qa_data)),
    probabilities = rep(1, nrow(qa_data)),
    start = FALSE,
    index = NULL,
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
  learning_state$current_score <- rep(0L, length(learning_state$current_score))
  learning_state$probabilities <- rep(1, length(learning_state$probabilities))
  learning_state$start <- FALSE
  learning_state$index <- NULL
  learning_state$question <- ""
  learning_state$answer <- ""
  return(learning_state)
}
