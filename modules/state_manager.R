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
    probabilities = rep(1, nrow(qa_data)),
    prob_base = UI$DEFAULTS$PROBABILITY_BASE,
    zero_limit = UI$DEFAULTS$ZERO_LIMIT
  )
}

state_initialize_learning <- function() {
  list(
    question_count = 0L,
    correct_count = 0L,
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
