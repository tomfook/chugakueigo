source("constants.R")

state_initialize <- function(main_data, score_global, app_error) {
  list(
    #system
    app_error = app_error,

    #user
    user = DEFAULTS$USER,
    user_names = names(score_global),
    score.all = score_global,
    score = if(DEFAULTS$USER %in% names(score_global)) score_global[[DEFAULTS$USER]] else rep(0L, nrow(main_data))
    )
}

state_initialize_config <- function(main_data) {
  list(
    range_min = 1L,
    range_max = nrow(main_data),
    probabilities = rep(1, nrow(main_data)),
    prob_base = DEFAULTS$PROB_BASE,
    zero_limit = DEFAULTS$ZERO_LIMIT
  )
}

state_initialize_learning_session <- function() {
  list(
    trial = 0L,
    ok = 0L,
    start = FALSE,
    index = NULL,
    question = "",
    answer = "",
    correct_answer = ""
  )
}
