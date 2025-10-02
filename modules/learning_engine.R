# ================================
# LEARNING ENGINE MODULE
# Handles question selection, text shuffling, and learning session logic
# ================================

source("constants.R")
source("shuffle_config.R")

# =================
# QUESTION SELECTION FUNCTIONS
# =================
learning_select_next_question <- function(learning_state, qa_data) {
  question_index <- sample(
    length(learning_state$probabilities),
    1,
    prob = learning_state$probabilities
  )

    shuffled_qa <- learning_shuffle_question(qa_data$question[question_index], qa_data$answer[question_index])

    list(
      question_id = qa_data$question_id[question_index],
      question = shuffled_qa$q,
      answer = shuffled_qa$a
    )
}

# =================
# LEARNING SESSION FUNCTIONS
# =================
learning_new_question <- function(learning_state, qa_data, config_state) {
  next_q <- learning_select_next_question(learning_state, qa_data)
  learning_state$question_id <- next_q$question_id
  learning_state$question <- next_q$question
  learning_state$correct_answer <- next_q$answer
  learning_state$answer <- ""
  learning_state$question_count <- learning_state$question_count + 1L
  return(learning_state)
}

learning_update_probability <- function(learning_state, config_state, effective_score) {
  base_probabilities <- (abs(config_state$prob_base - learning_state$correct_count * LEARNING$PROBABILITY$MULTIPLIER -1) + 1)^(-effective_score) * (cumsum(effective_score == 0L) <= config_state$zero_limit)

  #masking
  learning_state$probabilities <- base_probabilities
  question_ids <- as.integer(names(effective_score))
  out_of_range_ids <- question_ids[(question_ids < config_state$range_min) | (question_ids > config_state$range_max)]
  learning_state$probabilities[as.character(out_of_range_ids)] <- 0

  return(learning_state)
}

learning_start_session <- function(learning_state, qa_data, config_state) {
  learning_state$start <- TRUE
  learning_state$question_count <- 0L
  learning_state$correct_count <- 0L
  learning_state <- learning_new_question(learning_state, qa_data, config_state)
  return(learning_state)
}

learning_handle_feedback <- function(learning_state, qa_data, config_state, is_correct) {
  if (!learning_state$start) {
    return(list(success = FALSE, updated_learning_state = learning_state, message = "Learning not started"))
  }

  if (learning_state$answer == "") {
    return(list(success = FALSE, updated_learning_state = learning_state, message = "Confirm Answer!"))
  }

  if (is_correct) {
    learning_state$current_score[learning_state$question_id] <- learning_state$current_score[learning_state$question_id] + 1L
    learning_state$correct_count <- learning_state$correct_count + 1L
  }

  learning_state <- learning_new_question(learning_state, qa_data, config_state)

  return(list(success = TRUE, updated_learning_state = learning_state, message = ""))
}

# =================
# SHUFFLE FUNCTIONS
# Text randomization to prevent memorization
# =================

learning_apply_shuffle_keywords <- function(qa_pair, keywords, selection_rate) {
  selected_keywords <- keywords[runif(length(keywords)) < selection_rate]

  for (keyword in selected_keywords) {
    for (direction in c(1, 2)) {
      pattern <- paste0("\\b", keyword$english[direction], "\\b")
      if (grepl(pattern, qa_pair$a, ignore.case = FALSE)) {
	opposite <- if (direction == 1) 2 else 1
	qa_pair$q <- gsub(keyword$japanese[direction], keyword$japanese[opposite], qa_pair$q, fixed = TRUE)
	qa_pair$a <- gsub(pattern, keyword$english[opposite], qa_pair$a, ignore.case = FALSE)
	break
      }
    }
  }

  return(qa_pair)
}
learning_shuffle_question <- function(q, a){
    qa_pair <- list(q = q, a = a)
    qa_pair <- learning_apply_shuffle_keywords(qa_pair, SHUFFLE_KEYWORDS, LEARNING$SHUFFLE$SELECTION_RATE)
    return(list(q = qa_pair$q, a = qa_pair$a))
}
