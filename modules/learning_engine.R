# ================================
# LEARNING ENGINE MODULE
# Handles question selection, text shuffling, and learning session logic
# ================================

source("constants.R")
source("shuffle_config.R")

# =================
# QUESTION SELECTION FUNCTIONS
# =================
learning_select_next_question <- function(qa_data, learning_state) {
  question_index <- sample(
    length(learning_state$probabilities),
    1,
    prob = learning_state$probabilities
  )

    shuffled_qa <- learning_shuffle_question(qa_data$question[question_index], qa_data$answer[question_index])

    list(
      index = question_index,
      question = shuffled_qa$q,
      answer = shuffled_qa$a
    )
}

# =================
# LEARNING SESSION FUNCTIONS
# =================
learning_new_question <- function(learning_state, qa_data, config_state) {
  next_q <- learning_select_next_question(qa_data, learning_state)
  learning_state$index <- next_q$index
  learning_state$question <- next_q$question
  learning_state$correct_answer <- next_q$answer
  learning_state$answer <- ""
  learning_state$question_count <- learning_state$question_count + 1L
  return(learning_state)
}

learning_update_range <- function(config_state, slider_range, qa_count) {
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

learning_update_probability <- function(learning_state, config_state, effective_score) {
  base_probabilities <- (abs(config_state$prob_base - learning_state$correct_count * LEARNING$PROBABILITY$MULTIPLIER -1) + 1)^(-effective_score) * (cumsum(effective_score == 0L) <= config_state$zero_limit)
  range_mask <- (seq_along(effective_score) >= config_state$range_min) & (seq_along(effective_score) <= config_state$range_max)
  learning_state$probabilities <- base_probabilities * range_mask

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
    learning_state$current_score[learning_state$index] <- learning_state$current_score[learning_state$index] + 1L
    learning_state$correct_count <- learning_state$correct_count + 1L
  }

  learning_state <- learning_new_question(learning_state, qa_data, config_state)

  return(list(success = TRUE, updated_learning_state = learning_state, message = ""))
}

# =================
# SHUFFLE FUNCTIONS
# Text randomization to prevent memorization
# =================

learning_match_english <- function(pattern, text) {
  word_pattern <- paste0("\\b", pattern, "\\b")

  return(grepl(word_pattern, text, ignore.case = FALSE))
}
learning_replace_with_boundaries <- function(text, old_word, new_word, use_boundaries = TRUE) {
  if (use_boundaries) {
    word_pattern <- paste0("\\b", old_word, "\\b")
    return(gsub(word_pattern, new_word, text, ignore.case = FALSE))
  } else {
    return(gsub(old_word, new_word, text, fixed = TRUE))
  }
}
learning_apply_single_keyword <- function(qa_pair, keyword_item) {
  if (learning_match_english(keyword_item$english[1], qa_pair$a)) {
    qa_pair$q <- learning_replace_with_boundaries(qa_pair$q, keyword_item$japanese[1], keyword_item$japanese[2], use_boundaries = FALSE)
    qa_pair$a <- learning_replace_with_boundaries(qa_pair$a, keyword_item$english[1], keyword_item$english[2], use_boundaries = TRUE)
  } else if (learning_match_english(keyword_item$english[2], qa_pair$a)) {
    qa_pair$q <- learning_replace_with_boundaries(qa_pair$q, keyword_item$japanese[2], keyword_item$japanese[1], use_boundaries = FALSE)
    qa_pair$a <- learning_replace_with_boundaries(qa_pair$a, keyword_item$english[2], keyword_item$english[1], use_boundaries = TRUE)
  }
  return(qa_pair)
}
learning_select_shuffle_keywords <- function(keyword_list, selection_rate) {
  selection_flags <- runif(length(keyword_list)) < selection_rate
  return(keyword_list[selection_flags])
}
learning_shuffle_question <- function(q, a){
    qa_pair <- list(q = q, a = a)
    selected_keywords <- learning_select_shuffle_keywords(SHUFFLE_KEYWORDS, LEARNING$SHUFFLE$SELECTION_RATE)

    for (keyword in selected_keywords) {
      qa_pair <- learning_apply_single_keyword(qa_pair, keyword)
    }
    
    return(list(q = qa_pair$q, a = qa_pair$a))
}
