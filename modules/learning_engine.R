source("constants.R")
source("shuffle_config.R")

learning_select_shuffle_keywords <- function(keyword_list, selection_rate) {
  selection_flags <- runif(length(keyword_list)) < selection_rate
  return(keyword_list[selection_flags])
}
learning_shuffle_question <- function(q, a){
    qa_mod <- list(q = q, a = a)
    
    shuffle_text <- function(keywords, qa){
      q <- qa$q
      a <- qa$a
      
      selected_keywords <- learning_select_shuffle_keywords(keywords, SHUFFLE$SELECTION_RATE)
      for (i in seq_along(selected_keywords)){
         item <- selected_keywords[[i]]
         if (grepl(item$english[1], a)){
           q <- gsub(item$japanese[1], item$japanese[2], q)
           a <- gsub(item$english[1], item$english[2], a)
         } else if (grepl(item$english[2], a)){
           q <- gsub(item$japanese[2], item$japanese[1], q)
           a <- gsub(item$english[2], item$english[1], a)
         }
      }
      return (list(q = q, a=a))
    }
    
    qa_mod <- shuffle_text(SHUFFLE_KEYWORDS, qa_mod)
    
    return(list(q = qa_mod$q, a = qa_mod$a))
}

learning_select_next_question <- function(main, config_state) {
  question_index <- sample(
    config_state$range_max - config_state$range_min + 1L,
    1,
    prob = config_state$probabilities
  ) + (config_state$range_min - 1L)

    shuffled_qa <- learning_shuffle_question(main$question[question_index], main$answer[question_index])

    list(
      index = question_index,
      question = shuffled_qa$q,
      answer = shuffled_qa$a
    )
}

learning_new_question <- function(main, qa_state, config_state) {
  next_q <- learning_select_next_question(main, config_state)
  qa_state$index <- next_q$index
  qa_state$question <- next_q$question
  qa_state$answer.remember <- next_q$answer
  qa_state$answer <- ""
  return(qa_state)
}

learning_update_range_and_probability <- function(config_state, qa, slider_range, prob_base, zero_limit, main_data, learning_session_state) {
  if(is.null(slider_range[1])) {
    config_state$range_min <- 1L
  } else {
    config_state$range_min <- slider_range[1]
  }

  if(is.null(slider_range[2])) {
    config_state$range_max <- nrow(main_data)
  } else {
    config_state$range_max <- slider_range[2]
  }

  score_range <- qa$score[seq(config_state$range_min, config_state$range_max)]
  config_state$probabilities <- (abs(prob_base - learning_session_state$ok * SCORING$MULTIPLIER - 1) + 1)^(-score_range) * (cumsum(score_range == 0L) <= zero_limit)
  return(config_state)

}

learning_start_session <- function(qa, main_data, config_state) {
  qa <- learning_new_question(main_data, qa, config_state)
  return(qa)
}

learning_handle_ok_feedback <- function(qa, main_data, config_state, learning_session_state) {
  if (!learning_session_state$start) {
    return(list(success = FALSE, updated_qa = qa, message = "Learning not started"))
  }

  if (qa$answer == "") {
    return(list(success = FALSE, updated_qa = qa, message = "Confirm Answer!"))
  }

  qa$score[qa$index] <- qa$score[qa$index] + 1L
  qa <- learning_new_question(main_data, qa, config_state)

  return(list(success = TRUE, updated_qa = qa, message = ""))
}

learning_handle_ng_feedback <- function(qa, main_data, config_state, learning_session_state) {
  if (!learning_session_state$start) {
    return(list(success = FALSE, updated_qa = qa, message = "Learning not started"))
  }

  if (qa$answer == "") {
    return(list(success = FALSE, updated_qa = qa, message = "Confirm Answer!"))
  }

  qa <- learning_new_question(main_data, qa, config_state)

  return(list(success = TRUE, updated_qa = qa, message = ""))
}
