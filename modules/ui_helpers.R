library(dplyr)

ui_render_welcome <- function(input, qa) {
  renderText({
    if(qa$start) {
      trial.prefix <- dplyr::case_when(
       qa$trial %in% 11:13 ~ "th",
       qa$trial %% 10 == 1 ~ "st",
       qa$trial %% 10 == 2 ~ "nd",
       qa$trial %% 10 == 3 ~ "rd",
       TRUE ~ "th"
     )
      paste0(
	     input$select.user, "'s ", qa$trial, trial.prefix, " Trial", ", (OK: ", qa$ok, ")"
	     )
    } else {
      paste("Not started. Press the start button")
    }
  })
}

ui_calculate_total_score <- function(scores) {
  sum(scores)
}
ui_render_score_total<- function(qa) {
  renderText({
    total <- ui_calculate_total_score(qa$score)
    paste("Total score:", total)
  })
}

ui_get_weak_questions <- function(questions_data, scores, limit = 5) {
  questions_data %>%
    mutate(score = scores) %>%
    arrange(score) %>%
    head(limit)
}
ui_render_score_weak <- function(main, qa, limit = 5) {
  renderTable({
    ui_get_weak_questions(main, qa$score, limit)
  })
}

ui_format_qanda_data <- function(question, answer) {
  tibble::tibble(
    ` ` = c("Q.", "A."),
    sentence = paste0(c(question, answer), "")
  )
}
ui_render_qanda <- function(qa) {
  renderTable({
    ui_format_qanda_data(qa$question, qa$answer)
  })
}

ui_prepare_questions_data <- function(questions_data, scores) {
  questions_data %>%
    mutate(score = scores) %>%
    select(question, answer, score)
}
ui_render_questions_table <- function(main, qa) {
  DT::renderDataTable({
    ui_prepare_questions_data(main, qa$score)
  })
}
