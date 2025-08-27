library(dplyr)
source("constants.R")

#==============
# UI render functions (UI生成) 
#==============
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

ui_render_action_start <- function(is_started) {
  renderUI({
    actionButton("action.start", label = "Start Learning",
      style = if_else(is_started, STYLES$INACTIVE, STYLES$ACTIVE)
    )
  })
}

ui_render_action_save <- function(scores_match) {
  renderUI({
    actionButton("action.save", label = "Save Score", style = if_else(scores_match, STYLES$INACTIVE, STYLES$ACTIVE)
    )
  })
}


#=======
# UI Observer Functions (UI更新)
#=======
ui_observe_user_selection <- function(session, qa){
  observe({
    updateSelectInput(session, "select.user", choices = qa$namelist)
  })
}

ui_observe_delete_choices <- function(session, qa) {
  observe({
    delete_choices <- qa$namelist[qa$namelist != "guest"]
    updateSelectInput(
      session,
      "select.userdelete",
      choices = if(length(delete_choices) > 0) delete_choices else "No users to delete"
      )
  })
}
