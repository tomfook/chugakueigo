library(dplyr)
source("constants.R")

#==============
# UI render functions 
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

ui_render_score_total<- function(qa) {
  renderText({
    total <- sum(qa$score)
    paste("Total score:", total)
  })
}

ui_render_score_weak <- function(main, qa, limit = 5) {
  renderTable({
    main %>%
      mutate(score = qa$score) %>%
      arrange(score) %>%
      head(limit)
  })
}

ui_render_qanda <- function(qa) {
  renderTable({
    tibble::tibble(
      ` ` = c("Q.", "A."),
      sentence = paste0(c(qa$question, qa$answer), "")
    )
  })
}

ui_render_questions_table <- function(main, qa) {
  DT::renderDataTable({
    main %>%
      mutate(score = qa$score) %>%
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


#=======
# UI Observer Functions 
#=======
ui_observe_user_selection <- function(session, qa){
  observe({
    updateSelectInput(session, "select.user", choices = qa$namelist)
  })
}

ui_observe_delete_choices <- function(session, qa) {
  observe({
    delete_choices <- qa$namelist[qa$namelist != DEFAULTS$USER]
    updateSelectInput(
      session,
      "select.userdelete",
      choices = if(length(delete_choices) > 0) delete_choices else "No users to delete"
      )
  })
}


#======
# UI Show Notification Functions
#======
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
