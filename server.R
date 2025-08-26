library(shiny) 
library(dplyr)
source("modules/data_manager.R")
source("modules/user_manager.R")
source("constants.R")

calculate_question_probability <- function(score_range, prob_base, ok_count, zero_limit) {
  (abs(prob_base - ok_count * SCORING$MULTIPLIER - 1) + 1)^(-score_range) *
    (cumsum(score_range == 0L) <= zero_limit)
}

shinyServer(function(input, output, session){ 
  init_data <- initialize_data()
  main <- init_data$main
  score_global <- init_data$score_global

  qa <- reactiveValues() 
  qa$start <- FALSE
  qa$score <- rep(0L, nrow(main))
  qa$namelist <- names(score_global)
  qa$score.all <- score_global

#render UI
  output$html.slider.qrange <- renderUI({ 
    sliderInput(
      "slider.qrange",
      label = h4("Range of Question"),
      min = 1L,
      max = nrow(main),
      value = c(1,nrow(main)))
  }) 
  output$html.action.start <- renderUI({ 
    actionButton("action.start", label = "Start Learning",
      style = if_else(qa$start, STYLES$INACTIVE, STYLES$ACTIVE)
)
  })
  output$html.action.save <- renderUI({ 
    actionButton("action.save", label = "Save Score",
      style = if_else(identical(qa$score.all[[input$select.user]], qa$score), STYLES$INACTIVE, STYLES$ACTIVE)
      )
  })

# user selection update
  observe({
    updateSelectInput(session, "select.user", choices = qa$namelist)
  })

# useradd
  add_new_user <- function(username, qa_state, main_data) {
    validation <- validate_username(username)
    if (!validation$valid) {
      return(list(success = FALSE, message = validation$message))
    }

    result <- create_user_scores(username, qa_state$score.all, nrow(main_data))
    if (!result$success) {
      return(result)
    }

    qa_state$score.all <- result$updated_scores
    qa_state$namelist <- result$updated_namelist

    return(list(success = TRUE, message = result$message))
  }

  observeEvent(input$action.useradd,{
    result <- add_new_user(input$textinp.useradd, qa, main)
    showNotification(result$message, type = if(result$success) "message" else "error")
    if (result$success) {
      updateTextInput(session, "textinp.useradd", value = "")
    }
  })

#remove user
  remove_user <- function(username, qa_state) {
    result <- remove_user_from_scores(username, qa_state$score.all)
    if (!result$success) {
      return(result)
    }
  
    tryCatch({
      write.table(result$updated_scores, PATHS$SCORES, row.names = FALSE, sep = ",")

      qa_state$score.all <- result$updated_scores
      qa_state$namelist <- result$updated_namelist

      return(list(success = TRUE, message = result$message))
    }, error = function(e) {
      return(list(success = FALSE, message = paste("Error removing user:", e$message)))
    })
  }

  
  observe({
    delete_choices <- qa$namelist[qa$namelist != "guest"]
    updateSelectInput(
      session,
      "select.userdelete", 
      choices = if(length(delete_choices) > 0) delete_choices else "No users to delete"
    )
  })

  observeEvent(input$action.userdelete, {
    selected_user <- input$select.userdelete

    if (is.null(selected_user) || selected_user == "No users to delete") {
      showNotification("No user selected for deletion.", type = "warning")
      return()
    }

    if (selected_user == input$select.user) {
      showNotification("Cannot delete currently selected use. Please switch to another user first.", type = "warning")
      return()
    }

    result <- remove_user(selected_user, qa)
    showNotification(result$message, type = if(result$success) "message" else "error")
  })


#user account
  observeEvent(input$select.user,{
    qa$trial <- 0L
    qa$ok <- 0L
    qa$start <- FALSE
    qa$user <- input$select.user
    qa$score <- qa$score.all[[input$select.user]]
    qa$index <- NULL
    qa$question <- ""
    qa$answer <- ""
  }) 

#learning
  observe({
    if(is.null(input$slider.qrange[1])){
      qa$range.min <- 1L
    } else {
      qa$range.min <- input$slider.qrange[1]
    }
    if(is.null(input$slider.qrange[2])){
      qa$range.max <- nrow(main)
    } else {
      qa$range.max <- input$slider.qrange[2]
    }

    qa$prob <- calculate_question_probability(
      score_range = qa$score[seq(qa$range.min, qa$range.max)],
      prob_base = input$prob.base,
      ok_count = qa$ok,
      zero_limit = input$zeronum
      )

  })

  source("shuffle_text.R")
  select_next_question <- function(main, qa_state){
    question_index <- sample(
      qa_state$range.max - qa_state$range.min + 1L,
      1,
      prob = qa_state$prob
    ) + (qa_state$range.min - 1L)

    shuffled_qa <- shuffleQuestion(main$question[question_index], main$answer[question_index])

    list(
      index = question_index,
      question = shuffled_qa$q,
      answer = shuffled_qa$a
    )
  }
  
  newQuestion <- function(){
    next_q <- select_next_question(main, qa)
    qa$index <- next_q$index
    qa$question <- next_q$question
    qa$answer.remember <- next_q$answer
    qa$answer <- "" 
    qa$trial <- qa$trial + 1L
  } 
  observeEvent(input$action.start,{ 
    qa$trial <- 0L
    qa$ok <- 0L
    qa$start <- TRUE
    newQuestion()
  })
  observeEvent(input$action.answer,{
    qa$answer <- qa$answer.remember
  }) 
  observeEvent(input$action.ok,{
    if(qa$start){
      if(qa$answer != ""){
        qa$score[qa$index] <- qa$score[qa$index] + 1L
	qa$ok <- qa$ok + 1L
        newQuestion()
      }else if(qa$answer == ""){
        showNotification("Confirm Answer!")
      }
    }
  }) 
  observeEvent(input$action.ng,{
    if(qa$start){
      if(qa$answer != ""){
        newQuestion()
      }else if(qa$answer == ""){
        showNotification("Confirm Answer!")
      }
    }
  }) 
  output$about <- renderText({
    #paste0("Question Number:", qa$index)#, " / Your Score:", qa$score[qa$index])
  }) 
  output$qanda <- renderTable({
    tibble::tibble(` `= c("Q.", "A."), sentence = paste0((c(qa$question, qa$answer)), ""))
  }) 

#save score
  observeEvent(input$action.save,{ 
    if(qa$user == input$select.user){
      qa$score.all <- read_score(qa = main, path = PATHS$SCORES)
      qa$score.all[[input$select.user]] <- qa$score
      write.table(qa$score.all, PATHS$SCORES, row.names=FALSE, sep = ",")
      qa$score.all <- qa$score.all %>%
        mutate(guest = 0L) %>%
        select(guest, everything())
    }else{ 
      showNotification("You switched user account.")
    }
  }) 

#current status
  output$welcome <- renderText({
    if(qa$start){
      trial.prefix <- dplyr::case_when(
        qa$trial %in% 11:13 ~ "th",
        qa$trial %% 10 == 1 ~ "st",
        qa$trial %% 10 == 2 ~ "nd",
        qa$trial %% 10 == 3 ~ "rd",
        TRUE ~ "th"
      ) 
      paste0(
       input$select.user, "'s ", qa$trial, trial.prefix, " Trial",
       ", (OK: ", qa$ok, ")"
       )
    }else{
      paste("Not started. Press the start button")
    }
  })

#score
  output$score.total <- renderText({
    paste("Total score:", sum(qa$score))
  })
  output$score.weak <- renderTable({ 
    main %>%
      mutate(score = qa$score) %>%
      arrange(score) %>%
      head(5)
  }) 

#questions
  output$dt.questions <- DT::renderDataTable({
    main %>%
      mutate(score = qa$score) %>%
      select(question, answer, score)
  }) 
})

