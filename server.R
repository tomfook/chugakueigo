library(shiny) 
library(dplyr)

main <- read.csv("data/qlist.csv", comment = "#", stringsAsFactor = FALSE)

read.score <- function(qa, path = "data/score.csv"){
  score <- read.csv(path, comment = "#", stringsAsFactor = FALSE)
  if(nrow(qa) > nrow(score)){
    zero.score <- head(score, nrow(qa) - nrow(score))
    zero.score[] <- 0L
    score <- bind_rows(score, zero.score)
  }
  return(score)
} 

shinyServer(function(input, output, session){ 
#external data
  main <- read.csv("data/qlist.csv", comment = "#", stringsAsFactor = FALSE) %>%
    filter(question != "", answer != "")

  score.global <- read.score(qa = main, path = "data/score.csv") %>%
    mutate(guest = 0L) %>%
    select(guest, everything())

  qa <- reactiveValues() 
  qa$start <- FALSE
  qa$score <- rep(0L, nrow(main))
  qa$namelist <- names(score.global)
  qa$score.all <- score.global

#render UI
  output$html.slider.qrange <- renderUI({ 
    sliderInput("slider.qrange", label = h4("Range of Question"), min = 1L, max = nrow(main), value = c(0,nrow(main)))
  }) 
  output$html.action.start <- renderUI({ 
    actionButton("action.start", label = "Start Learning",
      style = if_else(qa$start, 
	 "color:gray;",
         "color:black;"
      ) 
)
  })
  output$html.action.save <- renderUI({ 
    actionButton("action.save", label = "Save Score",
      style = if_else(identical(qa$score.all[[input$select.user]], qa$score),
        "color:gray;",
	"color:black;"
	) 
      )
  })

# user selection update
  observe({
    updateSelectInput(session, "select.user", choices = qa$namelist)
  })

# useradd
  observeEvent(input$action.useradd,{
    if(!(input$textinp.useradd %in% qa$namelist)){
      qa$namelist <- c(qa$namelist, input$textinp.useradd)
      qa$score.all[[input$textinp.useradd]] <- rep(0L, nrow(main))
      showNotification(paste("User", input$textinp.useradd, "was added."))
      updateTextInput(session, "textinp.useradd", value = "")
    }else{
      showNotification("Your name has been already resistered.")
    }
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
    qa$range.min <- if_else(is.null(input$slider.qrange[1]), 1L, input$slider.qrange[1])
    qa$range.max <- if_else(is.null(input$slider.qrange[2]), nrow(main), input$slider.qrange[2]) 
    qa$prob <- (abs(input$prob.base - qa$ok * 0.005 - 1) + 1)^(-qa$score[seq(qa$range.min, qa$range.max)]) *
      (cumsum(qa$score[seq(qa$range.min, qa$range.max)] == 0L) <= input$zeronum)
  })
  newQuestion <- function(){
    a <- sample(
      qa$range.max - qa$range.min + 1L,
      1,
      prob = qa$prob
    ) + (qa$range.min - 1L)
    qa$index <- a
    qa$question <- main$question[a]
    qa$answer.remember <- main$answer[a]
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
      qa$score.all <- read.score(qa = main, path = "data/score.csv")
      qa$score.all[[input$select.user]] <- qa$score
      write.table(qa$score.all, "data/score.csv", row.names=FALSE, sep = ",")
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

