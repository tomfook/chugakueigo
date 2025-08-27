library(dplyr)
source("constants.R")

data_read_score <- function(qa, path = "data/score.csv") {
  if (!file.exists(path)) {
    score <- data.frame(guest = rep(0L, nrow(qa)))
    write.table(score, path, row.names = FALSE, sep = ",")
    return(list(success = TRUE, data = score, message = "Score file created as it didn't exist"))
  }

  tryCatch({
    score <- read.csv(path, comment = "#", stringsAsFactors = FALSE)
  }, error = function(e) {
    return(list(success = FALSE, data = NULL, message = paste("Error reading score file:", e$message)))
  })

  if (nrow(qa) > nrow(score)) {
    zero.score <- head(score, nrow(qa) - nrow(score))
    zero.score[] <- 0L
    score <- bind_rows(score, zero.score)
  }

  return(list(success = TRUE, data = score, message = "Score data loaded successfully"))
}

data_initialize <- function() {
  tryCatch({
    main <- read.csv(PATHS$QUESTIONS, comment = "#", stringsAsFactors = FALSE) %>%
      filter(question != "", answer != "")

    if (nrow(main) == 0) {
      return(list(success = FALSE, data = NULL, message = "No valid questions found in qlist.csv"))
    }

    score_result <- data_read_score(qa = main, path = PATHS$SCORES)
    if (!score_result$success) {
      return(list(success = FALSE, data = NULL, message = paste("Failed to initialize:", score_result$message)))
    }
    score_global <- score_result$data %>%
      mutate(guest = 0L) %>%
      select(guest, everything())

    if (nrow(main) != nrow(score_global)) {
      warning("Question and score data length mismatch. This has been automatically corrected.")
    }

    return(list(success = TRUE, data = list(main = main, score_global = score_global), message = "Data initialized successfully"))
  }, error = function(e) {
    return(list(success = FALSE, data = NULL, message = paste("Failed to initialize data:", e$message)))
  })
}

data_save_user_score <- function(username, current_user, user_scores, qa_data) {
  if (username != current_user) {
    return(list(
      success = FALSE,
      updated_scores = NULL,
      message = "You switched user account."
    ))
  }

  tryCatch({
    score_result <- data_read_score(qa = qa_data, path = PATHS$SCORES)
    if (!score_result$success) {
      return(list(success = FALSE, updated_scores = NULL, message = score_result$message))
    }
    score_all <- score_result$data
    score_all[[username]] <- user_scores
    write.table(score_all, PATHS$SCORES, row.names = FALSE, sep = ",")

    updated_scores <- score_all %>%
      mutate(guest = 0L) %>%
      select(guest, everything())

    return(list(
      success = TRUE,
      updated_scores = updated_scores,
      message = paste("Score saved for user:", username)
    ))
  }, error = function(e) {
    return(list(
      success = FALSE,
      updated_scores = NULL,
      message = paste("Error saving score:", e$message)
    ))
  })
}
