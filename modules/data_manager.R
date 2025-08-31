# =============================
# DATA MANAGER MODULE
# Handles data file operations: reading, writing, and initialization
# =============================

library(dplyr)
source("constants.R")

# =============================
# Core File Operations
# =============================
data_read_score <- function(qa_data, path = "data/score.csv") {
  if (!file.exists(path)) {
    score <- data.frame(guest = rep(0L, nrow(qa_data)))
    write.table(score, path, row.names = FALSE, sep = ",")
    return(list(success = TRUE, data = score, message = "Score file created as it didn't exist"))
  }

  tryCatch({
    score <- read.csv(path, comment = "#", stringsAsFactors = FALSE)
  }, error = function(e) {
    return(list(success = FALSE, data = NULL, message = paste("Error reading score file:", e$message)))
  })

  if (nrow(qa_data) > nrow(score)) {
    missing_rows <- nrow(qa_data) - nrow(score)
    padding_data <- list()
    for (col_name in names(score)) {
      padding_data[[col_name]] <- rep(0L, missing_rows)
    }

    padding_rows <- data.frame(padding_data)
    score <- bind_rows(score, padding_rows)
  }

  return(list(success = TRUE, data = score, message = "Score data loaded successfully"))
}

# ==============================
# Application Initialization
# ==============================

data_initialize <- function() {
  tryCatch({
    qa_data <- read.csv(DATA$PATHS$QUESTIONS, comment = "#", stringsAsFactors = FALSE) %>%
      filter(question != "", answer != "")

    if (nrow(qa_data) == 0) {
      return(list(success = FALSE, data = NULL, message = "No valid questions found in qlist.csv"))
    }

    score_result <- data_read_score(qa_data = qa_data, path = DATA$PATHS$SCORES)
    if (!score_result$success) {
      return(list(success = FALSE, data = NULL, message = paste("Failed to initialize:", score_result$message)))
    }
    score_global <- score_result$data %>%
      mutate(guest = 0L) %>%
      select(guest, everything())

    if (nrow(qa_data) != nrow(score_global)) {
      warning("Question and score data length mismatch. This has been automatically corrected.")
    }

    return(list(success = TRUE, data = list(qa_data = qa_data, score_global = score_global), message = "Data initialized successfully"))
  }, error = function(e) {
    return(list(success = FALSE, data = NULL, message = paste("Failed to initialize data:", e$message)))
  })
}

# ===============================
# Data Persistence Operations
# ===============================

data_save_user_score <- function(username, current_user, user_scores, qa_data) {
  if (username != current_user) {
    return(list(
      success = FALSE,
      data = NULL,
      message = "You switched user account."
    ))
  }

  tryCatch({
    score_result <- data_read_score(qa_data = qa_data, path = DATA$PATHS$SCORES)
    if (!score_result$success) {
      return(list(success = FALSE, data = NULL, message = score_result$message))
    }
    existing_scores <- score_result$data
    existing_scores[[username]] <- user_scores
    write.table(existing_scores, DATA$PATHS$SCORES, row.names = FALSE, sep = ",")

    updated_scores <- existing_scores %>%
      mutate(guest = 0L) %>%
      select(guest, everything())

    return(list(
      success = TRUE,
      data = updated_scores,
      message = paste("Score saved for user:", username)
    ))
  }, error = function(e) {
    return(list(
      success = FALSE,
      data = NULL,
      message = paste("Error saving score:", e$message)
    ))
  })
}
