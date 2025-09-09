# =============================
# DATA MANAGER MODULE
# Handles data file operations: reading, writing, and initialization
# =============================

library(dplyr)
library(googlesheets4)
source("constants.R")
gs4_auth(path = DATA$PATHS$SERVICE_ACCOUNT_KEY)


# =============================
# Core File Operations
# =============================
data_read_score <- function(qa_count) {
  tryCatch({
    # Google Sheetsから読み込み
    score <- read_sheet(DATA$SHEETS$SCORES, col_types = "c")

    numeric_cols <- names(score)
    for(col in numeric_cols) {
      score[[col]] <- as.integer(score[[col]])
    }

    if (qa_count > nrow(score)) {
      missing_rows <- qa_count - nrow(score)
      padding_data <- list()
      for (col_name in names(score)) {
        padding_data[[col_name]] <- rep(0L, missing_rows)
      }
      padding_rows <- data.frame(padding_data)
      score <- bind_rows(score, padding_rows)
    }

    return(list(success = TRUE, data = score, message = "Score data loaded successfully"))
    }, error = function(e) {
      return(list(success = FALSE, data = NULL, message = paste("Error reading from Google Sheets:", e$message)))
  })
}

# ==============================
# Application Initialization
# ==============================

data_initialize <- function() {
  tryCatch({
    qa_data <- read.csv(DATA$PATHS$QUESTIONS, comment = "#", stringsAsFactors = FALSE) %>%
      filter(question != "", answer != "")
    qa_count <- nrow(qa_data)

    if (qa_count == 0) {
      return(list(success = FALSE, data = NULL, message = "No valid questions found in qlist.csv"))
    }

    score_result <- data_read_score(qa_count = qa_count)
    if (!score_result$success) {
      return(list(success = FALSE, data = NULL, message = paste("Failed to initialize:", score_result$message)))
    }
    score_global <- score_result$data %>%
      mutate(guest = 0L) %>%
      select(guest, everything())

    if (qa_count != nrow(score_global)) {
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

data_save_user_score <- function(username, current_user, user_scores, qa_count) {
  if (username != current_user) {
    return(list(
      success = FALSE,
      data = NULL,
      message = "You switched user account."
    ))
  }

  tryCatch({
    score_result <- data_read_score(qa_count = qa_count)
    if (!score_result$success) {
      return(list(success = FALSE, data = NULL, message = score_result$message))
    }
    existing_scores <- score_result$data
    existing_scores[[username]] <- user_scores

    sheet_write(existing_scores, ss = DATA$SHEETS$SCORES, sheet = "Sheet1")

    updated_scores <- existing_scores %>%
      mutate(guest = 0L) %>%
      select(guest, everything())

    return(list(
      success = TRUE,
      data = updated_scores,
      message = paste("Score saved to Google Sheets for user:", username)
    ))
  }, error = function(e) {
    return(list(
      success = FALSE,
      data = NULL,
      message = paste("Error saving to Google Sheets:", e$message)
    ))
  })
}
